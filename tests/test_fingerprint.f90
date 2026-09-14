! Fuzzy fingerprints of the test images, and the check against the stored ones.
!
! Different compilers do not produce bit-identical images: the same coverage
! computed with and without fused multiply-add rounds to a different 8-bit
! value on a few thousand edge pixels. A byte hash would fail on that, and
! storing every reference image would put megabytes in the repository. So
! each image is reduced to 64 words of 64 bits (4096 bits), stored in
! tests/fingerprints.txt, and compared by counting differing bits within each
! word. A word is one tile of the image, so a small change stays concentrated
! in its tile instead of being diluted by the whole picture.
!
! How the words are made (all integer arithmetic, so every compiler computes
! the same bits):
!
!   1. ink = 255 - value in each of R, G, B, so a white background is zero.
!   2. The ink is spread bilinearly onto a NCX x NCY grid of cells, in units
!      of 255*4*w*h per pixel ("full-ink pixels").
!   3. A discrete Laplacian of the cell grid. A large area shaded one level
!      darker changes the cells but not the differences between neighbours;
!      a tick, a glyph or a line edge changes both.
!   4. The grid is cut into TX x TY tiles. Bit j of a tile's word takes a
!      fixed random sparse +-1 combination S of the tile's cells and keeps
!      the parity of floor((S + offset)/PERIOD). A change of size d in S
!      flips the bit with probability about d/PERIOD, so noise flips few bits
!      and a real change flips about half the word.
!
! Measured on this suite: gfortran with and without -O3 differ in 105 images
! by one intensity level on up to 21,000 pixels, which moves no word by more
! than 6 bits. A tick moved by 1 px, a missing label, a one letter typo in a
! title or a changed line colour move a word by 18 to 40 bits. Two unrelated
! words differ in 32 bits on average. A tick moved by 0.25 px or less passes.

module test_fingerprint
    use, intrinsic :: iso_fortran_env, only: int64
    use test_image_io, only: read_png_rgb, read_gif_frames
    implicit none
    private

    public :: fp_check_png, fp_check_gif, fp_finish

    ! FPHash-4096, version 1. Changing any of these invalidates every stored
    ! fingerprint, which then have to be regenerated with --update.
    integer, parameter :: NCX = 160, NCY = 128
    integer, parameter :: TX = 8, TY = 8
    integer, parameter :: NWORDS = TX*TY
    integer, parameter :: CW = NCX/TX, CH = NCY/TY
    integer, parameter :: NFEAT = 3*CW*CH
    integer, parameter :: PERIOD = 16
    integer, parameter :: DENSITY = 4
    integer, parameter :: SALT = 12345

    ! The most bits any one word may differ by before the case fails.
    integer, parameter :: TOLERANCE = 10

    integer(int64), parameter :: MPRIME = 2147483647_int64
    character(len=16), parameter :: HEXDIG = "0123456789abcdef"

    ! The random combinations do not depend on the image, so they are drawn
    ! once: for tile t and bit j, entries proj_start(t,j) .. proj_start(t,j+1)-1
    ! of proj hold +-(k+1) for tile cell k, and proj_off(t,j) the raw offset.
    integer, allocatable :: proj(:), proj_start(:, :)
    integer(int64), allocatable :: proj_off(:, :)

    type :: result_t
        character(len=64) :: name = ""
        integer :: w = 0, h = 0
        integer(int64) :: words(NWORDS) = 0
    end type result_t

    type(result_t), allocatable :: got(:)
    integer :: ngot = 0

contains

    ! ------------------------------------------------------------------
    ! The fingerprint
    ! ------------------------------------------------------------------

    pure integer(int64) function lcg(x)
        integer(int64), intent(in) :: x
        lcg = modulo(48271_int64*x, MPRIME)
    end function lcg

    subroutine init_projections()
        integer :: pass, t, j, k, n, r
        integer(int64) :: seed

        if (allocated(proj)) return
        allocate (proj_start(0:NWORDS - 1, 0:64), proj_off(0:NWORDS - 1, 0:63))
        do pass = 1, 2
            n = 0
            do t = 0, NWORDS - 1
                do j = 0, 63
                    proj_start(t, j) = n + 1
                    seed = 1 + modulo(int(t, int64)*1000003_int64 + j*7919_int64 + SALT, MPRIME - 1)
                    seed = lcg(lcg(seed))
                    do k = 0, NFEAT - 1
                        seed = lcg(seed)
                        r = int(modulo(seed, int(2*DENSITY, int64)))
                        if (r > 1) cycle
                        n = n + 1
                        if (pass == 2) then
                            if (r == 0) then
                                proj(n) = k + 1
                            else
                                proj(n) = -(k + 1)
                            end if
                        end if
                    end do
                    seed = lcg(seed)
                    proj_off(t, j) = seed*65536_int64 + lcg(seed)
                end do
                proj_start(t, 64) = n + 1
            end do
            if (pass == 1) allocate (proj(n))
        end do
    end subroutine init_projections

    ! Where pixel k (0-based, of n) lands along one axis of nc cells: two
    ! cells and their weights, which sum to 2n.
    subroutine axis_weights(k, n, nc, idx, wt)
        integer, intent(in) :: k, n, nc
        integer, intent(out) :: idx(2)
        integer(int64), intent(out) :: wt(2)
        integer(int64) :: u, fr, two_n

        two_n = 2_int64*n
        u = (2_int64*k + 1)*nc - n
        fr = modulo(u, two_n)
        idx(1) = int((u - fr)/two_n)
        idx(2) = idx(1) + 1
        wt(1) = two_n - fr
        wt(2) = fr
        idx = max(0, min(nc - 1, idx))
    end subroutine axis_weights

    subroutine fingerprint(rgb, words)
        integer, intent(in) :: rgb(:, :, :)
        integer(int64), intent(out) :: words(NWORDS)
        integer(int64), allocatable :: f(:, :, :), g(:, :, :)
        integer(int64) :: wx(2), wy(2), wyj, s, per, q
        integer :: w, h, x, y, c, ink, ix(2), iy(2), i, j, im, ip, jm, jp
        integer :: t, ti, tj, b, e, kk, cell

        call init_projections()
        w = size(rgb, 2)
        h = size(rgb, 3)

        allocate (f(3, 0:NCX - 1, 0:NCY - 1))
        f = 0
        do y = 0, h - 1
            call axis_weights(y, h, NCY, iy, wy)
            do x = 0, w - 1
                call axis_weights(x, w, NCX, ix, wx)
                do c = 1, 3
                    ink = 255 - rgb(c, x + 1, y + 1)
                    if (ink == 0) cycle
                    do j = 1, 2
                        wyj = wy(j)*ink
                        do i = 1, 2
                            f(c, ix(i), iy(j)) = f(c, ix(i), iy(j)) + wx(i)*wyj
                        end do
                    end do
                end do
            end do
        end do

        allocate (g(3, 0:NCX - 1, 0:NCY - 1))
        do j = 0, NCY - 1
            jm = max(0, j - 1)
            jp = min(NCY - 1, j + 1)
            do i = 0, NCX - 1
                im = max(0, i - 1)
                ip = min(NCX - 1, i + 1)
                g(:, i, j) = 4*f(:, i, j) - f(:, im, j) - f(:, ip, j) - f(:, i, jm) - f(:, i, jp)
            end do
        end do

        per = int(PERIOD, int64)*255_int64*4_int64*int(w, int64)*int(h, int64)
        words = 0
        do tj = 0, TY - 1
            do ti = 0, TX - 1
                t = tj*TX + ti
                do b = 0, 63
                    s = 0
                    do e = proj_start(t, b), proj_start(t, b + 1) - 1
                        kk = abs(proj(e)) - 1
                        ! cell kk of the tile, in the order rows, columns, channels
                        c = mod(kk, 3) + 1
                        cell = kk/3
                        if (proj(e) > 0) then
                            s = s + g(c, ti*CW + mod(cell, CW), tj*CH + cell/CW)
                        else
                            s = s - g(c, ti*CW + mod(cell, CW), tj*CH + cell/CW)
                        end if
                    end do
                    q = s + modulo(proj_off(t, b), per)
                    q = (q - modulo(q, per))/per
                    if (modulo(q, 2_int64) == 1) words(t + 1) = ibset(words(t + 1), b)
                end do
            end do
        end do
    end subroutine fingerprint

    ! ------------------------------------------------------------------
    ! Recording and checking
    ! ------------------------------------------------------------------

    subroutine record(name, rgb)
        character(len=*), intent(in) :: name
        integer, intent(in) :: rgb(:, :, :)
        type(result_t), allocatable :: tmp(:)

        if (.not. allocated(got)) allocate (got(256))
        if (ngot == size(got)) then
            allocate (tmp(2*size(got)))
            tmp(1:ngot) = got(1:ngot)
            call move_alloc(tmp, got)
        end if
        ngot = ngot + 1
        got(ngot)%name = name
        got(ngot)%w = size(rgb, 2)
        got(ngot)%h = size(rgb, 3)
        call fingerprint(rgb, got(ngot)%words)
    end subroutine record

    subroutine fp_check_png(name, path)
        character(len=*), intent(in) :: name, path
        integer, allocatable :: rgb(:, :, :)

        call read_png_rgb(path, rgb)
        call record(name, rgb)
    end subroutine fp_check_png

    ! Every frame is fingerprinted on its own, as name_f01, name_f02, ...
    subroutine fp_check_gif(name, path)
        character(len=*), intent(in) :: name, path
        integer, allocatable :: frames(:, :, :, :)
        character(len=8) :: suffix
        integer :: k

        call read_gif_frames(path, frames)
        do k = 1, size(frames, 4)
            write (suffix, "('_f',i2.2)") k
            call record(name//trim(suffix), frames(:, :, :, k))
        end do
    end subroutine fp_check_gif

    function to_hex(words) result(s)
        integer(int64), intent(in) :: words(NWORDS)
        character(len=16*NWORDS) :: s
        integer :: k, d, nib

        do k = 1, NWORDS
            do d = 0, 15
                nib = int(iand(ishft(words(k), -4*(15 - d)), 15_int64))
                s((k - 1)*16 + d + 1:(k - 1)*16 + d + 1) = HEXDIG(nib + 1:nib + 1)
            end do
        end do
    end function to_hex

    subroutine from_hex(s, words, ok)
        character(len=*), intent(in) :: s
        integer(int64), intent(out) :: words(NWORDS)
        logical, intent(out) :: ok
        integer :: k, d, nib

        words = 0
        ok = len_trim(s) == 16*NWORDS
        if (.not. ok) return
        do k = 1, NWORDS
            do d = 0, 15
                nib = index(HEXDIG, s((k - 1)*16 + d + 1:(k - 1)*16 + d + 1)) - 1
                if (nib < 0) then
                    ok = .false.
                    return
                end if
                words(k) = ior(ishft(words(k), 4), int(nib, int64))
            end do
        end do
    end subroutine from_hex

    logical function update_requested()
        character(len=64) :: arg
        integer :: i

        update_requested = .false.
        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            if (trim(arg) == "--update") update_requested = .true.
        end do
    end function update_requested

    ! Compare everything recorded against the stored fingerprints in path, or
    ! with --update on the command line, replace them. Stops with a nonzero
    ! exit code when any case is missing, has changed size, or differs by more
    ! than TOLERANCE bits in some word.
    subroutine fp_finish(path)
        character(len=*), intent(in) :: path
        character(len=64), allocatable :: rname(:)
        integer, allocatable :: rw(:), rh(:)
        integer(int64), allocatable :: rwords(:, :)
        logical, allocatable :: seen(:)
        character(len=16*NWORDS + 200) :: line
        character(len=64) :: nm
        character(len=16*NWORDS) :: hex
        integer :: u, ios, nref, i, k, r, d, dmax, kmax, nfail, worst, wv, hv
        logical :: ok, exists
        character(len=64) :: worst_name

        if (update_requested()) then
            open (newunit=u, file=path, status="replace", action="write")
            write (u, "(a)") "# fplot test fingerprints, FPHash-4096 v1 (tests/test_fingerprint.f90)."
            write (u, "(a)") "# Regenerate after an intended change of the pictures: fpm test -- --update"
            write (u, "(a)") "# name width height fingerprint"
            do i = 1, ngot
                write (u, "(a,1x,i0,1x,i0,1x,a)") trim(got(i)%name), got(i)%w, got(i)%h, &
                    to_hex(got(i)%words)
            end do
            close (u)
            print "(a,i0,a)", "fingerprints: wrote ", ngot, " to "//path
            return
        end if

        inquire (file=path, exist=exists)
        if (.not. exists) then
            print "(a)", "fingerprints: "//path//" not found; create it with --update"
            error stop 1
        end if

        allocate (rname(ngot + 64), rw(ngot + 64), rh(ngot + 64), rwords(NWORDS, ngot + 64))
        nref = 0
        open (newunit=u, file=path, status="old", action="read")
        do
            read (u, "(a)", iostat=ios) line
            if (ios /= 0) exit
            if (len_trim(line) == 0 .or. line(1:1) == "#") cycle
            if (nref == size(rname)) error stop "fingerprints: more stored cases than expected"
            read (line, *, iostat=ios) nm, wv, hv, hex
            call from_hex(hex, rwords(:, nref + 1), ok)
            if (ios /= 0 .or. .not. ok) then
                print "(a)", "fingerprints: cannot read line: "//trim(line(1:80))
                error stop 1
            end if
            nref = nref + 1
            rname(nref) = nm
            rw(nref) = wv
            rh(nref) = hv
        end do
        close (u)

        allocate (seen(nref))
        seen = .false.
        nfail = 0
        worst = 0
        worst_name = "-"
        do i = 1, ngot
            r = 0
            do k = 1, nref
                if (rname(k) == got(i)%name) then
                    r = k
                    exit
                end if
            end do
            if (r == 0) then
                print "(a)", "  FAIL "//trim(got(i)%name)//": no stored fingerprint"
                nfail = nfail + 1
                cycle
            end if
            seen(r) = .true.
            if (rw(r) /= got(i)%w .or. rh(r) /= got(i)%h) then
                print "(a,i0,a,i0,a,i0,a,i0)", "  FAIL "//trim(got(i)%name)//": size ", &
                    got(i)%w, "x", got(i)%h, ", stored ", rw(r), "x", rh(r)
                nfail = nfail + 1
                cycle
            end if
            dmax = 0
            kmax = 1
            do k = 1, NWORDS
                d = popcnt(ieor(got(i)%words(k), rwords(k, r)))
                if (d > dmax) then
                    dmax = d
                    kmax = k
                end if
            end do
            if (dmax > worst) then
                worst = dmax
                worst_name = got(i)%name
            end if
            if (dmax > TOLERANCE) then
                print "(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a)", "  FAIL "//trim(got(i)%name)// &
                    ": ", dmax, " bits differ near x=", (mod(kmax - 1, TX)*got(i)%w)/TX, &
                    "..", ((mod(kmax - 1, TX) + 1)*got(i)%w)/TX, ", y=", &
                    (((kmax - 1)/TX)*got(i)%h)/TY, "..", ((((kmax - 1)/TX) + 1)*got(i)%h)/TY, &
                    " (limit ", TOLERANCE, ")"
                nfail = nfail + 1
            end if
        end do
        do k = 1, nref
            if (.not. seen(k)) then
                print "(a)", "  FAIL "//trim(rname(k))//": stored but not produced"
                nfail = nfail + 1
            end if
        end do

        print "(a,i0,a,i0,a,i0,a)", "fingerprints: ", ngot, " images, largest difference ", &
            worst, " bits (", TOLERANCE, " allowed) in "//trim(worst_name)
        if (nfail > 0) then
            print "(a,i0,a)", "fingerprints: ", nfail, " failed; if the change is intended, run with --update"
            error stop 1
        end if
        print "(a)", "fingerprints: OK"
    end subroutine fp_finish

end module test_fingerprint
