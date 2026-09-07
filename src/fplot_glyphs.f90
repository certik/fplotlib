! fplot_glyphs — DejaVu Sans outlines for the backends that draw text
! themselves.
!
! A raster file cannot defer to a font the way SVG and PDF do, so the PNG
! backend fills text as outlines and the outlines have to be part of the
! library. They are the ones matplotlib itself draws with.
!
! What is compiled in is what the font stores: quadratic contours in
! integer font units, packed into one base85 string in fplot_glyphs_data
! (written by tools/gen_glyphs.py) and unpacked here the first time a
! glyph is asked for. Keeping the font's own representation rather than
! the cubics the rendering API wants is what makes the data 40 KB of
! source instead of 528 KB.
!
! The bit stream, most significant bit first:
!
!   * the number of outlines, Elias-gamma coded: the length in zeros,
!     then the number itself, whose leading one is that terminating bit;
!   * per outline, its number of contours and each contour's number of
!     points, all gamma coded, and then for every point in turn one bit
!     saying whether it lies on the curve and its x and y as deltas from
!     the point before it (the first is a delta from the origin);
!   * one bit per code point in the table saying whether the font has
!     anything for it, the four faces covering the same ones, and then
!     for each of those code points in each face an advance width and
!     the number of the outline it draws. The width is 12 raw bits in
!     the first face and a delta elsewhere, guessed from the same
!     character in the face two back — the faces run regular, bold,
!     oblique, bold oblique, so that is the upright the slanted one is a
!     copy of, and the guess is usually exact. The outline number is one
!     bit when it is the next one not yet used, which it usually is
!     since outlines are numbered in the order they are first drawn, and
!     otherwise that bit and 10 more, 0 meaning nothing to draw.
!     Outlines that repeat between faces are stored once and pointed at
!     twice.
!
! A delta is folded to a non-negative number, small magnitudes staying
! small, and then written as the position of its leading one — a
! magnitude class, spelled with the fixed prefix code in CLEN — followed
! by the bits below that one. This is what deflate does with distances.
!
! Coordinates are in font units; divide by EM and multiply by the point
! size. glyph_verbs and glyph_points hand back cubics, matching
! fplot_render's path verbs, so the backend fills a glyph with exactly
! the code that fills any other path: a quadratic is elevated exactly,
! its control points landing at p0 + 2/3 (q - p0) and p2 + 2/3 (q - p2),
! and an on-curve point that TrueType leaves implied between two
! off-curve ones is put back at their midpoint.
!
! Four faces are stacked in one table: regular, bold, oblique and bold
! oblique, selected by the face argument.
module fplot_glyphs
    use iso_fortran_env, only: int16, int64
    use fplot_style, only: dp
    use fplot_render, only: VERB_MOVE, VERB_LINE, VERB_CUBIC, VERB_CLOSE
    use fplot_glyphs_data, only: EM, ASCENT, DESCENT, XHEIGHT, &
                                 ALPHABET, BLOB, &
                                 NCH, NFACE, NSLOT, NOUT, NCONT, NPT, &
                                 NBLK, BLK_FIRST, BLK_LAST, BLK_OFF
    implicit none
    private

    public :: EM, ASCENT, DESCENT, XHEIGHT
    public :: FACE_REGULAR, FACE_BOLD, FACE_OBLIQUE, FACE_BOLD_OBLIQUE
    public :: glyph_advance, glyph_verbs, glyph_points

    integer, parameter :: FACE_REGULAR = 1
    integer, parameter :: FACE_BOLD = 2
    integer, parameter :: FACE_OBLIQUE = 3
    integer, parameter :: FACE_BOLD_OBLIQUE = 4

    ! Bits per slot record, and the code lengths of the fourteen
    ! magnitude classes. tools/gen_glyphs.py writes the blob with the
    ! same three numbers and the same fourteen lengths.
    integer, parameter :: ADV_BITS = 12
    integer, parameter :: OUT_BITS = 10
    integer, parameter :: NCLASS = 14
    integer, parameter :: CLEN(0:NCLASS - 1) = &
        [2, 10, 9, 8, 7, 6, 5, 4, 3, 2, 3, 4, 4, 10]
    integer, parameter :: MAXLEN = 10

    ! The unpacked table. Module variables are saved, so the work is
    ! done once however many figures are drawn.
    logical :: loaded = .false.
    integer(int16) :: QX(NPT), QY(NPT)      ! points, in font units
    logical :: QON(NPT)                     ! on the curve, or a control point
    integer :: CBEG(NCONT + 1)              ! first point of each contour
    integer :: OBEG(NOUT + 1)               ! first contour of each outline
    integer :: SADV(NSLOT), SOUT(NSLOT)     ! advance width, outline number

    ! The canonical prefix code for the magnitude classes, built from
    ! CLEN: how many codes have each length, the smallest of them, and
    ! the symbols in the order the codes run.
    integer :: NLEN(MAXLEN), FIRST(MAXLEN), CBASE(MAXLEN), CSYM(NCLASS)

    ! The bit stream while it is being read.
    integer, allocatable :: BUF(:)
    integer :: BPOS

contains

    ! --- the API -----------------------------------------------------

    ! Advance width in font units.
    function glyph_advance(c, face) result(a)
        integer, intent(in) :: c
        integer, intent(in), optional :: face
        real(dp) :: a
        call ensure_loaded()
        a = real(SADV(slot(c, face)), dp)
    end function glyph_advance

    ! The verbs for one character, empty when there is nothing to draw.
    subroutine glyph_verbs(c, v, n, face)
        integer, intent(in) :: c
        integer, allocatable, intent(out) :: v(:)
        integer, intent(out) :: n
        integer, intent(in), optional :: face
        real(dp), allocatable :: x(:), y(:)
        integer :: np
        call outline_of(slot(c, face), v, n, x, y, np)
    end subroutine glyph_verbs

    ! The points for one character, in font units.
    subroutine glyph_points(c, x, y, n, face)
        integer, intent(in) :: c
        real(dp), allocatable, intent(out) :: x(:), y(:)
        integer, intent(out) :: n
        integer, intent(in), optional :: face
        integer, allocatable :: v(:)
        integer :: nv
        call outline_of(slot(c, face), v, nv, x, y, n)
    end subroutine glyph_points

    ! Index into the flat tables. Code points outside the table are
    ! treated as spaces so that an unexpected one cannot crash a plot,
    ! and an unknown face falls back on the regular one.
    pure integer function slot(c, face)
        integer, intent(in) :: c
        integer, intent(in), optional :: face
        integer :: f, i, b
        f = FACE_REGULAR
        if (present(face)) f = face
        if (f < 1 .or. f > NFACE) f = FACE_REGULAR
        i = 1
        do b = 1, NBLK
            if (c >= BLK_FIRST(b) .and. c <= BLK_LAST(b)) then
                i = c - BLK_FIRST(b) + BLK_OFF(b)
            end if
        end do
        slot = (f - 1)*NCH + i
    end function slot

    ! --- quadratics to the path the backends fill --------------------

    ! One slot as verbs and points. Both are built together because the
    ! walk that produces one produces the other, and the caller asks for
    ! them a character at a time.
    subroutine outline_of(i, v, nv, x, y, np)
        integer, intent(in) :: i
        integer, allocatable, intent(out) :: v(:)
        integer, intent(out) :: nv
        real(dp), allocatable, intent(out) :: x(:), y(:)
        integer, intent(out) :: np
        integer, allocatable :: tv(:)
        real(dp), allocatable :: tx(:), ty(:)
        integer :: o, ci, nc, npts, j

        call ensure_loaded()
        nv = 0
        np = 0
        o = SOUT(i)
        if (o == 0) then
            allocate (v(0), x(0), y(0))
            return
        end if

        ! A contour of n points comes to at most n segments, so at most
        ! n + 2 verbs and 1 + 3n points once the cubics are written out.
        nc = OBEG(o + 1) - OBEG(o)
        npts = CBEG(OBEG(o + 1)) - CBEG(OBEG(o))
        allocate (tv(npts + 2*nc), tx(3*npts + nc), ty(3*npts + nc))

        do j = 0, nc - 1
            ci = OBEG(o) + j
            call add_contour(CBEG(ci), CBEG(ci + 1) - CBEG(ci), tv, nv, tx, ty, np)
        end do

        allocate (v(nv), x(np), y(np))
        v = tv(1:nv)
        x = tx(1:np)
        y = ty(1:np)
    end subroutine outline_of

    ! One contour appended to the path being built. TrueType lets a
    ! contour start anywhere, so it is turned to start at a point on the
    ! curve; if it has none, the start is implied halfway between the
    ! last point and the first.
    subroutine add_contour(b, n, v, nv, x, y, np)
        integer, intent(in) :: b, n
        integer, intent(inout) :: v(:), nv
        real(dp), intent(inout) :: x(:), y(:)
        integer, intent(inout) :: np
        real(dp) :: sx(n + 2), sy(n + 2)
        logical :: son(n + 2)
        integer :: first, j, k, m, jn
        real(dp) :: cx, cy, ex, ey

        first = 0
        do j = 1, n
            if (QON(b + j - 1)) then
                first = j
                exit
            end if
        end do

        if (first == 0) then
            sx(1) = 0.5_dp*(real(QX(b), dp) + real(QX(b + n - 1), dp))
            sy(1) = 0.5_dp*(real(QY(b), dp) + real(QY(b + n - 1), dp))
            do j = 1, n
                sx(j + 1) = real(QX(b + j - 1), dp)
                sy(j + 1) = real(QY(b + j - 1), dp)
                son(j + 1) = .false.
            end do
            m = n + 1
        else
            sx(1) = real(QX(b + first - 1), dp)
            sy(1) = real(QY(b + first - 1), dp)
            do j = 1, n - 1
                k = b + mod(first - 1 + j, n)
                sx(j + 1) = real(QX(k), dp)
                sy(j + 1) = real(QY(k), dp)
                son(j + 1) = QON(k)
            end do
            m = n
        end if
        ! The start comes round again as the point the contour ends on.
        son(1) = .true.
        sx(m + 1) = sx(1)
        sy(m + 1) = sy(1)
        son(m + 1) = .true.

        nv = nv + 1
        v(nv) = VERB_MOVE
        np = np + 1
        x(np) = sx(1)
        y(np) = sy(1)
        cx = sx(1)
        cy = sy(1)

        j = 2
        do while (j <= m + 1)
            if (son(j)) then
                ! A line back to the start is what closing the contour
                ! already says, so the last one is left out.
                if (j < m + 1) then
                    nv = nv + 1
                    v(nv) = VERB_LINE
                    np = np + 1
                    x(np) = sx(j)
                    y(np) = sy(j)
                end if
                cx = sx(j)
                cy = sy(j)
                j = j + 1
            else
                ! Two control points in a row have the point they share
                ! left out of the font; it sits at their midpoint.
                if (son(j + 1)) then
                    ex = sx(j + 1)
                    ey = sy(j + 1)
                    jn = j + 2
                else
                    ex = 0.5_dp*(sx(j) + sx(j + 1))
                    ey = 0.5_dp*(sy(j) + sy(j + 1))
                    jn = j + 1
                end if
                nv = nv + 1
                v(nv) = VERB_CUBIC
                x(np + 1) = cx + (2.0_dp/3.0_dp)*(sx(j) - cx)
                y(np + 1) = cy + (2.0_dp/3.0_dp)*(sy(j) - cy)
                x(np + 2) = ex + (2.0_dp/3.0_dp)*(sx(j) - ex)
                y(np + 2) = ey + (2.0_dp/3.0_dp)*(sy(j) - ey)
                x(np + 3) = ex
                y(np + 3) = ey
                np = np + 3
                cx = ex
                cy = ey
                j = jn
            end if
        end do

        nv = nv + 1
        v(nv) = VERB_CLOSE
    end subroutine add_contour

    ! --- unpacking the blob ------------------------------------------

    subroutine ensure_loaded()
        if (loaded) return
        call unpack()
        loaded = .true.
    end subroutine ensure_loaded

    subroutine unpack()
        integer :: i, j, k, f, s, nc, np, ip, ic, used, xx, yy
        logical :: pres(NCH)

        call from_base85()
        call build_code()
        BPOS = 0

        if (get_gamma() /= NOUT) error stop "fplot_glyphs: blob is not the one this module expects"

        ip = 0
        ic = 0
        OBEG(1) = 1
        CBEG(1) = 1
        do i = 1, NOUT
            nc = get_gamma()
            do j = 1, nc
                CBEG(ic + j + 1) = CBEG(ic + j) + get_gamma()
            end do
            np = CBEG(ic + nc + 1) - CBEG(ic + 1)
            xx = 0
            yy = 0
            do k = 1, np
                QON(ip + k) = get_bit() == 1
                xx = xx + get_delta()
                yy = yy + get_delta()
                QX(ip + k) = int(xx, int16)
                QY(ip + k) = int(yy, int16)
            end do
            ip = ip + np
            ic = ic + nc
            OBEG(i + 1) = ic + 1
        end do

        ! The slots. A code point the table has nothing for takes one bit
        ! here and no room in any face; everything else is written for
        ! each face in turn, and read against what is already known.
        do i = 1, NCH
            pres(i) = get_bit() == 1
        end do
        SADV = 0
        SOUT = 0
        used = 0
        do f = 1, NFACE
            do i = 1, NCH
                if (.not. pres(i)) cycle
                s = (f - 1)*NCH + i
                if (f == 1) then
                    SADV(s) = get_bits(ADV_BITS)
                else
                    SADV(s) = SADV(s - min(f - 1, 2)*NCH) + get_delta()
                end if
                if (get_bit() == 1) then
                    used = used + 1
                    SOUT(s) = used
                else
                    SOUT(s) = get_bits(OUT_BITS)
                end if
            end do
        end do

        if (ip /= NPT .or. ic /= NCONT .or. used /= NOUT) then
            error stop "fplot_glyphs: blob does not fill the tables"
        end if
        deallocate (BUF)
    end subroutine unpack

    ! Five characters back into four bytes, most significant first.
    subroutine from_base85()
        integer :: val(0:127), i, j, k, n
        integer(int64) :: acc
        val = -1
        do i = 1, len(ALPHABET)
            val(iachar(ALPHABET(i:i))) = i - 1
        end do
        n = len(BLOB)/5
        allocate (BUF(4*n))
        do k = 0, n - 1
            acc = 0
            do j = 1, 5
                acc = acc*85 + int(val(iachar(BLOB(5*k + j:5*k + j))), int64)
            end do
            do j = 1, 4
                BUF(4*k + j) = int(ibits(acc, 8*(4 - j), 8))
            end do
        end do
    end subroutine from_base85

    ! The canonical code the class lengths describe: codes of one length
    ! run consecutively, in order of the class they stand for, and each
    ! length starts where the one before it left off, shifted up a bit.
    subroutine build_code()
        integer :: l, s, code, k
        NLEN = 0
        do s = 0, NCLASS - 1
            NLEN(CLEN(s)) = NLEN(CLEN(s)) + 1
        end do
        code = 0
        k = 0
        do l = 1, MAXLEN
            FIRST(l) = code
            CBASE(l) = k
            do s = 0, NCLASS - 1
                if (CLEN(s) == l) then
                    k = k + 1
                    CSYM(k) = s
                end if
            end do
            code = 2*(code + NLEN(l))
        end do
    end subroutine build_code

    integer function get_bit()
        get_bit = ibits(BUF(BPOS/8 + 1), 7 - mod(BPOS, 8), 1)
        BPOS = BPOS + 1
    end function get_bit

    integer function get_bits(n)
        integer, intent(in) :: n
        integer :: i
        get_bits = 0
        do i = 1, n
            get_bits = 2*get_bits + get_bit()
        end do
    end function get_bits

    integer function get_gamma()
        integer :: zeros
        zeros = 0
        do while (get_bit() == 0)
            zeros = zeros + 1
        end do
        get_gamma = ior(ishft(1, zeros), get_bits(zeros))
    end function get_gamma

    integer function get_class()
        integer :: code, l, idx
        code = 0
        do l = 1, MAXLEN
            code = 2*code + get_bit()
            idx = code - FIRST(l)
            if (idx >= 0 .and. idx < NLEN(l)) then
                get_class = CSYM(CBASE(l) + idx + 1)
                return
            end if
        end do
        error stop "fplot_glyphs: the glyph blob has a code this module cannot read"
    end function get_class

    ! One coordinate delta: its magnitude class, the bits under the
    ! leading one, and then the fold back to a signed number.
    integer function get_delta()
        integer :: c, u
        c = get_class()
        if (c == 0) then
            u = 0
        else
            u = ior(ishft(1, c - 1), get_bits(c - 1))
        end if
        if (mod(u, 2) == 0) then
            get_delta = u/2
        else
            get_delta = -(u + 1)/2
        end if
    end function get_delta

end module fplot_glyphs
