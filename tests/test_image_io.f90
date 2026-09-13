! Reading back what the tests wrote: PNG and GIF, decoded to RGB.
!
! The fingerprints in test_fingerprint are taken from the files on disk
! rather than from the renderer's memory, so that the PNG and GIF encoders
! are tested along with everything that drew the picture. Only what fplot
! itself writes has to be read: 8-bit PNG without interlacing, and GIF with
! whole frames and no interlacing. Inflate follows zlib's puff.c.

module test_image_io
    implicit none
    private

    public :: read_png_rgb, read_gif_frames

    type :: huff_t
        integer :: count(0:15) = 0
        integer :: symbol(0:319) = 0
    end type huff_t

    ! A byte stream read least significant bit first, which is how both
    ! deflate and GIF's LZW pack their codes.
    type :: bits_t
        integer, allocatable :: inp(:)
        integer :: ipos = 1
        integer :: bitbuf = 0
        integer :: bitcnt = 0
    end type bits_t

    integer, parameter :: LBASE(0:28) = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, &
        15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, &
        195, 227, 258]
    integer, parameter :: LEXT(0:28) = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, &
        2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0]
    integer, parameter :: DBASE(0:29) = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, &
        33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, &
        3073, 4097, 6145, 8193, 12289, 16385, 24577]
    integer, parameter :: DEXT(0:29) = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, &
        5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]
    integer, parameter :: CLORDER(19) = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, &
        11, 4, 12, 3, 13, 2, 14, 1, 15]

contains

    subroutine read_bytes(path, b)
        character(len=*), intent(in) :: path
        integer, allocatable, intent(out) :: b(:)
        character(len=:), allocatable :: raw
        integer :: u, n, i

        open (newunit=u, file=path, access="stream", form="unformatted", &
              status="old", action="read")
        inquire (unit=u, size=n)
        allocate (character(len=n) :: raw)
        read (u) raw
        close (u)
        allocate (b(n))
        do i = 1, n
            b(i) = iachar(raw(i:i))
        end do
    end subroutine read_bytes

    function getbits(s, n) result(v)
        type(bits_t), intent(inout) :: s
        integer, intent(in) :: n
        integer :: v

        v = s%bitbuf
        do while (s%bitcnt < n)
            if (s%ipos > size(s%inp)) error stop "image_io: truncated stream"
            v = ior(v, ishft(s%inp(s%ipos), s%bitcnt))
            s%ipos = s%ipos + 1
            s%bitcnt = s%bitcnt + 8
        end do
        s%bitbuf = ishft(v, -n)
        s%bitcnt = s%bitcnt - n
        v = iand(v, ishft(1, n) - 1)
    end function getbits

    ! ------------------------------------------------------------------
    ! Inflate
    ! ------------------------------------------------------------------

    subroutine build(h, lens, n)
        type(huff_t), intent(out) :: h
        integer, intent(in) :: lens(0:), n
        integer :: offs(16), s, l

        h%count = 0
        do s = 0, n - 1
            h%count(lens(s)) = h%count(lens(s)) + 1
        end do
        offs(1) = 0
        do l = 1, 15
            offs(l + 1) = offs(l) + h%count(l)
        end do
        do s = 0, n - 1
            if (lens(s) /= 0) then
                h%symbol(offs(lens(s))) = s
                offs(lens(s)) = offs(lens(s)) + 1
            end if
        end do
    end subroutine build

    function decode(s, h) result(sym)
        type(bits_t), intent(inout) :: s
        type(huff_t), intent(in) :: h
        integer :: sym, code, first, idx, l, cnt

        code = 0
        first = 0
        idx = 0
        do l = 1, 15
            code = ior(code, getbits(s, 1))
            cnt = h%count(l)
            if (code - cnt < first) then
                sym = h%symbol(idx + (code - first))
                return
            end if
            idx = idx + cnt
            first = (first + cnt)*2
            code = code*2
        end do
        error stop "image_io: bad huffman code"
    end function decode

    subroutine codes(s, lc, dc, out, opos)
        type(bits_t), intent(inout) :: s
        type(huff_t), intent(in) :: lc, dc
        integer, intent(inout) :: out(:), opos
        integer :: sym, ln, dist, k

        do
            sym = decode(s, lc)
            if (sym < 256) then
                opos = opos + 1
                out(opos) = sym
            else if (sym == 256) then
                return
            else
                sym = sym - 257
                ln = LBASE(sym) + getbits(s, LEXT(sym))
                sym = decode(s, dc)
                dist = DBASE(sym) + getbits(s, DEXT(sym))
                do k = 1, ln
                    opos = opos + 1
                    out(opos) = out(opos - dist)
                end do
            end if
        end do
    end subroutine codes

    subroutine inflate(s, out)
        type(bits_t), intent(inout) :: s
        integer, intent(inout) :: out(:)
        integer :: last, btype, n, i, nlen, ndist, ncode, sym, rep, prev, opos
        integer :: lens(0:319), dl(0:29)
        type(huff_t) :: lc, dc

        opos = 0
        do
            last = getbits(s, 1)
            btype = getbits(s, 2)
            select case (btype)
            case (0)
                s%bitbuf = 0
                s%bitcnt = 0
                n = s%inp(s%ipos) + 256*s%inp(s%ipos + 1)
                s%ipos = s%ipos + 4
                out(opos + 1:opos + n) = s%inp(s%ipos:s%ipos + n - 1)
                opos = opos + n
                s%ipos = s%ipos + n
            case (1)
                lens(0:143) = 8
                lens(144:255) = 9
                lens(256:279) = 7
                lens(280:287) = 8
                call build(lc, lens, 288)
                lens(0:29) = 5
                call build(dc, lens, 30)
                call codes(s, lc, dc, out, opos)
            case (2)
                nlen = getbits(s, 5) + 257
                ndist = getbits(s, 5) + 1
                ncode = getbits(s, 4) + 4
                lens = 0
                do i = 1, ncode
                    lens(CLORDER(i)) = getbits(s, 3)
                end do
                call build(lc, lens, 19)
                i = 0
                do while (i < nlen + ndist)
                    sym = decode(s, lc)
                    if (sym < 16) then
                        lens(i) = sym
                        i = i + 1
                    else
                        prev = 0
                        if (sym == 16) then
                            prev = lens(i - 1)
                            rep = 3 + getbits(s, 2)
                        else if (sym == 17) then
                            rep = 3 + getbits(s, 3)
                        else
                            rep = 11 + getbits(s, 7)
                        end if
                        lens(i:i + rep - 1) = prev
                        i = i + rep
                    end if
                end do
                dl = 0
                dl(0:ndist - 1) = lens(nlen:nlen + ndist - 1)
                call build(lc, lens, nlen)
                call build(dc, dl, 30)
                call codes(s, lc, dc, out, opos)
            case default
                error stop "image_io: bad deflate block type"
            end select
            if (last == 1) exit
        end do
    end subroutine inflate

    integer function be32(b, i)
        integer, intent(in) :: b(:), i
        be32 = ((b(i)*256 + b(i + 1))*256 + b(i + 2))*256 + b(i + 3)
    end function be32

    ! ------------------------------------------------------------------
    ! PNG: rgb(3, w, h), composited onto white where there is alpha.
    ! ------------------------------------------------------------------

    subroutine read_png_rgb(path, rgb)
        character(len=*), intent(in) :: path
        integer, allocatable, intent(out) :: rgb(:, :, :)
        integer, allocatable :: b(:), idat(:), raw(:)
        integer :: i, p, ln, w, h, depth, ctype, bpp, nidat, fsize
        integer :: x, y, c, stride, f, a, bb, cc, v, pa, pb, pc, base, alpha
        character(len=4) :: typ
        type(bits_t) :: s

        call read_bytes(path, b)
        fsize = size(b)
        allocate (idat(fsize))
        nidat = 0
        w = 0
        h = 0
        depth = 0
        ctype = -1
        p = 9
        do while (p + 8 <= fsize)
            ln = be32(b, p)
            typ = achar(b(p + 4))//achar(b(p + 5))//achar(b(p + 6))//achar(b(p + 7))
            if (typ == "IHDR") then
                w = be32(b, p + 8)
                h = be32(b, p + 12)
                depth = b(p + 16)
                ctype = b(p + 17)
                if (b(p + 20) /= 0) error stop "image_io: interlaced PNG"
            else if (typ == "IDAT") then
                idat(nidat + 1:nidat + ln) = b(p + 8:p + 7 + ln)
                nidat = nidat + ln
            else if (typ == "IEND") then
                exit
            end if
            p = p + 12 + ln
        end do
        if (depth /= 8) error stop "image_io: only 8-bit PNG is read"
        select case (ctype)
        case (0); bpp = 1
        case (2); bpp = 3
        case (4); bpp = 2
        case (6); bpp = 4
        case default; error stop "image_io: unsupported PNG colour type"
        end select

        stride = w*bpp
        allocate (s%inp(nidat - 2))
        s%inp = idat(3:nidat)    ! past the two byte zlib header
        allocate (raw(h*(stride + 1)))
        call inflate(s, raw)

        allocate (rgb(3, w, h))
        do y = 1, h
            base = (y - 1)*(stride + 1) + 1
            f = raw(base)
            do i = 1, stride
                a = 0
                bb = 0
                cc = 0
                if (i > bpp) a = raw(base + i - bpp)
                if (y > 1) bb = raw(base + i - (stride + 1))
                if (i > bpp .and. y > 1) cc = raw(base + i - bpp - (stride + 1))
                v = raw(base + i)
                select case (f)
                case (1)
                    v = v + a
                case (2)
                    v = v + bb
                case (3)
                    v = v + (a + bb)/2
                case (4)
                    pa = abs(bb - cc)
                    pb = abs(a - cc)
                    pc = abs(a + bb - 2*cc)
                    if (pa <= pb .and. pa <= pc) then
                        v = v + a
                    else if (pb <= pc) then
                        v = v + bb
                    else
                        v = v + cc
                    end if
                end select
                raw(base + i) = iand(v, 255)
            end do
            do x = 1, w
                i = base + (x - 1)*bpp
                alpha = 255
                if (ctype == 4) alpha = raw(i + 2)
                if (ctype == 6) alpha = raw(i + 4)
                do c = 1, 3
                    if (ctype == 0 .or. ctype == 4) then
                        v = raw(i + 1)
                    else
                        v = raw(i + c)
                    end if
                    rgb(c, x, y) = (v*alpha + 255*(255 - alpha) + 127)/255
                end do
            end do
        end do
    end subroutine read_png_rgb

    ! ------------------------------------------------------------------
    ! GIF: rgb(3, w, h, nframes). Each frame starts from the previous one,
    ! so a frame smaller than the canvas lands where a viewer would put it.
    ! ------------------------------------------------------------------

    subroutine read_gif_frames(path, rgb)
        character(len=*), intent(in) :: path
        integer, allocatable, intent(out) :: rgb(:, :, :, :)
        integer, allocatable :: b(:), pal(:, :), lpal(:, :), ind(:)
        integer :: w, h, p, nf, packed, ngc, f, left, top, iw, ih, mcs
        integer :: x, y, k, nl

        call read_bytes(path, b)
        if (achar(b(1))//achar(b(2))//achar(b(3)) /= "GIF") error stop "image_io: not a GIF"
        w = b(7) + 256*b(8)
        h = b(9) + 256*b(10)
        packed = b(11)
        p = 14
        ngc = 0
        if (iand(packed, 128) /= 0) then
            ngc = 2**(iand(packed, 7) + 1)
            allocate (pal(3, 0:ngc - 1))
            do k = 0, ngc - 1
                pal(:, k) = b(p + 3*k:p + 3*k + 2)
            end do
            p = p + 3*ngc
        end if

        nf = count_frames(b, p)
        allocate (rgb(3, w, h, nf))
        rgb = 255
        f = 0
        do while (p <= size(b))
            select case (b(p))
            case (33)
                p = skip_blocks(b, p + 2)
            case (44)
                f = f + 1
                if (f > 1) rgb(:, :, :, f) = rgb(:, :, :, f - 1)
                left = b(p + 1) + 256*b(p + 2)
                top = b(p + 3) + 256*b(p + 4)
                iw = b(p + 5) + 256*b(p + 6)
                ih = b(p + 7) + 256*b(p + 8)
                packed = b(p + 9)
                if (iand(packed, 64) /= 0) error stop "image_io: interlaced GIF"
                p = p + 10
                if (iand(packed, 128) /= 0) then
                    nl = 2**(iand(packed, 7) + 1)
                    allocate (lpal(3, 0:nl - 1))
                    do k = 0, nl - 1
                        lpal(:, k) = b(p + 3*k:p + 3*k + 2)
                    end do
                    p = p + 3*nl
                else
                    allocate (lpal(3, 0:ngc - 1))
                    lpal = pal
                end if
                mcs = b(p)
                call lzw_decode(b, p + 1, mcs, iw*ih, ind, p)
                do y = 1, ih
                    do x = 1, iw
                        k = ind((y - 1)*iw + x)
                        rgb(:, left + x, top + y, f) = lpal(:, k)
                    end do
                end do
                deallocate (lpal)
            case (59)
                exit
            case default
                error stop "image_io: bad GIF block"
            end select
        end do
    end subroutine read_gif_frames

    integer function skip_blocks(b, p0) result(p)
        integer, intent(in) :: b(:), p0
        p = p0
        do while (b(p) /= 0)
            p = p + b(p) + 1
        end do
        p = p + 1
    end function skip_blocks

    integer function count_frames(b, p0) result(nf)
        integer, intent(in) :: b(:), p0
        integer :: p, packed
        nf = 0
        p = p0
        do while (p <= size(b))
            select case (b(p))
            case (33)
                p = skip_blocks(b, p + 2)
            case (44)
                nf = nf + 1
                packed = b(p + 9)
                p = p + 10
                if (iand(packed, 128) /= 0) p = p + 3*2**(iand(packed, 7) + 1)
                p = skip_blocks(b, p + 1)
            case default
                exit
            end select
        end do
    end function count_frames

    ! Variable width LZW as GIF uses it: codes grow from mcs+1 bits up to 12,
    ! a clear code resets the table. The data arrives in sub-blocks, which
    ! are joined first; pnext is left just past the terminating empty block.
    subroutine lzw_decode(b, p0, mcs, npix, ind, pnext)
        integer, intent(in) :: b(:), p0, mcs, npix
        integer, allocatable, intent(out) :: ind(:)
        integer, intent(out) :: pnext
        type(bits_t) :: s
        integer :: prefix(0:4095), suffix(0:4095), length(0:4095), firstc(0:4095)
        integer :: p, n, clear, eoi, csize, nxt, prev, code, cur, pos, k

        n = 0
        p = p0
        do while (b(p) /= 0)
            n = n + b(p)
            p = p + b(p) + 1
        end do
        pnext = p + 1
        allocate (s%inp(n))
        n = 0
        p = p0
        do while (b(p) /= 0)
            s%inp(n + 1:n + b(p)) = b(p + 1:p + b(p))
            n = n + b(p)
            p = p + b(p) + 1
        end do

        allocate (ind(npix))
        clear = 2**mcs
        eoi = clear + 1
        do k = 0, clear - 1
            prefix(k) = -1
            suffix(k) = k
            length(k) = 1
            firstc(k) = k
        end do
        csize = mcs + 1
        nxt = eoi + 1
        prev = -1
        pos = 0
        do
            if (s%ipos > size(s%inp) .and. s%bitcnt < csize) exit
            code = getbits(s, csize)
            if (code == clear) then
                csize = mcs + 1
                nxt = eoi + 1
                prev = -1
                cycle
            end if
            if (code == eoi) exit
            if (prev == -1) then
                pos = pos + 1
                ind(pos) = suffix(code)
                prev = code
                cycle
            end if
            if (nxt < 4096) then
                prefix(nxt) = prev
                length(nxt) = length(prev) + 1
                firstc(nxt) = firstc(prev)
                if (code < nxt) then
                    suffix(nxt) = firstc(code)
                else
                    suffix(nxt) = firstc(prev)
                end if
                nxt = nxt + 1
                if (nxt == 2**csize .and. csize < 12) csize = csize + 1
            end if
            ! write the string for code back to front
            cur = code
            do k = length(code), 1, -1
                ind(pos + k) = suffix(cur)
                cur = prefix(cur)
            end do
            pos = pos + length(code)
            prev = code
        end do
        if (pos /= npix) error stop "image_io: GIF frame has the wrong number of pixels"
    end subroutine lzw_decode

end module test_image_io
