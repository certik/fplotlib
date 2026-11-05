! fplotlib_png — the PNG container: filtering, deflate and chunks.
!
! Kept apart from the rasterizer because the two have nothing to say to each
! other: this module turns a finished pixel buffer into file bytes and knows
! nothing about drawing, and the rasterizer knows nothing about file format.
!
! The compressor is a real deflate rather than stored blocks. Stored blocks
! would be a dozen lines and would work, but a plot is mostly flat background,
! which is exactly what the row filters and LZ77 are good at: a typical figure
! comes out around thirty times smaller than it would uncompressed, which is
! the difference between a file worth attaching to an email and one that is
! not. Fixed Huffman codes are used rather than dynamic ones, which costs a
! few percent of size and saves the whole code-length-tree machinery.
!
! No zlib, no libpng, nothing to link.

module fplotlib_png
    implicit none
    private

    ! zlib_compress is here rather than in the PDF backend because a PDF
    ! FlateDecode stream is the same zlib stream a PNG carries, and one
    ! compressor is enough.
    public :: png_encode, apng_encode, zlib_compress

    ! LZ77 parameters. WSIZE is fixed by the format; CHAIN bounds how hard the
    ! matcher looks before settling, which is the usual size/speed dial.
    integer, parameter :: WSIZE = 32768
    integer, parameter :: HSIZE = 16384
    integer, parameter :: MIN_MATCH = 3, MAX_MATCH = 258
    integer, parameter :: CHAIN = 64

    ! IEND carries no payload.
    integer, parameter :: NO_DATA(0) = [integer ::]

    integer, parameter :: LEN_BASE(257:285) = [ &
        3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, &
        51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258]
    integer, parameter :: LEN_EXTRA(257:285) = [ &
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, &
        3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0]
    integer, parameter :: DIST_BASE(0:29) = [ &
        1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, &
        385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, &
        16385, 24577]
    integer, parameter :: DIST_EXTRA(0:29) = [ &
        0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, &
        9, 9, 10, 10, 11, 11, 12, 12, 13, 13]

    ! A growable byte buffer with a bit-level cursor, since deflate codes are
    ! not byte aligned but PNG chunks are.
    type :: bitbuf_t
        integer, allocatable :: b(:)
        integer :: n = 0
        integer :: acc = 0
        integer :: nbits = 0
    end type bitbuf_t

contains

    ! ------------------------------------------------------------------
    ! Byte buffer
    ! ------------------------------------------------------------------

    subroutine push(buf, v)
        type(bitbuf_t), intent(inout) :: buf
        integer, intent(in) :: v
        integer, allocatable :: t(:)

        if (.not. allocated(buf%b)) allocate (buf%b(65536))
        if (buf%n >= size(buf%b)) then
            allocate (t(2*size(buf%b)))
            t(1:buf%n) = buf%b(1:buf%n)
            call move_alloc(t, buf%b)
        end if
        buf%n = buf%n + 1
        buf%b(buf%n) = iand(v, 255)
    end subroutine push

    ! Deflate is little endian at the bit level: the first bit of the stream
    ! is the least significant bit of the first byte.
    subroutine put_bits(buf, code, nb)
        type(bitbuf_t), intent(inout) :: buf
        integer, intent(in) :: code, nb
        integer :: i

        do i = 0, nb - 1
            if (btest(code, i)) buf%acc = ibset(buf%acc, buf%nbits)
            buf%nbits = buf%nbits + 1
            if (buf%nbits == 8) then
                call push(buf, buf%acc)
                buf%acc = 0
                buf%nbits = 0
            end if
        end do
    end subroutine put_bits

    ! Huffman codes are written most significant bit first, the opposite of
    ! everything else in the stream, so they are reversed on the way out.
    subroutine put_code(buf, code, nb)
        type(bitbuf_t), intent(inout) :: buf
        integer, intent(in) :: code, nb
        integer :: i

        do i = nb - 1, 0, -1
            call put_bits(buf, merge(1, 0, btest(code, i)), 1)
        end do
    end subroutine put_code

    subroutine flush_bits(buf)
        type(bitbuf_t), intent(inout) :: buf

        if (buf%nbits > 0) then
            call push(buf, buf%acc)
            buf%acc = 0
            buf%nbits = 0
        end if
    end subroutine flush_bits

    ! ------------------------------------------------------------------
    ! Fixed Huffman alphabet, straight from RFC 1951 section 3.2.6.
    ! ------------------------------------------------------------------

    subroutine lit_code(sym, code, nb)
        integer, intent(in) :: sym
        integer, intent(out) :: code, nb

        if (sym <= 143) then
            code = 48 + sym; nb = 8
        else if (sym <= 255) then
            code = 400 + (sym - 144); nb = 9
        else if (sym <= 279) then
            code = sym - 256; nb = 7
        else
            code = 192 + (sym - 280); nb = 8
        end if
    end subroutine lit_code

    subroutine emit_literal(buf, v)
        type(bitbuf_t), intent(inout) :: buf
        integer, intent(in) :: v
        integer :: c, nb

        call lit_code(v, c, nb)
        call put_code(buf, c, nb)
    end subroutine emit_literal

    subroutine emit_match(buf, len, dist)
        type(bitbuf_t), intent(inout) :: buf
        integer, intent(in) :: len, dist
        integer :: s, c, nb, d

        do s = 285, 257, -1
            if (len >= LEN_BASE(s)) exit
        end do
        if (s == 285 .and. len < 258) s = 284
        call lit_code(s, c, nb)
        call put_code(buf, c, nb)
        if (LEN_EXTRA(s) > 0) call put_bits(buf, len - LEN_BASE(s), LEN_EXTRA(s))

        do d = 29, 0, -1
            if (dist >= DIST_BASE(d)) exit
        end do
        call put_code(buf, d, 5)
        if (DIST_EXTRA(d) > 0) call put_bits(buf, dist - DIST_BASE(d), DIST_EXTRA(d))
    end subroutine emit_match

    ! ------------------------------------------------------------------
    ! Deflate: greedy LZ77 over a hash chain, fixed Huffman output.
    ! ------------------------------------------------------------------

    function deflate(src, n) result(out)
        integer, intent(in) :: src(:), n
        integer, allocatable :: out(:)
        type(bitbuf_t) :: buf
        integer, allocatable :: head(:), prev(:)
        integer :: i, h, j, k, best_len, best_dist, cur, limit, mlen, nchain

        allocate (head(0:HSIZE - 1), prev(0:WSIZE - 1))
        head = 0
        prev = 0

        call put_bits(buf, 1, 1)   ! final block
        call put_bits(buf, 1, 2)   ! fixed Huffman

        i = 1
        do while (i <= n)
            best_len = 0
            best_dist = 0
            if (i + MIN_MATCH - 1 <= n) then
                h = hash3(src(i), src(i + 1), src(i + 2))
                cur = head(h)
                nchain = 0
                do while (cur > 0 .and. nchain < CHAIN)
                    if (i - cur >= WSIZE) exit
                    limit = min(MAX_MATCH, n - i + 1)
                    mlen = 0
                    do k = 0, limit - 1
                        if (src(cur + k) /= src(i + k)) exit
                        mlen = mlen + 1
                    end do
                    if (mlen > best_len) then
                        best_len = mlen
                        best_dist = i - cur
                        if (mlen >= MAX_MATCH) exit
                    end if
                    cur = prev(iand(cur, WSIZE - 1))
                    nchain = nchain + 1
                end do
            end if

            if (best_len >= MIN_MATCH) then
                call emit_match(buf, best_len, best_dist)
                do j = i, i + best_len - 1
                    if (j + MIN_MATCH - 1 <= n) then
                        h = hash3(src(j), src(j + 1), src(j + 2))
                        prev(iand(j, WSIZE - 1)) = head(h)
                        head(h) = j
                    end if
                end do
                i = i + best_len
            else
                call emit_literal(buf, src(i))
                if (i + MIN_MATCH - 1 <= n) then
                    h = hash3(src(i), src(i + 1), src(i + 2))
                    prev(iand(i, WSIZE - 1)) = head(h)
                    head(h) = i
                end if
                i = i + 1
            end if
        end do

        call emit_literal(buf, 256)   ! end of block
        call flush_bits(buf)

        allocate (out(buf%n))
        if (buf%n > 0) out = buf%b(1:buf%n)
    end function deflate

    ! Zlib's rolling hash, folded over three bytes. Deliberately not a
    ! multiplicative hash: the usual 2654435761 constant does not fit in a
    ! default integer, and a shift-xor mix costs nothing here.
    pure function hash3(a, b, c) result(h)
        integer, intent(in) :: a, b, c
        integer :: h

        h = iand(ieor(ishft(a, 5), b), HSIZE - 1)
        h = iand(ieor(ishft(h, 5), c), HSIZE - 1)
    end function hash3

    ! ------------------------------------------------------------------
    ! Checksums
    ! ------------------------------------------------------------------

    function crc32(buf, n) result(c)
        integer, intent(in) :: buf(:), n
        integer :: c, i, k, tmp

        c = -1
        do i = 1, n
            c = ieor(c, buf(i))
            do k = 1, 8
                tmp = iand(c, 1)
                c = ishft(c, -1)
                c = iand(c, 2147483647)   ! keep the shift logical
                if (tmp /= 0) c = ieor(c, -306674912)
            end do
        end do
        c = not(c)
    end function crc32

    function adler32(buf, n) result(a)
        integer, intent(in) :: buf(:), n
        integer :: a, s1, s2, i

        s1 = 1
        s2 = 0
        do i = 1, n
            s1 = mod(s1 + buf(i), 65521)
            s2 = mod(s2 + s1, 65521)
        end do
        a = s2*65536 + s1
    end function adler32

    ! ------------------------------------------------------------------
    ! Row filtering. PNG lets each row pick its own predictor; libpng's rule
    ! of thumb is to take whichever leaves the smallest total magnitude, on
    ! the grounds that small residuals are what deflate compresses best.
    ! It matters here: a plot has many identical rows, and the Up filter
    ! turns those into runs of zeros.
    ! ------------------------------------------------------------------

    pure function paeth(a, b, c) result(p)
        integer, intent(in) :: a, b, c
        integer :: p, pa, pb, pc

        pa = abs(b - c)
        pb = abs(a - c)
        pc = abs(a + b - 2*c)
        if (pa <= pb .and. pa <= pc) then
            p = a
        else if (pb <= pc) then
            p = b
        else
            p = c
        end if
    end function paeth

    ! ------------------------------------------------------------------
    ! Encode an RGBA buffer, row major from the top left, as PNG bytes.
    ! ------------------------------------------------------------------

    function png_encode(w, h, rgba) result(bytes)
        integer, intent(in) :: w, h
        integer, intent(in) :: rgba(4, w, h)
        character(len=:), allocatable :: bytes
        type(bitbuf_t) :: out

        call push_signature(out)
        call ihdr(out, w, h, 6)
        call chunk(out, "IDAT", filtered_zlib(w, h, 4, rgba))
        call chunk(out, "IEND", NO_DATA)
        bytes = to_string(out)
    end function png_encode

    ! ------------------------------------------------------------------
    ! Encode nframes RGB frames as an animated PNG. pix holds the frames as
    ! packed RGB bytes, one after another, as the animation reel keeps them.
    ! A delay is delay_num/delay_den seconds; loop true repeats forever.
    ! Every frame is a full, opaque replacement of the canvas, so the first
    ! frame is also the default image that viewers without APNG support show.
    ! ------------------------------------------------------------------

    function apng_encode(w, h, nframes, pix, delay_num, delay_den, loop) result(bytes)
        integer, intent(in) :: w, h, nframes, delay_num, delay_den
        character(len=*), intent(in) :: pix
        logical, intent(in) :: loop
        character(len=:), allocatable :: bytes
        integer, allocatable :: rgb(:, :, :), z(:), d(:)
        integer :: f, x, y, c, k, seq, base, nz
        type(bitbuf_t) :: out

        allocate (rgb(3, w, h))
        call push_signature(out)
        call ihdr(out, w, h, 2)

        allocate (d(8))
        call be32(nframes, d(1:4))
        d(5:8) = 0                  ! zero plays forever
        if (.not. loop) call be32(1, d(5:8))
        call chunk(out, "acTL", d)
        deallocate (d)

        seq = 0
        do f = 1, nframes
            base = (f - 1)*3*w*h
            do concurrent (y = 1:h, x = 1:w, c = 1:3)
                k = base + (y - 1)*3*w + (x - 1)*3 + c
                rgb(c, x, y) = iachar(pix(k:k))
            end do

            allocate (d(26))
            call be32(seq, d(1:4))
            call be32(w, d(5:8))
            call be32(h, d(9:12))
            d(13:20) = 0            ! x and y offset
            d(21) = iand(ishft(delay_num, -8), 255)
            d(22) = iand(delay_num, 255)
            d(23) = iand(ishft(delay_den, -8), 255)
            d(24) = iand(delay_den, 255)
            d(25) = 0               ! dispose: leave the canvas as it is
            d(26) = 0               ! blend: replace
            call chunk(out, "fcTL", d)
            deallocate (d)
            seq = seq + 1

            z = filtered_zlib(w, h, 3, rgb)
            if (f == 1) then
                call chunk(out, "IDAT", z)
            else
                nz = size(z)
                allocate (d(4 + nz))
                call be32(seq, d(1:4))
                d(5:) = z
                call chunk(out, "fdAT", d)
                deallocate (d)
                seq = seq + 1
            end if
        end do

        call chunk(out, "IEND", NO_DATA)
        bytes = to_string(out)
    end function apng_encode

    subroutine push_signature(out)
        type(bitbuf_t), intent(inout) :: out

        call push(out, 137); call push(out, 80); call push(out, 78)
        call push(out, 71); call push(out, 13); call push(out, 10)
        call push(out, 26); call push(out, 10)
    end subroutine push_signature

    function to_string(out) result(bytes)
        type(bitbuf_t), intent(in) :: out
        character(len=:), allocatable :: bytes
        integer :: i

        allocate (character(len=out%n) :: bytes)
        do i = 1, out%n
            bytes(i:i) = achar(out%b(i))
        end do
    end function to_string

    ! The zlib stream of an image's scanlines, each filtered with whichever of
    ! the five PNG filters looks cheapest. px is bpp interleaved channels.
    function filtered_zlib(w, h, bpp, px) result(z)
        integer, intent(in) :: w, h, bpp
        integer, intent(in) :: px(bpp, w, h)
        integer, allocatable :: z(:)
        integer, allocatable :: raw(:), comp(:), cur(:), up(:), try(:), best(:)
        integer :: stride, nraw, x, y, c, i, p, f, score, bscore, bf
        integer :: a_, b_, c_, v

        stride = w*bpp
        nraw = h*(stride + 1)
        allocate (raw(nraw), cur(stride), up(stride), try(stride), best(stride))
        up = 0
        p = 0

        do y = 1, h
            i = 0
            do x = 1, w
                do c = 1, bpp
                    i = i + 1
                    cur(i) = iand(px(c, x, y), 255)
                end do
            end do

            bf = 0
            bscore = huge(0)
            do f = 0, 4
                score = 0
                do i = 1, stride
                    a_ = 0
                    if (i > bpp) a_ = cur(i - bpp)
                    b_ = up(i)
                    c_ = 0
                    if (i > bpp) c_ = up(i - bpp)
                    select case (f)
                    case (0); v = cur(i)
                    case (1); v = cur(i) - a_
                    case (2); v = cur(i) - b_
                    case (3); v = cur(i) - (a_ + b_)/2
                    case default; v = cur(i) - paeth(a_, b_, c_)
                    end select
                    v = iand(v, 255)
                    try(i) = v
                    if (v > 127) v = v - 256
                    score = score + abs(v)
                end do
                if (score < bscore) then
                    bscore = score
                    bf = f
                    best = try
                end if
            end do

            p = p + 1
            raw(p) = bf
            raw(p + 1:p + stride) = best(1:stride)
            p = p + stride
            up = cur
        end do

        comp = deflate(raw, nraw)
        z = zlib_wrap(comp, raw, nraw)
    end function filtered_zlib

    ! Bytes in, a zlib stream out. Unlike a PNG the caller supplies its own
    ! layout, so nothing is filtered here.
    function zlib_compress(src) result(bytes)
        integer, intent(in) :: src(:)
        character(len=:), allocatable :: bytes
        integer, allocatable :: z(:)
        integer :: i, n

        n = size(src)
        z = zlib_wrap(deflate(src, n), src, n)
        allocate (character(len=size(z)) :: bytes)
        do i = 1, size(z)
            bytes(i:i) = achar(z(i))
        end do
    end function zlib_compress

    function zlib_wrap(comp, raw, nraw) result(z)
        integer, intent(in) :: comp(:), raw(:), nraw
        integer, allocatable :: z(:)
        integer :: n, a

        n = size(comp)
        allocate (z(n + 6))
        z(1) = 120        ! deflate, 32K window
        z(2) = 1          ! check bits, no preset dictionary
        z(3:n + 2) = comp
        a = adler32(raw, nraw)
        z(n + 3) = iand(ishft(a, -24), 255)
        z(n + 4) = iand(ishft(a, -16), 255)
        z(n + 5) = iand(ishft(a, -8), 255)
        z(n + 6) = iand(a, 255)
    end function zlib_wrap

    subroutine ihdr(out, w, h, ctype)
        type(bitbuf_t), intent(inout) :: out
        integer, intent(in) :: w, h, ctype
        integer :: d(13)

        call be32(w, d(1:4))
        call be32(h, d(5:8))
        d(9) = 8      ! bit depth
        d(10) = ctype ! 2 truecolor, 6 truecolor with alpha
        d(11) = 0     ! deflate
        d(12) = 0     ! adaptive filtering
        d(13) = 0     ! no interlace
        call chunk(out, "IHDR", d)
    end subroutine ihdr

    pure subroutine be32(v, d)
        integer, intent(in) :: v
        integer, intent(out) :: d(4)

        d(1) = iand(ishft(v, -24), 255)
        d(2) = iand(ishft(v, -16), 255)
        d(3) = iand(ishft(v, -8), 255)
        d(4) = iand(v, 255)
    end subroutine be32

    subroutine chunk(out, name, data)
        type(bitbuf_t), intent(inout) :: out
        character(len=4), intent(in) :: name
        integer, intent(in) :: data(:)
        integer :: d(4), i, n
        integer, allocatable :: tc(:)

        n = size(data)
        call be32(n, d)
        do i = 1, 4
            call push(out, d(i))
        end do
        allocate (tc(4 + n))
        do i = 1, 4
            tc(i) = iachar(name(i:i))
            call push(out, tc(i))
        end do
        if (n > 0) tc(5:4 + n) = data
        do i = 1, n
            call push(out, data(i))
        end do
        call be32(crc32(tc, 4 + n), d)
        do i = 1, 4
            call push(out, d(i))
        end do
    end subroutine chunk

end module fplotlib_png
