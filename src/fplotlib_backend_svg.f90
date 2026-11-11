! SVG backend: the fplotlib_render contract written out as XML.
!
! This is the reference implementation of renderer_t and the easiest of the
! four to read, because SVG has a native element for almost everything the
! API asks for. Where it does not, the translation is still local: a marker
! set becomes one <defs> shape plus a <use> per point, which is what
! matplotlib emits and what keeps a scatter plot from repeating the same
! outline a thousand times.
!
! Attribute order is fixed here rather than at the call site: fill first,
! then stroke, then dashes. fplotlib used to spell the same paint differently
! depending on which helper emitted it, which made the output harder to diff
! than it needed to be.
module fplotlib_backend_svg
    use fplotlib_style, only: dp
    use fplotlib_render
    use fplotlib_svg, only: svg_builder, builder_init, builder_append, &
                         builder_get, fmt_num, xml_escape_to
    use fplotlib_png, only: png_encode
    implicit none
    private

    public :: svg_renderer_t

    type, extends(renderer_t) :: svg_renderer_t
        type(svg_builder) :: b
        ! Serial numbers for the ids that <use> and clip-path refer to.
        integer :: n_marker = 0
        integer :: n_clip = 0
        ! The clip currently open as a <g>, so that a run of primitives
        ! sharing one clip pays for it once. The backend discovers this by
        ! itself; nothing in the API mentions it.
        type(clip_t) :: cur_clip
        integer :: cur_clip_id = 0
        logical :: in_clip = .false.
    contains
        procedure :: open_canvas => svg_open_canvas
        procedure :: close_canvas => svg_close_canvas
        procedure :: bytes => svg_bytes
        procedure :: draw_path => svg_draw_path
        procedure :: draw_rect => svg_draw_rect
        procedure :: draw_circle => svg_draw_circle
        procedure :: draw_markers => svg_draw_markers
        procedure :: draw_text => svg_draw_text
        procedure :: draw_image => svg_draw_image
        procedure :: begin_group => svg_begin_group
        procedure :: end_group => svg_end_group
    end type svg_renderer_t

contains

    subroutine put(self, s)
        class(svg_renderer_t), intent(inout) :: self
        character(len=*), intent(in) :: s
        call builder_append(self%b, s)
    end subroutine put

    subroutine put_num(self, x)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x
        character(len=64) :: s
        integer :: n
        call fmt_num(x, s, n)
        call builder_append(self%b, s(1:n))
    end subroutine put_num

    subroutine put_int(self, i)
        class(svg_renderer_t), intent(inout) :: self
        integer, intent(in) :: i
        character(len=12) :: t
        write (t, "(I0)") i
        call builder_append(self%b, trim(t))
    end subroutine put_int

    subroutine put_eol(self)
        class(svg_renderer_t), intent(inout) :: self
        call builder_append(self%b, new_line("a"))
    end subroutine put_eol

    ! "#rrggbb", lower case, which is the spelling matplotlib uses.
    subroutine put_color(self, rgb)
        class(svg_renderer_t), intent(inout) :: self
        integer, intent(in) :: rgb(3)
        character(len=7) :: s
        character(len=16), parameter :: hexd = "0123456789abcdef"
        integer :: i, v
        s(1:1) = "#"
        do i = 1, 3
            v = max(0, min(255, rgb(i)))
            s(2*i:2*i) = hexd(v/16 + 1:v/16 + 1)
            s(2*i + 1:2*i + 1) = hexd(mod(v, 16) + 1:mod(v, 16) + 1)
        end do
        call put(self, s)
    end subroutine put_color

    ! Everything about a paint except its geometry. Emitted in one fixed
    ! order so that two identical paints always produce identical text.
    subroutine put_paint(self, p, skip_fill)
        class(svg_renderer_t), intent(inout) :: self
        type(paint_t), intent(in) :: p
        logical, intent(in), optional :: skip_fill
        logical :: nofill
        integer :: i

        nofill = .false.
        if (present(skip_fill)) nofill = skip_fill

        if (.not. nofill) then
            call put(self, ' fill="')
            if (p%filled) then
                call put_color(self, p%fill_rgb)
            else
                call put(self, "none")
            end if
            call put(self, '"')
            if (p%filled .and. p%fill_alpha < 1.0_dp) then
                call put(self, ' fill-opacity="')
                call put_num(self, p%fill_alpha)
                call put(self, '"')
            end if
            if (p%filled .and. p%fill_rule == FILL_EVENODD) then
                call put(self, ' fill-rule="evenodd"')
            end if
        end if

        if (p%stroked) then
            call put(self, ' stroke="')
            call put_color(self, p%stroke_rgb)
            call put(self, '" stroke-width="')
            call put_num(self, p%line_width)
            call put(self, '"')
            if (p%stroke_alpha < 1.0_dp) then
                call put(self, ' stroke-opacity="')
                call put_num(self, p%stroke_alpha)
                call put(self, '"')
            end if
            ! butt and miter are the SVG defaults, so say nothing.
            select case (p%cap)
            case (CAP_ROUND); call put(self, ' stroke-linecap="round"')
            case (CAP_SQUARE); call put(self, ' stroke-linecap="square"')
            end select
            select case (p%join)
            case (JOIN_ROUND); call put(self, ' stroke-linejoin="round"')
            case (JOIN_BEVEL); call put(self, ' stroke-linejoin="bevel"')
            end select
            if (p%n_dash > 0) then
                call put(self, ' stroke-dasharray="')
                do i = 1, p%n_dash
                    if (i > 1) call put(self, ",")
                    call put_num(self, p%dash(i))
                end do
                call put(self, '"')
                if (p%dash_offset /= 0.0_dp) then
                    call put(self, ' stroke-dashoffset="')
                    call put_num(self, p%dash_offset)
                    call put(self, '"')
                end if
            end if
        end if
    end subroutine put_paint

    logical function same_clip(a, c)
        type(clip_t), intent(in) :: a, c
        same_clip = (a%on .eqv. c%on)
        if (.not. same_clip) return
        if (.not. a%on) return
        same_clip = (a%x == c%x) .and. (a%y == c%y) .and. &
                    (a%w == c%w) .and. (a%h == c%h)
    end function same_clip

    ! Open or close the <g clip-path=...> wrapper as the clip changes. This
    ! is purely a size optimization; drawing one clipped shape at a time
    ! would be equally correct.
    subroutine sync_clip(self, c)
        class(svg_renderer_t), intent(inout) :: self
        type(clip_t), intent(in) :: c

        if (same_clip(self%cur_clip, c)) return

        if (self%in_clip) then
            call put(self, "</g>")
            call put_eol(self)
            self%in_clip = .false.
        end if

        if (c%on) then
            self%n_clip = self%n_clip + 1
            self%cur_clip_id = self%n_clip
            call put(self, '<defs><clipPath id="clip')
            call put_int(self, self%cur_clip_id)
            call put(self, '"><rect x="')
            call put_num(self, c%x)
            call put(self, '" y="')
            call put_num(self, c%y)
            call put(self, '" width="')
            call put_num(self, c%w)
            call put(self, '" height="')
            call put_num(self, c%h)
            call put(self, '"/></clipPath></defs>')
            call put_eol(self)
            call put(self, '<g clip-path="url(#clip')
            call put_int(self, self%cur_clip_id)
            call put(self, ')">')
            call put_eol(self)
            self%in_clip = .true.
        end if

        self%cur_clip = c
    end subroutine sync_clip

    subroutine svg_open_canvas(self, width, height, bg_rgb, bg_alpha, x0, y0)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: width, height
        integer, intent(in), optional :: bg_rgb(3)
        real(dp), intent(in), optional :: bg_alpha
        real(dp), intent(in), optional :: x0, y0
        real(dp) :: ox, oy

        ox = 0.0_dp
        oy = 0.0_dp
        if (present(x0)) ox = x0
        if (present(y0)) oy = y0

        call builder_init(self%b)
        call put(self, '<?xml version="1.0" encoding="utf-8" standalone="no"?>')
        call put_eol(self)
        call put(self, '<svg xmlns="http://www.w3.org/2000/svg" ')
        call put(self, 'xmlns:xlink="http://www.w3.org/1999/xlink" width="')
        call put_num(self, width)
        call put(self, 'pt" height="')
        call put_num(self, height)
        call put(self, 'pt" viewBox="')
        call put_num(self, ox)
        call put(self, " ")
        call put_num(self, oy)
        call put(self, " ")
        call put_num(self, width)
        call put(self, " ")
        call put_num(self, height)
        call put(self, '" version="1.1">')
        call put_eol(self)

        if (present(bg_rgb)) then
            call put(self, '<rect x="')
            call put_num(self, ox)
            call put(self, '" y="')
            call put_num(self, oy)
            call put(self, '" width="')
            call put_num(self, width)
            call put(self, '" height="')
            call put_num(self, height)
            call put(self, '" fill="')
            call put_color(self, bg_rgb)
            call put(self, '"')
            if (present(bg_alpha)) then
                if (bg_alpha < 1.0_dp) then
                    call put(self, ' fill-opacity="')
                    call put_num(self, bg_alpha)
                    call put(self, '"')
                end if
            end if
            call put(self, "/>")
            call put_eol(self)
        end if
    end subroutine svg_open_canvas

    subroutine svg_close_canvas(self)
        class(svg_renderer_t), intent(inout) :: self
        type(clip_t) :: none
        call sync_clip(self, none)
        call put(self, "</svg>")
        call put_eol(self)
    end subroutine svg_close_canvas

    function svg_bytes(self) result(s)
        class(svg_renderer_t), intent(in) :: self
        character(len=:), allocatable :: s
        s = builder_get(self%b)
    end function svg_bytes

    ! Writes the d="" attribute body. Shared by draw_path and by the marker
    ! shape definition.
    subroutine put_path_data(self, x, y, verbs, n_verb)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in) :: verbs(:), n_verb
        integer :: i, k, j
        logical :: first

        k = 0
        first = .true.
        do i = 1, n_verb
            select case (verbs(i))
            case (VERB_MOVE)
                k = k + 1
                if (.not. first) call put(self, " ")
                call put(self, "M ")
                call put_num(self, x(k))
                call put(self, " ")
                call put_num(self, y(k))
                first = .false.
            case (VERB_LINE)
                k = k + 1
                call put(self, " L ")
                call put_num(self, x(k))
                call put(self, " ")
                call put_num(self, y(k))
            case (VERB_CUBIC)
                call put(self, " C")
                do j = 1, 3
                    k = k + 1
                    call put(self, " ")
                    call put_num(self, x(k))
                    call put(self, " ")
                    call put_num(self, y(k))
                end do
            case (VERB_CLOSE)
                call put(self, " Z")
            end select
        end do
    end subroutine put_path_data

    subroutine svg_draw_path(self, x, y, verbs, n_verb, paint)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in) :: verbs(:), n_verb
        type(paint_t), intent(in) :: paint

        if (n_verb <= 0) return
        call sync_clip(self, paint%clip)
        call put(self, '<path d="')
        call put_path_data(self, x, y, verbs, n_verb)
        call put(self, '"')
        call put_paint(self, paint)
        call put(self, "/>")
        call put_eol(self)
    end subroutine svg_draw_path

    subroutine svg_draw_rect(self, x, y, w, h, paint, radius)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y, w, h
        type(paint_t), intent(in) :: paint
        real(dp), intent(in), optional :: radius

        call sync_clip(self, paint%clip)
        call put(self, '<rect x="')
        call put_num(self, x)
        call put(self, '" y="')
        call put_num(self, y)
        call put(self, '" width="')
        call put_num(self, w)
        call put(self, '" height="')
        call put_num(self, h)
        call put(self, '"')
        if (present(radius)) then
            if (radius > 0.0_dp) then
                call put(self, ' rx="')
                call put_num(self, radius)
                call put(self, '"')
            end if
        end if
        call put_paint(self, paint)
        call put(self, "/>")
        call put_eol(self)
    end subroutine svg_draw_rect

    subroutine svg_draw_circle(self, cx, cy, r, paint)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: cx, cy, r
        type(paint_t), intent(in) :: paint

        call sync_clip(self, paint%clip)
        call put(self, '<circle cx="')
        call put_num(self, cx)
        call put(self, '" cy="')
        call put_num(self, cy)
        call put(self, '" r="')
        call put_num(self, r)
        call put(self, '"')
        call put_paint(self, paint)
        call put(self, "/>")
        call put_eol(self)
    end subroutine svg_draw_circle

    ! One <defs> shape and a <use> per point. For a thousand point scatter
    ! this is the difference between naming the outline once and repeating
    ! it a thousand times.
    subroutine svg_draw_markers(self, x, y, mx, my, mverbs, n_mverb, paint)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in) :: mx(:), my(:)
        integer, intent(in) :: mverbs(:), n_mverb
        type(paint_t), intent(in) :: paint
        integer :: i

        if (size(x) <= 0 .or. n_mverb <= 0) return
        call sync_clip(self, paint%clip)

        self%n_marker = self%n_marker + 1
        call put(self, '<defs><path id="m')
        call put_int(self, self%n_marker)
        call put(self, '" d="')
        call put_path_data(self, mx, my, mverbs, n_mverb)
        call put(self, '"/></defs>')
        call put_eol(self)

        do i = 1, size(x)
            call put(self, '<use xlink:href="#m')
            call put_int(self, self%n_marker)
            call put(self, '" x="')
            call put_num(self, x(i))
            call put(self, '" y="')
            call put_num(self, y(i))
            call put(self, '"')
            call put_paint(self, paint)
            call put(self, "/>")
            call put_eol(self)
        end do
    end subroutine svg_draw_markers

    subroutine svg_draw_text(self, x, y, s, font, paint, anchor, baseline, angle)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        type(font_t), intent(in) :: font
        type(paint_t), intent(in) :: paint
        integer, intent(in) :: anchor, baseline
        real(dp), intent(in) :: angle
        character(len=6*len(s)) :: esc
        integer :: en

        call sync_clip(self, paint%clip)
        call put(self, '<text x="')
        call put_num(self, x)
        call put(self, '" y="')
        call put_num(self, y)
        call put(self, '" text-anchor="')
        select case (anchor)
        case (ANCHOR_START); call put(self, "start")
        case (ANCHOR_END); call put(self, "end")
        case default; call put(self, "middle")
        end select
        call put(self, '"')
        select case (baseline)
        case (BASE_MIDDLE); call put(self, ' dominant-baseline="middle"')
        case (BASE_TOP); call put(self, ' dominant-baseline="hanging"')
        end select
        call put(self, ' font-family="')
        select case (font%family)
        case (FAMILY_SERIF); call put(self, "DejaVu Serif, serif")
        case (FAMILY_MONO); call put(self, "DejaVu Sans Mono, monospace")
        case default; call put(self, "DejaVu Sans, sans-serif")
        end select
        call put(self, '" font-size="')
        call put_num(self, font%size)
        call put(self, '"')
        if (font%weight == WEIGHT_BOLD) call put(self, ' font-weight="bold"')
        if (font%slant == SLANT_ITALIC) call put(self, ' font-style="italic"')
        call put(self, ' fill="')
        if (paint%filled) then
            call put_color(self, paint%fill_rgb)
        else
            call put_color(self, [0, 0, 0])
        end if
        call put(self, '"')
        if (paint%filled .and. paint%fill_alpha < 1.0_dp) then
            call put(self, ' fill-opacity="')
            call put_num(self, paint%fill_alpha)
            call put(self, '"')
        end if
        if (angle /= 0.0_dp) then
            call put(self, ' transform="rotate(')
            call put_num(self, angle)
            call put(self, " ")
            call put_num(self, x)
            call put(self, " ")
            call put_num(self, y)
            call put(self, ')"')
        end if
        ! The default whitespace rule throws away a leading space, so a
        ! run that starts with one draws its first glyph where the space
        ! should have been. preserve keeps the space the layout measured.
        call put(self, ' xml:space="preserve"')
        call put(self, ">")
        ! Escaping is the backend's business: the caller hands over the text
        ! the user asked for, and each format spells it its own way.
        call xml_escape_to(s, esc, en)
        call put(self, esc(1:en))
        call put(self, "</text>")
        call put_eol(self)
    end subroutine svg_draw_text

    ! A raster becomes a PNG in a base64 data URI, which is what matplotlib
    ! emits and what lets the whole image be one element. Smoothing is turned
    ! off: the samples are the data, and interpolating them would show the
    ! reader values that were never measured.
    subroutine svg_draw_image(self, x, y, w, h, rgba, nx, ny, paint)
        class(svg_renderer_t), intent(inout) :: self
        real(dp), intent(in) :: x, y, w, h
        integer, intent(in) :: nx, ny
        integer, intent(in) :: rgba(4, nx, ny)
        type(paint_t), intent(in) :: paint

        if (nx < 1 .or. ny < 1) return
        call sync_clip(self, paint%clip)
        call put(self, '<image x="')
        call put_num(self, x)
        call put(self, '" y="')
        call put_num(self, y)
        call put(self, '" width="')
        call put_num(self, w)
        call put(self, '" height="')
        call put_num(self, h)
        call put(self, '" preserveAspectRatio="none" image-rendering="pixelated"')
        if (paint%fill_alpha < 1.0_dp) then
            call put(self, ' opacity="')
            call put_num(self, paint%fill_alpha)
            call put(self, '"')
        end if
        call put(self, ' xlink:href="data:image/png;base64,')
        call put_base64(self, png_encode(nx, ny, rgba))
        call put(self, '"/>')
        call put_eol(self)
    end subroutine svg_draw_image

    ! Three bytes become four characters; a short final group is padded with
    ! "=" so the length still divides by four.
    subroutine put_base64(self, s)
        class(svg_renderer_t), intent(inout) :: self
        character(len=*), intent(in) :: s
        character(len=64), parameter :: TBL = &
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        character(len=:), allocatable :: out
        integer :: i, n, k, v, b(3), have

        n = len(s)
        allocate (character(len=4*((n + 2)/3)) :: out)
        k = 0
        do i = 1, n, 3
            have = min(3, n - i + 1)
            b = 0
            b(1:have) = [(iachar(s(i + v - 1:i + v - 1)), v=1, have)]
            v = ishft(b(1), 16) + ishft(b(2), 8) + b(3)
            out(k + 1:k + 1) = TBL(iand(ishft(v, -18), 63) + 1:iand(ishft(v, -18), 63) + 1)
            out(k + 2:k + 2) = TBL(iand(ishft(v, -12), 63) + 1:iand(ishft(v, -12), 63) + 1)
            if (have > 1) then
                out(k + 3:k + 3) = TBL(iand(ishft(v, -6), 63) + 1:iand(ishft(v, -6), 63) + 1)
            else
                out(k + 3:k + 3) = "="
            end if
            if (have > 2) then
                out(k + 4:k + 4) = TBL(iand(v, 63) + 1:iand(v, 63) + 1)
            else
                out(k + 4:k + 4) = "="
            end if
            k = k + 4
        end do
        call put(self, out)
    end subroutine put_base64

    subroutine svg_begin_group(self, name)
        class(svg_renderer_t), intent(inout) :: self
        character(len=*), intent(in) :: name
        call put(self, '<g id="')
        call put(self, name)
        call put(self, '">')
        call put_eol(self)
    end subroutine svg_begin_group

    subroutine svg_end_group(self)
        class(svg_renderer_t), intent(inout) :: self
        type(clip_t) :: none
        ! A clip must not outlive the group that contains it.
        call sync_clip(self, none)
        call put(self, "</g>")
        call put_eol(self)
    end subroutine svg_end_group

end module fplotlib_backend_svg
