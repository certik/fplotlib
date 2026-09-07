! fplot_draw - turning a figure into a picture.
!
! Everything here reads the figure state and writes to a renderer: the layout
! of the axes on the canvas, the mapping from data to canvas coordinates, the
! ticks and their labels, and one renderer for each kind of series. Nothing
! here changes what is plotted, which is what keeps this apart from the
! plotting API in fplot.f90.
module fplot_draw
    use fplot_colors
    use fplot_style
    use fplot_scale
    use fplot_cmap
    use fplot_contour
    use fplot_tri, only: delaunay
    use fplot_ticks
    use fplot_svg
    use fplot_render
    use fplot_backend_svg
    use fplot_backend_pdf
    use fplot_backend_eps
    use fplot_gif, only: gif_encode
    use fplot_proj3d
    use fplot_backend_png
    use fplot_mathtext
    use fplot_dates
    use fplot_state
    use fplot_artist
    implicit none
    public

contains

    subroutine ensure_fig()
        if (.not. fig_initialized) call clf()
        if (cur_fig < 1) then
            cur_fig = next_free_fig()
            call grow_figs(cur_fig)
            figs(cur_fig)%live = .true.
        end if
        ! No axes yet: create a single full-figure axes (pylab default).
        if (cur_i < 1 .or. cur_i > n_ax) then
            call new_axes_grid(1, 1)
            cur_i = 1
        end if
    end subroutine ensure_fig

    subroutine grow_figs(k)
        integer, intent(in) :: k
        type(figure_t), allocatable :: tmp(:)
        integer :: i
        if (.not. allocated(figs)) allocate (figs(0))
        if (k <= size(figs)) return
        allocate (tmp(k))
        do i = 1, size(figs)
            tmp(i) = figs(i)
        end do
        call move_alloc(tmp, figs)
    end subroutine grow_figs

    ! Lowest number not currently in use, matching pyplot's figure numbering.
    function next_free_fig() result(k)
        integer :: k
        if (.not. allocated(figs)) then
            k = 1
            return
        end if
        do k = 1, size(figs)
            if (.not. figs(k)%live) return
        end do
        k = size(figs) + 1
    end function next_free_fig

    subroutine clf()
        cur_i = 0
        if (allocated(ax)) deallocate (ax)
        n_ax = 0
        grid_m = 0
        grid_n = 0
        grid_sparse = .false.
        fig_suptitle = ""
        fig_left = MARGIN_LEFT
        fig_right = MARGIN_RIGHT
        fig_bottom = MARGIN_BOTTOM
        fig_top = MARGIN_TOP
        fig_wspace = WSPACE
        fig_hspace = HSPACE
        fig_constrained = .false.
        fig_wratio = 1.0_dp
        fig_hratio = 1.0_dp
        def_title = TITLE_FONT
        def_label = LABEL_FONT
        def_tick = TICK_FONT
        def_legend = LEGEND_FONT
        fig_suptitle_size = SUPTITLE_FONT
        fig_suptitle_w = WEIGHT_NORMAL
        fig_suptitle_sl = SLANT_ROMAN
        fig_initialized = .true.
    end subroutine clf

    ! Replacing the grid discards existing axes.
    subroutine new_axes_grid(m, n)
        integer, intent(in) :: m, n
        integer :: i, r, c
        real(dp) :: w, h, dx, dy

        cur_i = 0
        if (allocated(ax)) deallocate (ax)

        n_ax = m * n
        allocate (ax(n_ax))
        do i = 1, n_ax
            call apply_font_defaults(ax(i))
            ax(i)%g_row = (i - 1) / n
            ax(i)%g_col = mod(i - 1, n)
        end do
        grid_m = m
        grid_n = n
        grid_sparse = .false.
        call layout_grid()
    end subroutine new_axes_grid

    ! Baseline of the x tick labels below the axes. Reduces to matplotlib's
    ! 16.0 at the default tick size and grows with the ascent of larger text.
    pure function xtick_gap(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = 8.4_dp + 0.76_dp * a%xtick_size
    end function xtick_gap

    subroutine apply_font_defaults(a)
        type(axes_t), intent(inout) :: a
        a%grid_on = rc_grid
        a%title_size = def_title
        a%xlabel_size = def_label
        a%ylabel_size = def_label
        a%xtick_size = def_tick
        a%ytick_size = def_tick
        a%legend_size = def_legend
    end subroutine apply_font_defaults

    ! The i-th color of the active cycle, wrapping. This is what "C0".."C9"
    ! resolve to as well, so a style changes both at once.
    pure function cycle_color(idx) result(col)
        integer, intent(in) :: idx
        character(len=7) :: col
        integer :: i
        if (rc_n_cycle <= 0) then
            col = color_from_C(idx)
        else
            i = mod(idx, rc_n_cycle)
            if (i < 0) i = i + rc_n_cycle
            col = rc_cycle(i + 1)
        end if
    end function cycle_color

    ! Place the existing axes in the current margins. Called again whenever
    ! those margins move, so the axes objects themselves survive.
    ! Where each column starts and how wide it is, in figure fractions.
    ! The gap between cells is the same everywhere, as it is in matplotlib:
    ! wspace and hspace are fractions of the average cell, not of each one,
    ! so uneven ratios change the cells and leave the gaps alone.
    subroutine grid_edges(nc, lo, hi, space, ratio, pos, len)
        integer, intent(in) :: nc
        real(dp), intent(in) :: lo, hi, space, ratio(MAX_RATIO)
        real(dp), intent(out) :: pos(MAX_RATIO), len(MAX_RATIO)
        real(dp) :: cell, sep, total, sumr, p
        integer :: k

        cell = (hi - lo)/(real(nc, dp) + space*real(nc - 1, dp))
        sep = space*cell
        total = cell*real(nc, dp)
        sumr = 0.0_dp
        do k = 1, nc
            sumr = sumr + max(ratio(k), 0.0_dp)
        end do
        if (sumr <= 0.0_dp) sumr = real(nc, dp)
        p = lo
        do k = 1, nc
            len(k) = total*max(ratio(k), 0.0_dp)/sumr
            pos(k) = p
            p = p + len(k) + sep
        end do
    end subroutine grid_edges

    subroutine layout_grid()
        integer :: i, r, c
        real(dp) :: xpos(MAX_RATIO), xlen(MAX_RATIO)
        real(dp) :: ypos(MAX_RATIO), ylen(MAX_RATIO)

        if (grid_m < 1 .or. grid_n < 1) return
        if (grid_m > MAX_RATIO .or. grid_n > MAX_RATIO) return

        call grid_edges(grid_n, fig_left, fig_right, fig_wspace, fig_wratio, xpos, xlen)
        ! Rows are laid out from the top, since that is how they are counted.
        call grid_edges(grid_m, 1.0_dp - fig_top, 1.0_dp - fig_bottom, fig_hspace, &
                        fig_hratio, ypos, ylen)

        do i = 1, n_ax
            if (ax(i)%fixed_pos .or. ax(i)%inset_of > 0) cycle
            ! Twins sit exactly on top of the axes they were made from.
            r = max(ax(i)%share_x, ax(i)%share_y)
            if (r >= 1) cycle
            r = ax(i)%g_row          ! row from the top
            c = ax(i)%g_col          ! column from the left
            ax(i)%left = xpos(c + 1)
            ax(i)%right = xpos(c + ax(i)%g_colspan) + xlen(c + ax(i)%g_colspan)
            ax(i)%top = 1.0_dp - ypos(r + 1)
            ax(i)%bottom = 1.0_dp - (ypos(r + ax(i)%g_rowspan) + ylen(r + ax(i)%g_rowspan))
        end do

        do i = 1, n_ax
            r = ax(i)%inset_of
            if (r < 1) cycle
            ax(i)%left = ax(r)%left + ax(i)%inset_rect(1)*(ax(r)%right - ax(r)%left)
            ax(i)%bottom = ax(r)%bottom + ax(i)%inset_rect(2)*(ax(r)%top - ax(r)%bottom)
            ax(i)%right = ax(i)%left + ax(i)%inset_rect(3)*(ax(r)%right - ax(r)%left)
            ax(i)%top = ax(i)%bottom + ax(i)%inset_rect(4)*(ax(r)%top - ax(r)%bottom)
        end do

        do i = 1, n_ax
            r = max(ax(i)%share_x, ax(i)%share_y)
            if (r < 1) cycle
            ax(i)%left = ax(r)%left
            ax(i)%right = ax(r)%right
            ax(i)%bottom = ax(r)%bottom
            ax(i)%top = ax(r)%top
        end do
    end subroutine layout_grid

    subroutine tight_layout(pad)
        real(dp), intent(in), optional :: pad
        real(dp) :: p, W, H, need_l, need_b, need_t, need_r, inner_l, inner_b
        real(dp) :: inner_t, gp
        integer :: i

        call ensure_fig()
        p = 1.08_dp * TICK_FONT
        ! constrained_layout pads by a flat 1/24 inch instead.
        if (fig_constrained) p = PT_PER_IN / 24.0_dp
        if (present(pad)) p = pad * TICK_FONT
        W = fig_w_in * PT_PER_IN
        H = fig_h_in * PT_PER_IN
        need_l = 0.0_dp
        need_b = 0.0_dp
        need_t = 0.0_dp
        need_r = 0.0_dp
        inner_l = 0.0_dp
        inner_b = 0.0_dp
        inner_t = 0.0_dp
        do i = 1, n_ax
            ! An axes the caller placed itself is not the grid's business.
            if (ax(i)%fixed_pos .or. ax(i)%inset_of > 0) cycle
            need_l = max(need_l, decor_left(ax(i)))
            need_b = max(need_b, decor_bottom(ax(i), W, H))
            need_t = max(need_t, decor_top(ax(i)))
            ! Only axes away from the left column and the bottom row put
            ! decorations into the gaps between subplots.
            if (ax(i)%g_col > 0) inner_l = max(inner_l, decor_left(ax(i)))
            if (ax(i)%g_row + ax(i)%g_rowspan < grid_m) &
                inner_b = max(inner_b, decor_bottom(ax(i), W, H))
            ! and the row below puts its title into the same gap.
            if (ax(i)%g_row > 0) inner_t = max(inner_t, decor_top(ax(i)))
        end do
        if (len_trim(fig_suptitle) > 0) need_t = need_t + LABEL_BOX * fig_suptitle_size

        fig_left = (p + need_l) / W
        fig_right = 1.0_dp - (p + need_r) / W
        fig_bottom = (p + need_b) / H
        fig_top = 1.0_dp - (p + need_t) / H

        ! Inner subplots carry the same decorations, so the gaps between them
        ! have to hold those decorations and the same pad again.
        ! constrained_layout pads both sides of a gap; tight_layout the one.
        gp = p
        if (fig_constrained) gp = 2.0_dp * p
        fig_wspace = spacing_for((gp + inner_l) / W, fig_right - fig_left, grid_n)
        fig_hspace = spacing_for((gp + inner_b + inner_t) / H, fig_top - fig_bottom, grid_m)
        call layout_grid()
    end subroutine tight_layout

    ! Fraction of a cell that leaves a gap of g between n cells spanning
    ! span, all in figure fractions.
    pure function spacing_for(g, span, n) result(f)
        real(dp), intent(in) :: g, span
        integer, intent(in) :: n
        real(dp) :: f, denom
        f = 0.0_dp
        if (n < 2) return
        denom = span - g * real(n - 1, dp)
        if (denom <= 0.0_dp) return
        f = g * real(n, dp) / denom
    end function spacing_for

    ! Point widths and heights of the decorations outside each axes edge.
    function decor_left(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = TICK_LEN + 2.0_dp + tick_label_width(a)
        ! The label is drawn a fixed distance out, so with narrow tick
        ! labels it reaches further than they do and it is what decides
        ! how much room the axes needs on its left.
        if (len_trim(a%ylabel) > 0) &
            v = ylabel_out(a) + 0.24_dp * a%ylabel_size
    end function decor_left

    ! Baseline of the y label, in points outside the axes. matplotlib
    ! hangs it off the tick labels, so a plot whose ticks read 1 to 7
    ! carries its label much closer in than one reading -1.00 to 1.00.
    function ylabel_out(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = TICK_LEN + 2.0_dp + tick_label_width(a) + a%ylabel_pad &
            + 0.76_dp * a%ylabel_size
    end function ylabel_out

    function decor_bottom(a, W, H) result(v)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: W, H
        real(dp) :: v, bx, by, bw, bh
        v = TICK_LEN + 1.0_dp + 1.15_dp * a%xtick_size
        ! A horizontal colorbar hangs below the axes, with tick labels of
        ! its own under that.
        if (a%cbar_on .and. a%cbar_horiz) then
            call cbar_box(a, W, H, bx, by, bw, bh)
            v = v + by + bh - (1.0_dp - a%bottom) * H &
                + xtick_gap(a) + 0.4_dp * a%xtick_size
            if (len_trim(a%cbar_label) > 0) v = v + LABEL_BOX * a%xlabel_size
        end if
        if (a%xtick_rot /= 0.0_dp) v = v + tick_label_width(a) * &
                                        abs(sin(a%xtick_rot * PI / 180.0_dp))
        if (len_trim(a%xlabel) > 0) &
            v = v + LABEL_BOX * a%xlabel_size + a%xlabel_pad - LABEL_PAD
    end function decor_bottom

    ! How far the decorations reach to the right of the axes box. Only a
    ! colorbar and its labels live out there.
    function decor_right(a, W, H) result(v)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: W, H
        real(dp) :: v, bx, by, bw, bh
        v = 0.0_dp
        if (.not. a%cbar_on) return
        if (a%cbar_horiz) return
        call cbar_box(a, W, H, bx, by, bw, bh)
        v = bx + bw - a%right * W + 7.0_dp
        if (len_trim(a%cbar_label) > 0) then
            v = v + 34.0_dp
        else
            v = v + 4.0_dp * a%ytick_size
        end if
    end function decor_right

    ! Bounding box of everything actually drawn, in canvas points. This is
    ! what savefig(bbox_inches="tight") crops to, and it is built from the
    ! same decoration estimates that tight_layout uses.
    subroutine drawn_bbox(W, H, x0, y0, x1, y1)
        real(dp), intent(in) :: W, H
        real(dp), intent(out) :: x0, y0, x1, y1
        integer :: i
        real(dp) :: l, r, t, bt
        x0 = W
        y0 = H
        x1 = 0.0_dp
        y1 = 0.0_dp
        do i = 1, n_ax
            l = ax(i)%left * W - decor_left(ax(i))
            r = ax(i)%right * W + decor_right(ax(i), W, H)
            t = (1.0_dp - ax(i)%top) * H - decor_top(ax(i))
            bt = (1.0_dp - ax(i)%bottom) * H + decor_bottom(ax(i), W, H)
            x0 = min(x0, l)
            x1 = max(x1, r)
            y0 = min(y0, t)
            y1 = max(y1, bt)
        end do
        if (len_trim(fig_suptitle) > 0) &
            y0 = min(y0, (1.0_dp - SUPTITLE_Y) * H + 4.2_dp - fig_suptitle_size)
        if (x1 <= x0 .or. y1 <= y0) then
            x0 = 0.0_dp
            y0 = 0.0_dp
            x1 = W
            y1 = H
        end if
    end subroutine drawn_bbox

    function decor_top(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v
        v = 0.0_dp
        if (len_trim(a%title) > 0) v = LABEL_BOX * a%title_size
    end function decor_top

    ! Widest tick label an axes will draw, in points. Tick text is digits,
    ! a sign and a point, all of which are the same width in DejaVu Sans.
    function tick_label_width(a) result(v)
        type(axes_t), intent(in) :: a
        real(dp) :: v, xmin, xmax, ymin, ymax
        real(dp) :: t(MAX_TICKS)
        character(len=64) :: lbl
        integer :: nt, i, ln, du
        v = 0.0_dp
        call compute_limits(a, xmin, xmax, ymin, ymax)
        call axis_ticks(a%n_yticks, a%ytick_pos, min(ymin, ymax), max(ymin, ymax), &
                        a%ysc, nbins_for(a%ytick_nbins, 0.0_dp, a%ytick_size, .false.), &
                        a%y_date, t, nt, du, a%ytick_base)
        do i = 1, nt
            call tick_label(a%ytick_labeled, a%ytick_lab, i, t(i), a%ysc, &
                            axis_fmt(t, nt, a, .false., min(ymin, ymax), &
                                     max(ymin, ymax)), du, lbl, ln)
            if (math_is(lbl(1:ln))) then
                v = max(v, math_width(lbl(1:ln), a%ytick_size))
            else
                v = max(v, real(ln, dp) * DIGIT_W * a%ytick_size)
            end if
        end do
    end function tick_label_width

    ! Whether an artist shows up in the legend at all. matplotlib keeps
    ! out anything whose label starts with an underscore, which is how an
    ! artist that must carry a name for other reasons stays out of it.
    pure function in_legend(s) result(yes)
        type(series_t), intent(in) :: s
        logical :: yes
        yes = len_trim(s%label) > 0
        if (yes) yes = s%label(1:1) /= "_"
    end function in_legend

    ! Drop the tick at one or both ends of a located set.
    pure subroutine prune_ticks(prune, t, n)
        character(len=*), intent(in) :: prune
        real(dp), intent(inout) :: t(:)
        integer, intent(inout) :: n
        if (n <= 0) return
        select case (trim(prune))
        case ("lower", "both")
            t(1:n - 1) = t(2:n)
            n = n - 1
        end select
        if (n <= 0) return
        select case (trim(prune))
        case ("upper", "both")
            n = n - 1
        end select
    end subroutine prune_ticks

    ! Every spelling of a color matplotlib accepts: "#rgb", "#rrggbb",
    ! "#rrggbbaa", a CSS4/X11 or "tab:" name, a single-letter code, a "C<n>"
    ! cycle index, or a greyscale fraction such as "0.5". Returns an empty
    ! string if the color is not recognised, which lets callers fall back to
    ! the cycle. An "#rrggbbaa" alpha comes back through alpha_out.
    function resolve_color(color, alpha_out) result(col)
        character(len=*), intent(in), optional :: color
        real(dp), intent(out), optional :: alpha_out
        character(len=7) :: col
        character(len=:), allocatable :: c
        integer :: m, n, ios
        real(dp) :: g

        col = ""
        if (present(alpha_out)) alpha_out = -1.0_dp
        if (.not. present(color)) return
        if (len_trim(color) == 0) return
        c = trim(adjustl(color))
        n = len(c)

        if (c(1:1) == "#") then
            select case (n)
            case (4)
                ! "#rgb" is shorthand for "#rrggbb" with each digit doubled.
                col = "#" // c(2:2) // c(2:2) // c(3:3) // c(3:3) // c(4:4) // c(4:4)
            case (7)
                col = c
            case (9)
                col = c(1:7)
                if (present(alpha_out)) alpha_out = real(hex_byte(c(8:9)), dp) / 255.0_dp
            end select
            return
        end if

        if (n >= 2 .and. c(1:1) == "C") then
            m = -1
            read (c(2:n), *, iostat=ios) m
            if (ios == 0 .and. m >= 0) then
                col = cycle_color(m)
                return
            end if
        end if

        if (n == 1) then
            col = color_from_char(c(1:1))
            if (len_trim(col) > 0) return
        end if

        col = color_from_name(c)
        if (len_trim(col) > 0) return

        ! A bare number is a shade of grey, "0" black through "1" white.
        read (c, *, iostat=ios) g
        if (ios == 0 .and. g >= 0.0_dp .and. g <= 1.0_dp) then
            m = nint(g * 255.0_dp)
            col = "#" // hex_pair(m) // hex_pair(m) // hex_pair(m)
        end if
    end function resolve_color

    ! The colour of one image sample as a hex string, for the paths that
    ! draw the image as rectangles rather than as a raster.
    pure function img_hex(a, i, j) result(hex)
        type(axes_t), intent(in) :: a
        integer, intent(in) :: i, j
        character(len=7) :: hex
        character(len=16), parameter :: D = "0123456789abcdef"
        integer :: c(4), k
        if (.not. a%has_rgb) then
            hex = img_color(a, a%img(i, j))
            return
        end if
        call img_rgba(a, i, j, c)
        hex = "#000000"
        do k = 1, 3
            hex(2*k:2*k) = D(c(k)/16 + 1:c(k)/16 + 1)
            hex(2*k + 1:2*k + 1) = D(mod(c(k), 16) + 1:mod(c(k), 16) + 1)
        end do
    end function img_hex

    ! The colour of one image sample, ready for the raster.
    pure subroutine img_rgba(a, i, j, c)
        type(axes_t), intent(in) :: a
        integer, intent(in) :: i, j
        integer, intent(out) :: c(4)
        c(4) = 255
        if (.not. a%has_rgb) then
            if (img_bad_hidden(a, a%img(i, j))) then
                ! matplotlib leaves missing samples fully transparent
                ! unless a colour was set aside for them.
                c = [255, 255, 255, 0]
                return
            end if
        end if
        if (a%has_rgb) then
            c(1:3) = nint(255.0_dp*a%img_rgb(i, j, 1:3))
            if (size(a%img_rgb, 3) >= 4) c(4) = nint(255.0_dp*a%img_rgb(i, j, 4))
        else
            c(1:3) = hex_rgb(img_color(a, a%img(i, j)))
        end if
    end subroutine img_rgba

    ! Linear-interpolated quantile of an already sorted sample, matching the
    ! default of numpy.percentile.
    pure function quantile(v, q) result(r)
        real(dp), intent(in) :: v(:), q
        real(dp) :: r, h
        integer :: lo
        h = q * real(size(v) - 1, dp)
        lo = min(max(int(floor(h)), 0), size(v) - 1)
        if (lo + 1 >= size(v)) then
            r = v(size(v))
        else
            r = v(lo + 1) + (h - real(lo, dp)) * (v(lo + 2) - v(lo + 1))
        end if
    end function quantile

    ! Where the bar is drawn, in canvas points. The axes was already shrunk
    ! to make room for it, and the fractions are measured against the box it
    ! was cut from, so that box has to be recovered first.
    subroutine cbar_box(a, W, H, bx, by, bw, bh)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: W, H
        real(dp), intent(out) :: bx, by, bw, bh
        real(dp) :: keep, l0, w0, b0, h0, full

        keep = 1.0_dp - a%cbar_frac - a%cbar_pad
        if (keep < 0.05_dp) keep = 0.05_dp
        if (a%cbar_horiz) then
            h0 = (a%top - a%bottom) / keep
            b0 = a%top - h0
            full = (a%right - a%left) * W
            bw = a%cbar_shrink * full
            bx = a%left * W + 0.5_dp * (full - bw)
            bh = bw / a%cbar_aspect
            ! The bar hangs from the top of the strip it was given.
            by = H - (b0 + a%cbar_frac * h0) * H
        else
            l0 = a%left * W
            w0 = (a%right * W - l0) / keep
            full = (a%top - a%bottom) * H
            bh = a%cbar_shrink * full
            bx = l0 + (1.0_dp - a%cbar_frac) * w0
            by = (1.0_dp - a%top) * H + 0.5_dp * (full - bh)
            bw = bh / a%cbar_aspect
        end if
    end subroutine cbar_box

    ! Half the width of bar j, which uneven histogram bins make per-bar.
    pure function bar_hw(s, j) result(v)
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp) :: v
        v = 0.5_dp * s%width
        if (allocated(s%bwidth)) v = 0.5_dp * s%bwidth(j)
    end function bar_hw

    recursive subroutine compute_limits(a, xmin, xmax, ymin, ymax)
        type(axes_t), intent(in) :: a
        real(dp), intent(out) :: xmin, xmax, ymin, ymax
        integer :: i, j
        real(dp) :: xv, yv, ylo, yhi, xlo, xhi, dx, dy, hw
        real(dp) :: pxlo, pxhi, pylo, pyhi
        logical :: anyx, anyy, sticky_lo, sticky_hi, sx_lo, sx_hi

        anyx = .false.
        anyy = .false.
        sticky_lo = .false.
        sticky_hi = .false.
        sx_lo = .false.
        sx_hi = .false.
        xmin = huge(1.0_dp)
        xmax = -huge(1.0_dp)
        ymin = huge(1.0_dp)
        ymax = -huge(1.0_dp)
        pxlo = huge(1.0_dp)
        pxhi = -huge(1.0_dp)
        pylo = huge(1.0_dp)
        pyhi = -huge(1.0_dp)

        do i = 1, a%n_series
            ! Bars occupy a span in x; a hline/vline constrains one axis only.
            hw = 0.0_dp
            if (a%series(i)%kind == SERIES_BAR) hw = 0.5_dp * a%series(i)%width

            ! A pie sets its own limits, and horizontal bars use the two axes
            ! the other way round, so neither fits the loop below.
            if (a%series(i)%kind == SERIES_PIE) cycle
            ! An endless line has no extent of its own: it is drawn to
            ! whatever the rest of the plot decided the limits are.
            if (a%series(i)%kind == SERIES_AXLINE) cycle
            ! A band drawn along y reads the same three arrays with the
            ! axes the other way round.
            if (a%series(i)%kind == SERIES_FILL .and. a%series(i)%horiz) then
                do j = 1, a%series(i)%n
                    if (.not. (finite(a%series(i)%x(j)) .and. &
                               finite(a%series(i)%y(j)) .and. &
                               finite(a%series(i)%y2(j)))) cycle
                    anyx = .true.
                    anyy = .true.
                    xmin = min(xmin, a%series(i)%y(j), a%series(i)%y2(j))
                    xmax = max(xmax, a%series(i)%y(j), a%series(i)%y2(j))
                    ymin = min(ymin, a%series(i)%x(j))
                    ymax = max(ymax, a%series(i)%x(j))
                end do
                cycle
            end if
            ! A patch widens the limits but never asks for any of its own:
            ! matplotlib leaves an axes holding nothing but patches at its
            ! default square, and only stretches to them once something
            ! else has autoscaled it.
            if (a%series(i)%kind == SERIES_PATCH .and. .not. a%series(i)%patch_scales) then
                do j = 1, a%series(i)%n
                    pxlo = min(pxlo, a%series(i)%x(j))
                    pxhi = max(pxhi, a%series(i)%x(j))
                    pylo = min(pylo, a%series(i)%y(j))
                    pyhi = max(pyhi, a%series(i)%y(j))
                end do
                cycle
            end if
            if ((a%series(i)%kind == SERIES_BOX .or. &
                 a%series(i)%kind == SERIES_VIOLIN) .and. &
                .not. a%series(i)%box_vert) then
                ! Laid across, the two axes trade jobs.
                anyy = .true.
                sticky_lo = .true.
                sticky_hi = .true.
                ymin = min(ymin, a%series(i)%pos - 0.5_dp)
                ymax = max(ymax, a%series(i)%pos + 0.5_dp)
                do j = 1, a%series(i)%n
                    anyx = .true.
                    xmin = min(xmin, a%series(i)%y(j))
                    xmax = max(xmax, a%series(i)%y(j))
                end do
                cycle
            end if
            if (a%series(i)%kind == SERIES_BOX .or. &
                a%series(i)%kind == SERIES_VIOLIN) then
                anyx = .true.
                if (a%series(i)%kind == SERIES_BOX) then
                    ! matplotlib pins the category axis half a slot beyond the
                    ! outermost box, with no extra margin. A violin instead
                    ! autoscales to its own body like any other artist.
                    sx_lo = .true.
                    sx_hi = .true.
                    xmin = min(xmin, a%series(i)%pos - 0.5_dp)
                    xmax = max(xmax, a%series(i)%pos + 0.5_dp)
                else
                    xmin = min(xmin, a%series(i)%pos - 0.5_dp * a%series(i)%width)
                    xmax = max(xmax, a%series(i)%pos + 0.5_dp * a%series(i)%width)
                end if
                do j = 1, a%series(i)%n
                    anyy = .true.
                    ymin = min(ymin, a%series(i)%y(j))
                    ymax = max(ymax, a%series(i)%y(j))
                end do
                cycle
            end if
            ! A span constrains only its own axis; the other pair is a
            ! fraction of the axes, which cannot ask for any data room.
            if (a%series(i)%kind == SERIES_HSPAN) then
                anyy = .true.
                ymin = min(ymin, a%series(i)%y(1), a%series(i)%y(2))
                ymax = max(ymax, a%series(i)%y(1), a%series(i)%y(2))
                cycle
            end if
            if (a%series(i)%kind == SERIES_VSPAN) then
                anyx = .true.
                xmin = min(xmin, a%series(i)%x(1), a%series(i)%x(2))
                xmax = max(xmax, a%series(i)%x(1), a%series(i)%x(2))
                cycle
            end if
            if (a%series(i)%kind == SERIES_HLINES .or. &
                a%series(i)%kind == SERIES_VLINES) then
                do j = 1, a%series(i)%n
                    anyx = .true.
                    anyy = .true.
                    xmin = min(xmin, a%series(i)%x(j))
                    xmax = max(xmax, a%series(i)%x(j))
                    ymin = min(ymin, a%series(i)%y(j))
                    ymax = max(ymax, a%series(i)%y(j))
                    if (a%series(i)%kind == SERIES_HLINES) then
                        xmin = min(xmin, a%series(i)%y2(j))
                        xmax = max(xmax, a%series(i)%y2(j))
                    else
                        ymin = min(ymin, a%series(i)%y2(j))
                        ymax = max(ymax, a%series(i)%y2(j))
                    end if
                end do
                cycle
            end if
            if (a%series(i)%kind == SERIES_BARH) then
                do j = 1, a%series(i)%n
                    hw = bar_hw(a%series(i), j)
                    anyx = .true.
                    anyy = .true.
                    xmin = min(xmin, bar_base(a%series(i), j), &
                               bar_base(a%series(i), j) + a%series(i)%y(j))
                    xmax = max(xmax, bar_base(a%series(i), j), &
                               bar_base(a%series(i), j) + a%series(i)%y(j))
                    ymin = min(ymin, a%series(i)%x(j) - hw)
                    ymax = max(ymax, a%series(i)%x(j) + hw)
                end do
                if (xmin >= 0.0_dp) sx_lo = .true.
                if (xmax <= 0.0_dp) sx_hi = .true.
                cycle
            end if

            do j = 1, a%series(i)%n
                ! Missing data does not stretch the axes to infinity.
                if (a%series(i)%kind /= SERIES_HLINE) then
                    if (.not. finite(a%series(i)%x(j))) cycle
                end if
                if (a%series(i)%kind /= SERIES_VLINE) then
                    if (.not. finite(a%series(i)%y(j))) cycle
                end if
                ! A band whose other edge is missing is missing here too,
                ! and a NaN left in the comparisons below would poison the
                ! limits for every other point as well.
                if (a%series(i)%kind == SERIES_FILL) then
                    if (.not. finite(a%series(i)%y2(j))) cycle
                end if
                if (a%series(i)%kind /= SERIES_HLINE) then
                    if (a%series(i)%kind == SERIES_BAR) hw = bar_hw(a%series(i), j)
                    xv = a%series(i)%x(j)
                    xlo = xv - hw
                    xhi = xv + hw
                    if (a%series(i)%kind == SERIES_ERRORBAR) then
                        xlo = xv - arm(a%series(i)%exlo, j)
                        xhi = xv + arm(a%series(i)%exhi, j)
                    end if
                    if (a%xsc%kind == SCALE_LOG .and. xlo <= 0.0_dp) xlo = xhi
                    if (.not. (a%xsc%kind == SCALE_LOG .and. xlo <= 0.0_dp)) then
                        anyx = .true.
                        xmin = min(xmin, xlo)
                        xmax = max(xmax, xhi)
                    end if
                end if

                if (a%series(i)%kind /= SERIES_VLINE) then
                    yv = a%series(i)%y(j)
                    ylo = yv
                    yhi = yv
                    select case (a%series(i)%kind)
                    case (SERIES_BAR)
                        ! Bars are drawn from the y=0 baseline, or from the
                        ! series they were stacked on.
                        ylo = min(bar_base(a%series(i), j), bar_base(a%series(i), j) + yv)
                        yhi = max(bar_base(a%series(i), j), bar_base(a%series(i), j) + yv)
                        ylo = min(ylo, bar_base(a%series(i), j) + yv &
                                  - arm(a%series(i)%eylo, j))
                        yhi = max(yhi, bar_base(a%series(i), j) + yv &
                                  + arm(a%series(i)%eyhi, j))
                    case (SERIES_FILL)
                        ylo = min(yv, a%series(i)%y2(j))
                        yhi = max(yv, a%series(i)%y2(j))
                    case (SERIES_ERRORBAR)
                        ylo = yv - arm(a%series(i)%eylo, j)
                        yhi = yv + arm(a%series(i)%eyhi, j)
                    end select
                    ! On a log axis a bar reaching down to zero has no
                    ! bottom to speak of, so only its top counts towards the
                    ! limits.
                    if (a%ysc%kind == SCALE_LOG .and. ylo <= 0.0_dp) ylo = yhi
                    if (.not. (a%ysc%kind == SCALE_LOG .and. yhi <= 0.0_dp)) then
                        anyy = .true.
                        ymin = min(ymin, ylo)
                        ymax = max(ymax, yhi)
                    end if
                end if
            end do

            ! Matplotlib treats the bar baseline as a sticky edge: no margin
            ! is added on the side the bars grow from.
            if (a%series(i)%kind == SERIES_BAR .and. a%series(i)%n > 0) then
                if (ymin >= 0.0_dp) sticky_lo = .true.
                if (ymax <= 0.0_dp) sticky_hi = .true.
            end if
        end do

        if (a%has_cont .or. a%tight_lim) then
            anyx = .true.
            anyy = .true.
            xmin = min(xmin, a%cont_ext(1))
            xmax = max(xmax, a%cont_ext(2))
            ymin = min(ymin, a%cont_ext(3))
            ymax = max(ymax, a%cont_ext(4))
        end if

        if (a%has_img) then
            anyx = .true.
            anyy = .true.
            xmin = min(xmin, a%img_ext(1))
            xmax = max(xmax, a%img_ext(2))
            ymin = min(ymin, a%img_ext(3))
            ymax = max(ymax, a%img_ext(4))
        end if

        if (anyx .and. pxhi >= pxlo) then
            xmin = min(xmin, pxlo)
            xmax = max(xmax, pxhi)
        end if
        if (anyy .and. pyhi >= pylo) then
            ymin = min(ymin, pylo)
            ymax = max(ymax, pyhi)
        end if
        if (a%yroom_set) then
            anyy = .true.
            ymin = min(ymin, a%yroom(1))
            ymax = max(ymax, a%yroom(2))
            if (a%room_sticks) then
                sticky_lo = .true.
                sticky_hi = .true.
            end if
        end if
        if (a%xroom_set) then
            anyx = .true.
            xmin = min(xmin, a%xroom(1))
            xmax = max(xmax, a%xroom(2))
            if (a%room_sticks) then
                sx_lo = .true.
                sx_hi = .true.
            end if
        end if

        if (a%xlim_set) then
            xmin = a%xmin_user
            xmax = a%xmax_user
        else
            ! An axes with nothing to autoscale to keeps the plain unit
            ! square matplotlib gives it, margins and all.
            if (anyx) then
                call expand_limits(xmin, xmax, a%xsc, a%xmargin, sx_lo, sx_hi)
            else
                xmin = 0.0_dp
                xmax = 1.0_dp
            end if
        end if

        if (a%ylim_set) then
            ymin = a%ymin_user
            ymax = a%ymax_user
        else
            if (anyy) then
                call expand_limits(ymin, ymax, a%ysc, a%ymargin, sticky_lo, sticky_hi)
            else
                ymin = 0.0_dp
                ymax = 1.0_dp
            end if
        end if

        if (a%has_cont .or. a%tight_lim) then
            if (.not. a%xlim_set) then
                xmin = a%cont_ext(1)
                xmax = a%cont_ext(2)
            end if
            if (.not. a%ylim_set) then
                ymin = a%cont_ext(3)
                ymax = a%cont_ext(4)
            end if
        end if

        ! An image fits its extent exactly, and origin="upper" puts the first
        ! row at the top, which matplotlib expresses as a descending y axis.
        if (a%has_img) then
            if (.not. a%xlim_set) then
                xmin = a%img_ext(1)
                xmax = a%img_ext(2)
            end if
            if (.not. a%ylim_set) then
                ymin = a%img_ext(3)
                ymax = a%img_ext(4)
                if (a%img_origin_upper .and. .not. a%has_mesh) call swap(ymin, ymax)
            end if
        end if
        ! A twin borrows the shared axis wholesale, so the two sets of data
        ! stay registered against each other.
        if (a%sec_of > 0) then
            call compute_limits(ax(a%sec_of), xlo, xhi, ylo, yhi)
            if (a%sec_is_x) then
                xmin = a%sec_scale*xlo + a%sec_offset
                xmax = a%sec_scale*xhi + a%sec_offset
            else
                ymin = a%sec_scale*ylo + a%sec_offset
                ymax = a%sec_scale*yhi + a%sec_offset
            end if
        end if
        if (a%share_x > 0) call compute_limits(ax(a%share_x), xmin, xmax, ylo, yhi)
        if (a%share_y > 0) call compute_limits(ax(a%share_y), xlo, xhi, ymin, ymax)
        ! A share group spans everything its members hold, so the panels line
        ! up and a feature at one x is at the same place in all of them.
        if (a%link_x > 0) call union_limits(a%link_x, .true., xmin, xmax)
        if (a%link_y > 0) call union_limits(a%link_y, .false., ymin, ymax)
        ! An inverted axis is simply one whose limits run the other way; the
        ! data-to-device mapping and the tick locators already cope, since
        ! origin="upper" images have always produced a descending y.
        if (a%x_inv) call swap(xmin, xmax)
        if (a%y_inv) call swap(ymin, ymax)
    end subroutine compute_limits

    ! The limits of one axis across a share group. Each member is measured
    ! with its own link cleared, which is what stops this coming back here.
    recursive subroutine union_limits(root, is_x, lo, hi)
        integer, intent(in) :: root
        logical, intent(in) :: is_x
        real(dp), intent(inout) :: lo, hi
        type(axes_t) :: m
        real(dp) :: x0, x1, y0, y1
        integer :: i

        do i = 1, n_ax
            if (is_x) then
                if (ax(i)%link_x /= root) cycle
            else
                if (ax(i)%link_y /= root) cycle
            end if
            m = ax(i)
            m%link_x = 0
            m%link_y = 0
            call compute_limits(m, x0, x1, y0, y1)
            if (is_x) then
                lo = min(lo, x0)
                hi = max(hi, x1)
            else
                lo = min(lo, y0)
                hi = max(hi, y1)
            end if
        end do
    end subroutine union_limits

    ! Whether a value can be drawn at all. matplotlib treats NaN and
    ! infinity as missing data: the point is skipped, the line is broken
    ! there, and neither takes part in setting the limits.
    pure function finite(v) result(ok)
        real(dp), intent(in) :: v
        logical :: ok
        ok = (v == v) .and. abs(v) <= huge(1.0_dp)
    end function finite

    pure subroutine grow_about_centre(lo, hi, f)
        real(dp), intent(inout) :: lo, hi
        real(dp), intent(in) :: f
        real(dp) :: c, h
        c = 0.5_dp * (lo + hi)
        h = 0.5_dp * (hi - lo) * f
        lo = c - h
        hi = c + h
    end subroutine grow_about_centre

    pure subroutine swap(u, v)
        real(dp), intent(inout) :: u, v
        real(dp) :: t
        t = u
        u = v
        v = t
    end subroutine swap

    ! Pad a data range by the axis margin. A sticky edge (the bar
    ! baseline) is left exactly where it is.
    subroutine expand_limits(lo, hi, sc, margin, sticky_lo, sticky_hi)
        real(dp), intent(inout) :: lo, hi
        type(scale_t), intent(in) :: sc
        real(dp), intent(in) :: margin
        logical, intent(in) :: sticky_lo, sticky_hi
        real(dp) :: u0, u1, d

        if (sc%kind == SCALE_LOG) then
            if (lo <= 0.0_dp) lo = tiny(1.0_dp)
            if (hi <= lo) hi = lo * 10.0_dp
        end if

        if (margin <= 0.0_dp) return
        ! The margin is a fraction of the drawn length, so it has to be
        ! measured in transformed space; on a linear axis that is the same
        ! thing.
        u0 = scale_fwd(sc, lo)
        u1 = scale_fwd(sc, hi)
        d = u1 - u0
        if (abs(d) < 1.0e-30_dp) d = 1.0_dp
        if (.not. sticky_lo) lo = scale_inv(sc, u0 - margin * d)
        if (.not. sticky_hi) hi = scale_inv(sc, u1 + margin * d)
    end subroutine expand_limits

    pure function map_x(x, xmin, xmax, ax_l, ax_w, sc) result(px)
        real(dp), intent(in) :: x, xmin, xmax, ax_l, ax_w
        type(scale_t), intent(in) :: sc
        real(dp) :: px
        px = ax_l + axis_frac(x, xmin, xmax, sc) * ax_w
    end function map_x

    pure function map_y(y, ymin, ymax, ax_b, ax_h, sc) result(py)
        real(dp), intent(in) :: y, ymin, ymax, ax_b, ax_h
        type(scale_t), intent(in) :: sc
        real(dp) :: py
        py = ax_b - axis_frac(y, ymin, ymax, sc) * ax_h
    end function map_y

    ! Where v sits along the axis, as a fraction from the low end. Every
    ! scale is linear once the values have gone through its transform.
    pure function axis_frac(v, vmin, vmax, sc) result(t)
        real(dp), intent(in) :: v, vmin, vmax
        type(scale_t), intent(in) :: sc
        real(dp) :: t, u0, u1
        u0 = scale_fwd(sc, vmin)
        u1 = scale_fwd(sc, vmax)
        if (u1 == u0) then
            t = 0.5_dp
        else
            t = (scale_fwd(sc, v) - u0) / (u1 - u0)
        end if
    end function axis_frac

    pure function int_to_str(i) result(s)
        integer, intent(in) :: i
        character(len=:), allocatable :: s
        character(len=12) :: tmp
        write (tmp, "(I0)") i
        s = trim(tmp)
    end function int_to_str

    ! A shaded band: one pair of coordinates is data, the other a fraction
    ! of the axes box.
    subroutine append_span(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: xa, xb, ya, yb

        if (s%kind == SERIES_HSPAN) then
            xa = ax_l + s%x(1)*ax_w
            xb = ax_l + s%x(2)*ax_w
            ya = map_y(s%y(1), ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(s%y(2), ymin, ymax, ax_b, ax_h, ysc)
        else
            xa = map_x(s%x(1), xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(s%x(2), xmin, xmax, ax_l, ax_w, xsc)
            ya = ax_b - s%y(1)*ax_h
            yb = ax_b - s%y(2)*ax_h
        end if
        call append_rect(b, min(xa, xb), min(ya, yb), abs(xb - xa), &
                         abs(yb - ya), trim(s%color), s%alpha)
    end subroutine append_span

    ! One bar of a bar/hist series, drawn from the y = 0 baseline.
    subroutine append_bar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: xa, xb, ya, yb, hw

        hw = 0.5_dp * s%width
        if (allocated(s%bwidth)) hw = 0.5_dp * s%bwidth(j)
        if (s%kind == SERIES_BARH) then
            ! x holds the bar position and y its length, so the roles of the
            ! two axes are simply swapped.
            ya = map_y(s%x(j) - hw, ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(s%x(j) + hw, ymin, ymax, ax_b, ax_h, ysc)
            xa = map_x(bar_base(s, j), xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(bar_base(s, j) + s%y(j), xmin, xmax, ax_l, ax_w, xsc)
        else
            xa = map_x(s%x(j) - hw, xmin, xmax, ax_l, ax_w, xsc)
            xb = map_x(s%x(j) + hw, xmin, xmax, ax_l, ax_w, xsc)
            ya = map_y(bar_base(s, j), ymin, ymax, ax_b, ax_h, ysc)
            yb = map_y(bar_base(s, j) + s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        end if

        if (s%edgewidth > 0.0_dp) then
            call append_rect(b, min(xa, xb), min(ya, yb), abs(xb - xa), &
                             abs(yb - ya), point_color(s, j), s%alpha, &
                             trim(s%edgecolor), s%edgewidth)
        else
            call append_rect(b, min(xa, xb), min(ya, yb), abs(xb - xa), &
                             abs(yb - ya), point_color(s, j), s%alpha)
        end if
        if (len_trim(s%hatch) > 0) &
            call append_hatch(b, [xa, xb, xb, xa], [ya, ya, yb, yb], 4, &
                              trim(s%hatch), trim(s%hcolor), s%alpha)
        if (s%bar_labels) call append_bar_label(b, s, j, xa, xb, ya, yb)
        if (allocated(s%eyhi)) call append_bar_err(b, s, j, ymin, ymax, ax_b, &
                                                   ax_h, ysc, 0.5_dp*(xa + xb))
    end subroutine append_bar

    ! The error bar on top of a bar. It is centred on the end of the bar,
    ! which is where the value is, not on the value axis origin.
    subroutine append_bar_err(b, s, j, ymin, ymax, ax_b, ax_h, ysc, px)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: ymin, ymax, ax_b, ax_h, px
        type(scale_t), intent(in) :: ysc
        real(dp) :: top, plo, phi, cap

        top = bar_base(s, j) + s%y(j)
        plo = map_y(top - arm(s%eylo, j), ymin, ymax, ax_b, ax_h, ysc)
        phi = map_y(top + arm(s%eyhi, j), ymin, ymax, ax_b, ax_h, ysc)
        call append_line(b, px, plo, px, phi, trim(s%ecolor), s%elw, LINE_SOLID, 1.0_dp)
        cap = s%ecap
        if (cap <= 0.0_dp) return
        call append_line(b, px - cap, plo, px + cap, plo, trim(s%ecolor), &
                         s%elw, LINE_SOLID, 1.0_dp)
        call append_line(b, px - cap, phi, px + cap, phi, trim(s%ecolor), &
                         s%elw, LINE_SOLID, 1.0_dp)
    end subroutine append_bar_err

    ! Where a bar starts: y = 0 unless the series was stacked on another.
    pure function bar_base(s, j) result(v)
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp) :: v
        v = 0.0_dp
        if (allocated(s%y2)) v = s%y2(j)
    end function bar_base

    ! The value written at the end of a bar. The padding is in points, which
    ! is why this waits until the bar has been placed on the canvas.
    subroutine append_bar_label(b, s, j, xa, xb, ya, yb)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xa, xb, ya, yb
        character(len=32) :: txt
        real(dp) :: tx, ty, fs
        integer :: tn

        txt = ""
        fs = s%bar_label_size
        if (len_trim(s%bar_fmt) > 0) then
            write (txt, s%bar_fmt) s%y(j)
        else
            call format_tick_to(s%y(j), .false., txt, tn)
        end if
        if (s%kind == SERIES_BARH) then
            ! Just past the end of the bar, vertically centred on it.
            tx = xb + s%bar_pad + 1.0_dp
            if (xb < xa) tx = xb - s%bar_pad - 1.0_dp
            ty = 0.5_dp*(ya + yb) + 0.36_dp*fs
            if (xb < xa) then
                call append_text(b, tx, ty, trim(adjustl(txt)), "right", fs, rc_text_color)
            else
                call append_text(b, tx, ty, trim(adjustl(txt)), "left", fs, rc_text_color)
            end if
        else
            ! Above the top of the bar, or below it for a bar that hangs
            ! down. 0.21 em is the descent of the font, which is what puts
            ! the bottom of the text, not its baseline, at the padding.
            ty = min(ya, yb) - s%bar_pad - 0.21_dp*fs
            if (yb > ya) ty = max(ya, yb) + s%bar_pad + 0.73_dp*fs
            tx = 0.5_dp*(xa + xb)
            call append_text(b, tx, ty, trim(adjustl(txt)), "center", fs, rc_text_color)
        end if
    end subroutine append_bar_label

    ! Shaded region between y and y2, as a single closed polygon.
    ! A patch: filled first, then outlined, so the outline sits on top of
    ! its own fill exactly as it does in matplotlib.
    subroutine append_patch(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: px(s%n + 1), py(s%n + 1)
        type(paint_t) :: p
        integer :: j

        if (allocated(s%pverb)) then
            do j = 1, s%n
                px(j) = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
                py(j) = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
            end do
            if (s%patch_fill) then
                p = brush(trim(s%color), s%alpha)
                call b%draw_path(px(1:s%n), py(1:s%n), s%pverb, size(s%pverb), p)
            end if
            if (len_trim(s%edgecolor) > 0) then
                p = pen(trim(s%edgecolor), s%edgewidth, s%alpha)
                call b%draw_path(px(1:s%n), py(1:s%n), s%pverb, size(s%pverb), p)
            end if
            return
        end if

        if (s%n < 3) return
        do j = 1, s%n
            px(j) = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
            py(j) = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        end do
        px(s%n + 1) = px(1)
        py(s%n + 1) = py(1)
        if (s%patch_fill) call append_polygon(b, px, py, s%n, trim(s%color), &
                                              s%alpha, seal=s%patch_seal)
        if (len_trim(s%hatch) > 0) &
            call append_hatch(b, px, py, s%n, trim(s%hatch), trim(s%hcolor), s%alpha)
        if (len_trim(s%edgecolor) > 0) &
            call append_stroke_path(b, px, py, s%n + 1, trim(s%edgecolor), &
                                    s%edgewidth, s%alpha)
    end subroutine append_patch

    ! One filled polygon per arrow, shaped exactly as matplotlib shapes it:
    ! a shaft of width w, a head three w wide and five w long whose barbs
    ! reach back four and a half w. Everything is measured in shaft widths
    ! and then scaled, which is why the arrows keep their proportions
    ! however long they are.
    subroutine append_quiver(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp), parameter :: HEAD_W = 3.0_dp, HEAD_L = 5.0_dp, HEAD_AX = 4.5_dp
        real(dp) :: w, sc, sn, amean, mag, ln, ct, st, px(8), py(8), qx(8), qy(8)
        real(dp) :: x0, y0
        integer :: j, k

        if (s%n <= 0) return
        amean = 0.0_dp
        do j = 1, s%n
            amean = amean + hypot(s%qu(j), s%qv(j))
        end do
        amean = amean/real(s%n, dp)

        w = s%qwidth
        if (w < 0.0_dp) w = 0.06_dp/min(25.0_dp, max(8.0_dp, sqrt(real(s%n, dp))))
        w = w*ax_w
        sc = s%qscale
        if (sc < 0.0_dp) then
            sn = max(10.0_dp, sqrt(real(s%n, dp)))
            sc = 1.8_dp*amean*sn
            if (sc <= 0.0_dp) sc = 1.0_dp
        end if

        do j = 1, s%n
            mag = hypot(s%qu(j), s%qv(j))
            if (mag <= 0.0_dp) cycle
            ! Arrow length in shaft widths, so the outline below is pure
            ! geometry and needs no further conditioning.
            ln = mag*ax_w/(sc*w)
            ct = s%qu(j)/mag
            st = s%qv(j)/mag
            qx = [0.0_dp, ln - HEAD_AX, ln - HEAD_L, ln, ln - HEAD_L, ln - HEAD_AX, 0.0_dp, 0.0_dp]
            qy = 0.5_dp*[1.0_dp, 1.0_dp, HEAD_W, 0.0_dp, -HEAD_W, -1.0_dp, -1.0_dp, 1.0_dp]
            ! A vector too short for a shaft is drawn as head alone,
            ! shrunk to the length it has.
            if (ln < HEAD_L) then
                qx = (ln/HEAD_L)*[0.0_dp, HEAD_L - HEAD_AX, HEAD_L - HEAD_L, HEAD_L, &
                                  HEAD_L - HEAD_L, HEAD_L - HEAD_AX, 0.0_dp, 0.0_dp]
                qy = (ln/HEAD_L)*qy
            end if
            x0 = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
            y0 = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
            do k = 1, 8
                px(k) = x0 + w*(qx(k)*ct - qy(k)*st)
                py(k) = y0 - w*(qx(k)*st + qy(k)*ct)
            end do
            call append_polygon(b, px, py, 7, trim(s%color), s%alpha)
        end do
    end subroutine append_quiver

    ! matplotlib draws these as a FancyArrowPatch with the "-|>" style at a
    ! mutation scale of ten: a filled triangle four points long and four
    ! points across, with its tip on the curve.
    ! Draw the table. Cells are a fixed 1.2 line heights tall, the text is
    ! right aligned in the body, left in the row labels and centered in the
    ! column headings, each a tenth of a cell in from its edge.
    subroutine render_table(b, a, ax_l, ax_w, ax_b, ax_h)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: ax_l, ax_w, ax_b, ax_h
        real(dp), parameter :: PAD = 0.1_dp
        real(dp) :: fs, ch, rlw, tw, x0, y0, cx, cy
        real(dp), allocatable :: cw(:)
        integer :: nr, nc, nrows, i, j, r
        character(len=32) :: txt

        if (.not. a%has_table) return
        nr = size(a%tbl_cells, 1)
        nc = size(a%tbl_cells, 2)
        nrows = nr
        if (allocated(a%tbl_col)) nrows = nrows + 1

        fs = a%tbl_size
        ch = 1.2_dp*fs
        allocate (cw(nc))
        cw = a%tbl_w*ax_w
        tw = sum(cw)

        ! The row labels get whatever width their text needs, as matplotlib
        ! sizes that column automatically.
        rlw = 0.0_dp
        if (allocated(a%tbl_row)) then
            do i = 1, nr
                rlw = max(rlw, math_width(trim(a%tbl_row(i)), fs)*(1.0_dp + 2.0_dp*PAD))
            end do
        end if

        x0 = ax_l + 0.5_dp*ax_w - 0.5_dp*tw
        select case (trim(a%tbl_loc))
        case ("top")
            y0 = ax_b - ax_h - real(nrows, dp)*ch
        case ("center")
            y0 = ax_b - 0.5_dp*ax_h - 0.5_dp*real(nrows, dp)*ch
        case default
            y0 = ax_b
        end select

        do r = 1, nrows
            cy = y0 + real(r - 1, dp)*ch
            cx = x0
            if (allocated(a%tbl_row) .and. r > nrows - nr) then
                call table_cell(b, cx - rlw, cy, rlw, ch, &
                                trim(a%tbl_row(r - (nrows - nr))), "left", fs, PAD)
            end if
            do j = 1, nc
                if (r == 1 .and. allocated(a%tbl_col)) then
                    txt = a%tbl_col(j)
                    call table_cell(b, cx, cy, cw(j), ch, trim(txt), "center", fs, PAD)
                else
                    i = r - (nrows - nr)
                    txt = a%tbl_cells(i, j)
                    call table_cell(b, cx, cy, cw(j), ch, trim(txt), "right", fs, PAD)
                end if
                cx = cx + cw(j)
            end do
        end do
    end subroutine render_table

    ! One cell: a white box with a black edge, and its text placed by loc.
    subroutine table_cell(b, x, y, w, h, s, loc, fs, pad)
        class(renderer_t), intent(inout) :: b
        real(dp), intent(in) :: x, y, w, h, fs, pad
        character(len=*), intent(in) :: s, loc
        real(dp) :: xs(5), ys(5), tx

        if (w <= 0.0_dp) return
        xs = [x, x + w, x + w, x, x]
        ys = [y, y, y + h, y + h, y]
        call append_polygon(b, xs, ys, 4, "#ffffff", 1.0_dp)
        call append_stroke_path(b, xs, ys, 5, "#000000", 1.0_dp, 1.0_dp)
        if (len_trim(s) == 0) return
        select case (loc)
        case ("left")
            tx = x + pad*w
        case ("center")
            tx = x + 0.5_dp*w
        case default
            tx = x + (1.0_dp - pad)*w
        end select
        call append_text(b, tx, y + 0.5_dp*h + 0.36_dp*fs, s, loc, fs, "#000000")
    end subroutine table_cell

    subroutine append_arrowheads(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        ! "-|>" at a mutation scale of ten: four points long and two either
        ! side. The barbs are then pushed back far enough that the stroke
        ! around the head does not overshoot the line it sits on.
        real(dp), parameter :: HEAD_L = 4.0_dp, HEAD_W = 2.0_dp
        real(dp) :: x0, y0, dx, dy, mag, ct, st, px(4), py(4)
        real(dp) :: dist, cs, sn, d, lw
        integer :: j

        dist = hypot(HEAD_L, HEAD_W)
        cs = HEAD_L/dist
        sn = HEAD_W/dist
        lw = s%linewidth
        d = dist + 0.5_dp*lw/sn

        do j = 1, s%n
            x0 = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
            y0 = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
            ! The direction is given in data units, so it has to go through
            ! the same mapping before it means anything on the page.
            dx = map_x(s%x(j) + s%qu(j), xmin, xmax, ax_l, ax_w, xsc) - x0
            dy = map_y(s%y(j) + s%qv(j), ymin, ymax, ax_b, ax_h, ysc) - y0
            mag = hypot(dx, dy)
            if (mag <= 0.0_dp) cycle
            ct = dx/mag
            st = dy/mag
            ! matplotlib shrinks both ends of the arrow by two points before
            ! putting the head on, so the tip lands short of the point given.
            x0 = x0 - 2.0_dp*ct
            y0 = y0 - 2.0_dp*st
            px(1) = x0
            py(1) = y0
            px(2) = x0 - d*(cs*ct - sn*st)
            py(2) = y0 - d*(cs*st + sn*ct)
            px(3) = x0 - d*(cs*ct + sn*st)
            py(3) = y0 - d*(cs*st - sn*ct)
            px(4) = px(1)
            py(4) = py(1)
            call append_polygon(b, px, py, 3, trim(s%color), s%alpha)
            call append_stroke_path(b, px, py, 4, trim(s%color), lw, s%alpha)
        end do
    end subroutine append_arrowheads

    subroutine append_fill(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: j, np, j0, j1
        real(dp), allocatable :: px(:), py(:)
        logical, allocatable :: ok(:)

        ! Missing data cuts the band in two, as it does in matplotlib: each
        ! unbroken run of points becomes a polygon of its own.
        allocate (ok(s%n), px(2 * s%n), py(2 * s%n))
        do j = 1, s%n
            ok(j) = finite(s%x(j)) .and. finite(s%y(j)) .and. finite(s%y2(j))
        end do

        j0 = 1
        do while (j0 <= s%n)
            if (.not. ok(j0)) then
                j0 = j0 + 1
                cycle
            end if
            j1 = j0
            do while (j1 < s%n)
                if (.not. ok(j1 + 1)) exit
                j1 = j1 + 1
            end do
            np = j1 - j0 + 1
            if (np >= 2) then
                do j = 1, np
                    if (s%horiz) then
                        px(j) = map_x(s%y(j0 + j - 1), xmin, xmax, ax_l, ax_w, xsc)
                        py(j) = map_y(s%x(j0 + j - 1), ymin, ymax, ax_b, ax_h, ysc)
                        px(2*np - j + 1) = map_x(s%y2(j0 + j - 1), xmin, xmax, ax_l, ax_w, xsc)
                        py(2*np - j + 1) = py(j)
                    else
                        px(j) = map_x(s%x(j0 + j - 1), xmin, xmax, ax_l, ax_w, xsc)
                        py(j) = map_y(s%y(j0 + j - 1), ymin, ymax, ax_b, ax_h, ysc)
                        ! Return along the lower edge to close the band.
                        px(2*np - j + 1) = px(j)
                        py(2*np - j + 1) = map_y(s%y2(j0 + j - 1), ymin, ymax, ax_b, ax_h, ysc)
                    end if
                end do
                call append_polygon(b, px, py, 2 * np, trim(s%color), s%alpha)
                if (len_trim(s%hatch) > 0) &
                    call append_hatch(b, px, py, 2 * np, trim(s%hatch), &
                                      trim(s%hcolor), s%alpha)
            end if
            j0 = j1 + 1
        end do
    end subroutine append_fill

    ! An endless line, clipped to the axes rectangle in pixels by Liang and
    ! Barsky's parameter test, which needs no special case for a line that
    ! happens to be vertical or horizontal.
    subroutine append_axline(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: x1, y1, x2, y2, dx, dy, t0, t1

        x1 = map_x(s%x(1), xmin, xmax, ax_l, ax_w, xsc)
        y1 = map_y(s%y(1), ymin, ymax, ax_b, ax_h, ysc)
        x2 = map_x(s%x(2), xmin, xmax, ax_l, ax_w, xsc)
        y2 = map_y(s%y(2), ymin, ymax, ax_b, ax_h, ysc)
        dx = x2 - x1
        dy = y2 - y1
        if (abs(dx) < 1.0e-9_dp .and. abs(dy) < 1.0e-9_dp) return

        t0 = -1.0e9_dp
        t1 = 1.0e9_dp
        call slab(-dx, x1 - ax_l, t0, t1)
        call slab(dx, ax_l + ax_w - x1, t0, t1)
        call slab(-dy, y1 - (ax_b - ax_h), t0, t1)
        call slab(dy, ax_b - y1, t0, t1)
        if (t0 > t1) return
        call append_line(b, x1 + t0*dx, y1 + t0*dy, x1 + t1*dx, y1 + t1*dy, &
                         trim(s%color), s%linewidth, s%linestyle, s%alpha)
    end subroutine append_axline

    ! One edge of the clipping rectangle: p is the outward component of the
    ! direction, q the distance to the edge. p < 0 means the line enters
    ! through this edge, p > 0 that it leaves through it.
    subroutine slab(p, q, t0, t1)
        real(dp), intent(in) :: p, q
        real(dp), intent(inout) :: t0, t1
        real(dp) :: r
        if (abs(p) < 1.0e-12_dp) then
            ! Parallel to the edge: either wholly inside it, or nothing.
            if (q < 0.0_dp) then
                t0 = 1.0_dp
                t1 = 0.0_dp
            end if
            return
        end if
        r = q/p
        if (p < 0.0_dp) then
            t0 = max(t0, r)
        else
            t1 = min(t1, r)
        end if
    end subroutine slab

    ! Vertical error bar with caps for point j.
    subroutine append_errorbar(b, s, j, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: px, py, plo, phi, cap, elw
        character(len=7) :: ec

        px = map_x(s%x(j), xmin, xmax, ax_l, ax_w, xsc)
        py = map_y(s%y(j), ymin, ymax, ax_b, ax_h, ysc)
        cap = s%width
        elw = s%elw
        ec = s%ecolor
        if (len_trim(ec) == 0) ec = s%color

        if (allocated(s%eylo) .or. allocated(s%eyhi)) then
            plo = map_y(s%y(j) - arm(s%eylo, j), ymin, ymax, ax_b, ax_h, ysc)
            phi = map_y(s%y(j) + arm(s%eyhi, j), ymin, ymax, ax_b, ax_h, ysc)
            call append_line(b, px, plo, px, phi, trim(ec), elw, LINE_SOLID, s%alpha)
            if (cap > 0.0_dp) then
                call append_line(b, px - cap, plo, px + cap, plo, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
                call append_line(b, px - cap, phi, px + cap, phi, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
            end if
        end if

        if (allocated(s%exlo) .or. allocated(s%exhi)) then
            plo = map_x(s%x(j) - arm(s%exlo, j), xmin, xmax, ax_l, ax_w, xsc)
            phi = map_x(s%x(j) + arm(s%exhi, j), xmin, xmax, ax_l, ax_w, xsc)
            call append_line(b, plo, py, phi, py, trim(ec), elw, LINE_SOLID, s%alpha)
            if (cap > 0.0_dp) then
                call append_line(b, plo, py - cap, plo, py + cap, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
                call append_line(b, phi, py - cap, phi, py + cap, trim(ec), &
                                 elw, LINE_SOLID, s%alpha)
            end if
        end if
    end subroutine append_errorbar

    ! One error arm, zero where the series has none.
    pure function arm(e, j) result(v)
        real(dp), allocatable, intent(in) :: e(:)
        integer, intent(in) :: j
        real(dp) :: v
        v = 0.0_dp
        if (allocated(e)) v = e(j)
    end function arm

    ! User-set tick positions win over the automatic locator.
    ! How many intervals matplotlib would ask its locator for: as many as
    ! the axis is long enough to label, assuming tick text about three
    ! times as wide as it is tall, and never more than nine.
    pure function tick_space(length, size, horizontal) result(n)
        real(dp), intent(in) :: length, size
        logical, intent(in) :: horizontal
        integer :: n
        real(dp) :: w
        if (horizontal) then
            w = 3.0_dp*size
        else
            w = 2.0_dp*size
        end if
        if (w <= 0.0_dp) then
            n = 9
        else
            n = max(1, min(9, int(length/w)))
        end if
    end function tick_space

    ! How many intervals to ask the locator for: what the user asked for
    ! with MaxNLocator, else as many as the axis is long enough to label.
    ! A length of zero is the layout pass, which has no geometry yet and
    ! uses matplotlib's own default of nine.
    pure function nbins_for(nbins, length, size, horizontal) result(n)
        integer, intent(in) :: nbins
        real(dp), intent(in) :: length, size
        logical, intent(in) :: horizontal
        integer :: n
        if (nbins > 0) then
            n = nbins
        else if (length <= 0.0_dp) then
            n = 9
        else
            n = tick_space(length, size, horizontal)
        end if
    end function nbins_for

    ! base, when it is positive, is MultipleLocator: ticks every base
    ! units rather than wherever the automatic locator would put them.
    subroutine axis_ticks(n_user, user_pos, vmin, vmax, sc, nbins, is_date, &
                          t, nt, date_unit, base)
        integer, intent(in) :: n_user
        real(dp), intent(in) :: user_pos(MAX_TICKS), vmin, vmax
        type(scale_t), intent(in) :: sc
        integer, intent(in) :: nbins
        logical, intent(in) :: is_date
        real(dp), intent(out) :: t(MAX_TICKS)
        integer, intent(out) :: nt
        integer, intent(out) :: date_unit
        real(dp), intent(in), optional :: base

        date_unit = 0
        if (present(base)) then
            if (base > 0.0_dp .and. n_user == 0) then
                call multiple_ticks(vmin, vmax, base, t, nt)
                return
            end if
        end if
        if (n_user < 0) then
            nt = 0
        else if (n_user > 0) then
            nt = n_user
            t(1:nt) = user_pos(1:nt)
        else if (is_date) then
            call date_ticks(vmin, vmax, nbins, t, nt, date_unit)
        else
            select case (sc%kind)
            case (SCALE_LOG)
                call log_ticks(vmin, vmax, t, nt)
            case (SCALE_SYMLOG)
                call symlog_ticks(vmin, vmax, t, nt)
            case default
                call linear_ticks(vmin, vmax, nbins, t, nt)
            end select
        end if
    end subroutine axis_ticks

    ! Minor ticks subdivide each major interval; a log axis already places its
    ! majors one decade apart, so the 2..9 multiples are what belong between.
    subroutine minor_positions(t, nt, vmin, vmax, sc, m, nm)
        real(dp), intent(in) :: t(MAX_TICKS), vmin, vmax
        integer, intent(in) :: nt
        type(scale_t), intent(in) :: sc
        real(dp), intent(out) :: m(MAX_MINOR)
        integer, intent(out) :: nm
        integer :: i, k
        real(dp) :: step, v

        nm = 0
        ! A log axis has its minors between the decades whether or not any
        ! decade falls inside the view, which a short axis need not.
        if (sc%kind == SCALE_LOG) then
            if (vmin <= 0.0_dp .or. vmax <= vmin) return
            do i = floor(log10(vmin)), floor(log10(vmax))
                do k = 2, 9
                    call push_minor(m, nm, real(k, dp)*10.0_dp**i, vmin, vmax)
                end do
            end do
            return
        end if
        if (nt < 2) return
        do i = 1, nt - 1
            step = (t(i + 1) - t(i)) / 5.0_dp
            do k = 1, 4
                call push_minor(m, nm, t(i) + real(k, dp) * step, vmin, vmax)
            end do
        end do
    end subroutine minor_positions

    subroutine push_minor(m, nm, v, vmin, vmax)
        real(dp), intent(inout) :: m(MAX_MINOR)
        integer, intent(inout) :: nm
        real(dp), intent(in) :: v, vmin, vmax
        if (v < vmin .or. v > vmax) return
        if (nm >= MAX_MINOR) return
        nm = nm + 1
        m(nm) = v
    end subroutine push_minor

    ! Box, median, whiskers at 1.5 IQR clipped to the data, and the outliers
    ! beyond them as open circles.
    subroutine append_box(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: q1, q2, q3, iqr, wlo, whi, hw, cw, ci, mean
        real(dp) :: bx(12), by(12), px, py
        real(dp) :: lo_c, hi_c, ctr, nlo_c, nhi_c, mlo, mhi
        integer :: j, np

        q1 = quantile(s%y(1:s%n), 0.25_dp)
        q2 = quantile(s%y(1:s%n), 0.5_dp)
        q3 = quantile(s%y(1:s%n), 0.75_dp)
        iqr = q3 - q1
        wlo = q1
        whi = q3
        do j = 1, s%n
            if (s%y(j) >= q1 - s%whis*iqr) then
                wlo = s%y(j)
                exit
            end if
        end do
        do j = s%n, 1, -1
            if (s%y(j) <= q3 + s%whis*iqr) then
                whi = s%y(j)
                exit
            end if
        end do

        hw = 0.5_dp*s%width
        cw = 0.25_dp*s%width
        ! The three offsets across the category axis: the box edges, the
        ! waist of a notched box, and the centre line the whiskers run on.
        lo_c = pos_coord(s, s%pos - hw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        hi_c = pos_coord(s, s%pos + hw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        nlo_c = pos_coord(s, s%pos - cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        nhi_c = pos_coord(s, s%pos + cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ctr = pos_coord(s, s%pos, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)

        ! matplotlib's notch reaches 1.57 IQR / sqrt(N) either side of the
        ! median, which is the 95% interval for it.
        ci = 1.57_dp*iqr/sqrt(real(s%n, dp))
        mlo = q2 - ci
        mhi = q2 + ci
        if (.not. s%box_notch) then
            mlo = q2
            mhi = q2
        end if

        ! The box outline, waisted at the median when notched.
        if (s%box_notch) then
            call box_pt(s, lo_c, q1, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, mlo, bx, by, 3, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, nhi_c, q2, bx, by, 4, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, mhi, bx, by, 5, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q3, bx, by, 6, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q3, bx, by, 7, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, mhi, bx, by, 8, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, nlo_c, q2, bx, by, 9, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, mlo, bx, by, 10, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q1, bx, by, 11, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            np = 11
        else
            call box_pt(s, lo_c, q1, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, hi_c, q3, bx, by, 3, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q3, bx, by, 4, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, lo_c, q1, bx, by, 5, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            np = 5
        end if

        if (s%box_fill) call append_polygon(b, bx(1:np - 1), by(1:np - 1), np - 1, &
                                            trim(s%hcolor), s%alpha)
        call append_stroke_path(b, bx(1:np), by(1:np), np, trim(s%color), 1.0_dp, s%alpha)

        ! Whiskers along the centre line, with a cap on each.
        call box_seg(b, s, ctr, q1, ctr, wlo, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_seg(b, s, ctr, q3, ctr, whi, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_seg(b, s, nlo_c, wlo, nhi_c, wlo, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_seg(b, s, nlo_c, whi, nhi_c, whi, trim(s%color), xmin, xmax, ymin, ymax, &
                     ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ! The median, drawn between the waist of a notched box.
        if (s%box_notch) then
            call box_seg(b, s, nlo_c, q2, nhi_c, q2, "#ff7f0e", xmin, xmax, ymin, ymax, &
                         ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        else
            call box_seg(b, s, lo_c, q2, hi_c, q2, "#ff7f0e", xmin, xmax, ymin, ymax, &
                         ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end if

        do j = 1, s%n
            if (s%y(j) >= wlo .and. s%y(j) <= whi) cycle
            call box_pt(s, ctr, s%y(j), bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call append_open_circle(b, bx(1), by(1), 3.0_dp, trim(s%color))
        end do

        ! matplotlib marks the mean with a green triangle.
        if (s%box_mean) then
            mean = sum(s%y(1:s%n))/real(s%n, dp)
            call box_pt(s, ctr, mean, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            px = bx(1)
            py = by(1)
            call append_markers(b, MARKER_TRI_UP, [px], [py], 1, 6.0_dp, "#2ca02c", s%alpha)
        end if
    end subroutine append_box

    ! The canvas coordinate of a position on the category axis, whichever
    ! axis that is for this box.
    function pos_coord(s, v, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc) result(c)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: v, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: c
        if (s%box_vert) then
            c = map_x(v, xmin, xmax, ax_l, ax_w, xsc)
        else
            c = map_y(v, ymin, ymax, ax_b, ax_h, ysc)
        end if
    end function pos_coord

    ! Store one point of the box, given an already mapped category
    ! coordinate and a value still in data units.
    subroutine box_pt(s, c, v, bx, by, k, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: c, v, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        real(dp), intent(inout) :: bx(:), by(:)
        integer, intent(in) :: k
        type(scale_t), intent(in) :: xsc, ysc
        if (s%box_vert) then
            bx(k) = c
            by(k) = map_y(v, ymin, ymax, ax_b, ax_h, ysc)
        else
            bx(k) = map_x(v, xmin, xmax, ax_l, ax_w, xsc)
            by(k) = c
        end if
    end subroutine box_pt

    ! One straight piece of box furniture, from a category/value pair to
    ! another.
    subroutine box_seg(b, s, c0, v0, c1, v1, color, xmin, xmax, ymin, ymax, &
                       ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: c0, v0, c1, v1
        character(len=*), intent(in) :: color
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: bx(2), by(2)

        call box_pt(s, c0, v0, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_pt(s, c1, v1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call append_line(b, bx(1), by(1), bx(2), by(2), color, 1.0_dp, LINE_SOLID, s%alpha)
    end subroutine box_seg

    ! Mirrored Gaussian kernel density estimate, plus the min/max/range bars
    ! matplotlib draws over it.
    subroutine append_violin(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer, parameter :: NK = 100
        real(dp) :: g(NK), d(NK)
        real(dp) :: px(2*NK), py(2*NK)
        real(dp) :: lo, hi, mu, var, h, dmax, hw, cw, u, med
        real(dp) :: c_lo, c_hi, ctr
        integer :: i, j

        lo = s%y(1)
        hi = s%y(s%n)
        if (hi <= lo) return

        mu = sum(s%y(1:s%n))/real(s%n, dp)
        var = sum((s%y(1:s%n) - mu)**2)/real(s%n - 1, dp)
        ! Scott's rule, as used by scipy's gaussian_kde and so by matplotlib.
        h = sqrt(var)*real(s%n, dp)**(-0.2_dp)
        if (h <= 0.0_dp) return

        do i = 1, NK
            g(i) = lo + (hi - lo)*real(i - 1, dp)/real(NK - 1, dp)
            d(i) = 0.0_dp
            do j = 1, s%n
                u = (g(i) - s%y(j))/h
                d(i) = d(i) + exp(-0.5_dp*u*u)
            end do
        end do
        dmax = maxval(d)
        if (dmax <= 0.0_dp) return

        hw = 0.5_dp*s%width
        cw = 0.5_dp*hw
        do i = 1, NK
            call box_pt(s, pos_coord(s, s%pos - hw*d(i)/dmax, xmin, xmax, ymin, ymax, &
                                     ax_l, ax_w, ax_b, ax_h, xsc, ysc), &
                        g(i), px, py, i, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call box_pt(s, pos_coord(s, s%pos + hw*d(i)/dmax, xmin, xmax, ymin, ymax, &
                                     ax_l, ax_w, ax_b, ax_h, xsc, ysc), &
                        g(i), px, py, 2*NK + 1 - i, xmin, xmax, ymin, ymax, &
                        ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end do
        call append_polygon(b, px, py, 2*NK, trim(s%color), 0.3_dp)

        c_lo = pos_coord(s, s%pos - cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        c_hi = pos_coord(s, s%pos + cw, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ctr = pos_coord(s, s%pos, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)

        ! box_fill stands for "the extrema were turned off" on a violin.
        if (.not. s%box_fill) then
            call violin_bar(b, s, ctr, lo, ctr, hi, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call violin_bar(b, s, c_lo, lo, c_hi, lo, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call violin_bar(b, s, c_lo, hi, c_hi, hi, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end if
        if (s%box_mean) &
            call violin_bar(b, s, c_lo, mu, c_hi, mu, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        ! box_notch stands for "show the median" on a violin.
        if (s%box_notch) then
            med = quantile(s%y(1:s%n), 0.5_dp)
            call violin_bar(b, s, c_lo, med, c_hi, med, xmin, xmax, ymin, ymax, &
                            ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        end if
    end subroutine append_violin

    ! One bar of violin furniture, in the violin's own colour and weight.
    subroutine violin_bar(b, s, c0, v0, c1, v1, xmin, xmax, ymin, ymax, &
                          ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: c0, v0, c1, v1
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp) :: bx(2), by(2)

        call box_pt(s, c0, v0, bx, by, 1, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call box_pt(s, c1, v1, bx, by, 2, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        call append_line(b, bx(1), by(1), bx(2), by(2), trim(s%color), 1.5_dp, LINE_SOLID, 1.0_dp)
    end subroutine violin_bar

    ! One <path> per wedge: a radius out, the arc, and back to the centre.
    subroutine append_pie(b, s, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        integer :: i
        real(dp) :: tot, a0, a1, cx, cy, rx, ry, dir, off, mid

        tot = sum(s%y(1:s%n))
        if (tot <= 0.0_dp) return
        cx = map_x(0.0_dp, xmin, xmax, ax_l, ax_w, linear_scale)
        cy = map_y(0.0_dp, ymin, ymax, ax_b, ax_h, linear_scale)

        rx = s%pie_radius*ax_w/(xmax - xmin)
        ry = s%pie_radius*ax_h/(ymax - ymin)
        dir = 1.0_dp
        if (.not. s%pie_ccw) dir = -1.0_dp

        a1 = s%pie_start
        do i = 1, s%n
            a0 = a1
            a1 = a0 + dir*2.0_dp*PI*s%y(i)/tot
            ! An exploded wedge is the same wedge about a centre pushed
            ! out along its own mid angle.
            off = 0.0_dp
            if (allocated(s%pexp)) off = s%pexp(i)*s%pie_radius
            mid = 0.5_dp*(a0 + a1)
            call append_wedge(b, cx + off*ax_w/(xmax - xmin)*cos(mid), &
                              cy - off*ax_h/(ymax - ymin)*sin(mid), rx, ry, &
                              a0, a1, trim(s%pcolor(i)), s%alpha, &
                              edge=trim(s%hcolor), ewidth=s%linewidth)
        end do
    end subroutine append_pie

    ! Level lines, one level at a time. The triangles give unordered
    ! segments, so they are first chained end to end into whole contours:
    ! a contour has to be a single line before a label can be dropped into
    ! the middle of it and the line broken to make room.
    subroutine append_contour_lines(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        real(dp), allocatable :: ex(:, :), ey(:, :), vx(:), vy(:), lx(:), ly(:)
        logical, allocatable :: used(:)
        integer :: nr, nc, nlev, nseg, i, j, k, tri, ns, np, nv, i0, i1
        real(dp) :: dx, dy, gx(2), gy(2), tx(3), ty(3), tv(3), sx(2), sy(2)
        real(dp) :: t, tol, cx, cy, ang, halfw
        character(len=32) :: lbl
        integer :: nl, dec

        nr = size(a%cz, 1)
        nc = size(a%cz, 2)
        nlev = size(a%clev)
        dx = (a%cont_ext(2) - a%cont_ext(1))/real(nc - 1, dp)
        dy = (a%cont_ext(4) - a%cont_ext(3))/real(nr - 1, dp)
        tol = 1.0e-9_dp*(abs(dx) + abs(dy))
        dec = tick_decimals(a%clev, nlev)

        nseg = 2*(nr - 1)*(nc - 1)
        allocate (ex(2, nseg), ey(2, nseg), used(nseg))
        allocate (vx(nseg + 1), vy(nseg + 1), lx(nseg + 1), ly(nseg + 1))

        do k = 1, nlev
            ns = 0
            do i = 1, nr - 1
                gy(1) = a%cont_ext(3) + real(i - 1, dp)*dy
                gy(2) = gy(1) + dy
                do j = 1, nc - 1
                    gx(1) = a%cont_ext(1) + real(j - 1, dp)*dx
                    gx(2) = gx(1) + dx
                    do tri = 1, 2
                        call cell_triangle(a%cz, i, j, gx, gy, tri, tx, ty, tv)
                        call tri_level(tx, ty, tv, a%clev(k), sx, sy, np)
                        if (np /= 2) cycle
                        ns = ns + 1
                        ex(:, ns) = sx
                        ey(:, ns) = sy
                    end do
                end do
            end do
            if (ns == 0) cycle

            t = real(k - 1, dp)/real(max(nlev - 1, 1), dp)
            call format_tick_fixed(a%clev(k), dec, lbl, nl)
            halfw = 0.5_dp*real(nl, dp)*DIGIT_W*a%clab_size + 3.0_dp

            used(1:ns) = .false.
            do
                call chain_polyline(ex, ey, used, ns, tol, i, vx, vy, nv)
                if (nv == 0) exit
                do j = 1, nv
                    lx(j) = map_x(vx(j), xmin, xmax, ax_l, ax_w, xsc)
                    ly(j) = map_y(vy(j), ymin, ymax, ax_b, ax_h, ysc)
                end do
                i0 = 0
                ! Every separate run of a level gets its own label, as
                ! matplotlib labels each of them.
                if (a%cont_labels) call label_window(lx, ly, nv, halfw, i0, i1, cx, cy, ang)
                if (i0 > 0) then
                    call append_stroke_path(b, lx(1:i0), ly(1:i0), i0, &
                                            cmap_color(a%cont_cmap, t), 1.5_dp, 1.0_dp)
                    call append_stroke_path(b, lx(i1:nv), ly(i1:nv), nv - i1 + 1, &
                                            cmap_color(a%cont_cmap, t), 1.5_dp, 1.0_dp)
                    call append_text(b, cx, cy + 0.36_dp*a%clab_size, lbl(1:nl), &
                                     "middle", a%clab_size, &
                                     cmap_color(a%cont_cmap, t), ang)
                else
                    call append_stroke_path(b, lx(1:nv), ly(1:nv), nv, &
                                            cmap_color(a%cont_cmap, t), 1.5_dp, 1.0_dp)
                end if
            end do
        end do
    end subroutine append_contour_lines

    ! Take the first segment nobody has used yet and grow it in both
    ! directions for as long as another segment starts where this one ends.
    ! Returns nv = 0 once every segment belongs to a line.
    subroutine chain_polyline(ex, ey, used, ns, tol, id, vx, vy, nv)
        real(dp), intent(in) :: ex(:, :), ey(:, :), tol
        logical, intent(inout) :: used(:)
        integer, intent(in) :: ns
        integer, intent(out) :: id, nv
        real(dp), intent(out) :: vx(:), vy(:)
        integer :: s, i, head, tail, e
        real(dp) :: hx, hy, tx, ty

        nv = 0
        id = 0
        s = 0
        do i = 1, ns
            if (.not. used(i)) then
                s = i
                exit
            end if
        end do
        if (s == 0) return
        id = s
        used(s) = .true.
        ! The line is built from the middle outwards, so it is laid down
        ! back to front and then walked forwards.
        head = size(vx)/2
        tail = head + 1
        vx(head) = ex(1, s); vy(head) = ey(1, s)
        vx(tail) = ex(2, s); vy(tail) = ey(2, s)

        do
            tx = vx(tail); ty = vy(tail)
            e = 0
            do i = 1, ns
                if (used(i)) cycle
                if (near(ex(1, i), ey(1, i), tx, ty, tol)) then
                    e = 2
                else if (near(ex(2, i), ey(2, i), tx, ty, tol)) then
                    e = 1
                else
                    cycle
                end if
                used(i) = .true.
                tail = tail + 1
                vx(tail) = ex(e, i); vy(tail) = ey(e, i)
                exit
            end do
            if (e == 0 .or. tail >= size(vx)) exit
        end do

        do
            hx = vx(head); hy = vy(head)
            e = 0
            do i = 1, ns
                if (used(i)) cycle
                if (near(ex(1, i), ey(1, i), hx, hy, tol)) then
                    e = 2
                else if (near(ex(2, i), ey(2, i), hx, hy, tol)) then
                    e = 1
                else
                    cycle
                end if
                used(i) = .true.
                head = head - 1
                vx(head) = ex(e, i); vy(head) = ey(e, i)
                exit
            end do
            if (e == 0 .or. head <= 1) exit
        end do

        nv = tail - head + 1
        vx(1:nv) = vx(head:tail)
        vy(1:nv) = vy(head:tail)
    end subroutine chain_polyline

    pure function near(ax1, ay1, bx, by, tol) result(q)
        real(dp), intent(in) :: ax1, ay1, bx, by, tol
        logical :: q
        q = abs(ax1 - bx) <= tol .and. abs(ay1 - by) <= tol
    end function near

    ! Where to break the line for its label: the straightest stretch long
    ! enough to hold it, which is what matplotlib looks for too. i0 comes
    ! back as 0 when the line is too short to be worth breaking.
    subroutine label_window(px, py, n, halfw, i0, i1, cx, cy, ang)
        real(dp), intent(in) :: px(:), py(:), halfw
        integer, intent(in) :: n
        integer, intent(out) :: i0, i1
        real(dp), intent(out) :: cx, cy, ang
        real(dp) :: cum(n), dev, bdev, dxs, dys, len2, d
        integer :: m, j, ja, jb

        i0 = 0
        i1 = 0
        cx = 0.0_dp; cy = 0.0_dp; ang = 0.0_dp
        cum(1) = 0.0_dp
        do m = 2, n
            cum(m) = cum(m - 1) + hypot(px(m) - px(m - 1), py(m) - py(m - 1))
        end do
        if (cum(n) < 3.0_dp*halfw) return

        bdev = huge(1.0_dp)
        do m = 2, n - 1
            if (cum(m) < halfw .or. cum(n) - cum(m) < halfw) cycle
            ja = m
            do while (ja > 1 .and. cum(m) - cum(ja) < halfw)
                ja = ja - 1
            end do
            jb = m
            do while (jb < n .and. cum(jb) - cum(m) < halfw)
                jb = jb + 1
            end do
            dxs = px(jb) - px(ja)
            dys = py(jb) - py(ja)
            len2 = dxs*dxs + dys*dys
            if (len2 <= 0.0_dp) cycle
            dev = 0.0_dp
            do j = ja, jb
                d = abs(dxs*(py(j) - py(ja)) - dys*(px(j) - px(ja)))/sqrt(len2)
                dev = max(dev, d)
            end do
            if (dev < bdev) then
                bdev = dev
                i0 = ja
                i1 = jb
            end if
        end do
        if (i0 == 0) return

        cx = 0.5_dp*(px(i0) + px(i1))
        cy = 0.5_dp*(py(i0) + py(i1))
        ! Device y grows downwards, and a label is never written upside
        ! down: past the vertical it reads the other way round.
        ang = atan2(-(py(i1) - py(i0)), px(i1) - px(i0))*180.0_dp/PI
        if (ang > 90.0_dp) ang = ang - 180.0_dp
        if (ang < -90.0_dp) ang = ang + 180.0_dp
    end subroutine label_window

    ! Walk every cell as two triangles, emitting filled bands or level lines.
    subroutine append_contour(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: nr, nc, i, j, k, tri, nq, ns, nlev, v
        real(dp) :: dx, dy, gx(2), gy(2)
        real(dp) :: tx(3), ty(3), tv(3)
        real(dp) :: qx(MAX_POLY), qy(MAX_POLY)
        real(dp) :: sx(2), sy(2)
        real(dp) :: px(MAX_POLY), py(MAX_POLY)
        real(dp) :: lo, hi, t

        nr = size(a%cz, 1)
        nc = size(a%cz, 2)
        nlev = size(a%clev)
        dx = (a%cont_ext(2) - a%cont_ext(1)) / real(nc - 1, dp)
        dy = (a%cont_ext(4) - a%cont_ext(3)) / real(nr - 1, dp)

        if (.not. a%cont_filled) then
            call append_contour_lines(b, a, xmin, xmax, ymin, ymax, &
                                      ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            return
        end if

        do i = 1, nr - 1
            gy(1) = a%cont_ext(3) + real(i - 1, dp) * dy
            gy(2) = gy(1) + dy
            do j = 1, nc - 1
                gx(1) = a%cont_ext(1) + real(j - 1, dp) * dx
                gx(2) = gx(1) + dx

                do tri = 1, 2
                    call cell_triangle(a%cz, i, j, gx, gy, tri, tx, ty, tv)

                    if (a%cont_filled) then
                        do k = 1, nlev - 1
                            lo = a%clev(k)
                            hi = a%clev(k + 1)
                            call tri_band(tx, ty, tv, lo, hi, qx, qy, nq)
                            if (nq < 3) cycle
                            do v = 1, nq
                                px(v) = map_x(qx(v), xmin, xmax, ax_l, ax_w, xsc)
                                py(v) = map_y(qy(v), ymin, ymax, ax_b, ax_h, ysc)
                            end do
                            t = (real(k, dp) - 0.5_dp) / real(nlev - 1, dp)
                            call append_polygon(b, px, py, nq, &
                                                cmap_color(a%cont_cmap, t), 1.0_dp, seal=.true.)
                        end do
                    end if
                end do
            end do
        end do
    end subroutine append_contour

    ! The two triangles of cell (i, j), sharing the diagonal.
    pure subroutine cell_triangle(z, i, j, gx, gy, tri, tx, ty, tv)
        real(dp), intent(in) :: z(:, :), gx(2), gy(2)
        integer, intent(in) :: i, j, tri
        real(dp), intent(out) :: tx(3), ty(3), tv(3)
        if (tri == 1) then
            tx = [gx(1), gx(2), gx(2)]
            ty = [gy(1), gy(1), gy(2)]
            tv = [z(i, j), z(i, j + 1), z(i + 1, j + 1)]
        else
            tx = [gx(1), gx(2), gx(1)]
            ty = [gy(1), gy(2), gy(2)]
            tv = [z(i, j), z(i + 1, j + 1), z(i + 1, j)]
        end if
    end subroutine cell_triangle

    ! An image is one draw_image call. Every format has a way to place a
    ! raster, and using it costs one object in the file where a rectangle per
    ! sample costs nr*nc: a 200x200 image is 40000 elements the other way.
    !
    ! A blit assumes the samples are evenly spaced on the canvas, which is
    ! true only while both axes are linear, so a log axis keeps the per-cell
    ! path below.
    subroutine append_image(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: nr, nc, i, j, si, sj, fx, fy, c(4)
        integer, allocatable :: rgba(:, :, :)
        real(dp) :: px0, px1, py0, py1
        type(paint_t) :: p
        logical :: flip_x, flip_y

        if (a%has_mesh .or. xsc%kind /= SCALE_LINEAR .or. ysc%kind /= SCALE_LINEAR) then
            call append_image_cells(b, a, xmin, xmax, ymin, ymax, &
                                    ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            return
        end if

        nr = size(a%img, 1)
        nc = size(a%img, 2)
        px0 = map_x(a%img_ext(1), xmin, xmax, ax_l, ax_w, xsc)
        px1 = map_x(a%img_ext(2), xmin, xmax, ax_l, ax_w, xsc)
        py0 = map_y(a%img_ext(3), ymin, ymax, ax_b, ax_h, ysc)
        py1 = map_y(a%img_ext(4), ymin, ymax, ax_b, ax_h, ysc)

        ! draw_image wants its rows from the top of the canvas down, and the
        ! image's own rows run up the extent, so whichever end of the extent
        ! lands at the top decides where row 1 goes. This is how origin=
        ! "upper" reaches the file: it descends the y axis, which is what
        ! puts the first row at the top.
        flip_x = px1 < px0
        flip_y = py0 > py1

        ! The raster is sent at roughly the size it will be drawn, which is
        ! what matplotlib does and for the same reason: a backend or a viewer
        ! asked to enlarge a 16x12 image will often interpolate it, and these
        ! images are small enough that interpolating turns the data into a
        ! blur. Repeating each sample a whole number of times is exactly
        ! nearest neighbour, so no value is invented by doing it.
        if (a%img_bilinear) then
            call append_image_smooth(b, a, px0, px1, py0, py1, flip_x, flip_y)
            return
        end if

        fx = fill_factor(abs(px1 - px0), nc)
        fy = fill_factor(abs(py1 - py0), nr)

        allocate (rgba(4, nc*fx, nr*fy))
        do i = 1, nr*fy
            si = (i - 1)/fy + 1
            if (flip_y) si = nr - si + 1
            do j = 1, nc*fx
                sj = (j - 1)/fx + 1
                if (flip_x) sj = nc - sj + 1
                call img_rgba(a, si, sj, c)
                rgba(1:4, j, i) = c
            end do
        end do

        p%clip = g_clip
        call b%draw_image(min(px0, px1), min(py0, py1), abs(px1 - px0), &
                          abs(py1 - py0), rgba, nc*fx, nr*fy, p)
    end subroutine append_image

    ! interpolation="bilinear": the raster is built at the size the image
    ! is drawn and every pixel is read from the four samples around it, so
    ! the picture comes out smooth rather than blocked. The samples sit at
    ! the middles of their cells, which is what puts the outermost half
    ! cell at a flat color rather than running off the data.
    subroutine append_image_smooth(b, a, px0, px1, py0, py1, flip_x, flip_y)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: px0, px1, py0, py1
        logical, intent(in) :: flip_x, flip_y
        integer, parameter :: MAX_SIDE = 2048
        integer, allocatable :: rgba(:, :, :)
        integer :: nr, nc, ow, oh, i, j, i0, i1, j0, j1
        real(dp) :: u, v, fu, fv, val, t
        type(paint_t) :: p

        nr = size(a%img, 1)
        nc = size(a%img, 2)
        ow = max(1, min(MAX_SIDE, nint(abs(px1 - px0))))
        oh = max(1, min(MAX_SIDE, nint(abs(py1 - py0))))
        allocate (rgba(4, ow, oh))

        do i = 1, oh
            v = (real(i, dp) - 0.5_dp)/real(oh, dp)*real(nr, dp) - 0.5_dp
            if (flip_y) v = real(nr - 1, dp) - v
            i0 = floor(v)
            fv = v - real(i0, dp)
            i1 = min(nr - 1, max(0, i0 + 1))
            i0 = min(nr - 1, max(0, i0))
            do j = 1, ow
                u = (real(j, dp) - 0.5_dp)/real(ow, dp)*real(nc, dp) - 0.5_dp
                if (flip_x) u = real(nc - 1, dp) - u
                j0 = floor(u)
                fu = u - real(j0, dp)
                j1 = min(nc - 1, max(0, j0 + 1))
                j0 = min(nc - 1, max(0, j0))
                val = (1.0_dp - fv)*((1.0_dp - fu)*a%img(i0 + 1, j0 + 1) &
                                     + fu*a%img(i0 + 1, j1 + 1)) &
                      + fv*((1.0_dp - fu)*a%img(i1 + 1, j0 + 1) &
                            + fu*a%img(i1 + 1, j1 + 1))
                rgba(1:3, j, i) = hex_rgb(img_color(a, val))
                rgba(4, j, i) = 255
            end do
        end do

        p%clip = g_clip
        call b%draw_image(min(px0, px1), min(py0, py1), abs(px1 - px0), &
                          abs(py1 - py0), rgba, ow, oh, p)
    end subroutine append_image_smooth

    ! How many times to repeat each sample so the raster covers the space it
    ! is drawn in, bounded so that a large image is left as it is rather than
    ! grown into something the file has to carry.
    pure function fill_factor(extent, n) result(f)
        real(dp), intent(in) :: extent
        integer, intent(in) :: n
        integer :: f
        integer, parameter :: MAX_SAMPLES = 2048

        f = max(1, ceiling(extent/real(max(n, 1), dp)))
        f = min(f, max(1, MAX_SAMPLES/max(n, 1)))
    end function fill_factor

    ! One rectangle per sample. Correct whatever the axes do to the spacing,
    ! and so the answer when a scale is not linear and a blit would put the
    ! samples in the wrong places.
    subroutine append_image_cells(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        integer :: nr, nc, i, j
        real(dp) :: xe0, xe1, ye0, ye1, px0, px1, py0, py1, t, dxc, dyc

        nr = size(a%img, 1)
        nc = size(a%img, 2)
        dxc = (a%img_ext(2) - a%img_ext(1)) / real(nc, dp)
        dyc = (a%img_ext(4) - a%img_ext(3)) / real(nr, dp)

        do i = 1, nr
            if (a%has_mesh) then
                ye0 = a%mesh_y(i)
                ye1 = a%mesh_y(i + 1)
            else
                ye0 = a%img_ext(3) + real(i - 1, dp) * dyc
                ye1 = ye0 + dyc
            end if
            py0 = map_y(ye0, ymin, ymax, ax_b, ax_h, ysc)
            py1 = map_y(ye1, ymin, ymax, ax_b, ax_h, ysc)
            do j = 1, nc
                if (a%has_mesh) then
                    xe0 = a%mesh_x(j)
                    xe1 = a%mesh_x(j + 1)
                else
                    xe0 = a%img_ext(1) + real(j - 1, dp) * dxc
                    xe1 = xe0 + dxc
                end if
                px0 = map_x(xe0, xmin, xmax, ax_l, ax_w, xsc)
                px1 = map_x(xe1, xmin, xmax, ax_l, ax_w, xsc)
                if (img_bad_hidden(a, a%img(i, j))) cycle
                call append_cell(b, min(px0, px1), min(py0, py1), &
                                 abs(px1 - px0), abs(py1 - py0), &
                                 img_hex(a, i, j))
            end do
        end do
    end subroutine append_image_cells

    ! The grid lines of an axes, drawn between the patches and the lines
    ! because that is where matplotlib's axes.axisbelow="line" puts them.
    subroutine append_grid(b, a, xt, nxt, yt, nyt, xm, nxm, ym, nym, &
                           xmin, xmax, ymin, ymax, &
                           ax_l, ax_r, ax_t, ax_b, ax_w, ax_h, xsc, ysc)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xt(:), yt(:), xm(:), ym(:)
        integer, intent(in) :: nxt, nyt, nxm, nym
        real(dp), intent(in) :: xmin, xmax, ymin, ymax
        real(dp), intent(in) :: ax_l, ax_r, ax_t, ax_b, ax_w, ax_h
        type(scale_t), intent(in) :: xsc, ysc
        character(len=7) :: col
        real(dp) :: p, lw, alpha, a2
        integer :: i, ls

        if (.not. a%grid_on) return
        col = rc_grid_color
        alpha = 1.0_dp
        if (len_trim(a%grid_color) > 0) col = resolve_color(trim(a%grid_color), a2)
        lw = rc_grid_lw
        if (a%grid_lw >= 0.0_dp) lw = a%grid_lw
        ls = LINE_SOLID
        if (a%grid_ls >= 0) ls = a%grid_ls
        if (a%grid_alpha >= 0.0_dp) alpha = a%grid_alpha
        if (grid_does(a, "major")) then
            call grid_lines(b, xt, nxt, yt, nyt)
        end if
        if (grid_does(a, "minor")) then
            call grid_lines(b, xm, nxm, ym, nym)
        end if
    contains
        subroutine grid_lines(bb, gx, ngx, gy, ngy)
            class(renderer_t), intent(inout) :: bb
            real(dp), intent(in) :: gx(:), gy(:)
            integer, intent(in) :: ngx, ngy
            if (a%grid_axis /= "y") then
                do i = 1, ngx
                    p = map_x(gx(i), xmin, xmax, ax_l, ax_w, xsc)
                    call append_line(bb, p, ax_t, p, ax_b, col, lw, ls, alpha)
                end do
            end if
            if (a%grid_axis /= "x") then
                do i = 1, ngy
                    p = map_y(gy(i), ymin, ymax, ax_b, ax_h, ysc)
                    call append_line(bb, ax_l, p, ax_r, p, col, lw, ls, alpha)
                end do
            end if
        end subroutine grid_lines
    end subroutine append_grid

    ! Does the grid cover this tier of ticks?
    pure logical function grid_does(a, which)
        type(axes_t), intent(in) :: a
        character(len=*), intent(in) :: which
        grid_does = a%grid_which == which .or. a%grid_which == "both"
    end function grid_does

    ! Minor ticks are wanted whenever they are asked for outright or the
    ! grid is drawn through them.
    pure logical function wants_minor(a)
        type(axes_t), intent(in) :: a
        wants_minor = a%minor_ticks .or. (a%grid_on .and. grid_does(a, "minor"))
    end function wants_minor

    ! Vertical gradient strip plus its own frame, ticks and labels.
    subroutine append_colorbar(b, a, idx, W, H)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        integer, intent(in) :: idx
        real(dp), intent(in) :: W, H
        real(dp) :: bx, bw, bt, bb, bh, y0, y1, t, v, py, px
        real(dp) :: cb_ticks(MAX_TICKS), lo, hi
        integer :: i, nt, ln, dec
        character(len=64) :: lbl
        character(len=512) :: esc

        call cbar_box(a, W, H, bx, bt, bw, bh)
        bb = bt + bh

        ! Slices are grown slightly so they abut without seams, so keep them
        ! inside the bar.
        call set_clip(bx, bt, bw, bh)
        if (allocated(a%img_bounds)) then
            ! One block per band, as long as the band is wide in the data.
            lo = a%img_bounds(1)
            hi = a%img_bounds(size(a%img_bounds))
            do i = 1, size(a%img_bounds) - 1
                y0 = (a%img_bounds(i) - lo) / (hi - lo)
                y1 = (a%img_bounds(i + 1) - lo) / (hi - lo)
                v = 0.5_dp * (a%img_bounds(i) + a%img_bounds(i + 1))
                call cbar_band(b, a%cbar_horiz, bx, bt, bw, bh, y0, y1, &
                               img_color(a, v))
            end do
        else
            do i = 1, CBAR_SLICES
                y0 = real(i - 1, dp) / real(CBAR_SLICES, dp)
                y1 = real(i, dp) / real(CBAR_SLICES, dp)
                t = (real(i, dp) - 0.5_dp) / real(CBAR_SLICES, dp)
                call cbar_band(b, a%cbar_horiz, bx, bt, bw, bh, y0, y1, &
                               map_color(a, t))
            end do
        end if
        call clear_clip()

        call b%draw_rect(bx, bt, bw, bh, pen(rc_spine_color, rc_spine_lw))

        lo = a%img_vmin
        hi = a%img_vmax
        if (allocated(a%img_bounds)) then
            ! A tick at every band edge, which is what the bands mean.
            nt = min(MAX_TICKS, size(a%img_bounds))
            cb_ticks(1:nt) = a%img_bounds(1:nt)
        else if (a%img_norm == NORM_LOG) then
            call log_ticks(lo, hi, cb_ticks, nt)
        else if (a%cbar_horiz) then
            call linear_ticks(lo, hi, tick_space(bw, a%xtick_size, .true.), &
                              cb_ticks, nt)
        else
            call linear_ticks(lo, hi, tick_space(bh, a%ytick_size, .false.), &
                              cb_ticks, nt)
        end if
        dec = -1
        if (.not. a%img_norm == NORM_LOG) dec = tick_decimals(cb_ticks, nt)

        do i = 1, nt
            v = cb_ticks(i)
            if (v < lo .or. v > hi) cycle
            t = (v - lo)/(hi - lo)
            ! Bands place their ticks at the edges they stand for; every
            ! other norm puts a tick where the norm puts the value.
            if (.not. allocated(a%img_bounds)) t = cmap_t(a, v)
            if (a%img_norm == NORM_LOG) then
                call format_tick_to(v, .true., lbl, ln)
            else
                call format_tick_fixed(v, dec, lbl, ln)
            end if
            if (a%cbar_horiz) then
                px = bx + t * bw
                call append_tick(b, px, bb, px, bb + 3.5_dp)
                call append_text(b, px, bb + xtick_gap(a), lbl(1:ln), &
                                 "middle", a%xtick_size, rc_text_color)
            else
                py = bb - t * bh
                call append_tick(b, bx + bw, py, bx + bw + 3.5_dp, py)
                call append_text(b, bx + bw + 7.0_dp, py + 3.5_dp, lbl(1:ln), &
                                 "left", a%ytick_size, rc_text_color)
            end if
        end do

        if (len_trim(a%cbar_label) > 0) then
            if (a%cbar_horiz) then
                call append_text(b, bx + 0.5_dp * bw, bb + xtick_gap(a) &
                                 + LABEL_BOX * a%xlabel_size, trim(a%cbar_label), &
                                 "middle", a%xlabel_size, rc_text_color)
            else
                call append_text(b, bx + bw + 34.0_dp, 0.5_dp * (bt + bb), &
                                 trim(a%cbar_label), "center", a%ylabel_size, &
                                 rc_text_color, 90.0_dp)
            end if
        end if
    end subroutine append_colorbar

    ! One slice of the bar, covering the fractions u0 to u1 of its length.
    ! A vertical bar runs from the bottom up, a horizontal one left to
    ! right, which is all that separates the two.
    subroutine cbar_band(b, horiz, bx, by, bw, bh, u0, u1, col)
        class(renderer_t), intent(inout) :: b
        logical, intent(in) :: horiz
        real(dp), intent(in) :: bx, by, bw, bh, u0, u1
        character(len=*), intent(in) :: col
        if (horiz) then
            call append_cell(b, bx + u0 * bw, by, (u1 - u0) * bw, bh, col)
        else
            call append_cell(b, bx, by + (1.0_dp - u1) * bh, bw, (u1 - u0) * bh, col)
        end if
    end subroutine cbar_band

    ! A place on the colormap as a colour, honouring a colormap the caller
    ! built themselves.
    pure function map_color(a, t) result(hex)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: t
        character(len=7) :: hex
        real(dp) :: u, f
        integer :: n, i0
        if (.not. allocated(a%cmap_list)) then
            hex = cmap_color(a%img_cmap, t)
            return
        end if
        n = size(a%cmap_list)
        if (n == 1) then
            hex = a%cmap_list(1)
            return
        end if
        u = max(0.0_dp, min(1.0_dp, t))*real(n - 1, dp)
        i0 = min(n - 2, int(u))
        f = u - real(i0, dp)
        hex = blend_hex(a%cmap_list(i0 + 1), a%cmap_list(i0 + 2), f)
    end function map_color

    ! Two colours mixed, as a colormap built from a list of stops mixes them.
    pure function blend_hex(c0, c1, f) result(hex)
        character(len=*), intent(in) :: c0, c1
        real(dp), intent(in) :: f
        character(len=7) :: hex
        character(len=16), parameter :: D = "0123456789abcdef"
        integer :: a3(3), b3(3), v, k
        a3 = hex_rgb(c0)
        b3 = hex_rgb(c1)
        hex = "#000000"
        do k = 1, 3
            v = nint(real(a3(k), dp) + f*real(b3(k) - a3(k), dp))
            v = max(0, min(255, v))
            hex(2*k:2*k) = D(v/16 + 1:v/16 + 1)
            hex(2*k + 1:2*k + 1) = D(mod(v, 16) + 1:mod(v, 16) + 1)
        end do
    end function blend_hex

    ! Whether a sample is simply not drawn: it is missing and nothing was
    ! said about what to put in its place.
    pure function img_bad_hidden(a, v) result(hidden)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: v
        logical :: hidden
        hidden = (.not. finite(v)) .and. len_trim(a%cmap_bad) == 0
    end function img_bad_hidden

    ! The colour of one value: the colours set aside for the values that
    ! fall outside the range, or off the scale altogether, take precedence
    ! over the colormap itself.
    pure function img_color(a, v) result(hex)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: v
        character(len=7) :: hex
        if (.not. finite(v)) then
            hex = a%cmap_bad
            if (len_trim(hex) == 0) hex = map_color(a, 0.0_dp)
            return
        end if
        if (v < a%img_vmin .and. len_trim(a%cmap_under) > 0) then
            hex = a%cmap_under
            return
        end if
        if (v > a%img_vmax .and. len_trim(a%cmap_over) > 0) then
            hex = a%cmap_over
            return
        end if
        hex = map_color(a, cmap_t(a, v))
    end function img_color

    ! matplotlib's SymLogNorm with base 10 and linscale 1: linear within
    ! linthresh of zero, logarithmic outside it, and continuous at the join.
    pure function symlog_fwd(v, linthresh) result(u)
        real(dp), intent(in) :: v, linthresh
        real(dp) :: u, adj, lt
        lt = max(linthresh, tiny(1.0_dp))
        adj = 1.0_dp/(1.0_dp - exp(-1.0_dp))
        if (abs(v) <= lt) then
            u = v*adj
        else
            u = sign(lt*(adj + log10(abs(v)/lt)), v)
        end if
    end function symlog_fwd

    ! Where a value sits on the colormap, in [0, 1].
    pure function cmap_t(a, v) result(t)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: v
        real(dp) :: t, lo, hi
        integer :: i, nb
        lo = a%img_vmin
        hi = a%img_vmax
        if (allocated(a%img_bounds)) then
            ! BoundaryNorm: find the band, then take the color matplotlib
            ! gives that band, which is band*(N-1)/(nband-1) of the map.
            nb = size(a%img_bounds) - 1
            i = 0
            do while (i < nb)
                if (v < a%img_bounds(i + 2)) exit
                i = i + 1
            end do
            if (v < a%img_bounds(1)) i = 0
            i = max(0, min(nb - 1, i))
            if (nb <= 1) then
                t = 0.0_dp
            else
                t = real(int(real(i, dp) * 255.0_dp / real(nb - 1, dp)), dp) / 255.0_dp
            end if
            return
        end if
        select case (a%img_norm)
        case (NORM_LOG)
            ! Anything at or below zero has no logarithm, so it takes the
            ! bottom of the map, as matplotlib's LogNorm does.
            if (v <= 0.0_dp .or. lo <= 0.0_dp .or. hi <= lo) then
                t = 0.0_dp
            else
                t = (log10(v) - log10(lo))/(log10(hi) - log10(lo))
            end if
        case (NORM_CENTER)
            ! TwoSlopeNorm: the centre lands in the middle of the map, and
            ! each side is stretched to fill its half.
            if (v < a%img_vcenter) then
                t = 0.0_dp
                if (a%img_vcenter > lo) &
                    t = 0.5_dp*(v - lo)/(a%img_vcenter - lo)
            else
                t = 1.0_dp
                if (hi > a%img_vcenter) &
                    t = 0.5_dp + 0.5_dp*(v - a%img_vcenter)/(hi - a%img_vcenter)
            end if
        case (NORM_POWER)
            t = (v - lo)/(hi - lo)
            t = max(0.0_dp, min(1.0_dp, t))**a%img_gamma
        case (NORM_SYMLOG)
            t = symlog_fwd(hi, a%img_linthresh) - symlog_fwd(lo, a%img_linthresh)
            if (t <= 0.0_dp) then
                t = 0.0_dp
            else
                t = (symlog_fwd(v, a%img_linthresh) &
                     - symlog_fwd(lo, a%img_linthresh))/t
            end if
        case default
            t = (v - lo)/(hi - lo)
        end select
    end function cmap_t

    ! matplotlib layers artists rather than drawing them in call order: an
    ! image is at 0, a patch at 1, the grid at 1.5, a line at 2 and text at
    ! 3. So a bar drawn after a line still sits under it, and the grid rules
    ! across the bars but not across the lines.
    pure function series_z(s) result(z)
        type(series_t), intent(in) :: s
        real(dp) :: z
        if (s%zorder >= 0.0_dp) then
            z = s%zorder
        else if (is_patch_series(s%kind) .or. s%kind == SERIES_PATCH .or. &
                 s%kind == SERIES_PIE .or. s%kind == SERIES_VIOLIN) then
            z = Z_PATCH
        else
            z = Z_LINE
        end if
    end function series_z

    ! Series drawn as filled shapes, which take a swatch in the legend.
    pure function is_patch_series(kd) result(v)
        integer, intent(in) :: kd
        logical :: v
        v = kd == SERIES_BAR .or. kd == SERIES_BARH .or. kd == SERIES_FILL .or. &
            kd == SERIES_HSPAN .or. kd == SERIES_VSPAN
    end function is_patch_series

    function point_color(s, j) result(col)
        type(series_t), intent(in) :: s
        integer, intent(in) :: j
        character(len=7) :: col
        if (allocated(s%pcolor)) then
            col = s%pcolor(j)
        else
            col = s%color
        end if
    end function point_color

    ! matplotlib's "best" needs a data-overlap search; upper right is the
    ! placement it picks for the common case, so we use it as the fallback.
    ! Place the legend box against a point given in axes coordinates. loc then
    ! names the corner of the box that touches that point, which is what lets
    ! loc="upper left" with bbox_to_anchor=[1.02, 1] sit outside the axes.
    subroutine legend_anchor(loc, ax_l, ax_t, ax_b, ax_w, bbox, &
                             leg_w, leg_h, leg_x, leg_y)
        character(len=*), intent(in) :: loc
        real(dp), intent(in) :: ax_l, ax_t, ax_b, ax_w, bbox(2), leg_w, leg_h
        real(dp), intent(out) :: leg_x, leg_y
        real(dp) :: px, py
        px = ax_l + bbox(1) * ax_w
        py = ax_b - bbox(2) * (ax_b - ax_t)
        if (index(loc, "right") > 0) then
            leg_x = px - leg_w
        else if (index(loc, "center") > 0 .and. index(loc, "left") == 0) then
            leg_x = px - 0.5_dp * leg_w
        else
            leg_x = px
        end if
        if (index(loc, "lower") > 0) then
            leg_y = py - leg_h
        else if (index(loc, "upper") > 0 .or. loc == "best") then
            leg_y = py
        else
            leg_y = py - 0.5_dp * leg_h
        end if
    end subroutine legend_anchor

    ! matplotlib's loc="best": try each of the ten placements in its own
    ! order and take the one the data runs into least, ties going to the
    ! earlier candidate. matplotlib counts artist vertices inside the box;
    ! so does this.
    function legend_best(a, xmin, xmax, ymin, ymax, xsc, ysc, &
                         ax_l, ax_r, ax_t, ax_b, leg_w, leg_h) result(loc)
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: xmin, xmax, ymin, ymax
        type(scale_t), intent(in) :: xsc, ysc
        real(dp), intent(in) :: ax_l, ax_r, ax_t, ax_b, leg_w, leg_h
        character(len=16) :: loc
        character(len=16), parameter :: cand(10) = &
            [character(len=16) :: "upper right", "upper left", "lower left", &
             "lower right", "center right", "center left", "center right", &
             "lower center", "upper center", "center"]
        integer :: k, i, j, bad, best
        real(dp) :: lx, ly, px, py, ax_w, ax_h

        ax_w = ax_r - ax_l
        ax_h = ax_b - ax_t
        loc = cand(1)
        best = huge(1)
        do k = 1, 10
            call legend_origin(cand(k), ax_l, ax_r, ax_t, ax_b, leg_w, leg_h, lx, ly)
            bad = 0
            do i = 1, a%n_series
                if (.not. allocated(a%series(i)%x)) cycle
                if (.not. allocated(a%series(i)%y)) cycle
                do j = 1, a%series(i)%n
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
                    if (px >= lx .and. px <= lx + leg_w .and. &
                        py >= ly .and. py <= ly + leg_h) bad = bad + 1
                end do
            end do
            if (bad < best) then
                best = bad
                loc = cand(k)
            end if
            if (best == 0) exit
        end do
    end function legend_best

    subroutine legend_origin(loc, ax_l, ax_r, ax_t, ax_b, leg_w, leg_h, leg_x, leg_y)
        character(len=*), intent(in) :: loc
        real(dp), intent(in) :: ax_l, ax_r, ax_t, ax_b, leg_w, leg_h
        real(dp), intent(out) :: leg_x, leg_y
        ! matplotlib's borderaxespad: half the legend font size.
        real(dp), parameter :: pad = 5.0_dp

        select case (trim(loc))
        case ("upper left", "left")
            leg_x = ax_l + pad
            leg_y = ax_t + pad
        case ("lower left")
            leg_x = ax_l + pad
            leg_y = ax_b - leg_h - pad
        case ("lower right")
            leg_x = ax_r - leg_w - pad
            leg_y = ax_b - leg_h - pad
        case ("lower center", "lower")
            leg_x = 0.5_dp * (ax_l + ax_r - leg_w)
            leg_y = ax_b - leg_h - pad
        case ("upper center", "upper")
            leg_x = 0.5_dp * (ax_l + ax_r - leg_w)
            leg_y = ax_t + pad
        case ("center")
            leg_x = 0.5_dp * (ax_l + ax_r - leg_w)
            leg_y = 0.5_dp * (ax_t + ax_b - leg_h)
        case ("center left")
            leg_x = ax_l + pad
            leg_y = 0.5_dp * (ax_t + ax_b - leg_h)
        case ("center right", "right")
            leg_x = ax_r - leg_w - pad
            leg_y = 0.5_dp * (ax_t + ax_b - leg_h)
        case default
            leg_x = ax_r - leg_w - pad
            leg_y = ax_t + pad
        end select
    end subroutine legend_origin

    ! f carries what the whole axis agreed on: the decimal count, or -1
    ! when the scale writes its own labels, and anything factored out.
    subroutine tick_label(labeled, lab, i, v, sc, f, date_unit, out, n)
        logical, intent(in) :: labeled
        character(len=24), intent(in) :: lab(MAX_TICKS)
        integer, intent(in) :: i
        real(dp), intent(in) :: v
        type(scale_t), intent(in) :: sc
        type(tickfmt_t), intent(in) :: f
        integer, intent(in) :: date_unit
        character(len=*), intent(out) :: out
        integer, intent(out) :: n
        if (labeled) then
            n = len_trim(lab(i))
            out(1:n) = trim(lab(i))
        else if (date_unit > 0) then
            call format_date(v, date_unit, out, n)
        else if (sc%kind == SCALE_LOG) then
            call format_tick_to(v, .true., out, n)
        else
            select case (f%style)
            case (FMT_PERCENT)
                call format_percent(v, f%whole, f%dec, out, n)
            case (FMT_COMMA)
                call format_grouped(v, f%dec, out, n)
            case default
                call format_tick_fixed((v - f%off)/10.0_dp**f%oom, f%dec, out, n)
            end select
        end if
    end subroutine tick_label

    ! The label for one minor tick, empty unless the axis is logarithmic
    ! and short enough that matplotlib would label its multiples.
    subroutine log_minor_label(v, sc, vmin, vmax, out, n)
        real(dp), intent(in) :: v, vmin, vmax
        type(scale_t), intent(in) :: sc
        character(len=*), intent(out) :: out
        integer, intent(out) :: n
        integer :: k, expn

        n = 0
        if (sc%kind /= SCALE_LOG) return
        if (vmin <= 0.0_dp .or. vmax <= vmin .or. v <= 0.0_dp) return
        expn = floor(log10(v) + 1.0e-9_dp)
        k = nint(v/10.0_dp**expn)
        if (.not. log_minor_labelled(k, log10(vmax/vmin))) return
        call format_log_minor_to(v, out, n)
    end subroutine log_minor_label

    ! The decimal count for one axis: none when the scale is not linear,
    ! since those label themselves.
    function axis_decimals(t, nt, sc) result(d)
        real(dp), intent(in) :: t(MAX_TICKS)
        integer, intent(in) :: nt
        type(scale_t), intent(in) :: sc
        integer :: d
        if (sc%kind /= SCALE_LINEAR) then
            d = -1
        else
            d = tick_decimals(t, nt)
        end if
    end function axis_decimals

    ! The same for an axis that may factor an offset or a power of ten out
    ! of its labels. Only a linear axis does; a log or date axis writes its
    ! own labels and has nothing to factor.
    function axis_fmt(t, nt, a, is_x, vmin, vmax) result(f)
        real(dp), intent(in) :: t(MAX_TICKS), vmin, vmax
        integer, intent(in) :: nt
        type(axes_t), intent(in) :: a
        logical, intent(in) :: is_x
        type(tickfmt_t) :: f
        type(scale_t) :: sc

        sc = a%ysc
        if (is_x) sc = a%xsc
        if (sc%kind /= SCALE_LINEAR) then
            f%dec = -1
            return
        end if
        if (is_x) then
            f%style = a%xfmt_style
            f%whole = a%xfmt_whole
            call tick_offset(t, nt, vmin, vmax, f%off, f%oom, &
                             a%x_use_offset, a%x_scilo, a%x_scihi)
        else
            f%style = a%yfmt_style
            f%whole = a%yfmt_whole
            call tick_offset(t, nt, vmin, vmax, f%off, f%oom, &
                             a%y_use_offset, a%y_scilo, a%y_scihi)
        end if
        ! A named formatter writes the value itself, so nothing may be
        ! taken out of it first.
        if (f%style /= FMT_AUTO) then
            f%off = 0.0_dp
            f%oom = 0
        end if
        f%dec = tick_decimals_at(t, nt, f%off, f%oom)
        if (is_x .and. a%xfmt_dec >= 0) f%dec = a%xfmt_dec
        if (.not. is_x .and. a%yfmt_dec >= 0) f%dec = a%yfmt_dec
    end function axis_fmt

    ! A polar axes. The box holds the largest circle that fits: the angle
    ! runs anticlockwise from the right and the radius from the middle out,
    ! so the whole of the drawing is one change of coordinates away from
    ! everything else here.
    subroutine render_polar(b, a, ax_l, ax_r, ax_b, ax_t)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: ax_l, ax_r, ax_b, ax_t
        integer, parameter :: NC = 180
        real(dp) :: cx, cy, R, rmax, t(MAX_TICKS), cxs(NC + 1), cys(NC + 1)
        real(dp) :: ang, rr, lx, ly
        real(dp), allocatable :: px(:), py(:)
        character(len=64) :: lbl
        integer :: nt, i, j, k, n, unit, dec

        cx = 0.5_dp*(ax_l + ax_r)
        cy = 0.5_dp*(ax_t + ax_b)
        R = 0.5_dp*min(ax_r - ax_l, ax_b - ax_t)

        ! The radius starts at the middle and reaches 5% past the data, as
        ! matplotlib scales it.
        rmax = 0.0_dp
        do i = 1, a%n_series
            do j = 1, a%series(i)%n
                rmax = max(rmax, a%series(i)%y(j))
            end do
        end do
        if (a%ylim_set) rmax = a%ymax_user
        if (rmax <= 0.0_dp) rmax = 1.0_dp
        if (.not. a%ylim_set) rmax = 1.05_dp*rmax

        call linear_ticks(0.0_dp, rmax, tick_space(ax_b - ax_t, a%ytick_size, .false.), t, nt)
        dec = tick_decimals(t, nt)

        ! Grid: a circle at every radial tick and a spoke every 45 degrees.
        if (a%grid_on) then
            do k = 1, nt
                if (t(k) <= 0.0_dp .or. t(k) > rmax) cycle
                call polar_circle(cx, cy, R*t(k)/rmax, cxs, cys)
                call append_stroke_path(b, cxs, cys, NC + 1, rc_grid_color, 0.8_dp, 1.0_dp)
            end do
            do k = 0, 7
                ang = real(k, dp)*PI/4.0_dp
                call append_stroke_path(b, [cx, cx + R*cos(ang)], &
                                        [cy, cy - R*sin(ang)], 2, &
                                        rc_grid_color, 0.8_dp, 1.0_dp)
            end do
        end if

        ! The data, drawn straight through the change of coordinates.
        do i = 1, a%n_series
            n = a%series(i)%n
            if (n < 1) cycle
            allocate (px(n), py(n))
            do j = 1, n
                rr = R*a%series(i)%y(j)/rmax
                px(j) = cx + rr*cos(a%series(i)%x(j))
                py(j) = cy - rr*sin(a%series(i)%x(j))
            end do
            if (a%series(i)%linestyle /= LINE_NONE .and. n >= 2) &
                call append_stroke_path(b, px, py, n, trim(a%series(i)%color), &
                                        a%series(i)%linewidth, a%series(i)%alpha)
            if (a%series(i)%marker /= MARKER_NONE) &
                call append_markers(b, a%series(i)%marker, px, py, n, &
                                    a%series(i)%markersize, trim(a%series(i)%color), &
                                    a%series(i)%alpha)
            deallocate (px, py)
        end do

        ! The outer circle is the whole of the frame a polar axes has.
        call polar_circle(cx, cy, R, cxs, cys)
        call append_stroke_path(b, cxs, cys, NC + 1, rc_spine_color, 0.8_dp, 1.0_dp)

        ! Angles are labelled outside the circle, radii along the 22.5
        ! degree line, which is where matplotlib puts them.
        do k = 0, 7
            ang = real(k, dp)*PI/4.0_dp
            ! The degree sign is byte 176, which the backends know how to
            ! write: a character reference in SVG, WinAnsi in PDF, and an
            ! outline of its own in PNG.
            write (lbl, "(I0,A)") k*45, achar(176)
            lx = cx + (R + 4.0_dp + 0.5_dp*a%xtick_size)*cos(ang)
            ly = cy - (R + 4.0_dp + 0.5_dp*a%xtick_size)*sin(ang) + 0.36_dp*a%xtick_size
            call append_text(b, lx, ly, trim(lbl), "middle", a%xtick_size, rc_text_color)
        end do
        do k = 1, nt
            if (t(k) <= 0.0_dp .or. t(k) > rmax) cycle
            call format_tick_fixed(t(k), dec, lbl, n)
            rr = R*t(k)/rmax
            lx = cx + rr*cos(22.5_dp*PI/180.0_dp)
            ly = cy - rr*sin(22.5_dp*PI/180.0_dp) + 0.36_dp*a%ytick_size
            call append_text(b, lx, ly, lbl(1:n), "middle", a%ytick_size, rc_text_color)
        end do

        ! The title clears the angle label above the circle rather than
        ! sitting on the box, which would run straight through it.
        if (len_trim(a%title) > 0) &
            call append_text(b, cx, cy - R - 4.0_dp - 1.6_dp*a%xtick_size &
                             - 0.5_dp*a%title_size, trim(a%title), &
                             "center", a%title_size, rc_text_color)
    end subroutine render_polar

    ! Data bounds, then matplotlib's 3D margins: five percent in x and y,
    ! none in z, and then the whole box widened by 25/24 about its centre,
    ! which is the compensation mplot3d applies to every 3D axes.
    subroutine limits3d(a, lims)
        type(axes_t), intent(in) :: a
        real(dp), intent(out) :: lims(6)
        real(dp), parameter :: MARG(3) = [0.05_dp, 0.05_dp, 0.0_dp]
        real(dp) :: lo(3), hi(3), c, d
        logical :: have
        integer :: i, k

        lo = huge(1.0_dp)
        hi = -huge(1.0_dp)
        have = .false.
        do i = 1, a%n_series
            select case (a%series(i)%kind)
            case (SERIES_LINE3D, SERIES_SCATTER3D, SERIES_TRISURF)
                if (a%series(i)%nolim) cycle
                do k = 1, a%series(i)%n
                    lo(1) = min(lo(1), a%series(i)%x(k))
                    hi(1) = max(hi(1), a%series(i)%x(k))
                    lo(2) = min(lo(2), a%series(i)%y(k))
                    hi(2) = max(hi(2), a%series(i)%y(k))
                    lo(3) = min(lo(3), a%series(i)%z(k))
                    hi(3) = max(hi(3), a%series(i)%z(k))
                end do
                have = .true.
            case (SERIES_BAR3D)
                lo(1) = min(lo(1), minval(a%series(i)%x))
                hi(1) = max(hi(1), maxval(a%series(i)%x) + a%series(i)%d3x)
                lo(2) = min(lo(2), minval(a%series(i)%y))
                hi(2) = max(hi(2), maxval(a%series(i)%y) + a%series(i)%d3y)
                lo(3) = min(lo(3), minval(a%series(i)%z))
                hi(3) = max(hi(3), maxval(a%series(i)%z2))
                have = .true.
            case (SERIES_SURFACE)
                lo(1) = min(lo(1), minval(a%series(i)%x))
                hi(1) = max(hi(1), maxval(a%series(i)%x))
                lo(2) = min(lo(2), minval(a%series(i)%y))
                hi(2) = max(hi(2), maxval(a%series(i)%y))
                lo(3) = min(lo(3), minval(a%series(i)%zg))
                hi(3) = max(hi(3), maxval(a%series(i)%zg))
                have = .true.
            end select
        end do
        if (.not. have) then
            lo = 0.0_dp
            hi = 1.0_dp
        end if

        do i = 1, 3
            ! An axis with no spread at all is widened the way matplotlib's
            ! nonsingular does it: by a twentieth of itself, or to plus and
            ! minus a twentieth when it sits on zero.
            if (hi(i) <= lo(i)) then
                if (abs(lo(i)) + abs(hi(i)) == 0.0_dp) then
                    lo(i) = -0.05_dp
                    hi(i) = 0.05_dp
                else
                    lo(i) = lo(i) - 0.05_dp*abs(lo(i))
                    hi(i) = hi(i) + 0.05_dp*abs(hi(i))
                end if
            end if
            d = hi(i) - lo(i)
            lo(i) = lo(i) - MARG(i)*d
            hi(i) = hi(i) + MARG(i)*d
            c = 0.5_dp*(lo(i) + hi(i))
            lo(i) = c + (lo(i) - c)*25.0_dp/24.0_dp
            hi(i) = c + (hi(i) - c)*25.0_dp/24.0_dp
            lims(2*i - 1) = lo(i)
            lims(2*i) = hi(i)
        end do

        ! Limits set by hand get none of that: they are taken as given.
        if (a%xlim_set) lims(1:2) = [a%xmin_user, a%xmax_user]
        if (a%ylim_set) lims(3:4) = [a%ymin_user, a%ymax_user]
        if (a%zlim_set) lims(5:6) = [a%zmin_user, a%zmax_user]
    end subroutine limits3d

    ! A data point to device points, with the projected depth alongside.
    subroutine dev3(M, bl, bt, side, x, y, z, ux, uy, uz)
        real(dp), intent(in) :: M(4, 4), bl, bt, side, x, y, z
        real(dp), intent(out) :: ux, uy, uz
        real(dp) :: px, py, span

        call proj3d_point(M, x, y, z, px, py, uz)
        span = PROJ3D_VIEW_MAX - PROJ3D_VIEW_MIN
        ux = bl + (px - PROJ3D_VIEW_MIN)/span*side
        uy = bt + (PROJ3D_VIEW_MAX - py)/span*side
    end subroutine dev3

    ! Push a point away from the middle of the box along every axis but
    ! one, which is how mplot3d keeps tick and axis labels clear of the box.
    pure function move_out(p, centers, d, keep) result(q)
        real(dp), intent(in) :: p(3), centers(3), d(3)
        integer, intent(in) :: keep
        real(dp) :: q(3)
        integer :: j

        q = p
        do j = 1, 3
            if (j == keep) cycle
            if (p(j) < centers(j)) then
                q(j) = p(j) - d(j)
            else
                q(j) = p(j) + d(j)
            end if
        end do
    end function move_out

    ! Order 1..n so that key decreases: the painter's algorithm needs the
    ! far things first. Insertion sort, because n is a few thousand faces
    ! at worst and the sort is not what costs.
    pure subroutine order_far_first(key, n, idx)
        real(dp), intent(in) :: key(:)
        integer, intent(in) :: n
        integer, intent(out) :: idx(:)
        integer :: i, j, t

        do i = 1, n
            idx(i) = i
        end do
        do i = 2, n
            t = idx(i)
            j = i - 1
            do while (j >= 1)
                if (key(idx(j)) >= key(t)) exit
                idx(j + 1) = idx(j)
                j = j - 1
            end do
            idx(j + 1) = t
        end do
    end subroutine order_far_first

    subroutine render_axes3d(b, a, ax_l, ax_r, ax_b, ax_t)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: ax_l, ax_r, ax_b, ax_t
        ! The six faces of the box as corner numbers: the two x faces, then
        ! the two y faces, then the two z faces.
        integer, parameter :: PLANES(4, 6) = reshape([ &
                                             1, 4, 8, 5, 2, 3, 7, 6, &
                                             1, 2, 6, 5, 4, 3, 7, 8, &
                                             1, 2, 3, 4, 5, 6, 7, 8], [4, 6])
        ! The rcParams axes3d.*axis.panecolor greys.
        character(len=7), parameter :: PANE(3) = ["#f2f2f2", "#e6e6e6", "#ececec"]
        ! Which coordinate a tick mark runs along, per axis.
        integer, parameter :: TICKDIR(3) = [2, 1, 1]
        ! The two coordinates that pin each axis line to an edge of the box.
        integer, parameter :: JUG(2, 3) = reshape([2, 1, 1, 2, 1, 3], [2, 3])
        real(dp) :: lims(6), M(4, 4), side, bl, bt
        real(dp) :: ccx(8), ccy(8), ccz(8), dx(8), dy(8), dz(8)
        real(dp) :: mins(3), maxs(3), centers(3), deltas(3), minmax(3), maxmin(3)
        real(dp) :: e1(3), e2(3), p(3), q(3), lab_d(3), tick_d(3)
        real(dp) :: gx(3), gy(3), gd(3), ex(2), ey(2), tx(2), ty(2), td2
        real(dp) :: tk(MAX_TICKS, 3), none(MAX_TICKS)
        integer :: ntk(3), dec(3)
        real(dp) :: dpp, tdel, t_out, t_in, ang, fs
        logical :: highs(3)
        integer :: i, k, j1, j2, pl, td, ln, du
        character(len=64) :: lbl
        character(len=256) :: axl
        type(scale_t) :: lin

        ! mplot3d squares the axes box before drawing, so that the picture
        ! does not stretch when the figure does.
        side = min(ax_r - ax_l, ax_b - ax_t)
        bl = 0.5_dp*(ax_l + ax_r) - 0.5_dp*side
        bt = 0.5_dp*(ax_t + ax_b) - 0.5_dp*side

        call limits3d(a, lims)
        call proj3d_matrix(lims, a%elev, a%azim, M)
        do i = 1, 3
            mins(i) = lims(2*i - 1)
            maxs(i) = lims(2*i)
        end do
        centers = 0.5_dp*(mins + maxs)
        deltas = 0.08_dp*(maxs - mins)

        ccx = [lims(1), lims(2), lims(2), lims(1), lims(1), lims(2), lims(2), lims(1)]
        ccy = [lims(3), lims(3), lims(4), lims(4), lims(3), lims(3), lims(4), lims(4)]
        ccz = [lims(5), lims(5), lims(5), lims(5), lims(6), lims(6), lims(6), lims(6)]
        do k = 1, 8
            call dev3(M, bl, bt, side, ccx(k), ccy(k), ccz(k), dx(k), dy(k), dz(k))
        end do

        ! Of each pair of parallel faces, the one further from the camera is
        ! the one that gets a pane and a grid.
        do i = 1, 3
            highs(i) = sum(dz(PLANES(:, 2*i - 1)))/4.0_dp < sum(dz(PLANES(:, 2*i)))/4.0_dp
            if (highs(i)) then
                minmax(i) = maxs(i)
                maxmin(i) = mins(i)
            else
                minmax(i) = mins(i)
                maxmin(i) = maxs(i)
            end if
        end do

        do i = 1, 3
            pl = 2*i - 1
            if (highs(i)) pl = 2*i
            call append_polygon(b, dx(PLANES(:, pl)), dy(PLANES(:, pl)), 4, &
                                PANE(i), 0.5_dp, .true.)
        end do

        ! x and y run across the picture and z up it, so x and y are spaced
        ! as a horizontal axis is and z as a vertical one.
        none = 0.0_dp
        call axis_ticks(a%n_xticks, a%xtick_pos, lims(1), lims(2), lin, &
                        tick_space(side, a%xtick_size, .true.), .false., tk(:, 1), ntk(1), du)
        call axis_ticks(a%n_yticks, a%ytick_pos, lims(3), lims(4), lin, &
                        tick_space(side, a%ytick_size, .true.), .false., tk(:, 2), ntk(2), du)
        call axis_ticks(0, none, lims(5), lims(6), lin, &
                        tick_space(side, a%xtick_size, .false.), .false., tk(:, 3), ntk(3), du)
        do i = 1, 3
            dec(i) = axis_decimals(tk(:, i), ntk(i), lin)
        end do

        if (a%grid_on) then
            do i = 1, 3
                ! A grid line runs from the far edge of one pane, through
                ! the corner where the two panes meet, to the far edge of
                ! the other.
                j1 = mod(i, 3) + 1
                j2 = mod(i + 1, 3) + 1
                do k = 1, ntk(i)
                    if (tk(k, i) < mins(i) .or. tk(k, i) > maxs(i)) cycle
                    p = minmax
                    p(i) = tk(k, i)
                    q = p
                    q(j1) = maxmin(j1)
                    call dev3(M, bl, bt, side, q(1), q(2), q(3), gx(1), gy(1), gd(1))
                    call dev3(M, bl, bt, side, p(1), p(2), p(3), gx(2), gy(2), gd(2))
                    q = p
                    q(j2) = maxmin(j2)
                    call dev3(M, bl, bt, side, q(1), q(2), q(3), gx(3), gy(3), gd(3))
                    call append_stroke_path(b, gx, gy, 3, rc_grid_color, rc_grid_lw, 1.0_dp)
                end do
            end do
        end if

        ! What one point is worth in data units. A 3D axes has no one
        ! direction to measure a point along, so mplot3d averages the two
        ! sides of the box, and so do we.
        dpp = 48.0_dp/(2.0_dp*side)
        tick_d = (3.5_dp + 8.0_dp)*dpp*deltas
        lab_d = (LABEL_PAD + 21.0_dp)*dpp*deltas

        do i = 1, 3
            fs = a%xtick_size
            e1 = minmax
            e1(JUG(1, i)) = maxmin(JUG(1, i))
            e2 = e1
            e2(JUG(2, i)) = maxmin(JUG(2, i))
            call dev3(M, bl, bt, side, e1(1), e1(2), e1(3), ex(1), ey(1), td2)
            call dev3(M, bl, bt, side, e2(1), e2(2), e2(3), ex(2), ey(2), td2)
            call append_stroke_path(b, ex, ey, 2, rc_spine_color, rc_spine_lw, 1.0_dp)

            td = TICKDIR(i)
            tdel = deltas(td)
            if (.not. highs(td)) tdel = -tdel
            t_out = e1(td) + 0.1_dp*tdel
            t_in = e1(td) - 0.2_dp*tdel

            do k = 1, ntk(i)
                if (tk(k, i) < mins(i) .or. tk(k, i) > maxs(i)) cycle
                q = e1
                q(i) = tk(k, i)
                q(td) = t_out
                call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(1), ty(1), td2)
                q(td) = t_in
                call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(2), ty(2), td2)
                call append_stroke_path(b, tx, ty, 2, rc_spine_color, 0.8_dp, 1.0_dp)

                q(td) = e1(td)
                q = move_out(q, centers, tick_d, i)
                call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(1), ty(1), td2)
                ! All three axes label their ticks the way an x axis does:
                ! centred, and hanging from the top of the text.
                call format_tick_fixed(tk(k, i), dec(i), lbl, ln)
                call append_text(b, tx(1), ty(1) + 0.76_dp*fs, lbl(1:ln), &
                                 "center", fs, rc_text_color)
            end do

            axl = ""
            if (i == 1) axl = a%xlabel
            if (i == 2) axl = a%ylabel
            if (i == 3) axl = a%zlabel
            if (len_trim(axl) == 0) cycle
            q = move_out(0.5_dp*(e1 + e2), centers, lab_d, i)
            call dev3(M, bl, bt, side, q(1), q(2), q(3), tx(1), ty(1), td2)
            ! A short label stays upright; a long one is turned to lie along
            ! its own axis, which is where matplotlib draws the line too.
            ang = 0.0_dp
            if (len_trim(axl) > 4) ang = atan2(ey(1) - ey(2), ex(2) - ex(1))*180.0_dp/PI
            call append_text(b, tx(1), ty(1) + 0.36_dp*a%xlabel_size, trim(axl), &
                             "center", a%xlabel_size, rc_text_color, ang)
        end do

        call render_series3d(b, a, M, bl, bt, side)

        if (len_trim(a%title) > 0) &
            call append_text(b, bl + 0.5_dp*side, bt - 0.5_dp*a%title_size, &
                             trim(a%title), "center", a%title_size, rc_text_color)
    end subroutine render_axes3d

    subroutine render_series3d(b, a, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        real(dp), allocatable :: px(:), py(:), pd(:)
        integer, allocatable :: idx(:)
        real(dp) :: sat, lo, span, one(1), oney(1)
        integer :: i, k, n
        type(paint_t) :: p

        do i = 1, a%n_series
            n = a%series(i)%n
            select case (a%series(i)%kind)
            case (SERIES_LINE3D)
                allocate (px(n), py(n), pd(n))
                do k = 1, n
                    call dev3(M, bl, bt, side, a%series(i)%x(k), a%series(i)%y(k), &
                              a%series(i)%z(k), px(k), py(k), pd(k))
                end do
                if (a%series(i)%linestyle /= LINE_NONE) then
                    p = pen(a%series(i)%color, a%series(i)%linewidth, &
                            a%series(i)%alpha, a%series(i)%linestyle)
                    call b%draw_path(px, py, line_verbs(n), n, p)
                end if
                call append_markers(b, a%series(i)%marker, px, py, n, &
                                    a%series(i)%markersize, a%series(i)%color, &
                                    a%series(i)%alpha)
                deallocate (px, py, pd)
            case (SERIES_SCATTER3D)
                allocate (px(n), py(n), pd(n), idx(n))
                do k = 1, n
                    call dev3(M, bl, bt, side, a%series(i)%x(k), a%series(i)%y(k), &
                              a%series(i)%z(k), px(k), py(k), pd(k))
                end do
                call order_far_first(pd, n, idx)
                ! Depth shading: the further a point is, the more it fades
                ! into the background, down to three tenths opacity.
                lo = minval(pd)
                span = sqrt((maxval(px) - minval(px))**2 + (maxval(py) - minval(py))**2 &
                            + (maxval(pd) - lo)**2)
                do k = 1, n
                    sat = 1.0_dp
                    if (span > 0.0_dp) sat = 1.0_dp - (pd(idx(k)) - lo)/span
                    sat = max(0.3_dp, min(1.0_dp, sat))
                    one(1) = px(idx(k))
                    oney(1) = py(idx(k))
                    call append_markers(b, a%series(i)%marker, one, oney, 1, &
                                        a%series(i)%markersize, a%series(i)%color, &
                                        a%series(i)%alpha*sat)
                end do
                deallocate (px, py, pd, idx)
            case (SERIES_SURFACE)
                call render_surface(b, a%series(i), M, bl, bt, side)
            case (SERIES_TRISURF)
                call render_trisurf(b, a%series(i), M, bl, bt, side)
            case (SERIES_BAR3D)
                call render_bar3d(b, a%series(i), M, bl, bt, side)
            end select
        end do
    end subroutine render_series3d

    ! One quadrilateral per cell of the grid, lit by matplotlib's default
    ! light source and painted back to front.
    subroutine render_surface(b, s, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        real(dp), allocatable :: fx(:, :), fy(:, :), depth(:)
        integer, allocatable :: idx(:)
        real(dp) :: cx(4), cy(4), cz(4), ux, uy, uz
        real(dp) :: zlo, zhi, t
        integer :: nx, ny, nf, f, i, j, c, rgb(3)
        character(len=7) :: col

        nx = size(s%x)
        ny = size(s%y)
        nf = (nx - 1)*(ny - 1)
        if (nf <= 0) return
        if (s%wire) then
            call render_wireframe(b, s, M, bl, bt, side)
            return
        end if
        allocate (fx(4, nf), fy(4, nf), depth(nf), idx(nf))
        rgb = hex_rgb(s%color)
        zlo = minval(s%zg)
        zhi = maxval(s%zg)

        f = 0
        do j = 1, ny - 1
            do i = 1, nx - 1
                f = f + 1
                ! The perimeter of the cell, anticlockwise in index space.
                cx = [s%x(i), s%x(i + 1), s%x(i + 1), s%x(i)]
                cy = [s%y(j), s%y(j), s%y(j + 1), s%y(j + 1)]
                cz = [s%zg(j, i), s%zg(j, i + 1), s%zg(j + 1, i + 1), s%zg(j + 1, i)]
                depth(f) = 0.0_dp
                do c = 1, 4
                    call dev3(M, bl, bt, side, cx(c), cy(c), cz(c), ux, uy, uz)
                    fx(c, f) = ux
                    fy(c, f) = uy
                    depth(f) = depth(f) + 0.25_dp*uz
                end do
            end do
        end do

        call order_far_first(depth, nf, idx)

        f = 0
        do j = 1, ny - 1
            do i = 1, nx - 1
                f = f + 1
                cx = [s%x(i), s%x(i + 1), s%x(i + 1), s%x(i)]
                cy = [s%y(j), s%y(j), s%y(j + 1), s%y(j + 1)]
                cz = [s%zg(j, i), s%zg(j, i + 1), s%zg(j + 1, i + 1), s%zg(j + 1, i)]
                depth(f) = facet_light(cx, cy, cz)
            end do
        end do

        do f = 1, nf
            ! depth now carries the lighting factor for each face, and idx
            ! the order to paint them in.
            if (s%scmap >= 0) then
                ! A colormapped surface takes its color from the height of
                ! the cell, and is then lit exactly as a flat one is.
                j = (idx(f) - 1)/(nx - 1) + 1
                i = idx(f) - (j - 1)*(nx - 1)
                t = 0.0_dp
                if (zhi > zlo) t = (0.25_dp*(s%zg(j, i) + s%zg(j, i + 1) &
                                             + s%zg(j + 1, i) + s%zg(j + 1, i + 1)) &
                                    - zlo)/(zhi - zlo)
                rgb = hex_rgb(cmap_color(s%scmap, t))
            end if
            col = "#"//hex_pair(nint(rgb(1)*depth(idx(f)))) &
                  //hex_pair(nint(rgb(2)*depth(idx(f)))) &
                  //hex_pair(nint(rgb(3)*depth(idx(f))))
            call append_polygon(b, fx(:, idx(f)), fy(:, idx(f)), 4, col, s%alpha, .true.)
        end do
        deallocate (fx, fy, depth, idx)
    end subroutine render_surface

    ! One lit facet per triangle, painted back to front. The lighting is
    ! the surface's, so the two agree where they overlap.
    subroutine render_trisurf(b, s, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        real(dp), allocatable :: px(:), py(:), pz(:), depth(:)
        integer, allocatable :: idx(:)
        real(dp) :: tx(3), ty(3), zlo, zhi, t, shade
        integer :: n, nt, i, k, v(3), rgb(3)
        character(len=7) :: col

        n = s%n
        nt = size(s%tri, 2)
        if (nt < 1) return
        allocate (px(n), py(n), pz(n), depth(nt), idx(nt))
        do i = 1, n
            call dev3(M, bl, bt, side, s%x(i), s%y(i), s%z(i), px(i), py(i), pz(i))
        end do
        do k = 1, nt
            depth(k) = sum(pz(s%tri(:, k)))/3.0_dp
        end do
        call order_far_first(depth, nt, idx)

        rgb = hex_rgb(s%color)
        zlo = minval(s%z)
        zhi = maxval(s%z)
        do k = 1, nt
            v = s%tri(:, idx(k))
            tx = px(v)
            ty = py(v)
            shade = facet_light(s%x(v), s%y(v), s%z(v))
            if (s%scmap >= 0) then
                t = 0.0_dp
                if (zhi > zlo) t = (sum(s%z(v))/3.0_dp - zlo)/(zhi - zlo)
                rgb = hex_rgb(cmap_color(s%scmap, t))
            end if
            col = "#"//hex_pair(nint(rgb(1)*shade))//hex_pair(nint(rgb(2)*shade)) &
                  //hex_pair(nint(rgb(3)*shade))
            call append_polygon(b, tx, ty, 3, col, s%alpha, .true.)
        end do
        deallocate (px, py, pz, depth, idx)
    end subroutine render_trisurf

    ! How brightly matplotlib's default light source lights a facet, as a
    ! factor on its colour. The facet is given by its corners in data space.
    pure function facet_light(cx, cy, cz) result(f)
        real(dp), intent(in) :: cx(:), cy(:), cz(:)
        ! LightSource(azdeg=225, altdeg=19.4712), as a direction.
        real(dp), parameter :: AZ = (90.0_dp - 225.0_dp)*PI/180.0_dp
        real(dp), parameter :: ALT = 19.4712_dp*PI/180.0_dp
        real(dp) :: f, dir(3), v1(3), v2(3), nrm(3), nl, shade

        dir = [cos(AZ)*cos(ALT), sin(AZ)*cos(ALT), sin(ALT)]
        v1 = [cx(1) - cx(2), cy(1) - cy(2), cz(1) - cz(2)]
        v2 = [cx(2) - cx(3), cy(2) - cy(3), cz(2) - cz(3)]
        nrm = [v1(2)*v2(3) - v1(3)*v2(2), v1(3)*v2(1) - v1(1)*v2(3), &
               v1(1)*v2(2) - v1(2)*v2(1)]
        nl = sqrt(sum(nrm**2))
        shade = 0.0_dp
        if (nl > 0.0_dp) shade = dot_product(nrm/nl, dir)
        f = 0.3_dp + 0.7_dp*(shade + 1.0_dp)/2.0_dp
    end function facet_light

    ! Six faces per box, all of them wound anticlockwise seen from outside
    ! so that the light falls on them the way mplot3d's does, and the whole
    ! lot painted back to front together.
    subroutine render_bar3d(b, s, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        ! The unit cube's faces: -z, +z, -y, +y, -x, +x.
        integer, parameter :: FACE(3, 4, 6) = reshape([ &
            0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, &
            0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, &
            0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, &
            0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, &
            0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, &
            1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 1], [3, 4, 6])
        real(dp), allocatable :: fx(:, :), fy(:, :), depth(:), light(:)
        integer, allocatable :: idx(:)
        real(dp) :: cx(4), cy(4), cz(4), ux, uy, uz
        integer :: nf, f, i, k, c, rgb(3)
        character(len=7) :: col

        nf = 6*s%n
        if (nf < 1) return
        allocate (fx(4, nf), fy(4, nf), depth(nf), light(nf), idx(nf))
        rgb = hex_rgb(s%color)
        f = 0
        do i = 1, s%n
            do k = 1, 6
                f = f + 1
                do c = 1, 4
                    cx(c) = s%x(i) + s%d3x*real(FACE(1, c, k), dp)
                    cy(c) = s%y(i) + s%d3y*real(FACE(2, c, k), dp)
                    cz(c) = s%z(i) + (s%z2(i) - s%z(i))*real(FACE(3, c, k), dp)
                end do
                light(f) = facet_light(cx, cy, cz)
                depth(f) = 0.0_dp
                do c = 1, 4
                    call dev3(M, bl, bt, side, cx(c), cy(c), cz(c), ux, uy, uz)
                    fx(c, f) = ux
                    fy(c, f) = uy
                    depth(f) = depth(f) + 0.25_dp*uz
                end do
            end do
        end do

        call order_far_first(depth, nf, idx)
        do f = 1, nf
            col = "#"//hex_pair(nint(rgb(1)*light(idx(f)))) &
                  //hex_pair(nint(rgb(2)*light(idx(f)))) &
                  //hex_pair(nint(rgb(3)*light(idx(f))))
            call append_polygon(b, fx(:, idx(f)), fy(:, idx(f)), 4, col, s%alpha, .true.)
        end do
        deallocate (fx, fy, depth, light, idx)
    end subroutine render_bar3d

    ! Every line of the mesh, in front and behind alike: matplotlib hides
    ! nothing in a wireframe either.
    subroutine render_wireframe(b, s, M, bl, bt, side)
        class(renderer_t), intent(inout) :: b
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: M(4, 4), bl, bt, side
        real(dp), allocatable :: gx(:, :), gy(:, :)
        real(dp) :: uz
        integer :: nx, ny, i, j

        nx = size(s%x)
        ny = size(s%y)
        allocate (gx(ny, nx), gy(ny, nx))
        do j = 1, ny
            do i = 1, nx
                call dev3(M, bl, bt, side, s%x(i), s%y(j), s%zg(j, i), &
                          gx(j, i), gy(j, i), uz)
            end do
        end do
        do j = 1, ny
            call append_stroke_path(b, gx(j, :), gy(j, :), nx, s%color, s%linewidth, &
                                    s%alpha)
        end do
        do i = 1, nx
            call append_stroke_path(b, gx(:, i), gy(:, i), ny, s%color, s%linewidth, &
                                    s%alpha)
        end do
        deallocate (gx, gy)
    end subroutine render_wireframe

    pure subroutine polar_circle(cx, cy, r, px, py)
        real(dp), intent(in) :: cx, cy, r
        real(dp), intent(out) :: px(:), py(:)
        integer :: i, n
        real(dp) :: t

        n = size(px) - 1
        do i = 1, n + 1
            t = 2.0_dp*PI*real(i - 1, dp)/real(n, dp)
            px(i) = cx + r*cos(t)
            py(i) = cy - r*sin(t)
        end do
    end subroutine polar_circle

    subroutine render_axes(b, a, idx, W, H, clear)
        class(renderer_t), intent(inout) :: b
        type(axes_t), intent(in) :: a
        integer, intent(in) :: idx
        real(dp), intent(in) :: W, H
        ! matplotlib's transparent=True clears the axes patch as well.
        logical, intent(in) :: clear
        real(dp) :: ax_l, ax_r, ax_b, ax_t, ax_w, ax_h
        real(dp) :: xmin, xmax, ymin, ymax
        real(dp) :: xticks(MAX_TICKS), yticks(MAX_TICKS)
        real(dp) :: xminor(MAX_MINOR), yminor(MAX_MINOR)
        real(dp) :: span_x, span_y, sc, new_w, new_h
        integer :: nxt, nyt, nxm, nym, i, j, n, nl
        integer :: x_unit, y_unit
        real(dp) :: px, py, ms, r, mid
        real(dp) :: x_edge, x_out, y_edge, y_out
        character(len=64) :: lbl
        character(len=64) :: tx, ty
        type(tickfmt_t) :: xfmt, yfmt
        integer :: tn, tyn
        character(len=512) :: esc
        integer :: ln, en
        type(scale_t) :: xsc, ysc
        integer :: n_leg, k, max_lbl, n_col, n_row, lc, lr
        real(dp) :: leg_x, leg_y, leg_w, leg_h, row_h, col_w, ttl_h, leg_x0
        character(len=16) :: leg_loc
        real(dp), allocatable :: lx(:), ly(:), mkx(:), mky(:)
        logical, allocatable :: lstart(:)
        integer, allocatable :: ord(:)
        logical :: brk, grid_done
        integer :: nm
        integer :: k0, k1, ii
        type(paint_t) :: pnt

        ax_l = a%left * W
        ax_r = a%right * W
        ax_b = (1.0_dp - a%bottom) * H
        ax_t = (1.0_dp - a%top) * H
        ax_w = ax_r - ax_l
        ax_h = ax_b - ax_t

        if (a%polar) then
            call render_polar(b, a, ax_l, ax_r, ax_b, ax_t)
            return
        end if

        if (a%is3d) then
            call render_axes3d(b, a, ax_l, ax_r, ax_b, ax_t)
            return
        end if

        call compute_limits(a, xmin, xmax, ymin, ymax)

        if (a%aspect > 0.0_dp) then
            span_x = abs(xmax - xmin)
            span_y = abs(ymax - ymin)
            if (span_x > 0.0_dp .and. span_y > 0.0_dp) then
                if (a%aspect_datalim) then
                    ! Stretch whichever range is drawn too short, about its
                    ! own centre, so the box keeps the place it was given.
                    sc = (ax_h / span_y) / (ax_w / span_x) / a%aspect
                    if (sc > 1.0_dp) then
                        call grow_about_centre(ymin, ymax, sc)
                    else
                        call grow_about_centre(xmin, xmax, 1.0_dp / sc)
                    end if
                else
                    sc = min(ax_w / span_x, ax_h / (a%aspect * span_y))
                    new_w = sc * span_x
                    new_h = sc * a%aspect * span_y
                    ax_l = ax_l + 0.5_dp * (ax_w - new_w)
                    ax_t = ax_t + 0.5_dp * (ax_h - new_h)
                    ax_w = new_w
                    ax_h = new_h
                    ax_r = ax_l + ax_w
                    ax_b = ax_t + ax_h
                end if
            end if
        end if
        xsc = a%xsc
        ysc = a%ysc

        call axis_ticks(a%n_xticks, a%xtick_pos, xmin, xmax, xsc, &
                        nbins_for(a%xtick_nbins, ax_w, a%xtick_size, .true.), a%x_date, &
                        xticks, nxt, x_unit, a%xtick_base)
        call axis_ticks(a%n_yticks, a%ytick_pos, min(ymin, ymax), max(ymin, ymax), &
                        ysc, nbins_for(a%ytick_nbins, ax_h, a%ytick_size, .false.), &
                        a%y_date, yticks, nyt, y_unit, a%ytick_base)
        if (a%n_xticks == 0) call prune_ticks(a%xtick_prune, xticks, nxt)
        if (a%n_yticks == 0) call prune_ticks(a%ytick_prune, yticks, nyt)
        xfmt = axis_fmt(xticks, nxt, a, .true., xmin, xmax)
        yfmt = axis_fmt(yticks, nyt, a, .false., min(ymin, ymax), max(ymin, ymax))
        if (wants_minor(a) .or. a%xsc%kind == SCALE_LOG .or. &
            a%ysc%kind == SCALE_LOG) then
            call minor_positions(xticks, nxt, xmin, xmax, xsc, xminor, nxm)
            call minor_positions(yticks, nyt, ymin, ymax, ysc, yminor, nym)
        else
            nxm = 0
            nym = 0
        end if
        ! Minor ticks placed by hand stand in for the automatic ones, and
        ! asking for them is asking to see them.
        if (a%n_xminor > 0) then
            nxm = a%n_xminor
            xminor(1:nxm) = a%xminor_pos(1:nxm)
        end if
        if (a%n_yminor > 0) then
            nym = a%n_yminor
            yminor(1:nym) = a%yminor_pos(1:nym)
        end if

        ! axis("off") leaves only the artists: no frame, no ticks, no labels.
        if (a%frame_off) then
            nxt = 0
            nyt = 0
            nxm = 0
            nym = 0
        end if

        ! axes face
        if (.not. clear .and. .not. a%patch_off) then
            if (len_trim(a%facecolor) > 0) then
                call append_rect(b, ax_l, ax_t, ax_w, ax_h, trim(a%facecolor), &
                                 a%face_alpha)
            else
                call append_rect(b, ax_l, ax_t, ax_w, ax_h, rc_axes_face)
            end if
        end if

        if (a%has_img .or. a%has_cont) then
            call set_clip(ax_l, ax_t, ax_w, ax_h)
            if (a%has_img) &
                call append_image(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            if (a%has_cont) &
                call append_contour(b, a, xmin, xmax, ymin, ymax, ax_l, ax_w, ax_b, ax_h, xsc, ysc)
            call clear_clip()
        end if

        ! data, layered rather than in call order. Series of equal zorder
        ! keep the order they were added in, which is what matplotlib's
        ! stable sort does too.
        allocate (ord(max(1, a%n_series)))
        do i = 1, a%n_series
            ord(i) = i
        end do
        do i = 2, a%n_series
            k0 = ord(i)
            k1 = i - 1
            do while (k1 >= 1)
                if (series_z(a%series(ord(k1))) <= series_z(a%series(k0))) exit
                ord(k1 + 1) = ord(k1)
                k1 = k1 - 1
            end do
            ord(k1 + 1) = k0
        end do

        call set_clip(ax_l, ax_t, ax_w, ax_h)
        grid_done = .false.
        do ii = 1, a%n_series
            i = ord(ii)
            if (.not. grid_done .and. series_z(a%series(i)) >= Z_GRID) then
                call append_grid(b, a, xticks, nxt, yticks, nyt, xminor, nxm, &
                                 yminor, nym, xmin, xmax, &
                                 ymin, ymax, ax_l, ax_r, ax_t, ax_b, ax_w, ax_h, &
                                 xsc, ysc)
                grid_done = .true.
            end if
            n = a%series(i)%n
            if (n <= 0) cycle
            if (allocated(lstart)) deallocate (lstart)
            if (allocated(lx)) deallocate (lx)
            if (allocated(ly)) deallocate (ly)
            if (allocated(mkx)) deallocate (mkx)
            if (allocated(mky)) deallocate (mky)
            allocate (lx(n), ly(n), lstart(n), mkx(n), mky(n))

            select case (a%series(i)%kind)
            case (SERIES_BOX)
                call append_box(b, a%series(i), xmin, xmax, ymin, ymax, &
                                ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_VIOLIN)
                call append_violin(b, a%series(i), xmin, xmax, ymin, ymax, &
                                   ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_PIE)
                call append_pie(b, a%series(i), xmin, xmax, ymin, ymax, &
                                ax_l, ax_w, ax_b, ax_h)
                cycle
            case (SERIES_STEM)
                ! Stalk from the baseline to each sample, then the baseline
                ! itself, which matplotlib always draws in red.
                py = map_y(0.0_dp, ymin, ymax, ax_b, ax_h, ysc)
                do j = 1, n
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    call append_line(b, px, py, px, &
                                     map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc), &
                                     trim(a%series(i)%color), a%series(i)%linewidth, &
                                     LINE_SOLID, a%series(i)%alpha)
                end do
                call append_line(b, &
                                 map_x(a%series(i)%x(1), xmin, xmax, ax_l, ax_w, xsc), py, &
                                 map_x(a%series(i)%x(n), xmin, xmax, ax_l, ax_w, xsc), py, &
                                 "#d62728", a%series(i)%linewidth, LINE_SOLID, &
                                 a%series(i)%alpha)
                do j = 1, n
                    call append_marker(b, MARKER_CIRCLE, &
                                       map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc), &
                                       map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc), &
                                       a%series(i)%markersize, trim(a%series(i)%color), &
                                       a%series(i)%alpha)
                end do
                cycle
            case (SERIES_BAR, SERIES_BARH)
                do j = 1, n
                    call append_bar(b, a%series(i), j, xmin, xmax, ymin, ymax, &
                                    ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                end do
                cycle
            case (SERIES_FILL)
                call append_fill(b, a%series(i), xmin, xmax, ymin, ymax, &
                                 ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_HLINE)
                py = map_y(a%series(i)%y(1), ymin, ymax, ax_b, ax_h, ysc)
                call append_line(b, ax_l, py, ax_l + ax_w, py, &
                                 trim(a%series(i)%color), a%series(i)%linewidth, &
                                 a%series(i)%linestyle, a%series(i)%alpha)
                cycle
            case (SERIES_VLINE)
                px = map_x(a%series(i)%x(1), xmin, xmax, ax_l, ax_w, xsc)
                call append_line(b, px, ax_b, px, ax_b - ax_h, &
                                 trim(a%series(i)%color), a%series(i)%linewidth, &
                                 a%series(i)%linestyle, a%series(i)%alpha)
                cycle
            case (SERIES_AXLINE)
                call append_axline(b, a%series(i), xmin, xmax, ymin, ymax, &
                                   ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_HLINES)
                do j = 1, n
                    py = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
                    call append_line(b, &
                        map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc), py, &
                        map_x(a%series(i)%y2(j), xmin, xmax, ax_l, ax_w, xsc), py, &
                        trim(a%series(i)%color), a%series(i)%linewidth, &
                        a%series(i)%linestyle, a%series(i)%alpha)
                end do
                cycle
            case (SERIES_VLINES)
                do j = 1, n
                    px = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                    call append_line(b, px, &
                        map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc), px, &
                        map_y(a%series(i)%y2(j), ymin, ymax, ax_b, ax_h, ysc), &
                        trim(a%series(i)%color), a%series(i)%linewidth, &
                        a%series(i)%linestyle, a%series(i)%alpha)
                end do
                cycle
            case (SERIES_PATCH)
                call append_patch(b, a%series(i), xmin, xmax, ymin, ymax, &
                                  ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_QUIVER)
                call append_quiver(b, a%series(i), xmin, xmax, ymin, ymax, &
                                   ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_ARROWHEAD)
                call append_arrowheads(b, a%series(i), xmin, xmax, ymin, ymax, &
                                       ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_HSPAN, SERIES_VSPAN)
                call append_span(b, a%series(i), xmin, xmax, ymin, ymax, &
                                 ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                cycle
            case (SERIES_ERRORBAR)
                do j = 1, n
                    call append_errorbar(b, a%series(i), j, xmin, xmax, ymin, ymax, &
                                         ax_l, ax_w, ax_b, ax_h, xsc, ysc)
                end do
            end select

            ! Points that cannot be drawn are dropped once, and both the line
            ! and its markers then work from the same list. A point is
            ! missing if it is NaN or infinite, or if a log axis cannot
            ! place it; lstart then remembers where the line has to restart,
            ! because matplotlib leaves a gap rather than drawing across it.
            nl = 0
            brk = .true.
            do j = 1, n
                if (.not. finite(a%series(i)%x(j)) .or. &
                    .not. finite(a%series(i)%y(j)) .or. &
                    (a%xsc%kind == SCALE_LOG .and. a%series(i)%x(j) <= 0.0_dp) .or. &
                    (a%ysc%kind == SCALE_LOG .and. a%series(i)%y(j) <= 0.0_dp)) then
                    brk = .true.
                    cycle
                end if
                nl = nl + 1
                lx(nl) = map_x(a%series(i)%x(j), xmin, xmax, ax_l, ax_w, xsc)
                ly(nl) = map_y(a%series(i)%y(j), ymin, ymax, ax_b, ax_h, ysc)
                lstart(nl) = brk
                brk = .false.
            end do

            if (a%series(i)%linestyle /= LINE_NONE .and. nl >= 2) then
                pnt = pen(trim(a%series(i)%color), a%series(i)%linewidth, &
                          a%series(i)%alpha, a%series(i)%linestyle)
                if (a%series(i)%n_dash > 0) then
                    pnt%n_dash = a%series(i)%n_dash
                    pnt%dash(1:pnt%n_dash) = &
                        a%series(i)%dashes(1:a%series(i)%n_dash)
                end if
                pnt%join = JOIN_ROUND
                pnt%cap = CAP_BUTT
                k0 = 1
                do while (k0 <= nl)
                    k1 = k0 + 1
                    do while (k1 <= nl)
                        if (lstart(k1)) exit
                        k1 = k1 + 1
                    end do
                    if (k1 - k0 >= 2) &
                        call b%draw_path(lx(k0:k1 - 1), ly(k0:k1 - 1), &
                                         line_verbs(k1 - k0), k1 - k0, pnt)
                    k0 = k1
                end do
            end if

            if (a%series(i)%marker /= MARKER_NONE .and. nl > 0) then
                if (allocated(a%series(i)%psize) .or. allocated(a%series(i)%pcolor)) then
                    ! scatter gave every point its own size or color, so the
                    ! shape cannot be shared between them.
                    ms = a%series(i)%markersize
                    do j = 1, nl
                        if (allocated(a%series(i)%psize)) ms = a%series(i)%psize(j)
                        call append_marker(b, a%series(i)%marker, lx(j), ly(j), ms, &
                                           point_color(a%series(i), j), &
                                           a%series(i)%alpha)
                    end do
                else
                    ! markevery thins the points; the line keeps all of them.
                    nm = 0
                    do j = 1, nl, a%series(i)%markevery
                        nm = nm + 1
                        mkx(nm) = lx(j)
                        mky(nm) = ly(j)
                    end do
                    call append_markers(b, a%series(i)%marker, mkx, mky, nm, &
                                        a%series(i)%markersize, &
                                        trim(a%series(i)%color), a%series(i)%alpha, &
                                        face=trim(a%series(i)%mfc), &
                                        edge=trim(a%series(i)%mec), &
                                        ewidth=a%series(i)%mew)
                end if
            end if
        end do
        if (.not. grid_done) &
            call append_grid(b, a, xticks, nxt, yticks, nyt, xminor, nxm, &
                                 yminor, nym, xmin, xmax, &
                             ymin, ymax, ax_l, ax_r, ax_t, ax_b, ax_w, ax_h, xsc, ysc)
        ! annotations, in data coordinates
        do i = 1, a%n_texts
            ! figtext is placed on the canvas, so it waits until the clip
            ! is dropped below.
            if (a%texts(i)%in_fig) cycle
            if (a%texts(i)%in_axes) then
                px = ax_l + a%texts(i)%x*ax_w
                py = ax_b - a%texts(i)%y*ax_h
            else
                px = map_x(a%texts(i)%x, xmin, xmax, ax_l, ax_w, xsc)
                py = map_y(a%texts(i)%y, ymin, ymax, ax_b, ax_h, ysc)
            end if
            if (a%texts(i)%has_arrow) &
                call append_arrow(b, a%texts(i), px, py, &
                                  map_x(a%texts(i)%xtail, xmin, xmax, ax_l, ax_w, xsc), &
                                  map_y(a%texts(i)%ytail, ymin, ymax, ax_b, ax_h, ysc))
            call append_annotation(b, a%texts(i), px, py)
        end do

        call clear_clip()

        do i = 1, a%n_texts
            if (.not. a%texts(i)%in_fig) cycle
            call append_annotation(b, a%texts(i), a%texts(i)%x*W, &
                                   (1.0_dp - a%texts(i)%y)*H)
        end do

        ! spines. All four still go out as one <rect>, so a plot that leaves
        ! them alone renders exactly as it did before they could be hidden.
        if (.not. a%frame_off) then
            if (all(a%spine)) then
                call b%draw_rect(ax_l, ax_t, ax_w, ax_h, pen(rc_spine_color, rc_spine_lw))
            else
                if (a%spine(SPINE_LEFT)) call append_spine(b, ax_l, ax_t, ax_l, ax_b)
                if (a%spine(SPINE_RIGHT)) call append_spine(b, ax_r, ax_t, ax_r, ax_b)
                if (a%spine(SPINE_BOTTOM)) call append_spine(b, ax_l, ax_b, ax_r, ax_b)
                if (a%spine(SPINE_TOP)) call append_spine(b, ax_l, ax_t, ax_r, ax_t)
            end if
        end if

        ! x ticks, on the bottom unless this is a twiny
        if (a%x_top) then
            x_edge = ax_t
            x_out = -1.0_dp
        else
            x_edge = ax_b
            x_out = 1.0_dp
        end if
        if (a%yaxis_off) nyt = 0
        if (a%xaxis_off) nxt = 0
        if (a%xaxis_off .or. .not. (a%minor_ticks .or. a%n_xminor > 0 .or. &
                                    xsc%kind == SCALE_LOG)) nxm = 0
        if (a%yaxis_off .or. .not. (a%minor_ticks .or. a%n_yminor > 0 .or. &
                                    ysc%kind == SCALE_LOG)) nym = 0

        do i = 1, nxt
            px = map_x(xticks(i), xmin, xmax, ax_l, ax_w, xsc)
            call append_tick_at(b, px, x_edge, 0.0_dp, x_out, a%xtick_dir, a%xtick_len)
            if (.not. a%xticklabels_off) then
                call tick_label(a%xtick_labeled, a%xtick_lab, i, xticks(i), xsc, &
                                xfmt, x_unit, lbl, ln)
                call append_tick_text(b, px, x_edge + x_out * xtick_gap(a) - &
                                      merge(6.0_dp, 0.0_dp, a%x_top), lbl(1:ln), "center", &
                                      a%xtick_size, a%xtick_rot)
            end if
        end do
        do i = 1, nxm
            px = map_x(xminor(i), xmin, xmax, ax_l, ax_w, xsc)
            call append_tick_at(b, px, x_edge, 0.0_dp, x_out, a%xtick_dir, &
                                MINOR_FRAC * a%xtick_len)
            ! A log axis spanning a decade or less has too few decades to
            ! label, so matplotlib labels the multiples between them.
            if (a%xticklabels_off) cycle
            call log_minor_label(xminor(i), xsc, xmin, xmax, lbl, ln)
            if (ln > 0) &
                call append_tick_text(b, px, x_edge + x_out * xtick_gap(a) - &
                                      merge(6.0_dp, 0.0_dp, a%x_top), lbl(1:ln), &
                                      "center", a%xtick_size, a%xtick_rot)
        end do

        ! y ticks, on the left unless this is a twinx
        if (a%y_right) then
            y_edge = ax_r
            y_out = 1.0_dp
        else
            y_edge = ax_l
            y_out = -1.0_dp
        end if

        do i = 1, nyt
            py = map_y(yticks(i), ymin, ymax, ax_b, ax_h, ysc)
            call append_tick_at(b, y_edge, py, y_out, 0.0_dp, a%ytick_dir, a%ytick_len)
            if (.not. a%yticklabels_off) then
                call tick_label(a%ytick_labeled, a%ytick_lab, i, yticks(i), ysc, &
                                yfmt, y_unit, lbl, ln)
                call append_tick_text(b, y_edge + y_out * 7.0_dp, py + 3.5_dp, lbl(1:ln), &
                                      merge("left ", "right", a%y_right), &
                                      a%ytick_size, a%ytick_rot)
            end if
        end do
        do i = 1, nym
            py = map_y(yminor(i), ymin, ymax, ax_b, ax_h, ysc)
            call append_tick_at(b, y_edge, py, y_out, 0.0_dp, a%ytick_dir, &
                                MINOR_FRAC * a%ytick_len)
            if (a%yticklabels_off) cycle
            call log_minor_label(yminor(i), ysc, ymin, ymax, lbl, ln)
            if (ln > 0) &
                call append_tick_text(b, y_edge + y_out * 7.0_dp, py + 3.5_dp, &
                                      lbl(1:ln), merge("left ", "right", a%y_right), &
                                      a%ytick_size, a%ytick_rot)
        end do

        ! What the labels left out, written once at the end of the axis:
        ! matplotlib puts it past the far end of the x axis and above the
        ! top of the y axis, three points clear of the tick labels.
        if (nxt > 0 .and. .not. a%xticklabels_off) then
            call format_offset_text(xfmt%off, xfmt%oom, lbl, ln)
            if (ln > 0) &
                call append_text(b, ax_r, ax_b + xtick_gap(a) + 0.24_dp*a%xtick_size &
                                 + 3.0_dp + 0.76_dp*a%xtick_size, lbl(1:ln), "right", &
                                 a%xtick_size, rc_text_color)
        end if
        if (nyt > 0 .and. .not. a%yticklabels_off) then
            call format_offset_text(yfmt%off, yfmt%oom, lbl, ln)
            if (ln > 0) &
                call append_text(b, y_edge, ax_t - 3.0_dp, lbl(1:ln), &
                                 merge("right", "left ", a%y_right), &
                                 a%ytick_size, rc_text_color)
        end if

        if (len_trim(a%xlabel) > 0) then
            call append_text(b, 0.5_dp * (ax_l + ax_r), &
                             ax_b + xtick_gap(a) + 0.24_dp * a%xtick_size + 1.84_dp &
                             + a%xlabel_pad - LABEL_PAD &
                             + 0.76_dp * a%xlabel_size, trim(a%xlabel), &
                             "center", a%xlabel_size, rc_text_color, &
                             weight=a%xlabel_w, slant=a%xlabel_sl)
        end if

        if (len_trim(a%ylabel) > 0) then
            ! The right-hand label of a twinx faces the other way, so that it
            ! reads from outside the axes just as the left-hand one does.
            mid = y_edge + y_out * ylabel_out(a)
            call append_text(b, mid, 0.5_dp*(ax_t + ax_b), trim(a%ylabel), &
                             "center", a%ylabel_size, rc_text_color, &
                             merge(-90.0_dp, 90.0_dp, a%y_right), &
                             weight=a%ylabel_w, slant=a%ylabel_sl)
        end if

        ! title, centred over the axes unless it was asked to sit against
        ! one end of it
        if (len_trim(a%title) > 0) then
            select case (trim(a%title_loc))
            case ("left")
                call append_text(b, ax_l, ax_t - 0.5_dp*a%title_size, &
                                 trim(a%title), "left", a%title_size, &
                                 rc_text_color, weight=a%title_w, slant=a%title_sl)
            case ("right")
                call append_text(b, ax_r, ax_t - 0.5_dp*a%title_size, &
                                 trim(a%title), "right", a%title_size, &
                                 rc_text_color, weight=a%title_w, slant=a%title_sl)
            case default
                call append_text(b, 0.5_dp*(ax_l + ax_r), &
                                 ax_t - 0.5_dp*a%title_size, trim(a%title), &
                                 "center", a%title_size, rc_text_color, &
                                 weight=a%title_w, slant=a%title_sl)
            end select
        end if

        ! legend
        if (a%legend_on) then
            n_leg = 0
            max_lbl = 0
            do i = 1, a%n_series
                if (in_legend(a%series(i))) then
                    n_leg = n_leg + 1
                    max_lbl = max(max_lbl, len_trim(a%series(i)%label))
                end if
            end do
            if (n_leg > 0) then
                row_h = 18.0_dp * a%legend_size / LEGEND_FONT
                n_col = min(max(1, a%legend_ncol), n_leg)
                n_row = (n_leg + n_col - 1) / n_col
                ! Sample line (leg_x+8 .. leg_x+28), gap to the text at
                ! leg_x+34, the label itself, and a trailing pad.
                col_w = 34.0_dp + real(max_lbl, dp) * LEGEND_CHAR_W &
                        * a%legend_size / LEGEND_FONT + 8.0_dp
                leg_w = real(n_col, dp) * col_w
                ttl_h = 0.0_dp
                if (len_trim(a%legend_title) > 0) ttl_h = row_h
                leg_h = 8.0_dp + ttl_h + real(n_row, dp) * row_h
                if (a%legend_has_bbox) then
                    call legend_anchor(a%legend_loc, ax_l, ax_t, ax_b, &
                                       ax_w, a%legend_bbox, leg_w, leg_h, &
                                       leg_x, leg_y)
                else
                    leg_w = min(leg_w, ax_w - 16.0_dp)
                    leg_loc = a%legend_loc
                    if (trim(leg_loc) == "best") &
                        leg_loc = legend_best(a, xmin, xmax, ymin, ymax, xsc, ysc, &
                                              ax_l, ax_r, ax_t, ax_b, leg_w, leg_h)
                    call legend_origin(leg_loc, ax_l, ax_r, ax_t, ax_b, &
                                       leg_w, leg_h, leg_x, leg_y)
                end if
                if (len_trim(a%legend_title) > 0) then
                    call append_text(b, leg_x + 0.5_dp * leg_w, &
                                     leg_y + 4.0_dp + 0.5_dp * row_h + 3.5_dp, &
                                     trim(a%legend_title), "center", &
                                     a%legend_size, rc_text_color)
                end if
                if (a%legend_frame) then
                    pnt = brush(rc_legend_face)
                    pnt%stroked = .true.
                    pnt%stroke_rgb = hex_rgb(rc_legend_edge)
                    pnt%line_width = 0.8_dp
                    call b%draw_rect(leg_x, leg_y, leg_w, leg_h, pnt, 2.0_dp)
                end if
                leg_x0 = leg_x
                k = 0
                do i = 1, a%n_series
                    if (.not. in_legend(a%series(i))) cycle
                    k = k + 1
                    lc = (k - 1) / n_row
                    lr = k - 1 - lc * n_row
                    leg_x = leg_x0 + real(lc, dp) * col_w
                    py = leg_y + 4.0_dp + ttl_h + (real(lr, dp) + 0.5_dp) * row_h
                    if (is_patch_series(a%series(i)%kind)) then
                        ! Anything filled shows a swatch, not a line: a bar
                        ! or a band has no line to show.
                        call append_rect(b, leg_x + 8.0_dp, &
                                         py - 0.35_dp*a%legend_size, 20.0_dp, &
                                         0.7_dp*a%legend_size, &
                                         point_color(a%series(i), 1), &
                                         a%series(i)%alpha)
                    else if (a%series(i)%linestyle /= LINE_NONE) then
                        call append_line(b, leg_x + 8.0_dp, py, leg_x + 28.0_dp, py, &
                                         trim(a%series(i)%color), &
                                         a%series(i)%linewidth, &
                                         a%series(i)%linestyle, 1.0_dp)
                    end if
                    mid = leg_x + 18.0_dp
                    if (a%series(i)%marker /= MARKER_NONE) then
                        call append_marker(b, a%series(i)%marker, mid, py, &
                                           a%series(i)%markersize, &
                                           trim(a%series(i)%color), a%series(i)%alpha)
                    end if
                    call append_text(b, leg_x + 34.0_dp, py + 3.5_dp, &
                                     trim(a%series(i)%label), &
                                     "left", a%legend_size, rc_text_color)
                end do
            end if
        end if

        call render_table(b, a, ax_l, ax_w, ax_b, ax_h)
    end subroutine render_axes

    ! Draw the whole figure into any backend. Everything above this point is
    ! format independent; the only thing render_svg and its PDF and PNG
    ! counterparts add is the choice of renderer.
    subroutine render_figure(b, facecolor, transparent, bbox_inches, pad_inches)
        class(renderer_t), intent(inout) :: b
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        real(dp) :: W, H, vx, vy, vw, vh, bpad
        character(len=512) :: esc
        character(len=7) :: face
        logical :: clear
        integer :: i, en, n_grp

        call ensure_fig()
        ! constrained_layout fits the decorations just before the drawing
        ! goes out, when every label the figure will carry is known.
        if (fig_constrained) call tight_layout()
        clear = .false.
        if (present(transparent)) clear = transparent
        face = resolve_color(facecolor)
        if (len_trim(face) == 0) face = rc_fig_face
        call clear_clip()

        W = fig_w_in*PT_PER_IN
        H = fig_h_in*PT_PER_IN
        vx = 0.0_dp
        vy = 0.0_dp
        vw = W
        vh = H
        if (present(bbox_inches)) then
            if (lower(trim(bbox_inches)) == "tight") then
                bpad = 0.1_dp
                if (present(pad_inches)) bpad = pad_inches
                bpad = bpad*PT_PER_IN
                ! Cropping moves the window onto the drawing, so the drawing
                ! itself needs no translation.
                call drawn_bbox(W, H, vx, vy, vw, vh)
                vx = vx - bpad
                vy = vy - bpad
                vw = vw - vx + bpad
                vh = vh - vy + bpad
            else if (len_trim(bbox_inches) > 0) then
                error stop "fplot: bbox_inches must be 'tight'"
            end if
        end if

        ! transparent drops the figure patch entirely
        if (clear) then
            call b%open_canvas(vw, vh, x0=vx, y0=vy)
        else
            call b%open_canvas(vw, vh, bg_rgb=hex_rgb(face), x0=vx, y0=vy)
        end if

        ! matplotlib counts a colorbar as an axes in its own right, and it is
        ! drawn after the axes it belongs to, so the numbering interleaves.
        n_grp = 0
        do i = 1, n_ax
            n_grp = n_grp + 1
            call b%begin_group("axes_"//int_to_str(n_grp))
            call render_axes(b, ax(i), i, W, H, clear)
            call b%end_group()
            if (ax(i)%cbar_on) then
                n_grp = n_grp + 1
                call b%begin_group("axes_"//int_to_str(n_grp))
                call append_colorbar(b, ax(i), i, W, H)
                call b%end_group()
            end if
        end do

        ! suptitle (figure-level, above all axes)
        if (len_trim(fig_suptitle) > 0) then
            call append_text(b, 0.5_dp*W, (1.0_dp - SUPTITLE_Y)*H + 4.2_dp, &
                             trim(fig_suptitle), "center", fig_suptitle_size, &
                             rc_text_color, weight=fig_suptitle_w, &
                             slant=fig_suptitle_sl)
        end if

        call b%close_canvas()
    end subroutine render_figure

    function render_svg(facecolor, transparent, bbox_inches, pad_inches) result(svg)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: svg
        type(svg_renderer_t) :: r
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        svg = r%bytes()
    end function render_svg

    function render_pdf(facecolor, transparent, bbox_inches, pad_inches) result(pdf)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: pdf
        type(pdf_renderer_t) :: r
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        pdf = r%bytes()
    end function render_pdf

    ! Unlike the vector formats, dpi is not decorative here: it is what
    ! decides how many pixels the figure is rasterized into.
    function render_eps(facecolor, transparent, bbox_inches, pad_inches) result(eps)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: eps
        type(eps_renderer_t) :: r
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        eps = r%bytes()
    end function render_eps

    function render_png(facecolor, transparent, bbox_inches, pad_inches) result(png)
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        logical, intent(in), optional :: transparent
        real(dp), intent(in), optional :: pad_inches
        character(len=:), allocatable :: png
        type(png_renderer_t) :: r
        call r%set_dpi(fig_dpi)
        call render_figure(r, facecolor, transparent, bbox_inches, pad_inches)
        png = r%bytes()
    end function render_png

    pure function lower(s) result(t)
        character(len=*), intent(in) :: s
        character(len=len(s)) :: t
        integer :: i, c
        do i = 1, len(s)
            c = iachar(s(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) then
                t(i:i) = achar(c + 32)
            else
                t(i:i) = s(i:i)
            end if
        end do
    end function lower

end module fplot_draw
