! fplot — pure Fortran pylab-style SVG plotting library.
module fplot
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
    use fplot_draw
    implicit none
    private

    public :: dp
    public :: plot, scatter, semilogx, semilogy, loglog
    public :: set_xscale, set_yscale
    public :: bar, barh, hist, fill_between, fill_betweenx, stackplot
    public :: errorbar, axhline, axvline, axline
    public :: pcolormesh, pcolor, hist2d, hexbin
    public :: matshow, eventplot, broken_barh, streamplot, table
    public :: add_axes, secondary_xaxis, secondary_yaxis
    public :: add_rectangle, add_circle, add_ellipse, add_polygon
    public :: add_arrow, add_path
    public :: polar, set_polar
    public :: quiver
    public :: axhspan, axvspan, hlines, vlines, bar_label
    public :: step, stem, pie, boxplot, violinplot
    public :: axis, set_aspect, tick_params, spines
    public :: margins, autoscale
    public :: text, annotate, figtext, set_facecolor
    public :: xticks, yticks, minorticks_on, locator_params
    public :: ticklabel_format, tick_format, tick_locator
    public :: imshow, colorbar, contour, contourf, clabel
    public :: triplot, tripcolor, tricontour, tricontourf
    public :: title, xlabel, ylabel, grid, legend
    public :: xlim, ylim, clf, savefig, show, figure
    public :: get_xlim, get_ylim, invert_xaxis, invert_yaxis
    public :: set_bad, set_under, set_over, set_cmap_colors
    public :: figlegend
    public :: render_svg, render_pdf, render_png, render_eps
    public :: add_frame, save_animation
    public :: axes3d, plot3d, scatter3d, plot_surface, plot_wireframe
    public :: plot_trisurf, bar3d, quiver3d, contour3d
    public :: view_init, zlabel, zlim
    public :: subplot, subplot2grid, subplot_mosaic, gridspec, suptitle
    public :: subplots_adjust, tight_layout, constrained_layout
    public :: xaxis_date, yaxis_date, date_num
    public :: twinx, twiny
    public :: set_fontsize, set_zorder
    public :: close, gcf
    public :: axes, subplots, sca
    public :: style_use, rc

    ! A handle to one axes of the current figure, so that a script can say
    ! ax%plot(...) instead of selecting an axes and then drawing into it.
    !
    ! It holds an index, not a copy: fplot is stateful, and every binding is
    ! "make this the current axes, then call the module procedure of the same
    ! name". That keeps one implementation of each plot type rather than two,
    ! and means the two styles can be mixed freely. Anything without a
    ! binding is still reachable by making the axes current with sca.
    type :: axes
        integer :: idx = 0
    contains
        procedure :: sca => ax_sca
        procedure :: inset_axes => ax_inset_axes
        procedure :: secondary_xaxis => ax_secondary_xaxis
        procedure :: secondary_yaxis => ax_secondary_yaxis
        procedure, private :: ax_plot, ax_plot_cat, ax_plot_y
        generic :: plot => ax_plot, ax_plot_cat, ax_plot_y
        procedure :: scatter => ax_scatter
        procedure, private :: ax_bar, ax_bar_cat, ax_barh, ax_barh_cat
        generic :: bar => ax_bar, ax_bar_cat
        generic :: barh => ax_barh, ax_barh_cat
        procedure :: bar_label => ax_bar_label
        procedure :: hist => ax_hist
        procedure :: fill_between => ax_fill_between
        procedure :: fill_betweenx => ax_fill_betweenx
        procedure :: stackplot => ax_stackplot
        procedure :: errorbar => ax_errorbar
        procedure :: step => ax_step
        procedure :: stem => ax_stem
        procedure :: quiver => ax_quiver
        procedure :: set_polar => ax_set_polar
        procedure :: add_rectangle => ax_add_rectangle
        procedure :: add_circle => ax_add_circle
        procedure :: add_ellipse => ax_add_ellipse
        procedure :: add_polygon => ax_add_polygon
        procedure, private :: ax_imshow, ax_imshow_rgb
        generic :: imshow => ax_imshow, ax_imshow_rgb
        procedure :: xaxis_date => ax_xaxis_date
        procedure :: yaxis_date => ax_yaxis_date
        procedure :: pcolormesh => ax_pcolormesh
        procedure :: pcolor => ax_pcolormesh
        procedure :: clabel => ax_clabel
        procedure :: contour => ax_contour
        procedure :: contourf => ax_contourf
        procedure :: colorbar => ax_colorbar
        procedure :: margins => ax_margins
        procedure :: autoscale => ax_autoscale
        procedure :: axhline => ax_axhline
        procedure :: axvline => ax_axvline
        procedure :: axline => ax_axline
        procedure :: axhspan => ax_axhspan
        procedure :: axvspan => ax_axvspan
        procedure :: hlines => ax_hlines
        procedure :: vlines => ax_vlines
        procedure :: text => ax_text
        procedure :: annotate => ax_annotate
        procedure :: set_title => ax_set_title
        procedure :: set_xlabel => ax_set_xlabel
        procedure :: set_ylabel => ax_set_ylabel
        procedure :: set_xlim => ax_set_xlim
        procedure :: set_ylim => ax_set_ylim
        procedure :: get_xlim => ax_get_xlim
        procedure :: get_ylim => ax_get_ylim
        procedure :: invert_xaxis => ax_invert_xaxis
        procedure :: invert_yaxis => ax_invert_yaxis
        procedure :: set_xscale => ax_set_xscale
        procedure :: set_yscale => ax_set_yscale
        procedure :: set_xticks => ax_set_xticks
        procedure :: set_yticks => ax_set_yticks
        procedure :: set_aspect => ax_set_aspect
        procedure :: grid => ax_grid
        procedure :: set_facecolor => ax_set_facecolor
        procedure :: legend => ax_legend
        procedure :: tick_params => ax_tick_params
        procedure :: spines => ax_spines
        procedure :: axis => ax_axis
        procedure :: set_zorder => ax_set_zorder
        procedure :: axes3d => ax_axes3d
        procedure :: boxplot => ax_boxplot
        procedure :: violinplot => ax_violinplot
        procedure :: broken_barh => ax_broken_barh
        procedure :: eventplot => ax_eventplot
        procedure :: hexbin => ax_hexbin
        procedure :: hist2d => ax_hist2d
        procedure :: loglog => ax_loglog
        procedure :: semilogx => ax_semilogx
        procedure :: semilogy => ax_semilogy
        procedure :: matshow => ax_matshow
        procedure :: minorticks_on => ax_minorticks_on
        procedure :: pie => ax_pie
        procedure :: polar => ax_polar
        procedure :: streamplot => ax_streamplot
        procedure :: table => ax_table
        procedure :: tick_format => ax_tick_format
        procedure :: tick_locator => ax_tick_locator
        procedure :: ticklabel_format => ax_ticklabel_format
        procedure :: add_arrow => ax_add_arrow
        procedure :: add_path => ax_add_path
        procedure :: plot3d => ax_plot3d
        procedure :: scatter3d => ax_scatter3d
        procedure :: plot_surface => ax_plot_surface
        procedure :: plot_wireframe => ax_plot_wireframe
        procedure :: plot_trisurf => ax_plot_trisurf
        procedure :: bar3d => ax_bar3d
        procedure :: quiver3d => ax_quiver3d
        procedure :: contour3d => ax_contour3d
        procedure :: view_init => ax_view_init
        procedure :: set_zlabel => ax_set_zlabel
        procedure :: set_zlim => ax_set_zlim
        procedure :: twinx => ax_twinx
        procedure :: twiny => ax_twiny
    end type axes

    ! subplots(m, n, axs) fills a grid; the rank of axs decides whether the
    ! panels come back shaped or in a row, and a lone axes needs no counts.
    ! Categories instead of numbers on an axis: the same call with a list
    ! of names, which are placed at 0, 1, 2, ... and become the tick labels.
    interface bar
        module procedure bar_num, bar_cat
    end interface bar

    interface barh
        module procedure barh_num, barh_cat
    end interface barh

    interface plot
        module procedure plot_num, plot_cat, plot_y
    end interface plot

    interface imshow
        module procedure imshow_z, imshow_rgb
    end interface imshow

    ! One dataset or a whole block of them, a row to each.
    interface boxplot
        module procedure boxplot_one, boxplot_many
    end interface boxplot

    interface violinplot
        module procedure violinplot_one, violinplot_many
    end interface violinplot

    interface subplots
        module procedure subplots_grid, subplots_row, subplots_one
    end interface subplots
contains

    ! Copy the live figure state into slot k of the store, and back out again.
    ! These two are the only places that know the full field list.
    subroutine stash_fig(k)
        integer, intent(in) :: k
        figs(k)%live = .true.
        figs(k)%w_in = fig_w_in
        figs(k)%h_in = fig_h_in
        figs(k)%dpi = fig_dpi
        figs(k)%left = fig_left
        figs(k)%right = fig_right
        figs(k)%bottom = fig_bottom
        figs(k)%top = fig_top
        figs(k)%wspace = fig_wspace
        figs(k)%hspace = fig_hspace
        figs(k)%suptitle = fig_suptitle
        figs(k)%d_title = def_title
        figs(k)%d_label = def_label
        figs(k)%d_tick = def_tick
        figs(k)%d_legend = def_legend
        figs(k)%suptitle_size = fig_suptitle_size
        figs(k)%n_ax = n_ax
        figs(k)%cur_i = cur_i
        figs(k)%grid_m = grid_m
        figs(k)%grid_n = grid_n
        figs(k)%sparse = grid_sparse
        figs(k)%constrained = fig_constrained
        figs(k)%wratio = fig_wratio
        figs(k)%hratio = fig_hratio
        if (allocated(figs(k)%ax)) deallocate (figs(k)%ax)
        ! Allocated explicitly rather than relying on reallocation on
        ! assignment, which is not on by default in every compiler.
        if (allocated(ax)) then
            allocate (figs(k)%ax(size(ax)))
            figs(k)%ax = ax
        end if
    end subroutine stash_fig

    subroutine unstash_fig(k)
        integer, intent(in) :: k
        fig_w_in = figs(k)%w_in
        fig_h_in = figs(k)%h_in
        fig_dpi = figs(k)%dpi
        fig_left = figs(k)%left
        fig_right = figs(k)%right
        fig_bottom = figs(k)%bottom
        fig_top = figs(k)%top
        fig_wspace = figs(k)%wspace
        fig_hspace = figs(k)%hspace
        fig_wratio = figs(k)%wratio
        fig_hratio = figs(k)%hratio
        fig_suptitle = figs(k)%suptitle
        def_title = figs(k)%d_title
        def_label = figs(k)%d_label
        def_tick = figs(k)%d_tick
        def_legend = figs(k)%d_legend
        fig_suptitle_size = figs(k)%suptitle_size
        n_ax = figs(k)%n_ax
        cur_i = figs(k)%cur_i
        grid_m = figs(k)%grid_m
        grid_n = figs(k)%grid_n
        grid_sparse = figs(k)%sparse
        fig_constrained = figs(k)%constrained
        if (allocated(ax)) deallocate (ax)
        if (allocated(figs(k)%ax)) then
            allocate (ax(size(figs(k)%ax)))
            ax = figs(k)%ax
        end if
        fig_initialized = .true.
    end subroutine unstash_fig

    ! The number of the active figure, matplotlib's gcf().number.
    function gcf() result(num)
        integer :: num
        call ensure_fig()
        num = cur_fig
    end function gcf

    ! close() drops the active figure, close(num) a specific one and
    ! close(all=.true.) every one, freeing the axes and their series data.
    subroutine close(num, all)
        integer, intent(in), optional :: num
        logical, intent(in), optional :: all
        integer :: k, i
        if (present(all)) then
            if (all) then
                if (allocated(figs)) deallocate (figs)
                cur_fig = 0
                call clf()
                fig_initialized = .false.
                return
            end if
        end if
        k = cur_fig
        if (present(num)) k = num
        if (k < 1) return
        if (allocated(figs)) then
            if (k <= size(figs)) then
                figs(k)%live = .false.
                if (allocated(figs(k)%ax)) deallocate (figs(k)%ax)
            end if
        end if
        if (k == cur_fig) then
            call clf()
            fig_initialized = .false.
            cur_fig = 0
            ! Fall back to whichever figure is still open, as pyplot does.
            if (allocated(figs)) then
                do i = size(figs), 1, -1
                    if (figs(i)%live) then
                        cur_fig = i
                        call unstash_fig(i)
                        exit
                    end if
                end do
            end if
        end if
    end subroutine close

    subroutine figure(figsize, dpi, num)
        real(dp), intent(in), optional :: figsize(2), dpi
        integer, intent(in), optional :: num
        integer :: k
        ! Park the figure we are leaving so it can be returned to by number.
        if (cur_fig > 0 .and. fig_initialized) then
            call grow_figs(cur_fig)
            call stash_fig(cur_fig)
        end if
        if (present(num)) then
            if (num < 1) error stop "fplot: figure num must be positive"
            k = num
        else
            k = next_free_fig()
        end if
        call grow_figs(k)
        if (figs(k)%live .and. .not. present(figsize) .and. .not. present(dpi)) then
            ! Reselecting an existing figure resumes it untouched.
            cur_fig = k
            call unstash_fig(k)
            return
        end if
        cur_fig = k
        figs(k)%live = .true.
        call clf()
        fig_w_in = FIG_W_DEFAULT
        fig_h_in = FIG_H_DEFAULT
        fig_dpi = DPI_DEFAULT
        if (present(figsize)) then
            if (figsize(1) <= 0.0_dp .or. figsize(2) <= 0.0_dp) &
                error stop "fplot: figure figsize must be positive"
            fig_w_in = figsize(1)
            fig_h_in = figsize(2)
        end if
        if (present(dpi)) then
            if (dpi <= 0.0_dp) error stop "fplot: figure dpi must be positive"
            fig_dpi = dpi
        end if
    end subroutine figure

    subroutine set_cycle(c)
        character(len=7), intent(in) :: c(:)
        rc_n_cycle = min(size(c), MAX_CYCLE)
        rc_cycle(1:rc_n_cycle) = c(1:rc_n_cycle)
    end subroutine set_cycle

    ! matplotlib's style.use. Each style is just a block of rcParams, so
    ! this sets the same globals a user could set one at a time with rc().
    subroutine style_use(name)
        character(len=*), intent(in) :: name

        ! Start from the built-in defaults so styles do not accumulate.
        rc_n_cycle = 0
        rc_fig_face = "#ffffff"
        rc_axes_face = "#ffffff"
        rc_grid_color = "#b0b0b0"
        rc_text_color = "#000000"
        rc_spine_color = "#000000"
        rc_grid_lw = 0.8_dp
        rc_spine_lw = 0.8_dp
        rc_legend_face = "#ffffff"
        rc_legend_edge = "#cccccc"
        rc_lw = default_linewidth
        rc_grid = .false.
        def_title = 12.0_dp
        def_label = 10.0_dp
        def_tick = 10.0_dp
        def_legend = 10.0_dp

        select case (lower(trim(name)))
        case ("default", "classic")
        case ("ggplot")
            rc_axes_face = "#e5e5e5"
            rc_legend_face = "#e5e5e5"
            rc_grid_color = "#ffffff"
            rc_grid_lw = 1.0_dp
            rc_spine_color = "#ffffff"
            rc_spine_lw = 1.0_dp
            rc_text_color = "#555555"
            rc_grid = .true.
            def_title = 14.4_dp
            def_label = 12.0_dp
            call set_cycle(["#e24a33", "#348abd", "#988ed5", "#777777", &
                            "#fbc15e", "#8eba42", "#ffb5b8"])
        case ("seaborn", "seaborn-darkgrid")
            rc_axes_face = "#eaeaf2"
            rc_legend_face = "#eaeaf2"
            rc_grid_color = "#ffffff"
            rc_grid_lw = 1.0_dp
            rc_spine_color = "#eaeaf2"
            rc_text_color = "#262626"
            rc_grid = .true.
            call set_cycle(["#4c72b0", "#dd8452", "#55a868", "#c44e52", &
                            "#8172b3", "#937860", "#da8bc3", "#8c8c8c", &
                            "#ccb974", "#64b5cd"])
        case ("fivethirtyeight")
            rc_fig_face = "#f0f0f0"
            rc_axes_face = "#f0f0f0"
            rc_legend_face = "#f0f0f0"
            rc_grid_color = "#cbcbcb"
            rc_grid_lw = 1.0_dp
            rc_spine_color = "#f0f0f0"
            rc_text_color = "#555555"
            rc_lw = 4.0_dp
            rc_grid = .true.
            call set_cycle(["#008fd5", "#fc4f30", "#e5ae38", "#6d904f", &
                            "#8b8b8b", "#810f7c"])
        case ("dark_background")
            rc_fig_face = "#000000"
            rc_axes_face = "#000000"
            rc_legend_face = "#000000"
            rc_legend_edge = "#ffffff"
            rc_grid_color = "#555555"
            rc_spine_color = "#ffffff"
            rc_text_color = "#ffffff"
            call set_cycle(["#8dd3c7", "#feffb3", "#bfbbd9", "#fa8174", &
                            "#81b1d2", "#fdb462", "#b3de69", "#bc82bd", &
                            "#ccebc4", "#ffed6f"])
        case ("grayscale")
            call set_cycle(["#000000", "#545454", "#7f7f7f", "#b0b0b0", &
                            "#d4d4d4"])
        case default
            error stop "fplot: unknown style"
        end select
    end subroutine style_use

    ! Individual rcParams, for the settings a user is most likely to want
    ! without adopting a whole style.
    subroutine rc(figsize, dpi, fontsize, linewidth, grid, facecolor, &
                  axes_facecolor, grid_color, text_color, color_cycle)
        real(dp), intent(in), optional :: figsize(2), dpi, fontsize, linewidth
        logical, intent(in), optional :: grid
        character(len=*), intent(in), optional :: facecolor, axes_facecolor
        character(len=*), intent(in), optional :: grid_color, text_color
        character(len=7), intent(in), optional :: color_cycle(:)

        if (present(figsize)) then
            fig_w_in = figsize(1)
            fig_h_in = figsize(2)
        end if
        if (present(dpi)) fig_dpi = dpi
        if (present(fontsize)) then
            def_title = fontsize*1.2_dp
            def_label = fontsize
            def_tick = fontsize
            def_legend = fontsize
        end if
        if (present(linewidth)) rc_lw = linewidth
        if (present(grid)) rc_grid = grid
        if (present(facecolor)) rc_fig_face = resolve_color(facecolor)
        if (present(axes_facecolor)) rc_axes_face = resolve_color(axes_facecolor)
        if (present(grid_color)) rc_grid_color = resolve_color(grid_color)
        if (present(text_color)) then
            rc_text_color = resolve_color(text_color)
            rc_spine_color = rc_text_color
        end if
        if (present(color_cycle)) call set_cycle(color_cycle)
    end subroutine rc

    ! A second axes over the current one, sharing its x axis and putting its
    ! own y axis on the right. It becomes the current axes.
    subroutine twinx()
        call add_twin(.true.)
    end subroutine twinx

    subroutine twiny()
        call add_twin(.false.)
    end subroutine twiny

    subroutine add_twin(share_x_axis)
        logical, intent(in) :: share_x_axis
        type(axes_t), allocatable :: tmp(:)
        integer :: parent

        call ensure_fig()
        parent = cur_i

        call move_alloc(ax, tmp)
        allocate (ax(n_ax + 1))
        ax(1:n_ax) = tmp
        n_ax = n_ax + 1
        cur_i = n_ax

        ax(cur_i)%left = ax(parent)%left
        ax(cur_i)%right = ax(parent)%right
        ax(cur_i)%bottom = ax(parent)%bottom
        ax(cur_i)%top = ax(parent)%top
        ax(cur_i)%patch_off = .true.
        if (share_x_axis) then
            ax(cur_i)%share_x = parent
            ax(cur_i)%xsc = ax(parent)%xsc
            ax(cur_i)%xaxis_off = .true.
            ax(cur_i)%y_right = .true.
        else
            ax(cur_i)%share_y = parent
            ax(cur_i)%ysc = ax(parent)%ysc
            ax(cur_i)%yaxis_off = .true.
            ax(cur_i)%x_top = .true.
        end if
        ! matplotlib keeps counting through one cycle across twinned axes,
        ! so the second curve does not come out the same color as the first.
        ax(cur_i)%color_cycle = ax(parent)%color_cycle
    end subroutine add_twin

    subroutine subplots_adjust(left, right, bottom, top, wspace, hspace)
        real(dp), intent(in), optional :: left, right, bottom, top, wspace, hspace
        call ensure_fig()
        if (present(left)) fig_left = left
        if (present(right)) fig_right = right
        if (present(bottom)) fig_bottom = bottom
        if (present(top)) fig_top = top
        if (present(wspace)) fig_wspace = wspace
        if (present(hspace)) fig_hspace = hspace
        if (fig_right <= fig_left .or. fig_top <= fig_bottom) &
            error stop "fplot: subplots_adjust left<right and bottom<top required"
        call layout_grid()
    end subroutine subplots_adjust

    ! Shrink the margins to what the decorations actually need, so that long
    ! tick labels stop running into the neighbouring subplot or off the page.
    ! matplotlib's constrained_layout. Ours is the tight_layout fit, run at
    ! draw time rather than as a solve, which comes to the same thing for
    ! the plain grids a figure usually has.
    subroutine constrained_layout(on)
        logical, intent(in), optional :: on
        call ensure_fig()
        fig_constrained = .true.
        if (present(on)) fig_constrained = on
    end subroutine constrained_layout

    ! Select the i-th axes (row-major) of an m x n grid, creating the grid
    ! if it differs from the current one.
    ! Treat an axis as dates: its numbers are days since 1970-01-01, the
    ! ticks land on round dates and the labels are written as dates.
    subroutine xaxis_date()
        call ensure_fig()
        ax(cur_i)%x_date = .true.
    end subroutine xaxis_date

    subroutine yaxis_date()
        call ensure_fig()
        ax(cur_i)%y_date = .true.
    end subroutine yaxis_date

    subroutine subplot(m, n, i)
        integer, intent(in) :: m, n, i

        if (m < 1 .or. n < 1 .or. i < 1 .or. i > m * n) then
            print *, "fplot: invalid subplot indices: m=", m, " n=", n, " i=", i
            error stop
        end if

        if (.not. fig_initialized) call clf()
        if (grid_m /= m .or. grid_n /= n) call new_axes_grid(m, n)
        cur_i = i
    end subroutine subplot

    ! One axes spanning several cells of a shape(1) x shape(2) grid, with
    ! its top left corner at loc, counted from zero as matplotlib counts
    ! it. Cells nobody asks for stay empty, which is how a wide panel over
    ! two narrow ones is built.
    ! Grow the axes array by one and make the new axes current.
    subroutine push_axes()
        type(axes_t), allocatable :: tmp(:)

        call ensure_fig()
        if (n_ax > 0) then
            call move_alloc(ax, tmp)
            allocate (ax(n_ax + 1))
            ax(1:n_ax) = tmp
        else
            if (allocated(ax)) deallocate (ax)
            allocate (ax(1))
        end if
        n_ax = n_ax + 1
        cur_i = n_ax
        call apply_font_defaults(ax(cur_i))
    end subroutine push_axes

    ! An axes at a rectangle of the figure the caller chose, [left, bottom,
    ! width, height] in figure fractions. The grid never moves it.
    function add_axes(rect) result(h)
        real(dp), intent(in) :: rect(4)
        type(axes) :: h

        call push_axes()
        ax(cur_i)%fixed_pos = .true.
        ax(cur_i)%left = rect(1)
        ax(cur_i)%bottom = rect(2)
        ax(cur_i)%right = rect(1) + rect(3)
        ax(cur_i)%top = rect(2) + rect(4)
        h%idx = cur_i
    end function add_axes

    ! A small axes inside another one, its rectangle given in the fractions
    ! of that axes, so it goes on fitting when the layout moves.
    function ax_inset_axes(self, bounds) result(h)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: bounds(4)
        type(axes) :: h
        integer :: parent

        parent = self%idx
        call push_axes()
        ax(cur_i)%inset_of = parent
        ax(cur_i)%inset_rect = bounds
        call layout_grid()
        h%idx = cur_i
    end function ax_inset_axes

    function ax_secondary_xaxis(self, location, scale, offset) result(h)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        h = add_secondary(self%idx, .true., location, scale, offset)
    end function ax_secondary_xaxis

    function ax_secondary_yaxis(self, location, scale, offset) result(h)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        h = add_secondary(self%idx, .false., location, scale, offset)
    end function ax_secondary_yaxis

    function secondary_xaxis(location, scale, offset) result(h)
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        call ensure_fig()
        h = add_secondary(cur_i, .true., location, scale, offset)
    end function secondary_xaxis

    function secondary_yaxis(location, scale, offset) result(h)
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        call ensure_fig()
        h = add_secondary(cur_i, .false., location, scale, offset)
    end function secondary_yaxis

    ! A second axis along one edge, reading the same data in other units.
    ! matplotlib takes a pair of functions; here the two units are related
    ! by scale and offset, which is what a change of units amounts to.
    function add_secondary(parent, is_x, location, scale, offset) result(h)
        integer, intent(in) :: parent
        logical, intent(in) :: is_x
        character(len=*), intent(in), optional :: location
        real(dp), intent(in), optional :: scale, offset
        type(axes) :: h
        character(len=8) :: loc

        loc = "top"
        if (.not. is_x) loc = "right"
        if (present(location)) loc = location

        call push_axes()
        ! Sitting on the whole of its parent keeps the two axes registered
        ! against each other however the layout moves afterwards.
        ax(cur_i)%inset_of = parent
        ax(cur_i)%inset_rect = [0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp]
        ax(cur_i)%patch_off = .true.
        ax(cur_i)%sec_of = parent
        ax(cur_i)%sec_is_x = is_x
        if (present(scale)) ax(cur_i)%sec_scale = scale
        if (present(offset)) ax(cur_i)%sec_offset = offset
        if (is_x) then
            ax(cur_i)%yaxis_off = .true.
            ax(cur_i)%x_top = loc /= "bottom"
            ax(cur_i)%xsc = ax(parent)%xsc
        else
            ax(cur_i)%xaxis_off = .true.
            ax(cur_i)%y_right = loc /= "left"
            ax(cur_i)%ysc = ax(parent)%ysc
        end if
        call layout_grid()
        h%idx = cur_i
    end function add_secondary

    ! GridSpec's width_ratios and height_ratios: the columns and rows of the
    ! grid need not be equal. Call it before the subplots are made; the
    ! ratios stay with the figure and the panels are laid out to them.
    subroutine gridspec(width_ratios, height_ratios)
        real(dp), intent(in), optional :: width_ratios(:), height_ratios(:)
        integer :: k

        call ensure_fig()
        if (present(width_ratios)) then
            fig_wratio = 1.0_dp
            do k = 1, min(size(width_ratios), MAX_RATIO)
                fig_wratio(k) = width_ratios(k)
            end do
        end if
        if (present(height_ratios)) then
            fig_hratio = 1.0_dp
            do k = 1, min(size(height_ratios), MAX_RATIO)
                fig_hratio(k) = height_ratios(k)
            end do
        end if
    end subroutine gridspec

    function subplot2grid(shape, loc, rowspan, colspan) result(h)
        integer, intent(in) :: shape(2), loc(2)
        integer, intent(in), optional :: rowspan, colspan
        type(axes) :: h
        type(axes_t), allocatable :: tmp(:)
        integer :: rs, cs

        rs = 1
        cs = 1
        if (present(rowspan)) rs = max(1, rowspan)
        if (present(colspan)) cs = max(1, colspan)
        if (shape(1) < 1 .or. shape(2) < 1) then
            print *, "fplot: invalid subplot2grid shape:", shape
            error stop
        end if

        call ensure_fig()
        ! A grid of a different shape, or one that subplot filled in for
        ! us, is not ours to add to.
        if (.not. grid_sparse .or. grid_m /= shape(1) .or. grid_n /= shape(2)) then
            if (allocated(ax)) deallocate (ax)
            n_ax = 0
            grid_m = shape(1)
            grid_n = shape(2)
            grid_sparse = .true.
        end if

        if (n_ax > 0) then
            call move_alloc(ax, tmp)
            allocate (ax(n_ax + 1))
            ax(1:n_ax) = tmp
        else
            allocate (ax(1))
        end if
        n_ax = n_ax + 1
        cur_i = n_ax
        call apply_font_defaults(ax(cur_i))
        ax(cur_i)%g_row = max(0, min(loc(1), grid_m - 1))
        ax(cur_i)%g_col = max(0, min(loc(2), grid_n - 1))
        ax(cur_i)%g_rowspan = min(rs, grid_m - ax(cur_i)%g_row)
        ax(cur_i)%g_colspan = min(cs, grid_n - ax(cur_i)%g_col)
        call layout_grid()
        h%idx = cur_i
    end function subplot2grid

    ! matplotlib's subplot_mosaic, drawn as a picture of the figure: one
    ! string per row of the grid, one character per cell, and a panel for
    ! every distinct character, spanning the cells that carry it. A space
    ! or a full stop leaves the cell empty.
    !
    ! keys comes back holding the characters in the order they were first
    ! met, so axs(k) is the panel named keys(k:k).
    subroutine subplot_mosaic(rows, keys, axs)
        character(len=*), intent(in) :: rows(:)
        character(len=*), intent(out) :: keys
        type(axes), intent(out) :: axs(:)
        integer :: nr, nc, i, j, k, m, nk, r0, r1, c0, c1
        character(len=1) :: c

        call ensure_fig()
        call clf()
        nr = size(rows)
        nc = 0
        do i = 1, nr
            nc = max(nc, len_trim(rows(i)))
        end do
        keys = ""
        nk = 0
        if (nr < 1 .or. nc < 1) return

        do i = 1, nr
            do j = 1, min(nc, len(rows(i)))
                c = rows(i)(j:j)
                if (c == " " .or. c == ".") cycle
                if (index(keys, c) > 0) cycle
                nk = nk + 1
                if (nk > len(keys) .or. nk > size(axs)) return
                keys(nk:nk) = c

                ! The panel covers the block of cells carrying this
                ! character, which matplotlib requires to be a rectangle.
                r0 = nr
                r1 = 1
                c0 = nc
                c1 = 1
                do k = 1, nr
                    do m = 1, min(nc, len(rows(k)))
                        if (rows(k)(m:m) /= c) cycle
                        r0 = min(r0, k)
                        r1 = max(r1, k)
                        c0 = min(c0, m)
                        c1 = max(c1, m)
                    end do
                end do
                axs(nk) = subplot2grid([nr, nc], [r0 - 1, c0 - 1], &
                                       rowspan=r1 - r0 + 1, colspan=c1 - c0 + 1)
            end do
        end do
    end subroutine subplot_mosaic

    ! ----------------------------------------------------------------------
    ! Axes handles. subplots makes a new figure, as matplotlib's does, and
    ! hands back one handle per panel.
    ! ----------------------------------------------------------------------

    subroutine subplots_grid(nrows, ncols, axs, sharex, sharey)
        integer, intent(in) :: nrows, ncols
        type(axes), allocatable, intent(out) :: axs(:, :)
        logical, intent(in), optional :: sharex, sharey
        integer :: r, c

        call make_grid(nrows, ncols, sharex, sharey)
        allocate (axs(nrows, ncols))
        do r = 1, nrows
            do c = 1, ncols
                axs(r, c)%idx = (r - 1)*ncols + c
            end do
        end do
    end subroutine subplots_grid

    subroutine subplots_row(nrows, ncols, axs, sharex, sharey)
        integer, intent(in) :: nrows, ncols
        type(axes), allocatable, intent(out) :: axs(:)
        logical, intent(in), optional :: sharex, sharey
        integer :: i

        call make_grid(nrows, ncols, sharex, sharey)
        allocate (axs(nrows*ncols))
        do i = 1, nrows*ncols
            axs(i)%idx = i
        end do
    end subroutine subplots_row

    subroutine subplots_one(ax_out)
        type(axes), intent(out) :: ax_out

        call make_grid(1, 1)
        ax_out%idx = 1
    end subroutine subplots_one

    ! Sharing is expressed on the axes themselves: a group is named after its
    ! first member, and only the outer panels keep their tick labels, which
    ! is most of why anyone asks for it.
    subroutine make_grid(m, n, sharex, sharey)
        integer, intent(in) :: m, n
        logical, intent(in), optional :: sharex, sharey
        integer :: i, r, c
        logical :: sx, sy

        sx = .false.
        sy = .false.
        if (present(sharex)) sx = sharex
        if (present(sharey)) sy = sharey

        call figure()
        if (m /= 1 .or. n /= 1) call new_axes_grid(m, n)
        cur_i = 1

        do i = 1, n_ax
            r = (i - 1)/n + 1
            c = i - (r - 1)*n
            if (sx) then
                ax(i)%link_x = 1
                ax(i)%xticklabels_off = r < m
            end if
            if (sy) then
                ax(i)%link_y = 1
                ax(i)%yticklabels_off = c > 1
            end if
        end do
    end subroutine make_grid

    ! Make an axes current: the module-level spelling of matplotlib's sca,
    ! and the way to reach anything that has no binding on the handle.
    subroutine sca(a)
        type(axes), intent(in) :: a

        call ensure_fig()
        if (a%idx >= 1 .and. a%idx <= n_ax) cur_i = a%idx
    end subroutine sca

    subroutine ax_sca(self)
        class(axes), intent(in) :: self

        call ensure_fig()
        if (self%idx >= 1 .and. self%idx <= n_ax) cur_i = self%idx
    end subroutine ax_sca

    subroutine ax_plot(self, x, y, fmt, label, lw, color, marker, linestyle, &
                       alpha, markersize, markerfacecolor, markeredgecolor, &
                       markeredgewidth, markevery, drawstyle, dashes)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        call ax_sca(self)
        call plot(x, y, fmt, label, lw, color, marker, linestyle, alpha, &
                  markersize, markerfacecolor, markeredgecolor, &
                  markeredgewidth, markevery, drawstyle, dashes)
    end subroutine ax_plot

    subroutine ax_plot_y(self, ydata, fmt, label, lw, color, marker, linestyle, &
                         alpha, markersize, markerfacecolor, markeredgecolor, &
                         markeredgewidth, markevery, drawstyle, dashes)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: ydata(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        call ax_sca(self)
        call plot(ydata, fmt, label, lw, color, marker, linestyle, alpha, &
                  markersize, markerfacecolor, markeredgecolor, &
                  markeredgewidth, markevery, drawstyle, dashes)
    end subroutine ax_plot_y

    subroutine ax_plot_cat(self, cats, y, fmt, label, lw, color, marker, &
                           linestyle, alpha)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call plot(cats, y, fmt, label, lw, color, marker, linestyle, alpha)
    end subroutine ax_plot_cat

    subroutine ax_bar_cat(self, cats, height, width, color, label, alpha, &
                          bottom, colors, edgecolor, linewidth)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor
        character(len=*), intent(in), optional :: colors(:)
        call ax_sca(self)
        call bar(cats, height, width, color, label, alpha, bottom, colors, &
                 edgecolor, linewidth)
    end subroutine ax_bar_cat

    subroutine ax_barh_cat(self, cats, width, height, color, label, alpha, &
                           left, colors, edgecolor, linewidth)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor
        character(len=*), intent(in), optional :: colors(:)
        call ax_sca(self)
        call barh(cats, width, height, color, label, alpha, left, colors, &
                  edgecolor, linewidth)
    end subroutine ax_barh_cat

    subroutine ax_scatter(self, x, y, s, c, marker, label, alpha, sizes, cvals, &
                          cmap, vmin, vmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: s, alpha, vmin, vmax
        real(dp), intent(in), optional :: sizes(:), cvals(:)
        character(len=*), intent(in), optional :: c, marker, label, cmap
        call ax_sca(self)
        call scatter(x, y, s, c, marker, label, alpha, sizes, cvals, cmap, vmin, vmax)
    end subroutine ax_scatter

    subroutine ax_bar(self, x, height, width, color, label, alpha, bottom, &
                      colors, edgecolor, linewidth, hatch, yerr, align, &
                      tick_label, ecolor, capsize)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        real(dp), intent(in), optional :: yerr(:), capsize
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:), align, ecolor
        character(len=*), intent(in), optional :: tick_label(:)
        call ax_sca(self)
        call bar(x, height, width, color, label, alpha, bottom, colors, &
                 edgecolor, linewidth, hatch, yerr, align, tick_label, &
                 ecolor, capsize)
    end subroutine ax_bar

    subroutine ax_barh(self, y, width, height, color, label, alpha, left, &
                       colors, edgecolor, linewidth)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:), width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor
        character(len=*), intent(in), optional :: colors(:)
        call ax_sca(self)
        call barh(y, width, height, color, label, alpha, left, colors, &
                  edgecolor, linewidth)
    end subroutine ax_barh

    subroutine ax_bar_label(self, fmt, padding, fontsize)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: fmt
        real(dp), intent(in), optional :: padding, fontsize
        call ax_sca(self)
        call bar_label(fmt, padding, fontsize)
    end subroutine ax_bar_label

    subroutine ax_hist(self, x, bins, color, label, alpha, bin_edges, &
                       density, cumulative, histtype, weights, stacked, &
                       orientation, log, rwidth)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:)
        integer, intent(in), optional :: bins
        character(len=*), intent(in), optional :: color, label, histtype
        character(len=*), intent(in), optional :: orientation
        real(dp), intent(in), optional :: alpha, bin_edges(:), weights(:), rwidth
        logical, intent(in), optional :: density, cumulative, stacked, log
        call ax_sca(self)
        call hist(x, bins, color, label, alpha, bin_edges, density, &
                  cumulative, histtype, weights, stacked, orientation, &
                  log, rwidth)
    end subroutine ax_hist

    subroutine ax_fill_between(self, x, y1, y2, color, label, alpha, where, &
                               hatch, edgecolor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:)
        call ax_sca(self)
        call fill_between(x, y1, y2, color, label, alpha, where, hatch, edgecolor)
    end subroutine ax_fill_between

    subroutine ax_fill_betweenx(self, y, x1, x2, color, label, alpha, where, &
                                hatch, edgecolor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:), x1(:)
        real(dp), intent(in), optional :: x2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:)
        call ax_sca(self)
        call fill_betweenx(y, x1, x2, color, label, alpha, where, hatch, edgecolor)
    end subroutine ax_fill_betweenx

    subroutine ax_stackplot(self, x, y, labels, colors, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:, :)
        character(len=*), intent(in), optional :: labels(:), colors(:)
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call stackplot(x, y, labels, colors, alpha)
    end subroutine ax_stackplot

    subroutine ax_errorbar(self, x, y, yerr, fmt, color, label, capsize, &
                           marker, xerr, yerr_lo, yerr_hi, xerr_lo, xerr_hi)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: yerr(:), xerr(:)
        real(dp), intent(in), optional :: yerr_lo(:), yerr_hi(:)
        real(dp), intent(in), optional :: xerr_lo(:), xerr_hi(:)
        character(len=*), intent(in), optional :: fmt, color, label, marker
        real(dp), intent(in), optional :: capsize
        call ax_sca(self)
        call errorbar(x, y, yerr, fmt, color, label, capsize, marker, xerr, &
                      yerr_lo, yerr_hi, xerr_lo, xerr_hi)
    end subroutine ax_errorbar

    subroutine ax_step(self, x, y, where, label, color, lw, linestyle, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call step(x, y, where, label, color, lw, linestyle, alpha)
    end subroutine ax_step

    subroutine ax_set_polar(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call set_polar()
    end subroutine ax_set_polar

    subroutine ax_add_rectangle(self, xy, width, height, angle, facecolor, &
                                edgecolor, lw, alpha, fill, hatch)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_rectangle(xy, width, height, angle, facecolor, edgecolor, lw, &
                           alpha, fill, hatch)
    end subroutine ax_add_rectangle

    subroutine ax_add_circle(self, xy, radius, facecolor, edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy(2), radius
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_circle(xy, radius, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_circle

    subroutine ax_add_ellipse(self, xy, width, height, angle, facecolor, &
                              edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_ellipse(xy, width, height, angle, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_ellipse

    subroutine ax_add_polygon(self, x, y, facecolor, edgecolor, lw, alpha, fill, hatch)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_polygon(x, y, facecolor, edgecolor, lw, alpha, fill, hatch)
    end subroutine ax_add_polygon

    subroutine ax_set_zorder(self, z)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z
        call ax_sca(self)
        call set_zorder(z)
    end subroutine ax_set_zorder

    subroutine ax_axes3d(self, elev, azim)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: elev, azim
        call ax_sca(self)
        call axes3d(elev, azim)
    end subroutine ax_axes3d

    subroutine ax_boxplot(self, y, position, width, color, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call boxplot(y, position, width, color, label)
    end subroutine ax_boxplot

    subroutine ax_violinplot(self, y, position, width, color, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call violinplot(y, position, width, color, label)
    end subroutine ax_violinplot

    subroutine ax_broken_barh(self, xranges, yrange, color, alpha, edgecolor, lw)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xranges(:, :), yrange(2)
        character(len=*), intent(in), optional :: color, edgecolor
        real(dp), intent(in), optional :: alpha, lw
        call ax_sca(self)
        call broken_barh(xranges, yrange, color, alpha, edgecolor, lw)
    end subroutine ax_broken_barh

    subroutine ax_eventplot(self, positions, lineoffset, linelength, color, lw)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: positions(:)
        real(dp), intent(in), optional :: lineoffset, linelength, lw
        character(len=*), intent(in), optional :: color
        call ax_sca(self)
        call eventplot(positions, lineoffset, linelength, color, lw)
    end subroutine ax_eventplot

    subroutine ax_hexbin(self, x, y, gridsize, cmap, mincnt)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: gridsize, mincnt
        character(len=*), intent(in), optional :: cmap
        call ax_sca(self)
        call hexbin(x, y, gridsize, cmap, mincnt)
    end subroutine ax_hexbin

    subroutine ax_hist2d(self, x, y, bins, cmap, vmin, vmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: bins(2)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax
        call ax_sca(self)
        call hist2d(x, y, bins, cmap, vmin, vmax)
    end subroutine ax_hist2d

    subroutine ax_loglog(self, x, y, fmt, label, lw, color)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call loglog(x, y, fmt, label, lw, color)
    end subroutine ax_loglog

    subroutine ax_semilogx(self, x, y, fmt, label, lw, color)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call semilogx(x, y, fmt, label, lw, color)
    end subroutine ax_semilogx

    subroutine ax_semilogy(self, x, y, fmt, label, lw, color)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call semilogy(x, y, fmt, label, lw, color)
    end subroutine ax_semilogy

    subroutine ax_matshow(self, z, cmap, vmin, vmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax
        call ax_sca(self)
        call matshow(z, cmap, vmin, vmax)
    end subroutine ax_matshow

    subroutine ax_minorticks_on(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call minorticks_on()
    end subroutine ax_minorticks_on

    subroutine ax_pie(self, values, labels, cmap)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:), cmap
        call ax_sca(self)
        call pie(values, labels, cmap)
    end subroutine ax_pie

    subroutine ax_polar(self, theta, r, color, label, lw, linestyle, marker, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: color, label, linestyle, marker
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call polar(theta, r, color, label, lw, linestyle, marker, alpha)
    end subroutine ax_polar

    subroutine ax_streamplot(self, x, y, u, v, density, color, lw, arrowsize)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(dp), intent(in), optional :: density, lw, arrowsize
        character(len=*), intent(in), optional :: color
        call ax_sca(self)
        call streamplot(x, y, u, v, density, color, lw, arrowsize)
    end subroutine ax_streamplot

    subroutine ax_table(self, cell_text, col_labels, row_labels, col_widths, loc, fontsize)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: cell_text(:, :)
        character(len=*), intent(in), optional :: col_labels(:), row_labels(:), loc
        real(dp), intent(in), optional :: col_widths(:), fontsize
        call ax_sca(self)
        call table(cell_text, col_labels, row_labels, col_widths, loc, fontsize)
    end subroutine ax_table

    subroutine ax_tick_format(self, axis, style, decimals, whole)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: style
        character(len=*), intent(in), optional :: axis
        integer, intent(in), optional :: decimals
        real(dp), intent(in), optional :: whole
        call ax_sca(self)
        call tick_format(axis, style, decimals, whole)
    end subroutine ax_tick_format

    subroutine ax_tick_locator(self, axis, base, nbins)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: axis
        real(dp), intent(in), optional :: base
        integer, intent(in), optional :: nbins
        call ax_sca(self)
        call tick_locator(axis, base, nbins)
    end subroutine ax_tick_locator

    subroutine ax_ticklabel_format(self, axis, style, useoffset, scilimits)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: axis, style
        logical, intent(in), optional :: useoffset
        integer, intent(in), optional :: scilimits(2)
        call ax_sca(self)
        call ticklabel_format(axis, style, useoffset, scilimits)
    end subroutine ax_ticklabel_format

    subroutine ax_add_arrow(self, x, y, dx, dy, width, facecolor, edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x, y, dx, dy
        real(dp), intent(in), optional :: width, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_arrow(x, y, dx, dy, width, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_arrow

    subroutine ax_add_path(self, x, y, codes, facecolor, edgecolor, lw, alpha, fill)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: codes
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call ax_sca(self)
        call add_path(x, y, codes, facecolor, edgecolor, lw, alpha, fill)
    end subroutine ax_add_path

    subroutine ax_plot3d(self, x, y, z, fmt, label, lw, color, marker, linestyle, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call plot3d(x, y, z, fmt, label, lw, color, marker, linestyle, alpha)
    end subroutine ax_plot3d

    subroutine ax_scatter3d(self, x, y, z, s, c, marker, label, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:)
        real(dp), intent(in), optional :: s, alpha
        character(len=*), intent(in), optional :: c, marker, label
        call ax_sca(self)
        call scatter3d(x, y, z, s, c, marker, label, alpha)
    end subroutine ax_scatter3d

    subroutine ax_plot_trisurf(self, x, y, z, color, alpha, cmap)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: color, cmap
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call plot_trisurf(x, y, z, color, alpha, cmap)
    end subroutine ax_plot_trisurf

    subroutine ax_bar3d(self, x, y, z, dx, dy, dz, color, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:), dx, dy, dz(:)
        character(len=*), intent(in), optional :: color
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call bar3d(x, y, z, dx, dy, dz, color, alpha)
    end subroutine ax_bar3d

    subroutine ax_quiver3d(self, x, y, z, u, v, w, length, normalize, color, lw)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:), u(:), v(:), w(:)
        real(dp), intent(in), optional :: length, lw
        logical, intent(in), optional :: normalize
        character(len=*), intent(in), optional :: color
        call ax_sca(self)
        call quiver3d(x, y, z, u, v, w, length, normalize, color, lw)
    end subroutine ax_quiver3d

    subroutine ax_contour3d(self, x, y, z, levels, cmap)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        real(dp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap
        call ax_sca(self)
        call contour3d(x, y, z, levels, cmap)
    end subroutine ax_contour3d

    subroutine ax_plot_wireframe(self, x, y, z, color, alpha, lw)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color
        real(dp), intent(in), optional :: alpha, lw
        call ax_sca(self)
        call plot_wireframe(x, y, z, color, alpha, lw)
    end subroutine ax_plot_wireframe

    subroutine ax_plot_surface(self, x, y, z, color, alpha, cmap)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color, cmap
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call plot_surface(x, y, z, color, alpha, cmap)
    end subroutine ax_plot_surface

    subroutine ax_view_init(self, elev, azim)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: elev, azim
        call ax_sca(self)
        call view_init(elev, azim)
    end subroutine ax_view_init

    subroutine ax_set_zlabel(self, s)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        call ax_sca(self)
        call zlabel(s)
    end subroutine ax_set_zlabel

    subroutine ax_set_zlim(self, lo, hi)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: lo, hi
        call ax_sca(self)
        call zlim(lo, hi)
    end subroutine ax_set_zlim
    subroutine ax_quiver(self, x, y, u, v, color, scale, width, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: scale, width

        call ax_sca(self)
        call quiver(x, y, u, v, color, scale, width, label)
    end subroutine ax_quiver

    subroutine ax_stem(self, x, y, color, label, alpha)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call stem(x, y, color, label, alpha)
    end subroutine ax_stem

    subroutine ax_imshow(self, z, cmap, vmin, vmax, extent, origin, aspect, &
                         norm, interpolation, boundaries, vcenter, gamma, &
                         linthresh)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap, origin, aspect, norm
        character(len=*), intent(in), optional :: interpolation
        real(dp), intent(in), optional :: vmin, vmax, extent(4), boundaries(:)
        real(dp), intent(in), optional :: vcenter, gamma, linthresh
        call ax_sca(self)
        call imshow(z, cmap, vmin, vmax, extent, origin, aspect, norm, &
                    interpolation, boundaries, vcenter, gamma, linthresh)
    end subroutine ax_imshow

    subroutine ax_imshow_rgb(self, z, extent, origin, aspect)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :, :)
        character(len=*), intent(in), optional :: origin, aspect
        real(dp), intent(in), optional :: extent(4)
        call ax_sca(self)
        call imshow(z, extent, origin, aspect)
    end subroutine ax_imshow_rgb

    subroutine ax_xaxis_date(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call xaxis_date()
    end subroutine ax_xaxis_date

    subroutine ax_yaxis_date(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call yaxis_date()
    end subroutine ax_yaxis_date

    subroutine ax_pcolormesh(self, x, y, c, cmap, vmin, vmax, norm, vcenter, &
                             gamma, linthresh)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, norm
        real(dp), intent(in), optional :: vmin, vmax, vcenter, gamma, linthresh
        call ax_sca(self)
        call pcolormesh(x, y, c, cmap, vmin, vmax, norm, vcenter, gamma, linthresh)
    end subroutine ax_pcolormesh

    subroutine ax_clabel(self, fontsize)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: fontsize
        call ax_sca(self)
        call clabel(fontsize)
    end subroutine ax_clabel

    subroutine ax_contour(self, z, levels, cmap, extent)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call ax_sca(self)
        call contour(z, levels, cmap, extent)
    end subroutine ax_contour

    subroutine ax_contourf(self, z, levels, cmap, extent)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call ax_sca(self)
        call contourf(z, levels, cmap, extent)
    end subroutine ax_contourf

    subroutine ax_colorbar(self, label, orientation, fraction, pad, shrink, aspect)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: label, orientation
        real(dp), intent(in), optional :: fraction, pad, shrink, aspect
        call ax_sca(self)
        call colorbar(label, orientation, fraction, pad, shrink, aspect)
    end subroutine ax_colorbar

    subroutine ax_margins(self, m, x, y)
        class(axes), intent(in) :: self
        real(dp), intent(in), optional :: m, x, y
        call ax_sca(self)
        call margins(m, x, y)
    end subroutine ax_margins

    subroutine ax_autoscale(self, enable, axis, tight)
        class(axes), intent(in) :: self
        logical, intent(in), optional :: enable, tight
        character(len=*), intent(in), optional :: axis
        call ax_sca(self)
        call autoscale(enable, axis, tight)
    end subroutine ax_autoscale

    subroutine ax_axhline(self, y, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call axhline(y, color, linestyle, lw, label)
    end subroutine ax_axhline

    subroutine ax_axhspan(self, ymin, ymax, xmin, xmax, color, alpha, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: ymin, ymax
        real(dp), intent(in), optional :: xmin, xmax, alpha
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call axhspan(ymin, ymax, xmin, xmax, color, alpha, label)
    end subroutine ax_axhspan

    subroutine ax_axvspan(self, xmin, xmax, ymin, ymax, color, alpha, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xmin, xmax
        real(dp), intent(in), optional :: ymin, ymax, alpha
        character(len=*), intent(in), optional :: color, label
        call ax_sca(self)
        call axvspan(xmin, xmax, ymin, ymax, color, alpha, label)
    end subroutine ax_axvspan

    subroutine ax_hlines(self, y, xmin, xmax, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: y(:), xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call hlines(y, xmin, xmax, color, linestyle, lw, label)
    end subroutine ax_hlines

    subroutine ax_vlines(self, x, ymin, ymax, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x(:), ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call vlines(x, ymin, ymax, color, linestyle, lw, label)
    end subroutine ax_vlines

    subroutine ax_axvline(self, x, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call axvline(x, color, linestyle, lw, label)
    end subroutine ax_axvline

    subroutine ax_axline(self, xy1, xy2, slope, color, linestyle, lw, label)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xy1(2)
        real(dp), intent(in), optional :: xy2(2), slope
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call ax_sca(self)
        call axline(xy1, xy2, slope, color, linestyle, lw, label)
    end subroutine ax_axline

    subroutine ax_text(self, x, y, s, color, fontsize, ha, fontweight, &
                       fontstyle, transform)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: transform
        real(dp), intent(in), optional :: fontsize
        call ax_sca(self)
        call text(x, y, s, color, fontsize, ha, fontweight, fontstyle, &
                  transform=transform)
    end subroutine ax_text

    subroutine ax_annotate(self, s, x, y, xtext, ytext, color, fontsize, ha, &
                           fontweight, fontstyle, transform)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: x, y
        real(dp), intent(in), optional :: xtext, ytext, fontsize
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: transform
        call ax_sca(self)
        call annotate(s, x, y, xtext, ytext, color, fontsize, ha, &
                      fontweight, fontstyle, transform=transform)
    end subroutine ax_annotate

    subroutine ax_set_title(self, s, fontsize, fontweight, fontstyle, loc)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        character(len=*), intent(in), optional :: fontweight, fontstyle, loc
        call ax_sca(self)
        call title(s, fontsize, fontweight, fontstyle, loc)
    end subroutine ax_set_title

    subroutine ax_set_xlabel(self, s, fontsize, fontweight, fontstyle, labelpad)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ax_sca(self)
        call xlabel(s, fontsize, fontweight, fontstyle, labelpad)
    end subroutine ax_set_xlabel

    subroutine ax_set_ylabel(self, s, fontsize, fontweight, fontstyle, labelpad)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ax_sca(self)
        call ylabel(s, fontsize, fontweight, fontstyle, labelpad)
    end subroutine ax_set_ylabel

    subroutine ax_set_xlim(self, xmin, xmax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: xmin, xmax
        call ax_sca(self)
        call xlim(xmin, xmax)
    end subroutine ax_set_xlim

    subroutine ax_set_ylim(self, ymin, ymax)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: ymin, ymax
        call ax_sca(self)
        call ylim(ymin, ymax)
    end subroutine ax_set_ylim

    subroutine ax_get_xlim(self, xmin, xmax)
        class(axes), intent(in) :: self
        real(dp), intent(out) :: xmin, xmax
        call ax_sca(self)
        call get_xlim(xmin, xmax)
    end subroutine ax_get_xlim

    subroutine ax_get_ylim(self, ymin, ymax)
        class(axes), intent(in) :: self
        real(dp), intent(out) :: ymin, ymax
        call ax_sca(self)
        call get_ylim(ymin, ymax)
    end subroutine ax_get_ylim

    subroutine ax_invert_xaxis(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call invert_xaxis()
    end subroutine ax_invert_xaxis

    subroutine ax_invert_yaxis(self)
        class(axes), intent(in) :: self
        call ax_sca(self)
        call invert_yaxis()
    end subroutine ax_invert_yaxis

    subroutine ax_set_xscale(self, name, linthresh, linscale)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ax_sca(self)
        call set_xscale(name, linthresh, linscale)
    end subroutine ax_set_xscale

    subroutine ax_set_yscale(self, name, linthresh, linscale)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ax_sca(self)
        call set_yscale(name, linthresh, linscale)
    end subroutine ax_set_yscale

    subroutine ax_set_xticks(self, vals, labels, minor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ax_sca(self)
        call xticks(vals, labels, minor)
    end subroutine ax_set_xticks

    subroutine ax_set_yticks(self, vals, labels, minor)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ax_sca(self)
        call yticks(vals, labels, minor)
    end subroutine ax_set_yticks

    subroutine ax_set_aspect(self, ratio, adjustable)
        class(axes), intent(in) :: self
        real(dp), intent(in) :: ratio
        character(len=*), intent(in), optional :: adjustable
        call ax_sca(self)
        call set_aspect(ratio, adjustable)
    end subroutine ax_set_aspect

    subroutine ax_set_facecolor(self, color, alpha)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: color
        real(dp), intent(in), optional :: alpha
        call ax_sca(self)
        call set_facecolor(color, alpha)
    end subroutine ax_set_facecolor

    subroutine ax_grid(self, on, axis, which, color, linestyle, lw, alpha)
        class(axes), intent(in) :: self
        logical, intent(in) :: on
        character(len=*), intent(in), optional :: axis, which, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ax_sca(self)
        call grid(on, axis, which, color, linestyle, lw, alpha)
    end subroutine ax_grid

    subroutine ax_legend(self, loc, fontsize, ncol, frameon, title, bbox_to_anchor)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: loc, title
        real(dp), intent(in), optional :: fontsize, bbox_to_anchor(2)
        integer, intent(in), optional :: ncol
        logical, intent(in), optional :: frameon
        call ax_sca(self)
        call legend(loc, fontsize, ncol, frameon, title, bbox_to_anchor)
    end subroutine ax_legend

    subroutine ax_tick_params(self, axis, direction, length, labelsize, rotation)
        class(axes), intent(in) :: self
        character(len=*), intent(in), optional :: axis, direction
        real(dp), intent(in), optional :: length, labelsize, rotation
        call ax_sca(self)
        call tick_params(axis, direction, length, labelsize, rotation)
    end subroutine ax_tick_params

    subroutine ax_spines(self, left, right, bottom, top)
        class(axes), intent(in) :: self
        logical, intent(in), optional :: left, right, bottom, top
        call ax_sca(self)
        call spines(left, right, bottom, top)
    end subroutine ax_spines

    subroutine ax_axis(self, mode)
        class(axes), intent(in) :: self
        character(len=*), intent(in) :: mode
        call ax_sca(self)
        call axis(mode)
    end subroutine ax_axis

    ! A twin is a new axes, so it comes back as its own handle.
    function ax_twinx(self) result(t)
        class(axes), intent(in) :: self
        type(axes) :: t
        call ax_sca(self)
        call twinx()
        t%idx = cur_i
    end function ax_twinx

    function ax_twiny(self) result(t)
        class(axes), intent(in) :: self
        type(axes) :: t
        call ax_sca(self)
        call twiny()
        t%idx = cur_i
    end function ax_twiny

    subroutine suptitle(s, fontsize, fontweight, fontstyle)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ensure_fig()
        fig_suptitle = s
        if (present(fontsize)) fig_suptitle_size = fontsize
        if (present(fontweight)) fig_suptitle_w = weight_from_str(fontweight)
        if (present(fontstyle)) fig_suptitle_sl = slant_from_str(fontstyle)
    end subroutine suptitle

    subroutine title(s, fontsize, fontweight, fontstyle, loc)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize
        character(len=*), intent(in), optional :: fontweight, fontstyle, loc
        call ensure_fig()
        ax(cur_i)%title = s
        if (present(loc)) ax(cur_i)%title_loc = loc
        if (present(fontsize)) ax(cur_i)%title_size = fontsize
        if (present(fontweight)) ax(cur_i)%title_w = weight_from_str(fontweight)
        if (present(fontstyle)) ax(cur_i)%title_sl = slant_from_str(fontstyle)
    end subroutine title

    subroutine xlabel(s, fontsize, fontweight, fontstyle, labelpad)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ensure_fig()
        ax(cur_i)%xlabel = s
        if (present(labelpad)) ax(cur_i)%xlabel_pad = labelpad
        if (present(fontsize)) ax(cur_i)%xlabel_size = fontsize
        if (present(fontweight)) ax(cur_i)%xlabel_w = weight_from_str(fontweight)
        if (present(fontstyle)) ax(cur_i)%xlabel_sl = slant_from_str(fontstyle)
    end subroutine xlabel

    subroutine ylabel(s, fontsize, fontweight, fontstyle, labelpad)
        character(len=*), intent(in) :: s
        real(dp), intent(in), optional :: fontsize, labelpad
        character(len=*), intent(in), optional :: fontweight, fontstyle
        call ensure_fig()
        ax(cur_i)%ylabel = s
        if (present(labelpad)) ax(cur_i)%ylabel_pad = labelpad
        if (present(fontsize)) ax(cur_i)%ylabel_size = fontsize
        if (present(fontweight)) ax(cur_i)%ylabel_w = weight_from_str(fontweight)
        if (present(fontstyle)) ax(cur_i)%ylabel_sl = slant_from_str(fontstyle)
    end subroutine ylabel

    ! matplotlib's spellings for the two faces it can pick without a font
    ! file of its own.
    pure integer function weight_from_str(s)
        character(len=*), intent(in) :: s
        weight_from_str = WEIGHT_NORMAL
        if (s == "bold" .or. s == "heavy" .or. s == "black") &
            weight_from_str = WEIGHT_BOLD
    end function weight_from_str

    pure integer function slant_from_str(s)
        character(len=*), intent(in) :: s
        slant_from_str = SLANT_ROMAN
        if (s == "italic" .or. s == "oblique") slant_from_str = SLANT_ITALIC
    end function slant_from_str

    ! One place to set text sizes. size= sets everything at once, which is the
    ! common request; the individual arguments override it. Applies to the
    ! axes that already exist as well as any made later, so it works whether
    ! it is called before or after plotting.
    subroutine set_fontsize(size, title, labels, ticks, legend)
        real(dp), intent(in), optional :: size, title, labels, ticks, legend
        integer :: i

        call ensure_fig()
        if (present(size)) then
            ! Keep matplotlib's proportions: the title is a little larger than
            ! the axis labels, which are larger than the tick labels.
            def_title = size * TITLE_FONT / LABEL_FONT
            def_label = size
            def_tick = size * TICK_FONT / LABEL_FONT
            def_legend = size * LEGEND_FONT / LABEL_FONT
            fig_suptitle_size = size * SUPTITLE_FONT / LABEL_FONT
        end if
        if (present(title)) then
            def_title = title
            fig_suptitle_size = title
        end if
        if (present(labels)) def_label = labels
        if (present(ticks)) def_tick = ticks
        if (present(legend)) def_legend = legend

        do i = 1, n_ax
            ax(i)%title_size = def_title
            ax(i)%xlabel_size = def_label
            ax(i)%ylabel_size = def_label
            ax(i)%xtick_size = def_tick
            ax(i)%ytick_size = def_tick
            ax(i)%legend_size = def_legend
        end do
    end subroutine set_fontsize

    ! The background of one axes, where rc(axes_facecolor=) sets them all.
    subroutine set_facecolor(color, alpha)
        character(len=*), intent(in) :: color
        real(dp), intent(in), optional :: alpha
        call ensure_fig()
        ax(cur_i)%facecolor = resolve_color(color)
        if (present(alpha)) ax(cur_i)%face_alpha = alpha
    end subroutine set_facecolor

    subroutine grid(on, axis, which, color, linestyle, lw, alpha)
        logical, intent(in) :: on
        character(len=*), intent(in), optional :: axis, which, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call ensure_fig()
        ax(cur_i)%grid_on = on
        if (present(axis)) ax(cur_i)%grid_axis = axis
        if (present(which)) ax(cur_i)%grid_which = which
        if (present(color)) ax(cur_i)%grid_color = color
        if (present(linestyle)) ax(cur_i)%grid_ls = linestyle_from_str(linestyle)
        if (present(lw)) ax(cur_i)%grid_lw = lw
        if (present(alpha)) ax(cur_i)%grid_alpha = alpha
    end subroutine grid

    ! bbox_to_anchor is in axes coordinates, so (1.02, 1.0) with the default
    ! loc="upper right" is matplotlib's usual recipe for parking the legend
    ! just outside the right-hand edge. When it is given, loc names which
    ! corner of the legend sits on the anchor rather than a position in the
    ! axes, exactly as matplotlib treats it.
    ! One legend for the whole figure: an invisible axes covering the
    ! canvas, carrying a copy of every labelled series in the figure. The
    ! ordinary legend machinery then places it against the figure rather
    ! than against any one panel, which is exactly what figlegend means.
    subroutine figlegend(loc, fontsize, ncol, frameon, title)
        character(len=*), intent(in), optional :: loc, title
        real(dp), intent(in), optional :: fontsize
        integer, intent(in), optional :: ncol
        logical, intent(in), optional :: frameon
        integer :: i, j, is, host, n_before
        type(axes) :: h

        call ensure_fig()
        n_before = n_ax
        h = add_axes([0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp])
        host = h%idx
        ax(host)%patch_off = .true.
        ax(host)%frame_off = .true.
        do i = 1, n_before
            do j = 1, ax(i)%n_series
                if (.not. in_legend(ax(i)%series(j))) cycle
                call push_series(ax(host), is)
                ax(host)%series(is) = ax(i)%series(j)
                ! The copy is there for its label alone; nothing of it is
                ! drawn, and an empty axes autoscales to the unit square.
                if (allocated(ax(host)%series(is)%x)) &
                    deallocate (ax(host)%series(is)%x)
                if (allocated(ax(host)%series(is)%y)) &
                    deallocate (ax(host)%series(is)%y)
                ax(host)%series(is)%n = 0
            end do
        end do
        call legend(loc, fontsize, ncol, frameon, title)
        cur_i = 1
    end subroutine figlegend

    subroutine legend(loc, fontsize, ncol, frameon, title, bbox_to_anchor, labels)
        character(len=*), intent(in), optional :: loc, title
        character(len=*), intent(in), optional :: labels(:)
        real(dp), intent(in), optional :: fontsize, bbox_to_anchor(2)
        integer, intent(in), optional :: ncol
        logical, intent(in), optional :: frameon
        integer :: i
        call ensure_fig()
        ! matplotlib's legend(labels): the names are handed to the artists
        ! in the order they were added, whatever they were called before.
        if (present(labels)) then
            do i = 1, min(ax(cur_i)%n_series, size(labels))
                ax(cur_i)%series(i)%label = labels(i)
            end do
        end if
        ax(cur_i)%legend_on = .true.
        if (present(loc)) ax(cur_i)%legend_loc = loc
        if (present(fontsize)) ax(cur_i)%legend_size = fontsize
        if (present(ncol)) ax(cur_i)%legend_ncol = max(1, ncol)
        if (present(frameon)) ax(cur_i)%legend_frame = frameon
        if (present(title)) ax(cur_i)%legend_title = title
        if (present(bbox_to_anchor)) then
            ax(cur_i)%legend_bbox = bbox_to_anchor
            ax(cur_i)%legend_has_bbox = .true.
        end if
    end subroutine legend

    ! ------------------------------------------------------------------
    ! Formatters and locators. matplotlib reaches these through the axis
    ! object (set_major_formatter, set_major_locator) and a pyplot
    ! shortcut; there is one shortcut per job here instead, because a
    ! formatter object of our own would be a class with one method and
    ! nothing to say.
    ! ------------------------------------------------------------------

    ! matplotlib's ticklabel_format: whether an axis may factor an offset
    ! or a power of ten out of its labels, and from where.
    subroutine ticklabel_format(axis, style, useoffset, scilimits)
        character(len=*), intent(in), optional :: axis, style
        logical, intent(in), optional :: useoffset
        integer, intent(in), optional :: scilimits(2)
        logical :: dox, doy
        integer :: lo, hi

        call ensure_fig()
        call which_axis(axis, dox, doy)
        if (present(useoffset)) then
            if (dox) ax(cur_i)%x_use_offset = useoffset
            if (doy) ax(cur_i)%y_use_offset = useoffset
        end if
        lo = -5
        hi = 6
        if (present(style)) then
            select case (lower(style))
            case ("plain")
                ! Never factor a power of ten out: no limit is ever met.
                lo = -huge(1)/2
                hi = huge(1)/2
            case ("sci", "scientific")
                ! Always factor one out, which is what (0, 0) means.
                lo = 0
                hi = 0
            end select
        end if
        if (present(scilimits)) then
            lo = scilimits(1)
            hi = scilimits(2)
        end if
        if (present(style) .or. present(scilimits)) then
            if (dox) then
                ax(cur_i)%x_scilo = lo
                ax(cur_i)%x_scihi = hi
            end if
            if (doy) then
                ax(cur_i)%y_scilo = lo
                ax(cur_i)%y_scihi = hi
            end if
        end if
    end subroutine ticklabel_format

    ! One of matplotlib's named formatters: "percent" is PercentFormatter,
    ! "comma" is a StrMethodFormatter with a thousands separator, "fixed"
    ! is a FormatStrFormatter with a set number of decimals, and "auto" is
    ! the ScalarFormatter every axis starts with.
    subroutine tick_format(axis, style, decimals, whole)
        character(len=*), intent(in) :: style
        character(len=*), intent(in), optional :: axis
        integer, intent(in), optional :: decimals
        real(dp), intent(in), optional :: whole
        logical :: dox, doy
        integer :: st, dec

        call ensure_fig()
        call which_axis(axis, dox, doy)
        select case (lower(style))
        case ("percent"); st = FMT_PERCENT
        case ("comma", "thousands"); st = FMT_COMMA
        case ("fixed"); st = FMT_FIXED
        case default; st = FMT_AUTO
        end select
        dec = -1
        if (present(decimals)) dec = decimals
        ! A percentage with no decimals asked for is written whole, which
        ! is what PercentFormatter defaults to.
        if (st == FMT_PERCENT .and. dec < 0) dec = 0
        if (dox) then
            ax(cur_i)%xfmt_style = st
            ax(cur_i)%xfmt_dec = dec
            if (present(whole)) ax(cur_i)%xfmt_whole = whole
        end if
        if (doy) then
            ax(cur_i)%yfmt_style = st
            ax(cur_i)%yfmt_dec = dec
            if (present(whole)) ax(cur_i)%yfmt_whole = whole
        end if
    end subroutine tick_format

    ! base is MultipleLocator, nbins is MaxNLocator. Neither moves the
    ! limits; they only decide where the ticks land inside them.
    subroutine tick_locator(axis, base, nbins)
        character(len=*), intent(in), optional :: axis
        real(dp), intent(in), optional :: base
        integer, intent(in), optional :: nbins
        logical :: dox, doy

        call ensure_fig()
        call which_axis(axis, dox, doy)
        if (present(base)) then
            if (dox) ax(cur_i)%xtick_base = base
            if (doy) ax(cur_i)%ytick_base = base
        end if
        if (present(nbins)) then
            if (dox) ax(cur_i)%xtick_nbins = nbins
            if (doy) ax(cur_i)%ytick_nbins = nbins
        end if
    end subroutine tick_locator

    ! "x", "y" or "both", the way every matplotlib call that takes an axis
    ! name spells it. Absent means both.
    subroutine which_axis(axis, dox, doy)
        character(len=*), intent(in), optional :: axis
        logical, intent(out) :: dox, doy
        dox = .true.
        doy = .true.
        if (.not. present(axis)) return
        select case (lower(axis))
        case ("x"); doy = .false.
        case ("y"); dox = .false.
        end select
    end subroutine which_axis

    subroutine xticks(vals, labels, minor)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ensure_fig()
        if (is_minor(minor)) then
            call set_minor_ticks(vals, ax(cur_i)%n_xminor, ax(cur_i)%xminor_pos)
            return
        end if
        call set_ticks(vals, labels, ax(cur_i)%n_xticks, ax(cur_i)%xtick_pos, &
                       ax(cur_i)%xtick_labeled, ax(cur_i)%xtick_lab)
    end subroutine xticks

    subroutine yticks(vals, labels, minor)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        logical, intent(in), optional :: minor
        call ensure_fig()
        if (is_minor(minor)) then
            call set_minor_ticks(vals, ax(cur_i)%n_yminor, ax(cur_i)%yminor_pos)
            return
        end if
        call set_ticks(vals, labels, ax(cur_i)%n_yticks, ax(cur_i)%ytick_pos, &
                       ax(cur_i)%ytick_labeled, ax(cur_i)%ytick_lab)
    end subroutine yticks

    pure function is_minor(minor) result(v)
        logical, intent(in), optional :: minor
        logical :: v
        v = .false.
        if (present(minor)) v = minor
    end function is_minor

    subroutine set_minor_ticks(vals, n, pos)
        real(dp), intent(in) :: vals(:)
        integer, intent(out) :: n
        real(dp), intent(out) :: pos(MAX_TICKS)
        integer :: i
        n = min(size(vals), MAX_TICKS)
        do i = 1, n
            pos(i) = vals(i)
        end do
    end subroutine set_minor_ticks

    ! matplotlib's locator_params: how many intervals the locator may use,
    ! and whether to drop the tick at one end so that it does not collide
    ! with a neighbouring subplot.
    subroutine locator_params(axis, nbins, prune)
        character(len=*), intent(in), optional :: axis, prune
        integer, intent(in), optional :: nbins
        logical :: dox, doy
        call ensure_fig()
        call which_axis(axis, dox, doy)
        if (present(nbins)) then
            if (dox) ax(cur_i)%xtick_nbins = nbins
            if (doy) ax(cur_i)%ytick_nbins = nbins
        end if
        if (present(prune)) then
            if (dox) ax(cur_i)%xtick_prune = prune
            if (doy) ax(cur_i)%ytick_prune = prune
        end if
    end subroutine locator_params

    subroutine set_ticks(vals, labels, n, pos, labeled, lab)
        real(dp), intent(in) :: vals(:)
        character(len=*), intent(in), optional :: labels(:)
        integer, intent(out) :: n
        real(dp), intent(out) :: pos(MAX_TICKS)
        logical, intent(out) :: labeled
        character(len=24), intent(out) :: lab(MAX_TICKS)
        integer :: i
        ! An empty list means no ticks at all, which is not the same as
        ! never having asked for any.
        if (size(vals) == 0) then
            n = -1
            labeled = .false.
            return
        end if
        n = min(size(vals), MAX_TICKS)
        labeled = present(labels)
        do i = 1, n
            pos(i) = vals(i)
            if (labeled) lab(i) = labels(i)
        end do
    end subroutine set_ticks

    subroutine minorticks_on()
        call ensure_fig()
        ax(cur_i)%minor_ticks = .true.
    end subroutine minorticks_on

    subroutine xlim(xmin, xmax)
        real(dp), intent(in) :: xmin, xmax
        call ensure_fig()
        ax(cur_i)%xmin_user = xmin
        ax(cur_i)%xmax_user = xmax
        ax(cur_i)%xlim_set = .true.
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        real(dp), intent(in) :: ymin, ymax
        call ensure_fig()
        ax(cur_i)%ymin_user = ymin
        ax(cur_i)%ymax_user = ymax
        ax(cur_i)%ylim_set = .true.
    end subroutine ylim

    ! The limits actually in force, autoscaling included, so that a caller
    ! can place something relative to what the axes ended up showing.
    subroutine get_xlim(xmin, xmax)
        real(dp), intent(out) :: xmin, xmax
        real(dp) :: ylo, yhi
        call ensure_fig()
        call compute_limits(ax(cur_i), xmin, xmax, ylo, yhi)
    end subroutine get_xlim

    subroutine get_ylim(ymin, ymax)
        real(dp), intent(out) :: ymin, ymax
        real(dp) :: xlo, xhi
        call ensure_fig()
        call compute_limits(ax(cur_i), xlo, xhi, ymin, ymax)
    end subroutine get_ylim

    ! Turn an axis round, so that it counts down instead of up.
    subroutine invert_xaxis()
        call ensure_fig()
        ax(cur_i)%x_inv = .not. ax(cur_i)%x_inv
    end subroutine invert_xaxis

    subroutine invert_yaxis()
        call ensure_fig()
        ax(cur_i)%y_inv = .not. ax(cur_i)%y_inv
    end subroutine invert_yaxis

    ! matplotlib's artist.set_zorder, applied to the artist just drawn.
    ! Fortran has no artist objects to hang a keyword off, so rather than
    ! add a zorder= to every plotting call this names the one that would
    ! have taken it: the series most recently added to these axes.
    subroutine set_zorder(z)
        real(dp), intent(in) :: z

        call ensure_fig()
        if (ax(cur_i)%n_series < 1) &
            error stop "fplot: set_zorder needs something drawn first"
        if (z < 0.0_dp) error stop "fplot: zorder must not be negative"
        ax(cur_i)%series(ax(cur_i)%n_series)%zorder = z
    end subroutine set_zorder

    subroutine plot_num(x, y, fmt, label, lw, color, marker, linestyle, alpha, &
                        markersize, markerfacecolor, markeredgecolor, &
                        markeredgewidth, markevery, drawstyle, dashes)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        integer :: is
        real(dp), allocatable :: sx(:), sy(:)

        call ensure_fig()
        if (present(drawstyle)) then
            ! A drawstyle is a step under another name, so it is drawn by
            ! the same code rather than a second copy of it.
            call stair_points(x, y, step_where(drawstyle), sx, sy)
            call add_series(cur_i, sx, sy, fmt, label, lw, color, marker, &
                            linestyle, alpha)
        else
            call add_series(cur_i, x, y, fmt, label, lw, color, marker, &
                            linestyle, alpha)
        end if
        is = ax(cur_i)%n_series
        if (is < 1) return
        if (present(markersize)) ax(cur_i)%series(is)%markersize = markersize
        if (present(markerfacecolor)) &
            ax(cur_i)%series(is)%mfc = resolve_color(markerfacecolor)
        if (present(markeredgecolor)) &
            ax(cur_i)%series(is)%mec = resolve_color(markeredgecolor)
        if (present(markeredgewidth)) ax(cur_i)%series(is)%mew = markeredgewidth
        if (present(markevery)) ax(cur_i)%series(is)%markevery = max(1, markevery)
        if (present(dashes)) then
            ax(cur_i)%series(is)%n_dash = min(size(dashes), 4)
            ax(cur_i)%series(is)%dashes(1:ax(cur_i)%series(is)%n_dash) = &
                dashes(1:ax(cur_i)%series(is)%n_dash)
        end if
    end subroutine plot_num

    ! plot(y): matplotlib numbers the points 0, 1, 2 ... when it is given
    ! only one array, and so does this.
    subroutine plot_y(ydata, fmt, label, lw, color, marker, linestyle, alpha, &
                      markersize, markerfacecolor, markeredgecolor, &
                      markeredgewidth, markevery, drawstyle, dashes)
        real(dp), intent(in) :: ydata(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        character(len=*), intent(in), optional :: markerfacecolor, markeredgecolor
        character(len=*), intent(in), optional :: drawstyle
        real(dp), intent(in), optional :: lw, alpha, markersize, markeredgewidth
        real(dp), intent(in), optional :: dashes(:)
        integer, intent(in), optional :: markevery
        integer :: i
        real(dp) :: idx(size(ydata))

        do i = 1, size(ydata)
            idx(i) = real(i - 1, dp)
        end do
        call plot_num(idx, ydata, fmt, label, lw, color, marker, linestyle, alpha, &
                      markersize, markerfacecolor, markeredgecolor, &
                      markeredgewidth, markevery, drawstyle, dashes)
    end subroutine plot_y

    subroutine plot_cat(cats, y, fmt, label, lw, color, marker, linestyle, alpha)
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        call plot_num(category_positions(size(cats)), y, fmt, label, lw, color, &
                      marker, linestyle, alpha)
        call xticks(category_positions(size(cats)), cats)
    end subroutine plot_cat

    ! Marker-only plot. s is the marker area in points^2 (matplotlib's
    ! convention), so the marker size is its square root.
    ! s and c are the scalar forms; sizes and cvals are their per-point
    ! equivalents. Fortran cannot overload one dummy as scalar-or-array, so
    ! they are separate keywords rather than matplotlib's single s= and c=.
    subroutine scatter(x, y, s, c, marker, label, alpha, sizes, cvals, cmap, vmin, vmax, &
                       edgecolors, linewidths)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: s, alpha, vmin, vmax, linewidths
        real(dp), intent(in), optional :: sizes(:), cvals(:)
        character(len=*), intent(in), optional :: c, marker, label, cmap, edgecolors
        integer :: is, n, k, id
        real(dp) :: lo, hi

        call ensure_fig()
        if (present(marker)) then
            call add_series(cur_i, x, y, label=label, color=c, marker=marker, &
                            linestyle="None", alpha=alpha)
        else
            call add_series(cur_i, x, y, label=label, color=c, marker="o", &
                            linestyle="None", alpha=alpha)
        end if
        is = ax(cur_i)%n_series
        if (is < 1) return
        n = ax(cur_i)%series(is)%n

        ! matplotlib draws scatter markers with no edge at all by default,
        ! so an edge only appears once one is asked for.
        if (present(edgecolors)) &
            ax(cur_i)%series(is)%mec = resolve_color(edgecolors)
        if (present(linewidths)) ax(cur_i)%series(is)%mew = max(linewidths, 0.0_dp)

        ! matplotlib's s is an area in points squared.
        if (present(s)) ax(cur_i)%series(is)%markersize = sqrt(max(s, 0.0_dp))
        if (present(sizes)) then
            allocate (ax(cur_i)%series(is)%psize(n))
            do k = 1, n
                ax(cur_i)%series(is)%psize(k) = &
                    sqrt(max(sizes(min(k, size(sizes))), 0.0_dp))
            end do
        end if

        if (present(cvals)) then
            id = CMAP_VIRIDIS
            if (present(cmap)) id = cmap_from_str(cmap)
            lo = minval(cvals)
            hi = maxval(cvals)
            if (present(vmin)) lo = vmin
            if (present(vmax)) hi = vmax
            if (hi <= lo) hi = lo + 1.0_dp
            allocate (ax(cur_i)%series(is)%pcolor(n))
            do k = 1, n
                ax(cur_i)%series(is)%pcolor(k) = &
                    cmap_color(id, (cvals(min(k, size(cvals))) - lo) / (hi - lo))
            end do
            ax(cur_i)%has_cmap_src = .true.
            ax(cur_i)%img_cmap = id
            ax(cur_i)%img_vmin = lo
            ax(cur_i)%img_vmax = hi
        end if
    end subroutine scatter

    ! Choose the axis transform explicitly: "linear", "log" or "symlog".
    ! Until now the scale was only ever implied by which plotting call was
    ! used, which leaves no way to put a log axis under a bar chart.
    subroutine set_xscale(name, linthresh, linscale)
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ensure_fig()
        ax(cur_i)%xsc = make_scale(name, linthresh, linscale)
    end subroutine set_xscale

    subroutine set_yscale(name, linthresh, linscale)
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        call ensure_fig()
        ax(cur_i)%ysc = make_scale(name, linthresh, linscale)
    end subroutine set_yscale

    function make_scale(name, linthresh, linscale) result(s)
        character(len=*), intent(in) :: name
        real(dp), intent(in), optional :: linthresh, linscale
        type(scale_t) :: s

        select case (lower(name))
        case ("linear")
            s%kind = SCALE_LINEAR
        case ("log")
            s%kind = SCALE_LOG
        case ("symlog")
            s%kind = SCALE_SYMLOG
        case default
            error stop "fplot: unknown scale, expected linear, log or symlog"
        end select
        if (present(linthresh)) then
            if (linthresh <= 0.0_dp) error stop "fplot: linthresh must be positive"
            s%linthresh = linthresh
        end if
        if (present(linscale)) then
            if (linscale <= 0.0_dp) error stop "fplot: linscale must be positive"
            s%linscale = linscale
        end if
    end function make_scale

    subroutine semilogx(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%xsc%kind = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine semilogx

    subroutine semilogy(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%ysc%kind = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine semilogy

    subroutine loglog(x, y, fmt, label, lw, color)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color
        real(dp), intent(in), optional :: lw
        call ensure_fig()
        ax(cur_i)%xsc%kind = SCALE_LOG
        ax(cur_i)%ysc%kind = SCALE_LOG
        call add_series(cur_i, x, y, fmt, label, lw, color)
    end subroutine loglog

    ! Claim the next series slot, doubling the array when it is full.
    subroutine push_series(a, is)
        type(axes_t), intent(inout) :: a
        integer, intent(out) :: is
        type(series_t), allocatable :: tmp(:)
        integer :: cap, i

        if (.not. allocated(a%series)) allocate (a%series(INIT_SLOTS))
        cap = size(a%series)
        if (a%n_series >= cap) then
            allocate (tmp(2 * cap))
            do i = 1, cap
                tmp(i) = a%series(i)
            end do
            call move_alloc(tmp, a%series)
        end if
        a%n_series = a%n_series + 1
        is = a%n_series
    end subroutine push_series

    subroutine push_text(a, it)
        type(axes_t), intent(inout) :: a
        integer, intent(out) :: it
        type(text_t), allocatable :: tmp(:)
        integer :: cap, i

        if (.not. allocated(a%texts)) allocate (a%texts(INIT_SLOTS))
        cap = size(a%texts)
        if (a%n_texts >= cap) then
            allocate (tmp(2 * cap))
            do i = 1, cap
                tmp(i) = a%texts(i)
            end do
            call move_alloc(tmp, a%texts)
        end if
        a%n_texts = a%n_texts + 1
        it = a%n_texts
    end subroutine push_text

    ! Start a new series of the given kind and return its index, applying the
    ! shared bookkeeping (point count, color cycling, label).
    function new_shape_series(kd, x, y, color, label, alpha) result(is)
        integer, intent(in) :: kd
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        integer :: is, n
        real(dp) :: ca

        is = 0
        n = min(size(x), size(y))
        if (n <= 0) return
        call push_series(ax(cur_i), is)
        allocate (ax(cur_i)%series(is)%x(n), ax(cur_i)%series(is)%y(n))
        ax(cur_i)%series(is)%x(1:n) = x(1:n)
        ax(cur_i)%series(is)%y(1:n) = y(1:n)
        ax(cur_i)%series(is)%n = n
        ax(cur_i)%series(is)%kind = kd
        ax(cur_i)%series(is)%marker = MARKER_NONE
        ax(cur_i)%series(is)%linestyle = LINE_SOLID
        ax(cur_i)%series(is)%linewidth = rc_lw
        ax(cur_i)%series(is)%markersize = default_markersize
        ax(cur_i)%series(is)%label = ""
        if (present(label)) ax(cur_i)%series(is)%label = label
        if (present(alpha)) ax(cur_i)%series(is)%alpha = alpha

        ax(cur_i)%series(is)%color = resolve_color(color, ca)
        if (ca >= 0.0_dp .and. .not. present(alpha)) ax(cur_i)%series(is)%alpha = ca
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
    end function new_shape_series

    ! Draw z as an image. z is indexed (row, column) and, with the default
    ! origin="upper", row 1 is drawn at the top, which is why that case gives
    ! a descending y axis exactly as matplotlib does.
    subroutine imshow_z(z, cmap, vmin, vmax, extent, origin, aspect, norm, &
                        interpolation, boundaries, vcenter, gamma, linthresh)
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap, origin, aspect, norm, interpolation
        real(dp), intent(in), optional :: vmin, vmax, extent(4), boundaries(:)
        real(dp), intent(in), optional :: vcenter, gamma, linthresh
        integer :: nr, nc
        real(dp) :: lo, hi

        call ensure_fig()
        nr = size(z, 1)
        nc = size(z, 2)
        if (nr < 1 .or. nc < 1) return

        if (allocated(ax(cur_i)%img)) deallocate (ax(cur_i)%img)
        allocate (ax(cur_i)%img(nr, nc))
        ax(cur_i)%img = z
        ax(cur_i)%has_img = .true.
        ax(cur_i)%has_cmap_src = .true.

        ax(cur_i)%img_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%img_cmap = cmap_from_str(cmap)

        ax(cur_i)%img_bilinear = .false.
        if (present(interpolation)) &
            ax(cur_i)%img_bilinear = trim(interpolation) == "bilinear"

        ax(cur_i)%img_norm = NORM_LINEAR
        if (present(norm)) ax(cur_i)%img_norm = norm_from_str(norm)
        call norm_params(vcenter, gamma, linthresh)

        if (allocated(ax(cur_i)%img_bounds)) deallocate (ax(cur_i)%img_bounds)
        if (present(boundaries)) then
            if (size(boundaries) >= 2) then
                allocate (ax(cur_i)%img_bounds(size(boundaries)))
                ax(cur_i)%img_bounds = boundaries
            end if
        end if

        if (ax(cur_i)%img_norm == NORM_LOG) then
            ! A log scale cannot start at zero, so the smallest positive
            ! sample sets the bottom of the range.
            lo = huge(1.0_dp)
            if (any(z > 0.0_dp)) lo = minval(z, mask=(z > 0.0_dp))
            hi = maxval(z)
        else if (any(z == z)) then
            ! Values that are not there at all say nothing about the range.
            lo = minval(z, mask=(z == z))
            hi = maxval(z, mask=(z == z))
        else
            lo = 0.0_dp
            hi = 1.0_dp
        end if
        if (present(vmin)) lo = vmin
        if (present(vmax)) hi = vmax
        ! The bands say what the range is; anything outside them is clamped.
        if (allocated(ax(cur_i)%img_bounds)) then
            lo = ax(cur_i)%img_bounds(1)
            hi = ax(cur_i)%img_bounds(size(ax(cur_i)%img_bounds))
        end if
        if (hi <= lo) hi = lo + 1.0_dp
        ax(cur_i)%img_vmin = lo
        ax(cur_i)%img_vmax = hi

        ax(cur_i)%has_mesh = .false.
        ax(cur_i)%img_origin_upper = .true.
        if (present(origin)) ax(cur_i)%img_origin_upper = trim(origin) /= "lower"

        ! Pixel centres sit on integers, so the edges fall on the half values.
        if (present(extent)) then
            ax(cur_i)%img_ext = extent
        else
            ax(cur_i)%img_ext = [-0.5_dp, real(nc, dp) - 0.5_dp, &
                                 -0.5_dp, real(nr, dp) - 0.5_dp]
        end if

        ax(cur_i)%aspect = 1.0_dp
        if (present(aspect)) then
            if (trim(aspect) == "auto") ax(cur_i)%aspect = 0.0_dp
        end if
    end subroutine imshow_z

    ! An image whose colours are given directly: z is (row, column, channel)
    ! with three channels for RGB or four for RGBA, each in 0..1 as
    ! matplotlib reads a float array. There is nothing here for a colorbar
    ! to describe, so none is offered.
    subroutine imshow_rgb(z, extent, origin, aspect)
        real(dp), intent(in) :: z(:, :, :)
        character(len=*), intent(in), optional :: origin, aspect
        real(dp), intent(in), optional :: extent(4)
        integer :: nr, nc, nch

        call ensure_fig()
        nr = size(z, 1)
        nc = size(z, 2)
        nch = size(z, 3)
        if (nr < 1 .or. nc < 1 .or. nch < 3) return

        if (allocated(ax(cur_i)%img)) deallocate (ax(cur_i)%img)
        allocate (ax(cur_i)%img(nr, nc))
        ax(cur_i)%img = 0.0_dp
        if (allocated(ax(cur_i)%img_rgb)) deallocate (ax(cur_i)%img_rgb)
        allocate (ax(cur_i)%img_rgb(nr, nc, nch))
        ax(cur_i)%img_rgb = min(1.0_dp, max(0.0_dp, z))
        ax(cur_i)%has_img = .true.
        ax(cur_i)%has_rgb = .true.
        ax(cur_i)%has_cmap_src = .false.
        ax(cur_i)%has_mesh = .false.
        ax(cur_i)%img_bilinear = .false.
        ax(cur_i)%img_norm = NORM_LINEAR

        ax(cur_i)%img_origin_upper = .true.
        if (present(origin)) ax(cur_i)%img_origin_upper = trim(origin) /= "lower"
        if (present(extent)) then
            ax(cur_i)%img_ext = extent
        else
            ax(cur_i)%img_ext = [-0.5_dp, real(nc, dp) - 0.5_dp, &
                                 -0.5_dp, real(nr, dp) - 0.5_dp]
        end if
        ax(cur_i)%aspect = 1.0_dp
        if (present(aspect)) then
            if (trim(aspect) == "auto") ax(cur_i)%aspect = 0.0_dp
        end if
    end subroutine imshow_rgb

    ! matshow: an image of a matrix. The first row is at the top, the
    ! cells are square and the column numbers run along the top, which is
    ! how a matrix is written down.
    subroutine matshow(z, cmap, vmin, vmax)
        real(dp), intent(in) :: z(:, :)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax

        call imshow(z, cmap, vmin, vmax, aspect="equal")
        ax(cur_i)%x_top = .true.
    end subroutine matshow

    ! A row of events, each drawn as a stroke across the line it sits on.
    subroutine eventplot(positions, lineoffset, linelength, color, lw)
        real(dp), intent(in) :: positions(:)
        real(dp), intent(in), optional :: lineoffset, linelength, lw
        character(len=*), intent(in), optional :: color
        real(dp) :: off, len_

        off = 1.0_dp
        len_ = 1.0_dp
        if (present(lineoffset)) off = lineoffset
        if (present(linelength)) len_ = linelength
        call vlines(positions, off - 0.5_dp*len_, off + 0.5_dp*len_, &
                    color=color, lw=lw)
        ! matplotlib keeps a whole line length of room either side of the
        ! row, not the half it actually draws.
        if (ax(cur_i)%yroom_set) then
            ax(cur_i)%yroom = [min(ax(cur_i)%yroom(1), off - len_), &
                               max(ax(cur_i)%yroom(2), off + len_)]
        else
            ax(cur_i)%yroom = [off - len_, off + len_]
            ax(cur_i)%yroom_set = .true.
        end if
    end subroutine eventplot

    ! Bars along one row: each range is a start and a width, and the row
    ! is a bottom and a height.
    subroutine broken_barh(xranges, yrange, color, alpha, edgecolor, lw)
        real(dp), intent(in) :: xranges(:, :), yrange(2)
        character(len=*), intent(in), optional :: color, edgecolor
        real(dp), intent(in), optional :: alpha, lw
        integer :: i, is

        call ensure_fig()
        do i = 1, size(xranges, 1)
            call add_rectangle([xranges(i, 1), yrange(1)], xranges(i, 2), yrange(2), &
                               facecolor=color, edgecolor=edgecolor, lw=lw, alpha=alpha)
            is = ax(cur_i)%n_series
            if (is > 0) ax(cur_i)%series(is)%patch_scales = .true.
        end do
    end subroutine broken_barh

    ! A table of text, laid out the way matplotlib lays one out: every cell
    ! the same height, a fixed fraction of the axes wide unless told
    ! otherwise, and the whole block placed against an edge of the axes.
    subroutine table(cell_text, col_labels, row_labels, col_widths, loc, fontsize)
        character(len=*), intent(in) :: cell_text(:, :)
        character(len=*), intent(in), optional :: col_labels(:), row_labels(:), loc
        real(dp), intent(in), optional :: col_widths(:), fontsize
        integer :: nr, nc, i, j

        call ensure_fig()
        nr = size(cell_text, 1)
        nc = size(cell_text, 2)
        if (nr < 1 .or. nc < 1) return

        if (allocated(ax(cur_i)%tbl_cells)) deallocate (ax(cur_i)%tbl_cells)
        allocate (ax(cur_i)%tbl_cells(nr, nc))
        do i = 1, nr
            do j = 1, nc
                ax(cur_i)%tbl_cells(i, j) = cell_text(i, j)
            end do
        end do

        if (allocated(ax(cur_i)%tbl_col)) deallocate (ax(cur_i)%tbl_col)
        if (present(col_labels)) then
            allocate (ax(cur_i)%tbl_col(nc))
            do j = 1, min(nc, size(col_labels))
                ax(cur_i)%tbl_col(j) = col_labels(j)
            end do
        end if

        if (allocated(ax(cur_i)%tbl_row)) deallocate (ax(cur_i)%tbl_row)
        if (present(row_labels)) then
            allocate (ax(cur_i)%tbl_row(nr))
            do i = 1, min(nr, size(row_labels))
                ax(cur_i)%tbl_row(i) = row_labels(i)
            end do
        end if

        if (allocated(ax(cur_i)%tbl_w)) deallocate (ax(cur_i)%tbl_w)
        allocate (ax(cur_i)%tbl_w(nc))
        ax(cur_i)%tbl_w = 1.0_dp/real(nc, dp)
        if (present(col_widths)) then
            do j = 1, min(nc, size(col_widths))
                ax(cur_i)%tbl_w(j) = col_widths(j)
            end do
        end if

        ax(cur_i)%tbl_size = 10.0_dp
        if (present(fontsize)) ax(cur_i)%tbl_size = fontsize
        ax(cur_i)%tbl_loc = "bottom"
        if (present(loc)) ax(cur_i)%tbl_loc = loc
        ax(cur_i)%has_table = .true.
    end subroutine table

    ! Streamlines of a vector field. This follows matplotlib closely: the
    ! field is integrated with an adaptive Heun step in grid coordinates,
    ! seeds spiral inwards from the corner of a 30x30 mask, and a line stops
    ! as soon as it enters a cell another line already went through.
    !
    ! u and v are indexed (row, column), that is (y, x), as everywhere else.
    subroutine streamplot(x, y, u, v, density, color, lw, arrowsize)
        real(dp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(dp), intent(in), optional :: density, lw, arrowsize
        character(len=*), intent(in), optional :: color
        type(stream_t) :: st
        real(dp), parameter :: MINLENGTH = 0.1_dp
        real(dp), allocatable :: xs(:), ys(:), tx(:), ty(:), ahx(:), ahy(:), ahu(:), ahv(:)
        real(dp) :: dx, dy, x0, y0, dens, tot, sl, half, ca
        integer :: nx, ny, i, j, np, is, na, k, nseed, sx, sy, sd(5)
        character(len=32) :: col

        call ensure_fig()
        nx = size(x)
        ny = size(y)
        if (nx < 2 .or. ny < 2) return
        if (size(u, 1) /= ny .or. size(u, 2) /= nx) return
        if (size(v, 1) /= ny .or. size(v, 2) /= nx) return

        dx = x(2) - x(1)
        dy = y(2) - y(1)
        x0 = x(1)
        y0 = y(1)
        dens = 1.0_dp
        if (present(density)) dens = density

        ! Velocities in grid coordinates, and the speed in axes coordinates,
        ! which is what the arc length is measured in.
        st%nx = nx
        st%ny = ny
        allocate (st%u(ny, nx), st%v(ny, nx), st%sp(ny, nx))
        st%u = u/dx
        st%v = v/dy
        st%sp = sqrt((st%u/real(nx - 1, dp))**2 + (st%v/real(ny - 1, dp))**2)

        st%mnx = int(30.0_dp*dens)
        st%mny = int(30.0_dp*dens)
        if (st%mnx < 1 .or. st%mny < 1) return
        allocate (st%mask(st%mny, st%mnx), st%claim(2, st%mnx*st%mny))
        st%mask = 0
        st%g2mx = real(st%mnx - 1, dp)/real(nx - 1, dp)
        st%g2my = real(st%mny - 1, dp)/real(ny - 1, dp)
        ! Going back the other way through the reciprocal, not by dividing:
        ! a seed on the far edge must land a hair outside the grid, exactly
        ! as it does in matplotlib, or it starts a line that should not be.
        st%m2gx = 1.0_dp/st%g2mx
        st%m2gy = 1.0_dp/st%g2my

        col = resolve_color(color, ca)
        if (len_trim(col) == 0) then
            col = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if

        allocate (xs(MAX_STREAM_PTS), ys(MAX_STREAM_PTS))
        allocate (tx(MAX_STREAM_PTS), ty(MAX_STREAM_PTS))
        nseed = st%mnx*st%mny
        allocate (ahx(nseed), ahy(nseed), ahu(nseed), ahv(nseed))
        na = 0

        sx = 0
        sy = 0
        do i = 1, nseed
            call stream_seed(st%mnx, st%mny, i, sx, sy, sd)
            if (st%mask(sy + 1, sx + 1) /= 0) cycle
            call stream_line(st, real(sx, dp)*st%m2gx, real(sy, dp)*st%m2gy, &
                             xs, ys, np, tot)
            if (np < 2 .or. tot <= MINLENGTH) cycle

            do j = 1, np
                tx(j) = x0 + xs(j)*dx
                ty(j) = y0 + ys(j)*dy
            end do
            is = new_shape_series(SERIES_LINE, tx(1:np), ty(1:np), trim(col))
            if (is < 1) cycle
            if (present(lw)) ax(cur_i)%series(is)%linewidth = lw

            ! One arrowhead per line, at the halfway point along it.
            sl = 0.0_dp
            do j = 1, np - 1
                sl = sl + hypot(tx(j + 1) - tx(j), ty(j + 1) - ty(j))
            end do
            half = 0.5_dp*sl
            sl = 0.0_dp
            k = np - 1
            do j = 1, np - 1
                sl = sl + hypot(tx(j + 1) - tx(j), ty(j + 1) - ty(j))
                if (sl >= half) then
                    k = j
                    exit
                end if
            end do
            na = na + 1
            ! The tip sits where matplotlib puts the head of its arrow patch,
            ! at the middle of the segment the halfway point falls in.
            ahx(na) = 0.5_dp*(tx(k) + tx(k + 1))
            ahy(na) = 0.5_dp*(ty(k) + ty(k + 1))
            ahu(na) = tx(k + 1) - tx(k)
            ahv(na) = ty(k + 1) - ty(k)
        end do

        if (na > 0) then
            is = new_shape_series(SERIES_ARROWHEAD, ahx(1:na), ahy(1:na), trim(col))
            if (is > 0) then
                allocate (ax(cur_i)%series(is)%qu(na), ax(cur_i)%series(is)%qv(na))
                ax(cur_i)%series(is)%qu = ahu(1:na)
                ax(cur_i)%series(is)%qv = ahv(1:na)
            end if
        end if

        ! matplotlib makes the edges of the field sticky, so the axes end
        ! exactly on the grid with no margin.
        ax(cur_i)%xroom = [x(1), x(nx)]
        ax(cur_i)%xroom_set = .true.
        if (ax(cur_i)%yroom_set) then
            ax(cur_i)%yroom = [min(ax(cur_i)%yroom(1), y(1)), max(ax(cur_i)%yroom(2), y(ny))]
        else
            ax(cur_i)%yroom = [y(1), y(ny)]
            ax(cur_i)%yroom_set = .true.
        end if
        ax(cur_i)%room_sticks = .true.
    end subroutine streamplot

    ! Seed points spiral inwards from the corner of the mask, which is what
    ! gives the boundary streamlines first claim on the cells.
    ! st holds the four closing edges and the direction of travel, so that
    ! the walk keeps no state of its own.
    subroutine stream_seed(nx, ny, i, x, y, sd)
        integer, intent(in) :: nx, ny, i
        integer, intent(inout) :: x, y, sd(5)
        integer :: xfirst, yfirst, xlast, ylast, dirn

        if (i == 1) then
            sd = [0, 1, nx - 1, ny - 1, 0]
            x = 0
            y = 0
            return
        end if
        xfirst = sd(1)
        yfirst = sd(2)
        xlast = sd(3)
        ylast = sd(4)
        dirn = sd(5)
        select case (dirn)
        case (0)
            x = x + 1
            if (x >= xlast) then
                xlast = xlast - 1
                dirn = 1
            end if
        case (1)
            y = y + 1
            if (y >= ylast) then
                ylast = ylast - 1
                dirn = 2
            end if
        case (2)
            x = x - 1
            if (x <= xfirst) then
                xfirst = xfirst + 1
                dirn = 3
            end if
        case default
            y = y - 1
            if (y <= yfirst) then
                yfirst = yfirst + 1
                dirn = 0
            end if
        end select
        sd = [xfirst, yfirst, xlast, ylast, dirn]
    end subroutine stream_seed

    ! Bilinear lookup on the integer grid, with the position given in grid
    ! coordinates running 0..n-1.
    function stream_interp(a, xi, yi) result(r)
        real(dp), intent(in) :: a(:, :), xi, yi
        real(dp) :: r, xt, yt, a0, a1
        integer :: ix, iy, ixn, iyn

        ix = int(xi)
        iy = int(yi)
        ixn = min(ix + 1, size(a, 2) - 1)
        iyn = min(iy + 1, size(a, 1) - 1)
        xt = xi - real(ix, dp)
        yt = yi - real(iy, dp)
        a0 = a(iy + 1, ix + 1)*(1.0_dp - xt) + a(iy + 1, ixn + 1)*xt
        a1 = a(iyn + 1, ix + 1)*(1.0_dp - xt) + a(iyn + 1, ixn + 1)*xt
        r = a0*(1.0_dp - yt) + a1*yt
    end function stream_interp

    function stream_in_grid(st, xi, yi) result(r)
        type(stream_t), intent(in) :: st
        real(dp), intent(in) :: xi, yi
        logical :: r
        r = xi >= 0.0_dp .and. xi <= real(st%nx - 1, dp) .and. &
            yi >= 0.0_dp .and. yi <= real(st%ny - 1, dp)
    end function stream_in_grid

    ! The direction of travel at a point, normalised so that a step in the
    ! parameter is a step in arc length. code is 1 off the grid and 2 where
    ! the field stalls.
    subroutine stream_dir(st, xi, yi, dirn, dxi, dyi, code)
        type(stream_t), intent(in) :: st
        real(dp), intent(in) :: xi, yi
        integer, intent(in) :: dirn
        real(dp), intent(out) :: dxi, dyi
        integer, intent(out) :: code
        real(dp) :: ds_dt

        dxi = 0.0_dp
        dyi = 0.0_dp
        code = 0
        if (.not. stream_in_grid(st, xi, yi)) then
            code = 1
            return
        end if
        ds_dt = stream_interp(st%sp, xi, yi)
        if (ds_dt == 0.0_dp) then
            code = 2
            return
        end if
        dxi = real(dirn, dp)*stream_interp(st%u, xi, yi)/ds_dt
        dyi = real(dirn, dp)*stream_interp(st%v, xi, yi)/ds_dt
    end subroutine stream_dir

    ! Claim the mask cell a point falls in. ok comes back false when another
    ! streamline already went through it.
    subroutine stream_claim(st, xg, yg, ok)
        type(stream_t), intent(inout) :: st
        real(dp), intent(in) :: xg, yg
        logical, intent(out) :: ok
        integer :: mx, my

        ok = .true.
        mx = nint(xg*st%g2mx)
        my = nint(yg*st%g2my)
        if (mx == st%cx .and. my == st%cy) return
        if (st%mask(my + 1, mx + 1) /= 0) then
            ok = .false.
            return
        end if
        st%nclaim = st%nclaim + 1
        st%claim(1, st%nclaim) = mx
        st%claim(2, st%nclaim) = my
        st%mask(my + 1, mx + 1) = 1
        st%cx = mx
        st%cy = my
    end subroutine stream_claim

    ! A whole streamline through the seed: backwards from it, then forwards,
    ! joined up. The cells claimed along the way are released again if the
    ! line turns out to be too short to keep.
    subroutine stream_line(st, x0, y0, xs, ys, np, tot)
        type(stream_t), intent(inout) :: st
        real(dp), intent(in) :: x0, y0
        real(dp), intent(inout) :: xs(:), ys(:)
        integer, intent(out) :: np
        real(dp), intent(out) :: tot
        real(dp), allocatable :: bx(:), by(:), fx(:), fy(:)
        real(dp) :: sb, sf
        integer :: nb, nf, j
        logical :: ok

        np = 0
        tot = 0.0_dp
        st%nclaim = 0
        st%cx = -1
        st%cy = -1
        call stream_claim(st, x0, y0, ok)
        if (.not. ok) return

        allocate (bx(MAX_STREAM_PTS), by(MAX_STREAM_PTS))
        allocate (fx(MAX_STREAM_PTS), fy(MAX_STREAM_PTS))
        call stream_rk12(st, x0, y0, -1, bx, by, nb, sb)
        st%cx = nint(x0*st%g2mx)
        st%cy = nint(y0*st%g2my)
        call stream_rk12(st, x0, y0, 1, fx, fy, nf, sf)
        tot = sb + sf

        do j = nb, 1, -1
            np = np + 1
            xs(np) = bx(j)
            ys(np) = by(j)
        end do
        do j = 2, nf
            np = np + 1
            xs(np) = fx(j)
            ys(np) = fy(j)
        end do
        if (tot <= 0.1_dp) then
            do j = 1, st%nclaim
                st%mask(st%claim(2, j) + 1, st%claim(1, j) + 1) = 0
            end do
        end if
    end subroutine stream_line

    ! Heun's method with an adaptive step: cheap, and small enough a step to
    ! visit every mask cell on the way, which is what the mask needs.
    subroutine stream_rk12(st, x0, y0, dirn, xs, ys, np, tot)
        type(stream_t), intent(inout) :: st
        real(dp), intent(in) :: x0, y0
        integer, intent(in) :: dirn
        real(dp), intent(inout) :: xs(:), ys(:)
        integer, intent(out) :: np
        real(dp), intent(out) :: tot
        real(dp), parameter :: MAXERROR = 0.003_dp, MAXLENGTH = 4.0_dp
        real(dp) :: maxds, ds, xi, yi, k1x, k1y, k2x, k2y, dx1, dy1, dx2, dy2, err
        integer :: code
        logical :: ok

        maxds = min(1.0_dp/real(st%mnx, dp), 1.0_dp/real(st%mny, dp), 0.1_dp)
        ds = maxds
        tot = 0.0_dp
        xi = x0
        yi = y0
        np = 0

        do
            if (.not. stream_in_grid(st, xi, yi)) then
                if (np > 0) call stream_euler(st, xs, ys, np, dirn, tot)
                exit
            end if
            if (np >= size(xs)) exit
            np = np + 1
            xs(np) = xi
            ys(np) = yi

            call stream_dir(st, xi, yi, dirn, k1x, k1y, code)
            if (code == 1) then
                call stream_euler(st, xs, ys, np, dirn, tot)
                exit
            else if (code == 2) then
                exit
            end if
            call stream_dir(st, xi + ds*k1x, yi + ds*k1y, dirn, k2x, k2y, code)
            if (code == 1) then
                call stream_euler(st, xs, ys, np, dirn, tot)
                exit
            else if (code == 2) then
                exit
            end if

            dx1 = ds*k1x
            dy1 = ds*k1y
            dx2 = ds*0.5_dp*(k1x + k2x)
            dy2 = ds*0.5_dp*(k1y + k2y)
            ! The error is measured in axes coordinates, so that it means the
            ! same thing whatever the grid.
            err = hypot((dx2 - dx1)/real(st%nx - 1, dp), (dy2 - dy1)/real(st%ny - 1, dp))

            if (err < MAXERROR) then
                xi = xi + dx2
                yi = yi + dy2
                ! Leaving the grid ends the line here: matplotlib only takes
                ! the Euler step to the boundary when the trial point of the
                ! step, not the step itself, falls outside.
                if (.not. stream_in_grid(st, xi, yi)) exit
                call stream_claim(st, xi, yi, ok)
                if (.not. ok) exit
                if (tot + ds > MAXLENGTH) exit
                tot = tot + ds
            end if

            if (err == 0.0_dp) then
                ds = maxds
            else
                ds = min(maxds, 0.85_dp*ds*sqrt(MAXERROR/err))
            end if
        end do
    end subroutine stream_rk12

    ! One plain Euler step out to the edge of the grid, so a line that leaves
    ! the field ends on the boundary rather than short of it.
    subroutine stream_euler(st, xs, ys, np, dirn, tot)
        type(stream_t), intent(in) :: st
        real(dp), intent(inout) :: xs(:), ys(:), tot
        integer, intent(inout) :: np
        integer, intent(in) :: dirn
        real(dp) :: xi, yi, cx, cy, dsx, dsy, ds
        integer :: code

        if (np < 1 .or. np >= size(xs)) return
        xi = xs(np)
        yi = ys(np)
        call stream_dir(st, xi, yi, dirn, cx, cy, code)
        if (code /= 0) return
        dsx = huge(1.0_dp)
        dsy = huge(1.0_dp)
        if (cx < 0.0_dp) then
            dsx = xi/(-cx)
        else if (cx > 0.0_dp) then
            dsx = (real(st%nx - 1, dp) - xi)/cx
        end if
        if (cy < 0.0_dp) then
            dsy = yi/(-cy)
        else if (cy > 0.0_dp) then
            dsy = (real(st%ny - 1, dp) - yi)/cy
        end if
        ds = min(dsx, dsy)
        np = np + 1
        xs(np) = xi + cx*ds
        ys(np) = yi + cy*ds
        tot = tot + ds
    end subroutine stream_euler

    ! A two dimensional histogram: count the points into a grid of cells
    ! and hand the counts to pcolormesh, which is how matplotlib draws one.
    subroutine hist2d(x, y, bins, cmap, vmin, vmax)
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: bins(2)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: vmin, vmax
        integer :: nb(2), n, i, jx, jy
        real(dp) :: x0, x1, y0, y1
        real(dp), allocatable :: xe(:), ye(:), h(:, :)

        call ensure_fig()
        n = min(size(x), size(y))
        if (n < 1) return
        nb = [10, 10]
        if (present(bins)) nb = max(1, bins)

        x0 = minval(x(1:n)); x1 = maxval(x(1:n))
        y0 = minval(y(1:n)); y1 = maxval(y(1:n))
        if (x1 <= x0) x1 = x0 + 1.0_dp
        if (y1 <= y0) y1 = y0 + 1.0_dp

        allocate (xe(nb(1) + 1), ye(nb(2) + 1), h(nb(2), nb(1)))
        do i = 1, nb(1) + 1
            xe(i) = x0 + (x1 - x0)*real(i - 1, dp)/real(nb(1), dp)
        end do
        do i = 1, nb(2) + 1
            ye(i) = y0 + (y1 - y0)*real(i - 1, dp)/real(nb(2), dp)
        end do

        h = 0.0_dp
        do i = 1, n
            ! The last cell owns its right edge, as numpy's histogram does.
            jx = min(nb(1), 1 + int((x(i) - x0)/(x1 - x0)*real(nb(1), dp)))
            jy = min(nb(2), 1 + int((y(i) - y0)/(y1 - y0)*real(nb(2), dp)))
            h(jy, jx) = h(jy, jx) + 1.0_dp
        end do

        call pcolormesh(xe, ye, h, cmap, vmin, vmax)
    end subroutine hist2d

    ! Hexagonal binning. matplotlib lays two rectangular grids over the
    ! data, one offset half a cell from the other, and gives each point to
    ! whichever centre is nearer: those centres are the hexagon centres,
    ! and this is the same arithmetic.
    subroutine hexbin(x, y, gridsize, cmap, mincnt)
        real(dp), intent(in) :: x(:), y(:)
        integer, intent(in), optional :: gridsize, mincnt
        character(len=*), intent(in), optional :: cmap
        integer :: nx, ny, nx1, ny1, nx2, ny2, n, i, j, k, ix1, iy1, ix2, iy2, mc
        real(dp) :: x0, x1, y0, y1, sx, sy, ix, iy, d1, d2, pad, cmax, t, cxp, cyp
        real(dp) :: hx(6), hy(6)
        real(dp), allocatable :: c1(:, :), c2(:, :)

        call ensure_fig()
        n = min(size(x), size(y))
        if (n < 1) return
        nx = 100
        if (present(gridsize)) nx = max(1, gridsize)
        ny = int(real(nx, dp)/sqrt(3.0_dp))
        ny = max(1, ny)
        ! matplotlib draws every cell of the grid, empty ones included, at
        ! the bottom of the colormap. mincnt raises that floor.
        mc = 0
        if (present(mincnt)) mc = mincnt

        x0 = minval(x(1:n)); x1 = maxval(x(1:n))
        y0 = minval(y(1:n)); y1 = maxval(y(1:n))
        pad = 1.0e-9_dp*(x1 - x0)
        x0 = x0 - pad; x1 = x1 + pad
        if (x1 <= x0) x1 = x0 + 1.0_dp
        if (y1 <= y0) y1 = y0 + 1.0_dp
        sx = (x1 - x0)/real(nx, dp)
        sy = (y1 - y0)/real(ny, dp)

        nx1 = nx + 1; ny1 = ny + 1
        nx2 = nx; ny2 = ny
        allocate (c1(nx1, ny1), c2(nx2, ny2))
        c1 = 0.0_dp
        c2 = 0.0_dp

        do i = 1, n
            ix = (x(i) - x0)/sx
            iy = (y(i) - y0)/sy
            ix1 = nint(ix); iy1 = nint(iy)
            ix2 = floor(ix); iy2 = floor(iy)
            d1 = (ix - real(ix1, dp))**2 + 3.0_dp*(iy - real(iy1, dp))**2
            d2 = (ix - real(ix2, dp) - 0.5_dp)**2 + 3.0_dp*(iy - real(iy2, dp) - 0.5_dp)**2
            if (d1 < d2) then
                if (ix1 >= 0 .and. ix1 < nx1 .and. iy1 >= 0 .and. iy1 < ny1) &
                    c1(ix1 + 1, iy1 + 1) = c1(ix1 + 1, iy1 + 1) + 1.0_dp
            else
                if (ix2 >= 0 .and. ix2 < nx2 .and. iy2 >= 0 .and. iy2 < ny2) &
                    c2(ix2 + 1, iy2 + 1) = c2(ix2 + 1, iy2 + 1) + 1.0_dp
            end if
        end do

        cmax = max(maxval(c1), maxval(c2))
        if (cmax <= 0.0_dp) return

        ! The hexagon is a cell wide and two thirds of a cell tall at the
        ! points, which is what makes the two grids interlock.
        hx = 0.5_dp*sx*[1.0_dp, 1.0_dp, 0.0_dp, -1.0_dp, -1.0_dp, 0.0_dp]
        hy = (sy/3.0_dp)*[-0.5_dp, 0.5_dp, 1.0_dp, 0.5_dp, -0.5_dp, -1.0_dp]

        ax(cur_i)%img_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%img_cmap = cmap_from_str(cmap)
        ax(cur_i)%img_vmin = real(mc, dp)
        ax(cur_i)%img_vmax = cmax
        ax(cur_i)%has_cmap_src = .true.

        do k = 1, 2
            do i = 1, merge(nx1, nx2, k == 1)
                do j = 1, merge(ny1, ny2, k == 1)
                    if (k == 1) then
                        if (c1(i, j) < real(mc, dp)) cycle
                        t = (c1(i, j) - real(mc, dp))/max(cmax - real(mc, dp), 1.0_dp)
                        cxp = x0 + real(i - 1, dp)*sx
                        cyp = y0 + real(j - 1, dp)*sy
                    else
                        if (c2(i, j) < real(mc, dp)) cycle
                        t = (c2(i, j) - real(mc, dp))/max(cmax - real(mc, dp), 1.0_dp)
                        cxp = x0 + (real(i - 1, dp) + 0.5_dp)*sx
                        cyp = y0 + (real(j - 1, dp) + 0.5_dp)*sy
                    end if
                    call add_polygon(cxp + hx, cyp + hy, &
                                     facecolor=cmap_color(ax(cur_i)%img_cmap, t))
                end do
            end do
        end do

        ! Patches never ask for room of their own, so the axes is told what
        ! the binning covered, margins and all.
        call xlim(x0 - 0.05_dp*(x1 - x0), x1 + 0.05_dp*(x1 - x0))
        call ylim(y0 - 0.05_dp*(y1 - y0), y1 + 0.05_dp*(y1 - y0))
    end subroutine hexbin

    ! A grid of coloured cells with edges of the caller's choosing. x and
    ! y are the edges, one more than the samples along that direction; if
    ! they are the same length as the samples they are taken as centres,
    ! which is matplotlib's shading="nearest".
    subroutine pcolormesh(x, y, c, cmap, vmin, vmax, norm, vcenter, gamma, &
                          linthresh)
        real(dp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, norm
        real(dp), intent(in), optional :: vmin, vmax, vcenter, gamma, linthresh
        integer :: nr, nc
        real(dp) :: lo, hi

        call ensure_fig()
        nr = size(c, 1)
        nc = size(c, 2)
        if (nr < 1 .or. nc < 1) return
        if (size(x) < nc .or. size(y) < nr) return

        if (allocated(ax(cur_i)%img)) deallocate (ax(cur_i)%img)
        allocate (ax(cur_i)%img(nr, nc))
        ax(cur_i)%img = c
        ax(cur_i)%has_img = .true.
        ax(cur_i)%has_mesh = .true.
        ax(cur_i)%has_cmap_src = .true.
        ax(cur_i)%img_origin_upper = .false.

        if (allocated(ax(cur_i)%mesh_x)) deallocate (ax(cur_i)%mesh_x)
        if (allocated(ax(cur_i)%mesh_y)) deallocate (ax(cur_i)%mesh_y)
        call cell_edges(x, nc, ax(cur_i)%mesh_x)
        call cell_edges(y, nr, ax(cur_i)%mesh_y)

        ax(cur_i)%img_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%img_cmap = cmap_from_str(cmap)
        ax(cur_i)%img_norm = NORM_LINEAR
        if (present(norm)) ax(cur_i)%img_norm = norm_from_str(norm)
        call norm_params(vcenter, gamma, linthresh)

        if (ax(cur_i)%img_norm == NORM_LOG) then
            lo = huge(1.0_dp)
            if (any(c > 0.0_dp)) lo = minval(c, mask=(c > 0.0_dp))
            hi = maxval(c)
        else
            lo = minval(c)
            hi = maxval(c)
        end if
        if (present(vmin)) lo = vmin
        if (present(vmax)) hi = vmax
        if (hi <= lo) hi = lo + 1.0_dp
        ax(cur_i)%img_vmin = lo
        ax(cur_i)%img_vmax = hi

        ax(cur_i)%img_ext = [minval(ax(cur_i)%mesh_x), maxval(ax(cur_i)%mesh_x), &
                             minval(ax(cur_i)%mesh_y), maxval(ax(cur_i)%mesh_y)]
        ! A mesh does not force a square aspect, unlike an image.
        ax(cur_i)%aspect = 0.0_dp
    end subroutine pcolormesh

    ! matplotlib's pcolor differs from pcolormesh only in how it is drawn,
    ! and both come out of one cell loop here.
    subroutine pcolor(x, y, c, cmap, vmin, vmax, norm)
        real(dp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, norm
        real(dp), intent(in), optional :: vmin, vmax
        call pcolormesh(x, y, c, cmap, vmin, vmax, norm)
    end subroutine pcolor

    ! n+1 edges from either the edges themselves or the n cell centres.
    subroutine cell_edges(v, n, e)
        real(dp), intent(in) :: v(:)
        integer, intent(in) :: n
        real(dp), allocatable, intent(out) :: e(:)
        integer :: i

        allocate (e(n + 1))
        if (size(v) >= n + 1) then
            e = v(1:n + 1)
            return
        end if
        do i = 2, n
            e(i) = 0.5_dp*(v(i - 1) + v(i))
        end do
        if (n == 1) then
            e(1) = v(1) - 0.5_dp
            e(2) = v(1) + 0.5_dp
        else
            e(1) = v(1) - 0.5_dp*(v(2) - v(1))
            e(n + 1) = v(n) + 0.5_dp*(v(n) - v(n - 1))
        end if
    end subroutine cell_edges

    ! ------------------------------------------------------------------
    ! 3D axes. matplotlib picks the projection when the axes is created;
    ! with one current axes the same thing is said by turning that axes
    ! into a 3D one, and any of the 3D plotting calls does it implicitly.
    ! ------------------------------------------------------------------

    subroutine axes3d(elev, azim)
        real(dp), intent(in), optional :: elev, azim

        call ensure_fig()
        ax(cur_i)%is3d = .true.
        ! A 3D axes is gridded by default (rcParams axes3d.grid), unlike a
        ! 2D one.
        ax(cur_i)%grid_on = .true.
        if (present(elev)) ax(cur_i)%elev = elev
        if (present(azim)) ax(cur_i)%azim = azim
    end subroutine axes3d

    subroutine view_init(elev, azim)
        real(dp), intent(in), optional :: elev, azim

        call axes3d(elev, azim)
    end subroutine view_init

    subroutine zlabel(s)
        character(len=*), intent(in) :: s

        call ensure_fig()
        ax(cur_i)%zlabel = s
    end subroutine zlabel

    subroutine zlim(lo, hi)
        real(dp), intent(in) :: lo, hi

        call ensure_fig()
        ax(cur_i)%zlim_set = .true.
        ax(cur_i)%zmin_user = lo
        ax(cur_i)%zmax_user = hi
    end subroutine zlim

    subroutine plot3d(x, y, z, fmt, label, lw, color, marker, linestyle, alpha)
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        integer :: is, n

        call axes3d()
        call add_series(cur_i, x, y, fmt, label, lw, color, marker, linestyle, alpha)
        is = ax(cur_i)%n_series
        if (is < 1) return
        n = min(ax(cur_i)%series(is)%n, size(z))
        ax(cur_i)%series(is)%n = n
        ax(cur_i)%series(is)%kind = SERIES_LINE3D
        allocate (ax(cur_i)%series(is)%z(n))
        ax(cur_i)%series(is)%z(1:n) = z(1:n)
    end subroutine plot3d

    subroutine scatter3d(x, y, z, s, c, marker, label, alpha)
        real(dp), intent(in) :: x(:), y(:), z(:)
        real(dp), intent(in), optional :: s, alpha
        character(len=*), intent(in), optional :: c, marker, label
        integer :: is, n

        call axes3d()
        call scatter(x, y, s, c, marker, label, alpha)
        is = ax(cur_i)%n_series
        if (is < 1) return
        n = min(ax(cur_i)%series(is)%n, size(z))
        ax(cur_i)%series(is)%n = n
        ax(cur_i)%series(is)%kind = SERIES_SCATTER3D
        allocate (ax(cur_i)%series(is)%z(n))
        ax(cur_i)%series(is)%z(1:n) = z(1:n)
    end subroutine scatter3d

    ! z is indexed (row, column) = (y, x), as the 2D grids are.
    subroutine plot_surface(x, y, z, color, alpha, cmap)
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color, cmap
        real(dp), intent(in), optional :: alpha
        integer :: is, nx, ny

        call axes3d()
        ny = size(z, 1)
        nx = size(z, 2)
        if (nx < 2 .or. ny < 2) return
        if (size(x) < nx .or. size(y) < ny) return
        call push_series(ax(cur_i), is)
        ax(cur_i)%series(is)%kind = SERIES_SURFACE
        ax(cur_i)%series(is)%n = 0
        allocate (ax(cur_i)%series(is)%x(nx), ax(cur_i)%series(is)%y(ny))
        allocate (ax(cur_i)%series(is)%zg(ny, nx))
        ax(cur_i)%series(is)%x(1:nx) = x(1:nx)
        ax(cur_i)%series(is)%y(1:ny) = y(1:ny)
        ax(cur_i)%series(is)%zg = z(1:ny, 1:nx)
        ax(cur_i)%series(is)%color = resolve_color(color)
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
        if (present(cmap)) ax(cur_i)%series(is)%scmap = cmap_from_str(cmap)
        if (present(alpha)) ax(cur_i)%series(is)%alpha = alpha
    end subroutine plot_surface

    ! Scattered points in space, triangulated in the xy plane and drawn as
    ! lit facets. matplotlib triangulates with Qhull and shades the result
    ! exactly as it shades a surface, so this shares render_surface's light.
    subroutine plot_trisurf(x, y, z, color, alpha, cmap)
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: color, cmap
        real(dp), intent(in), optional :: alpha
        integer, allocatable :: tri(:, :)
        integer :: is, n, nt

        call axes3d()
        n = min(size(x), min(size(y), size(z)))
        if (n < 3) return
        call delaunay(x(1:n), y(1:n), tri, nt)
        if (nt < 1) return
        call push_series(ax(cur_i), is)
        ax(cur_i)%series(is)%kind = SERIES_TRISURF
        ax(cur_i)%series(is)%n = n
        allocate (ax(cur_i)%series(is)%x(n), ax(cur_i)%series(is)%y(n))
        allocate (ax(cur_i)%series(is)%z(n))
        allocate (ax(cur_i)%series(is)%tri(3, nt))
        ax(cur_i)%series(is)%x = x(1:n)
        ax(cur_i)%series(is)%y = y(1:n)
        ax(cur_i)%series(is)%z = z(1:n)
        ax(cur_i)%series(is)%tri = tri(:, 1:nt)
        ax(cur_i)%series(is)%color = resolve_color(color)
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
        if (present(cmap)) ax(cur_i)%series(is)%scmap = cmap_from_str(cmap)
        if (present(alpha)) ax(cur_i)%series(is)%alpha = alpha
    end subroutine plot_trisurf

    ! Boxes standing on the xy plane. Each is dx by dy wide and dz tall,
    ! and is drawn as six lit faces, which is what mplot3d does.
    subroutine bar3d(x, y, z, dx, dy, dz, color, alpha)
        real(dp), intent(in) :: x(:), y(:), z(:), dx, dy, dz(:)
        character(len=*), intent(in), optional :: color
        real(dp), intent(in), optional :: alpha
        integer :: is, n

        call axes3d()
        n = min(min(size(x), size(y)), min(size(z), size(dz)))
        if (n < 1) return
        call push_series(ax(cur_i), is)
        ax(cur_i)%series(is)%kind = SERIES_BAR3D
        ax(cur_i)%series(is)%n = n
        allocate (ax(cur_i)%series(is)%x(n), ax(cur_i)%series(is)%y(n))
        allocate (ax(cur_i)%series(is)%z(n), ax(cur_i)%series(is)%z2(n))
        ax(cur_i)%series(is)%x = x(1:n)
        ax(cur_i)%series(is)%y = y(1:n)
        ax(cur_i)%series(is)%z = z(1:n)
        ax(cur_i)%series(is)%z2 = z(1:n) + dz(1:n)
        ax(cur_i)%series(is)%d3x = dx
        ax(cur_i)%series(is)%d3y = dy
        ax(cur_i)%series(is)%color = resolve_color(color)
        if (len_trim(ax(cur_i)%series(is)%color) == 0) then
            ax(cur_i)%series(is)%color = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
        if (present(alpha)) ax(cur_i)%series(is)%alpha = alpha
    end subroutine bar3d

    ! Arrows in space. Each is three straight lines, the shaft and two
    ! barbs turned fifteen degrees off it, which is how mplot3d builds one.
    subroutine quiver3d(x, y, z, u, v, w, length, normalize, color, lw)
        real(dp), intent(in) :: x(:), y(:), z(:), u(:), v(:), w(:)
        real(dp), intent(in), optional :: length, lw
        logical, intent(in), optional :: normalize
        character(len=*), intent(in), optional :: color
        real(dp), parameter :: HEAD = 0.3_dp
        real(dp), parameter :: RANG = 15.0_dp*PI/180.0_dp
        real(dp) :: len_, d(3), tip(3), xp, yp, nrm, c, sn, barb(3), seg(2)
        real(dp) :: segx(2), segy(2), segz(2)
        character(len=32) :: col
        integer :: n, i, k

        call axes3d()
        n = min(min(size(x), size(y)), size(z))
        n = min(n, min(min(size(u), size(v)), size(w)))
        if (n < 1) return
        len_ = 1.0_dp
        if (present(length)) len_ = length
        col = resolve_color(color)
        if (len_trim(col) == 0) then
            col = cycle_color(ax(cur_i)%color_cycle)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle + 1
        end if
        c = cos(RANG)
        sn = sin(RANG)

        do i = 1, n
            d = [u(i), v(i), w(i)]
            if (present(normalize)) then
                if (normalize) then
                    nrm = sqrt(sum(d**2))
                    if (nrm > 0.0_dp) d = d/nrm
                end if
            end if
            tip = [x(i), y(i), z(i)] + len_*d
            segx = [x(i), tip(1)]
            segy = [y(i), tip(2)]
            segz = [z(i), tip(3)]
            call plot3d(segx, segy, segz, color=trim(col), lw=lw)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
            ax(cur_i)%series(ax(cur_i)%n_series)%nolim = .true.

            ! The axis to turn the shaft about is level and across it.
            nrm = sqrt(d(1)**2 + d(2)**2)
            if (nrm > 0.0_dp) then
                xp = d(2)/nrm
                yp = -d(1)/nrm
            else
                xp = 0.0_dp
                yp = 1.0_dp
            end if
            do k = 1, 2
                seg(1) = merge(sn, -sn, k == 1)
                barb = rotate_about(d, xp, yp, c, seg(1))
                segx = [tip(1), tip(1) - len_*HEAD*barb(1)]
                segy = [tip(2), tip(2) - len_*HEAD*barb(2)]
                segz = [tip(3), tip(3) - len_*HEAD*barb(3)]
                call plot3d(segx, segy, segz, color=trim(col), lw=lw)
                ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
                ax(cur_i)%series(ax(cur_i)%n_series)%nolim = .true.
            end do
        end do

        ! The limits come from where the arrows start, not where they reach.
        call plot3d(x(1:n), y(1:n), z(1:n), linestyle="none")
        ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
    end subroutine quiver3d

    ! Rodrigues' formula for a turn about the level axis (xp, yp, 0), with
    ! the cosine and sine of the angle given.
    pure function rotate_about(d, xp, yp, c, s) result(r)
        real(dp), intent(in) :: d(3), xp, yp, c, s
        real(dp) :: r(3)

        r(1) = (c + xp*xp*(1.0_dp - c))*d(1) + xp*yp*(1.0_dp - c)*d(2) + yp*s*d(3)
        r(2) = xp*yp*(1.0_dp - c)*d(1) + (c + yp*yp*(1.0_dp - c))*d(2) - xp*s*d(3)
        r(3) = -yp*s*d(1) + xp*s*d(2) + c*d(3)
    end function rotate_about

    ! Level lines of a grid, each drawn in space at the height of its own
    ! level. mplot3d does the same, and takes its limits from the grid
    ! rather than from the rounded levels.
    subroutine contour3d(x, y, z, levels, cmap)
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        real(dp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap
        real(dp), allocatable :: lev(:), ex(:, :), ey(:, :), vx(:), vy(:), vz(:)
        logical, allocatable :: used(:)
        real(dp) :: t(MAX_TICKS), gx(2), gy(2), tx(3), ty(3), tv(3), sx(2), sy(2)
        real(dp) :: tol, cx(2), cy(2), cz(2)
        integer :: nr, nc, nlev, nseg, ns, nv, i, j, k, c, np, cm, id

        call axes3d()
        nr = size(z, 1)
        nc = size(z, 2)
        if (nr < 2 .or. nc < 2) return
        if (size(x) < nc .or. size(y) < nr) return
        cm = CMAP_VIRIDIS
        if (present(cmap)) cm = cmap_from_str(cmap)
        if (present(levels)) then
            allocate (lev(size(levels)))
            lev = levels
        else
            call contour_levels(minval(z), maxval(z), 8, t, nlev)
            allocate (lev(nlev))
            lev = t(1:nlev)
        end if
        nlev = size(lev)
        if (nlev < 1) return

        nseg = 2*(nr - 1)*(nc - 1)
        allocate (ex(2, nseg), ey(2, nseg), used(nseg))
        allocate (vx(nseg + 1), vy(nseg + 1), vz(nseg + 1))
        tol = 1.0e-9_dp*(abs(x(nc) - x(1))/real(nc - 1, dp) &
                         + abs(y(nr) - y(1))/real(nr - 1, dp))

        do k = 1, nlev
            ns = 0
            do i = 1, nr - 1
                gy = [y(i), y(i + 1)]
                do j = 1, nc - 1
                    gx = [x(j), x(j + 1)]
                    do c = 1, 2
                        call cell_triangle(z, i, j, gx, gy, c, tx, ty, tv)
                        call tri_level(tx, ty, tv, lev(k), sx, sy, np)
                        if (np /= 2) cycle
                        ns = ns + 1
                        ex(:, ns) = sx
                        ey(:, ns) = sy
                    end do
                end do
            end do
            if (ns == 0) cycle

            used(1:ns) = .false.
            do
                call chain_polyline(ex, ey, used, ns, tol, id, vx, vy, nv)
                if (nv == 0) exit
                vz(1:nv) = lev(k)
                call plot3d(vx(1:nv), vy(1:nv), vz(1:nv), lw=1.5_dp, &
                            color=cmap_color(cm, real(k - 1, dp)/real(max(nlev - 1, 1), dp)))
                ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
                ax(cur_i)%series(ax(cur_i)%n_series)%nolim = .true.
            end do
        end do

        ! The corners of the grid, so that the box holds the data and not
        ! only the levels that happened to cross it.
        cx = [x(1), x(nc)]
        cy = [y(1), y(nr)]
        cz = [minval(z), maxval(z)]
        call plot3d(cx, cy, cz, linestyle="none")
        ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        deallocate (ex, ey, used, vx, vy, vz)
    end subroutine contour3d

    ! The same grid, ruled rather than filled. matplotlib draws every line
    ! of the mesh whether it is in front or behind, and so does this.
    subroutine plot_wireframe(x, y, z, color, alpha, lw)
        real(dp), intent(in) :: x(:), y(:), z(:, :)
        character(len=*), intent(in), optional :: color
        real(dp), intent(in), optional :: alpha, lw
        integer :: is

        call plot_surface(x, y, z, color, alpha)
        is = ax(cur_i)%n_series
        if (is < 1) return
        ax(cur_i)%series(is)%wire = .true.
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine plot_wireframe

    ! ----------------------------------------------------------------------
    ! Scattered points, triangulated. matplotlib triangulates with Qhull
    ! and draws the result with the ordinary artists; so does this, which
    ! is why these are so short.
    ! ----------------------------------------------------------------------

    ! The edges of the triangulation, as one line broken at every jump,
    ! which is how matplotlib draws it too.
    subroutine triplot(x, y, color, lw, linestyle, marker, label)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, linestyle, marker, label
        real(dp), intent(in), optional :: lw
        integer, allocatable :: tri(:, :)
        real(dp), allocatable :: ex(:), ey(:)
        integer :: nt, i, k
        real(dp) :: nan, zero

        call ensure_fig()
        call delaunay(x, y, tri, nt)
        if (nt < 1) return
        ! The break between one triangle and the next, which plot draws as
        ! a gap just as matplotlib does.
        zero = 0.0_dp
        nan = zero/zero
        ! Four points and a break for each triangle: round it and back to
        ! the start.
        allocate (ex(5*nt), ey(5*nt))
        k = 0
        do i = 1, nt
            ex(k + 1:k + 3) = x(tri(:, i))
            ey(k + 1:k + 3) = y(tri(:, i))
            ex(k + 4) = x(tri(1, i))
            ey(k + 4) = y(tri(1, i))
            ex(k + 5) = nan
            ey(k + 5) = nan
            k = k + 5
        end do
        call plot(ex, ey, color=color, lw=lw, linestyle=linestyle, label=label)
        ! matplotlib draws the points as a second artist, which is why
        ! they come out in the next colour of the cycle.
        if (present(marker)) &
            call plot(x, y, color=color, marker=marker, linestyle="none")
    end subroutine triplot

    ! A flat-shaded triangle per triangle, coloured by the mean of its
    ! three values, which is matplotlib's default shading.
    subroutine tripcolor(x, y, z, cmap, vmin, vmax, edgecolor, alpha)
        real(dp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: cmap, edgecolor
        real(dp), intent(in), optional :: vmin, vmax, alpha
        integer, allocatable :: tri(:, :)
        integer :: nt, i, cm
        real(dp) :: lo, hi, v, tx(3), ty(3)
        real(dp), allocatable :: fv(:)

        call ensure_fig()
        call delaunay(x, y, tri, nt)
        if (nt < 1) return
        cm = CMAP_VIRIDIS
        if (present(cmap)) cm = cmap_from_str(cmap)
        ! Flat shading colours a triangle by the mean of its corners, and
        ! it is those means, not the values themselves, that the colours
        ! and the colorbar are scaled to.
        allocate (fv(nt))
        do i = 1, nt
            fv(i) = sum(z(tri(:, i)))/3.0_dp
        end do
        lo = minval(fv)
        hi = maxval(fv)
        if (present(vmin)) lo = vmin
        if (present(vmax)) hi = vmax
        if (hi <= lo) hi = lo + 1.0_dp
        do i = 1, nt
            tx = x(tri(:, i))
            ty = y(tri(:, i))
            v = fv(i)
            call add_polygon(tx, ty, &
                             facecolor=cmap_color(cm, (v - lo)/(hi - lo)), &
                             edgecolor=edgecolor, alpha=alpha)
            ! Unlike a lone patch, a mesh cell is data and sets the limits,
            ! and its seam with the next cell must not show.
            ax(cur_i)%series(ax(cur_i)%n_series)%patch_scales = .true.
            ax(cur_i)%series(ax(cur_i)%n_series)%patch_seal = .true.
        end do
        ! What the colorbar reads.
        ax(cur_i)%has_cmap_src = .true.
        ax(cur_i)%img_cmap = cm
        ax(cur_i)%img_vmin = lo
        ax(cur_i)%img_vmax = hi
    end subroutine tripcolor

    ! The level lines of a field known at scattered points. Linear
    ! interpolation over a triangle crosses a level in one segment, so the
    ! same geometry that serves contour serves here.
    subroutine tricontour(x, y, z, levels, cmap, lw)
        real(dp), intent(in) :: x(:), y(:), z(:)
        real(dp), intent(in), optional :: levels(:), lw
        character(len=*), intent(in), optional :: cmap
        call add_tricontour(x, y, z, levels, cmap, lw, filled=.false.)
    end subroutine tricontour

    ! The same, with the bands between the levels filled.
    subroutine tricontourf(x, y, z, levels, cmap)
        real(dp), intent(in) :: x(:), y(:), z(:)
        real(dp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap
        call add_tricontour(x, y, z, levels, cmap, filled=.true.)
    end subroutine tricontourf

    subroutine add_tricontour(x, y, z, levels, cmap, lw, filled)
        real(dp), intent(in) :: x(:), y(:), z(:)
        real(dp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap
        real(dp), intent(in), optional :: lw
        logical, intent(in) :: filled
        integer, allocatable :: tri(:, :)
        real(dp), allocatable :: lev(:), sx(:), sy(:)
        real(dp) :: t(MAX_TICKS), tx(3), ty(3), tv(3)
        real(dp) :: qx(MAX_POLY), qy(MAX_POLY), ex(2), ey(2), nan, zero
        integer :: nt, nlev, i, k, ns, np, nq, cm

        call ensure_fig()
        call delaunay(x, y, tri, nt)
        if (nt < 1) return
        cm = CMAP_VIRIDIS
        if (present(cmap)) cm = cmap_from_str(cmap)
        if (present(levels)) then
            allocate (lev(size(levels)))
            lev = levels
        else
            call contour_levels(minval(z), maxval(z), 8, t, nlev)
            allocate (lev(nlev))
            lev = t(1:nlev)
        end if
        nlev = size(lev)
        if (nlev < 2) return
        zero = 0.0_dp
        nan = zero/zero

        if (filled) then
            do k = 1, nlev - 1
                do i = 1, nt
                    tx = x(tri(:, i))
                    ty = y(tri(:, i))
                    tv = z(tri(:, i))
                    call tri_band(tx, ty, tv, lev(k), lev(k + 1), qx, qy, nq)
                    if (nq < 3) cycle
                    call add_polygon(qx(1:nq), qy(1:nq), &
                        facecolor=cmap_color(cm, (real(k, dp) - 0.5_dp)/real(nlev - 1, dp)))
                    ax(cur_i)%series(ax(cur_i)%n_series)%patch_scales = .true.
                    ax(cur_i)%series(ax(cur_i)%n_series)%patch_seal = .true.
                end do
            end do
        else
            ! Three points per segment: the two ends and a break, which is
            ! enough because the segments need not be joined up to be drawn.
            allocate (sx(3*nt), sy(3*nt))
            do k = 1, nlev
                ns = 0
                do i = 1, nt
                    tx = x(tri(:, i))
                    ty = y(tri(:, i))
                    tv = z(tri(:, i))
                    call tri_level(tx, ty, tv, lev(k), ex, ey, np)
                    if (np /= 2) cycle
                    sx(ns + 1:ns + 2) = ex
                    sy(ns + 1:ns + 2) = ey
                    sx(ns + 3) = nan
                    sy(ns + 3) = nan
                    ns = ns + 3
                end do
                if (ns == 0) cycle
                call plot(sx(1:ns), sy(1:ns), lw=lw, &
                          color=cmap_color(cm, real(k - 1, dp)/real(nlev - 1, dp)))
                ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
            end do
        end if

        ax(cur_i)%has_cmap_src = .true.
        ax(cur_i)%img_cmap = cm
        ax(cur_i)%img_vmin = lev(1)
        ax(cur_i)%img_vmax = lev(nlev)
        ax(cur_i)%cont_ext = [minval(x), maxval(x), minval(y), maxval(y)]
        ax(cur_i)%tight_lim = .true.
    end subroutine add_tricontour

    subroutine contour(z, levels, cmap, extent)
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call add_contour(z, levels, cmap, extent, .false.)
    end subroutine contour

    ! Write each level into its own contour line. There is nothing to
    ! label until the lines are laid out in points, so this only records
    ! the wish and the drawing code does the work.
    subroutine clabel(fontsize)
        real(dp), intent(in), optional :: fontsize

        call ensure_fig()
        ax(cur_i)%cont_labels = .true.
        if (present(fontsize)) ax(cur_i)%clab_size = fontsize
    end subroutine clabel

    subroutine contourf(z, levels, cmap, extent)
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        call add_contour(z, levels, cmap, extent, .true.)
    end subroutine contourf

    ! z is indexed (row, column) with row 1 at the bottom, which is how
    ! matplotlib orients a contour set: unlike imshow, the y axis ascends.
    subroutine add_contour(z, levels, cmap, extent, filled)
        real(dp), intent(in) :: z(:, :)
        real(dp), intent(in), optional :: levels(:), extent(4)
        character(len=*), intent(in), optional :: cmap
        logical, intent(in) :: filled
        integer :: nr, nc, nt
        real(dp) :: t(MAX_TICKS)

        call ensure_fig()
        nr = size(z, 1)
        nc = size(z, 2)
        if (nr < 2 .or. nc < 2) return

        if (allocated(ax(cur_i)%cz)) deallocate (ax(cur_i)%cz)
        allocate (ax(cur_i)%cz(nr, nc))
        ax(cur_i)%cz = z
        ax(cur_i)%has_cont = .true.
        ax(cur_i)%cont_filled = filled

        ax(cur_i)%cont_cmap = CMAP_VIRIDIS
        if (present(cmap)) ax(cur_i)%cont_cmap = cmap_from_str(cmap)

        if (allocated(ax(cur_i)%clev)) deallocate (ax(cur_i)%clev)
        if (present(levels)) then
            allocate (ax(cur_i)%clev(size(levels)))
            ax(cur_i)%clev = levels
        else
            ! matplotlib picks round levels spanning the data.
            call contour_levels(minval(z), maxval(z), 8, t, nt)
            allocate (ax(cur_i)%clev(nt))
            ax(cur_i)%clev = t(1:nt)
        end if

        if (present(extent)) then
            ax(cur_i)%cont_ext = extent
        else
            ax(cur_i)%cont_ext = [0.0_dp, real(nc - 1, dp), 0.0_dp, real(nr - 1, dp)]
        end if

        ax(cur_i)%has_cmap_src = .true.
        ax(cur_i)%img_cmap = ax(cur_i)%cont_cmap
        ax(cur_i)%img_vmin = ax(cur_i)%clev(1)
        ax(cur_i)%img_vmax = ax(cur_i)%clev(size(ax(cur_i)%clev))
    end subroutine add_contour

    ! Staircase line. matplotlib draws this as an ordinary line through a
    ! doubled-up sequence of points, so building that sequence here is enough
    ! and the renderer needs to know nothing about steps.
    subroutine step(x, y, where, label, color, lw, linestyle, alpha)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label, color, linestyle
        real(dp), intent(in), optional :: lw, alpha
        character(len=8) :: w
        real(dp), allocatable :: sx(:), sy(:)

        w = "pre"
        if (present(where)) w = where
        call stair_points(x, y, w, sx, sy)
        if (size(sx) == 0) return
        call plot(sx, sy, label=label, color=color, lw=lw, &
                  linestyle=linestyle, alpha=alpha)
    end subroutine step

    ! matplotlib spells the same three shapes two ways: where= for step and
    ! drawstyle= for plot.
    pure function step_where(drawstyle) result(w)
        character(len=*), intent(in) :: drawstyle
        character(len=8) :: w
        select case (drawstyle)
        case ("steps-post", "steps"); w = "post"
        case ("steps-mid"); w = "mid"
        case default; w = "pre"
        end select
    end function step_where

    ! Each sample contributes two points: the tread of its step and the
    ! riser to the next one. Where the riser sits is what `where` selects.
    pure subroutine stair_points(x, y, where, sx, sy)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: where
        real(dp), allocatable, intent(out) :: sx(:), sy(:)
        integer :: n, i, k

        n = min(size(x), size(y))
        if (n <= 0) then
            allocate (sx(0), sy(0))
            return
        end if
        allocate (sx(2*n), sy(2*n))
        k = 0
        do i = 1, n
            if (trim(where) == "mid") then
                if (i == 1) then
                    sx(k + 1) = x(1)
                else
                    sx(k + 1) = 0.5_dp*(x(i - 1) + x(i))
                end if
                if (i == n) then
                    sx(k + 2) = x(n)
                else
                    sx(k + 2) = 0.5_dp*(x(i) + x(i + 1))
                end if
                sy(k + 1) = y(i)
                sy(k + 2) = y(i)
            else if (trim(where) == "post") then
                sx(k + 1) = x(i)
                sy(k + 1) = y(i)
                if (i == n) then
                    sx(k + 2) = x(n)
                else
                    sx(k + 2) = x(i + 1)
                end if
                sy(k + 2) = y(i)
            else
                sx(k + 1) = x(i)
                if (i == 1) then
                    sy(k + 1) = y(1)
                else
                    sy(k + 1) = y(i - 1)
                end if
                sx(k + 2) = x(i)
                sy(k + 2) = y(i)
            end if
            k = k + 2
        end do
    end subroutine stair_points

    ! Horizontal bars: y locates each bar and width is its length.
    subroutine barh_num(y, width, height, color, label, alpha, left, colors, &
                        edgecolor, linewidth, hatch)
        real(dp), intent(in) :: y(:), width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:)
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_BARH, y, width, color, label, alpha)
        if (is < 1) return
        if (present(height)) ax(cur_i)%series(is)%width = height
        call bar_options(is, left, colors, edgecolor, linewidth, hatch)
    end subroutine barh_num

    subroutine barh_cat(cats, width, height, color, label, alpha, left, &
                        colors, edgecolor, linewidth, hatch)
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: width(:)
        real(dp), intent(in), optional :: height, alpha, left(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:)
        call barh_num(category_positions(size(cats)), width, height, color, &
                      label, alpha, left, colors, edgecolor, linewidth, hatch)
        call yticks(category_positions(size(cats)), cats)
    end subroutine barh_cat

    ! Markers on stalks rising from y = 0, with a baseline along the bottom.
    subroutine stem(x, y, color, label, alpha)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: alpha
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_STEM, x, y, color, label, alpha)
        if (is < 1) return
        ax(cur_i)%series(is)%marker = MARKER_CIRCLE
    end subroutine stem

    ! Turn the current axes into a polar one: angle along x, radius along y.
    subroutine set_polar()
        call ensure_fig()
        ax(cur_i)%polar = .true.
        ! A polar axes carries its grid unless the caller says otherwise,
        ! which is how matplotlib draws one.
        ax(cur_i)%grid_on = .true.
    end subroutine set_polar

    ! pylab's polar(): make the axes polar and plot on it in one call.
    subroutine polar(theta, r, color, label, lw, linestyle, marker, alpha)
        real(dp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: color, label, linestyle, marker
        real(dp), intent(in), optional :: lw, alpha

        call ensure_fig()
        call set_polar()
        call plot(theta, r, color=color, label=label, lw=lw, &
                  linestyle=linestyle, marker=marker, alpha=alpha)
    end subroutine polar

    ! ----------------------------------------------------------------------
    ! Patches: plain shapes in data coordinates. Each one is kept as the
    ! ring of points it comes down to, so a circle drawn on axes of
    ! different scales leans exactly as matplotlib's does.
    ! ----------------------------------------------------------------------

    subroutine add_rectangle(xy, width, height, angle, facecolor, edgecolor, &
                             lw, alpha, fill, hatch)
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        real(dp) :: px(4), py(4), rx(4), ry(4), a, ca, sa
        integer :: i

        px = [0.0_dp, width, width, 0.0_dp]
        py = [0.0_dp, 0.0_dp, height, height]
        a = 0.0_dp
        if (present(angle)) a = angle
        ! A rectangle turns about the corner it is given, as matplotlib's does.
        ca = cos(a*PI/180.0_dp)
        sa = sin(a*PI/180.0_dp)
        do i = 1, 4
            rx(i) = xy(1) + px(i)*ca - py(i)*sa
            ry(i) = xy(2) + px(i)*sa + py(i)*ca
        end do
        call add_polygon(rx, ry, facecolor, edgecolor, lw, alpha, fill, hatch)
    end subroutine add_rectangle

    subroutine add_circle(xy, radius, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: xy(2), radius
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        call add_ellipse(xy, 2.0_dp*radius, 2.0_dp*radius, 0.0_dp, &
                         facecolor, edgecolor, lw, alpha, fill)
    end subroutine add_circle

    subroutine add_ellipse(xy, width, height, angle, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: xy(2), width, height
        real(dp), intent(in), optional :: angle, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        integer, parameter :: N = 72
        real(dp) :: px(N), py(N), t, cx, cy, a, ca, sa
        integer :: i

        a = 0.0_dp
        if (present(angle)) a = angle
        ca = cos(a*PI/180.0_dp)
        sa = sin(a*PI/180.0_dp)
        do i = 1, N
            t = 2.0_dp*PI*real(i - 1, dp)/real(N, dp)
            cx = 0.5_dp*width*cos(t)
            cy = 0.5_dp*height*sin(t)
            px(i) = xy(1) + cx*ca - cy*sa
            py(i) = xy(2) + cx*sa + cy*ca
        end do
        call add_polygon(px, py, facecolor, edgecolor, lw, alpha, fill)
    end subroutine add_ellipse

    subroutine add_polygon(x, y, facecolor, edgecolor, lw, alpha, fill, hatch)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor, hatch
        logical, intent(in), optional :: fill
        integer :: is

        call ensure_fig()
        is = new_shape_series(SERIES_PATCH, x, y, facecolor, alpha=alpha)
        if (is == 0) return
        ! A patch does not take a turn of the color cycle: matplotlib gives
        ! every one of them the same first color unless told otherwise.
        if (.not. present(facecolor)) then
            ax(cur_i)%series(is)%color = "#1f77b4"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        ax(cur_i)%series(is)%edgecolor = ""
        if (present(edgecolor)) ax(cur_i)%series(is)%edgecolor = resolve_color(edgecolor)
        ax(cur_i)%series(is)%edgewidth = 1.0_dp
        if (present(lw)) ax(cur_i)%series(is)%edgewidth = lw
        if (present(fill)) ax(cur_i)%series(is)%patch_fill = fill
        if (present(hatch)) ax(cur_i)%series(is)%hatch = hatch
        if (present(edgecolor)) ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
    end subroutine add_polygon

    ! matplotlib's Arrow patch: the same unit outline it uses, scaled to the
    ! length of (dx, dy) and to `width` across, then turned to point along
    ! the vector. It is built in data coordinates, as matplotlib builds it,
    ! so an axes that is not square shears the arrow in the same way.
    subroutine add_arrow(x, y, dx, dy, width, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: x, y, dx, dy
        real(dp), intent(in), optional :: width, lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        real(dp), parameter :: UX(7) = [0.0_dp, 0.0_dp, 0.8_dp, 0.8_dp, &
                                        1.0_dp, 0.8_dp, 0.8_dp]
        real(dp), parameter :: UY(7) = [0.1_dp, -0.1_dp, -0.1_dp, -0.3_dp, &
                                        0.0_dp, 0.3_dp, 0.1_dp]
        real(dp) :: w, ln, ct, st, ax_(7), ay(7), u, v
        integer :: j

        w = 1.0_dp
        if (present(width)) w = width
        ln = hypot(dx, dy)
        if (ln <= 0.0_dp) return
        ct = dx/ln
        st = dy/ln
        do j = 1, 7
            u = UX(j)*ln
            v = UY(j)*w
            ax_(j) = x + ct*u - st*v
            ay(j) = y + st*u + ct*v
        end do
        call add_polygon(ax_, ay, facecolor, edgecolor, lw, alpha, fill)
    end subroutine add_arrow

    ! An arbitrary path, as matplotlib's PathPatch draws one. `codes` has a
    ! letter per verb: M moves, L draws a line, C draws a cubic from the
    ! next three points and Z closes back to the last move.
    subroutine add_path(x, y, codes, facecolor, edgecolor, lw, alpha, fill)
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: codes
        real(dp), intent(in), optional :: lw, alpha
        character(len=*), intent(in), optional :: facecolor, edgecolor
        logical, intent(in), optional :: fill
        integer :: is, j, np, nv
        integer :: v(len_trim(codes))

        nv = len_trim(codes)
        np = 0
        do j = 1, nv
            select case (codes(j:j))
            case ("M", "m")
                v(j) = VERB_MOVE
                np = np + 1
            case ("L", "l")
                v(j) = VERB_LINE
                np = np + 1
            case ("C", "c")
                v(j) = VERB_CUBIC
                np = np + 3
            case ("Z", "z")
                v(j) = VERB_CLOSE
            case default
                print *, "fplot: add_path: unknown code ", codes(j:j)
                error stop
            end select
        end do
        if (np < 2 .or. np > min(size(x), size(y))) then
            print *, "fplot: add_path: codes need", np, "points, given", &
                min(size(x), size(y))
            error stop
        end if

        call add_polygon(x(1:np), y(1:np), facecolor, edgecolor, lw, alpha, fill)
        is = ax(cur_i)%n_series
        if (is < 1) return
        allocate (ax(cur_i)%series(is)%pverb(nv))
        ax(cur_i)%series(is)%pverb = v
    end subroutine add_path

    ! A field of arrows, one per point, pointing along (u, v). Left to
    ! itself matplotlib sizes the arrows from the field: the shaft is a
    ! fixed fraction of the axes width and the scale is set so a vector of
    ! average length draws an arrow of a comfortable size.
    subroutine quiver(x, y, u, v, color, scale, width, label)
        real(dp), intent(in) :: x(:), y(:), u(:), v(:)
        character(len=*), intent(in), optional :: color, label
        real(dp), intent(in), optional :: scale, width
        integer :: is, n

        call ensure_fig()
        n = min(size(x), size(y), size(u), size(v))
        if (n <= 0) return
        is = new_shape_series(SERIES_QUIVER, x(1:n), y(1:n), color, label)
        if (is == 0) return
        allocate (ax(cur_i)%series(is)%qu(n), ax(cur_i)%series(is)%qv(n))
        ax(cur_i)%series(is)%qu(1:n) = u(1:n)
        ax(cur_i)%series(is)%qv(1:n) = v(1:n)
        ! Arrows are black unless asked otherwise; they do not take a turn
        ! of the color cycle in matplotlib either.
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(scale)) ax(cur_i)%series(is)%qscale = scale
        if (present(width)) ax(cur_i)%series(is)%qwidth = width
    end subroutine quiver

    ! Pie chart. matplotlib turns the axes into a unit square centred on the
    ! origin and hides the frame, so the wedges are plain data-space geometry.
    subroutine pie(values, labels, cmap, explode, startangle, counterclock, &
                   autopct, pctdistance, labeldistance, radius, colors, &
                   edgecolor, linewidth)
        real(dp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:), cmap, autopct
        character(len=*), intent(in), optional :: colors(:), edgecolor
        real(dp), intent(in), optional :: explode(:), startangle, linewidth
        real(dp), intent(in), optional :: pctdistance, labeldistance, radius
        logical, intent(in), optional :: counterclock
        integer :: is, i, n
        real(dp) :: rad, pd, ld, off

        call ensure_fig()
        n = size(values)
        if (n <= 0) return
        if (any(values < 0.0_dp) .or. sum(values) <= 0.0_dp) return

        is = new_shape_series(SERIES_PIE, values, values)
        if (is < 1) return
        allocate (ax(cur_i)%series(is)%pcolor(n))
        do i = 1, n
            if (present(colors)) then
                ax(cur_i)%series(is)%pcolor(i) = &
                    resolve_color(colors(mod(i - 1, size(colors)) + 1))
            else if (present(cmap)) then
                ax(cur_i)%series(is)%pcolor(i) = &
                    cmap_color(cmap_from_str(cmap), real(i - 1, dp) / real(max(n - 1, 1), dp))
            else
                ax(cur_i)%series(is)%pcolor(i) = cycle_color(i - 1)
            end if
        end do

        if (present(edgecolor)) &
            ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
        if (present(linewidth)) ax(cur_i)%series(is)%linewidth = linewidth
        rad = 1.0_dp
        if (present(radius)) rad = radius
        ax(cur_i)%series(is)%pie_radius = rad
        if (present(startangle)) &
            ax(cur_i)%series(is)%pie_start = startangle*PI/180.0_dp
        if (present(counterclock)) ax(cur_i)%series(is)%pie_ccw = counterclock
        allocate (ax(cur_i)%series(is)%pexp(n))
        ax(cur_i)%series(is)%pexp = 0.0_dp
        if (present(explode)) then
            do i = 1, min(n, size(explode))
                ax(cur_i)%series(is)%pexp(i) = explode(i)
            end do
        end if

        ! matplotlib measures both distances in radii from the centre of
        ! the wedge, which is itself pushed out by explode.
        ld = 1.1_dp
        pd = 0.6_dp
        if (present(labeldistance)) ld = labeldistance
        if (present(pctdistance)) pd = pctdistance
        do i = 1, n
            off = ax(cur_i)%series(is)%pexp(i)
            if (present(labels)) then
                if (i <= size(labels)) &
                    call add_pie_text(ax(cur_i)%series(is), values, i, labels(i), &
                                      ld*rad, off*rad)
            end if
            if (present(autopct)) &
                call add_pie_text(ax(cur_i)%series(is), values, i, &
                                  pct_text(autopct, 100.0_dp*values(i)/sum(values)), &
                                  pd*rad, off*rad)
        end do

        call xlim(-1.25_dp, 1.25_dp)
        call ylim(-1.25_dp, 1.25_dp)
        ax(cur_i)%aspect = 1.0_dp
        ax(cur_i)%frame_off = .true.
    end subroutine pie

    ! The mid angle of wedge i, honouring where the pie starts and which
    ! way round it runs. Both the wedges and their labels are placed from
    ! here, so the two cannot drift apart.
    pure function pie_mid(s, values, i) result(mid)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: values(:)
        integer, intent(in) :: i
        real(dp) :: mid, tot, dir

        tot = sum(values)
        dir = 1.0_dp
        if (.not. s%pie_ccw) dir = -1.0_dp
        mid = s%pie_start + dir*PI*(sum(values(1:i - 1)) + sum(values(1:i)))/tot
    end function pie_mid

    ! Place one wedge's text at the given distance out along its mid angle,
    ! measured from the centre the wedge itself sits on.
    subroutine add_pie_text(s, values, i, lab, dist, off)
        type(series_t), intent(in) :: s
        real(dp), intent(in) :: values(:), dist, off
        integer, intent(in) :: i
        character(len=*), intent(in) :: lab
        real(dp) :: mid
        integer :: it

        mid = pie_mid(s, values, i)
        call push_text(ax(cur_i), it)
        ax(cur_i)%texts(it)%x = (dist + off)*cos(mid)
        ax(cur_i)%texts(it)%y = (dist + off)*sin(mid)
        ax(cur_i)%texts(it)%s = lab
        ax(cur_i)%texts(it)%ha = "center"
        ax(cur_i)%texts(it)%va = "center"
    end subroutine add_pie_text

    ! matplotlib's autopct format string, for the printf forms a pie chart
    ! actually uses: %f with an optional width and precision, and %% for a
    ! literal per cent sign. Anything else is copied out as it stands.
    function pct_text(fmt, v) result(out)
        character(len=*), intent(in) :: fmt
        real(dp), intent(in) :: v
        character(len=64) :: out
        character(len=32) :: num, spec
        integer :: i, j, dec, ios

        out = ""
        j = 0
        i = 1
        do while (i <= len_trim(fmt))
            if (fmt(i:i) /= "%") then
                j = j + 1
                out(j:j) = fmt(i:i)
                i = i + 1
                cycle
            end if
            if (i < len_trim(fmt)) then
                if (fmt(i + 1:i + 1) == "%") then
                    j = j + 1
                    out(j:j) = "%"
                    i = i + 2
                    cycle
                end if
            end if
            ! A conversion: read up to the terminating letter, then take
            ! the precision out of it.
            spec = ""
            i = i + 1
            do while (i <= len_trim(fmt))
                spec = trim(spec)//fmt(i:i)
                if (index("fFeEgG", fmt(i:i)) > 0) exit
                i = i + 1
            end do
            i = i + 1
            dec = 6
            if (index(spec, ".") > 0) then
                read (spec(index(spec, ".") + 1:len_trim(spec) - 1), *, iostat=ios) dec
                if (ios /= 0) dec = 1
            else
                dec = 0
            end if
            write (spec, "(I0)") max(0, dec)
            write (num, "(f0."//trim(spec)//")") v
            ! A value below one writes as ".5" without the leading zero.
            if (num(1:1) == ".") num = "0"//trim(num)
            out(j + 1:) = trim(num)
            j = j + len_trim(num)
        end do
    end function pct_text

    ! matplotlib's tick_params, for the settings that change what is drawn:
    ! which axis, the tick direction, its length, and the size and rotation
    ! of the tick labels.
    subroutine tick_params(axis, direction, length, labelsize, rotation)
        character(len=*), intent(in), optional :: axis, direction
        real(dp), intent(in), optional :: length, labelsize, rotation
        logical :: dox, doy
        real(dp) :: d

        call ensure_fig()
        dox = .true.
        doy = .true.
        if (present(axis)) then
            select case (lower(axis))
            case ("x")
                doy = .false.
            case ("y")
                dox = .false.
            case ("both")
            case default
                error stop "fplot: tick_params axis must be x, y or both"
            end select
        end if

        if (present(direction)) then
            select case (lower(direction))
            case ("out")
                d = 1.0_dp
            case ("in")
                d = -1.0_dp
            case ("inout")
                d = 0.0_dp
            case default
                error stop "fplot: tick direction must be in, out or inout"
            end select
            if (dox) ax(cur_i)%xtick_dir = d
            if (doy) ax(cur_i)%ytick_dir = d
        end if
        if (present(length)) then
            if (dox) ax(cur_i)%xtick_len = length
            if (doy) ax(cur_i)%ytick_len = length
        end if
        if (present(labelsize)) then
            if (dox) ax(cur_i)%xtick_size = labelsize
            if (doy) ax(cur_i)%ytick_size = labelsize
        end if
        if (present(rotation)) then
            if (dox) ax(cur_i)%xtick_rot = rotation
            if (doy) ax(cur_i)%ytick_rot = rotation
        end if
    end subroutine tick_params

    ! Show or hide individual spines. Absent arguments are left alone.
    subroutine spines(left, right, bottom, top)
        logical, intent(in), optional :: left, right, bottom, top
        call ensure_fig()
        if (present(left)) ax(cur_i)%spine(SPINE_LEFT) = left
        if (present(right)) ax(cur_i)%spine(SPINE_RIGHT) = right
        if (present(bottom)) ax(cur_i)%spine(SPINE_BOTTOM) = bottom
        if (present(top)) ax(cur_i)%spine(SPINE_TOP) = top
    end subroutine spines

    ! matplotlib's axis(): "on"/"off" for the frame, "equal"/"scaled" for
    ! square units, "tight" to drop the data margin, "auto" to undo them.
    subroutine axis(mode)
        character(len=*), intent(in) :: mode
        call ensure_fig()
        select case (lower(mode))
        case ("off")
            ax(cur_i)%frame_off = .true.
        case ("on")
            ax(cur_i)%frame_off = .false.
        case ("equal")
            call set_aspect(1.0_dp, "datalim")
        case ("scaled")
            call set_aspect(1.0_dp, "box")
        case ("tight")
            ax(cur_i)%xmargin = 0.0_dp
            ax(cur_i)%ymargin = 0.0_dp
        case ("auto")
            ax(cur_i)%aspect = 0.0_dp
            ax(cur_i)%xmargin = 0.05_dp
            ax(cur_i)%ymargin = 0.05_dp
        case default
            error stop "fplot: unknown axis mode"
        end select
    end subroutine axis

    ! matplotlib's margins(): the room left past the data, as a fraction of
    ! the drawn length. One value sets both axes, x= and y= one each.
    subroutine margins(m, x, y)
        real(dp), intent(in), optional :: m, x, y
        call ensure_fig()
        if (present(m)) then
            ax(cur_i)%xmargin = m
            ax(cur_i)%ymargin = m
        end if
        if (present(x)) ax(cur_i)%xmargin = x
        if (present(y)) ax(cur_i)%ymargin = y
    end subroutine margins

    ! matplotlib's autoscale(). enable=.false. pins the limits where the
    ! data has put them so far, which is the only way to stop an axis from
    ! growing with what is drawn next; enable=.true. hands it back to the
    ! data. tight= drops the margin, or puts the usual five percent back.
    subroutine autoscale(enable, axis, tight)
        logical, intent(in), optional :: enable, tight
        character(len=*), intent(in), optional :: axis
        logical :: dox, doy, on
        real(dp) :: xmn, xmx, ymn, ymx

        call ensure_fig()
        dox = .true.
        doy = .true.
        if (present(axis)) then
            select case (lower(axis))
            case ("x")
                doy = .false.
            case ("y")
                dox = .false.
            case ("both")
            case default
                error stop "fplot: autoscale axis must be x, y or both"
            end select
        end if

        if (present(tight)) then
            if (tight) then
                if (dox) ax(cur_i)%xmargin = 0.0_dp
                if (doy) ax(cur_i)%ymargin = 0.0_dp
            else
                if (dox) ax(cur_i)%xmargin = 0.05_dp
                if (doy) ax(cur_i)%ymargin = 0.05_dp
            end if
        end if

        on = .true.
        if (present(enable)) on = enable
        if (on) then
            if (dox) ax(cur_i)%xlim_set = .false.
            if (doy) ax(cur_i)%ylim_set = .false.
            return
        end if
        call compute_limits(ax(cur_i), xmn, xmx, ymn, ymx)
        if (dox) then
            ax(cur_i)%xmin_user = xmn
            ax(cur_i)%xmax_user = xmx
            ax(cur_i)%xlim_set = .true.
        end if
        if (doy) then
            ax(cur_i)%ymin_user = ymn
            ax(cur_i)%ymax_user = ymx
            ax(cur_i)%ylim_set = .true.
        end if
    end subroutine autoscale

    ! ratio is the length of one y unit over the length of one x unit.
    subroutine set_aspect(ratio, adjustable)
        real(dp), intent(in) :: ratio
        character(len=*), intent(in), optional :: adjustable
        call ensure_fig()
        if (ratio <= 0.0_dp) error stop "fplot: aspect ratio must be positive"
        ax(cur_i)%aspect = ratio
        ax(cur_i)%aspect_datalim = .false.
        if (present(adjustable)) then
            select case (lower(adjustable))
            case ("box")
                ax(cur_i)%aspect_datalim = .false.
            case ("datalim")
                ax(cur_i)%aspect_datalim = .true.
            case default
                error stop "fplot: adjustable must be box or datalim"
            end select
        end if
    end subroutine set_aspect

    ! One box per call. Fortran has no ragged arrays, so a group of datasets
    ! of differing sizes is built by calling this once per dataset rather than
    ! by passing matplotlib's list; datasets of one size can go in together
    ! as a matrix, a row to each.
    subroutine boxplot_one(y, position, width, color, label, vert, notch, &
                           showmeans, patch_artist, whis)
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width, whis
        character(len=*), intent(in), optional :: color, label
        logical, intent(in), optional :: vert, notch, showmeans, patch_artist
        integer :: is

        call add_dist_series(SERIES_BOX, y, position, width, color, label, 0.15_dp)
        is = ax(cur_i)%n_series
        if (is < 1) return
        if (present(vert)) ax(cur_i)%series(is)%box_vert = vert
        if (present(notch)) ax(cur_i)%series(is)%box_notch = notch
        if (present(showmeans)) ax(cur_i)%series(is)%box_mean = showmeans
        if (present(whis)) ax(cur_i)%series(is)%whis = whis
        if (present(patch_artist)) then
            ax(cur_i)%series(is)%box_fill = patch_artist
            ! A filled box takes the first cycle colour, as matplotlib's
            ! patch_artist boxes do, and keeps its black furniture.
            if (patch_artist .and. .not. present(color)) &
                ax(cur_i)%series(is)%hcolor = cycle_color(0)
        end if
    end subroutine boxplot_one

    ! A row per dataset. labels name the categories along the axis the
    ! boxes are ranged on.
    subroutine boxplot_many(y, labels, positions, width, color, vert, notch, &
                            showmeans, patch_artist, whis)
        real(dp), intent(in) :: y(:, :)
        character(len=*), intent(in), optional :: labels(:), color
        real(dp), intent(in), optional :: positions(:), width, whis
        logical, intent(in), optional :: vert, notch, showmeans, patch_artist
        integer :: k, nk
        real(dp), allocatable :: pos(:)
        logical :: up

        call ensure_fig()
        nk = size(y, 1)
        if (nk < 1) return
        allocate (pos(nk))
        do k = 1, nk
            pos(k) = real(k, dp)
            if (present(positions)) then
                if (k <= size(positions)) pos(k) = positions(k)
            end if
            call boxplot_one(y(k, :), pos(k), width, color, vert=vert, &
                             notch=notch, showmeans=showmeans, &
                             patch_artist=patch_artist, whis=whis)
        end do
        if (present(labels)) then
            up = .true.
            if (present(vert)) up = vert
            if (up) then
                call xticks(pos, labels)
            else
                call yticks(pos, labels)
            end if
        end if
    end subroutine boxplot_many

    subroutine violinplot_one(y, position, width, color, label, vert, showmeans, &
                              showmedians, showextrema)
        real(dp), intent(in) :: y(:)
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        logical, intent(in), optional :: vert, showmeans, showmedians, showextrema
        integer :: is

        call add_dist_series(SERIES_VIOLIN, y, position, width, color, label, 0.5_dp)
        is = ax(cur_i)%n_series
        if (is < 1) return
        if (present(vert)) ax(cur_i)%series(is)%box_vert = vert
        if (present(showmeans)) ax(cur_i)%series(is)%box_mean = showmeans
        ! matplotlib draws the extrema bars unless told not to, and the
        ! median only when asked.
        if (present(showmedians)) ax(cur_i)%series(is)%box_notch = showmedians
        if (present(showextrema)) ax(cur_i)%series(is)%box_fill = .not. showextrema
    end subroutine violinplot_one

    subroutine violinplot_many(y, labels, positions, width, color, vert, &
                               showmeans, showmedians, showextrema)
        real(dp), intent(in) :: y(:, :)
        character(len=*), intent(in), optional :: labels(:), color
        real(dp), intent(in), optional :: positions(:), width
        logical, intent(in), optional :: vert, showmeans, showmedians, showextrema
        integer :: k, nk
        real(dp), allocatable :: pos(:)
        logical :: up

        call ensure_fig()
        nk = size(y, 1)
        if (nk < 1) return
        allocate (pos(nk))
        do k = 1, nk
            pos(k) = real(k, dp)
            if (present(positions)) then
                if (k <= size(positions)) pos(k) = positions(k)
            end if
            call violinplot_one(y(k, :), pos(k), width, color, vert=vert, &
                                showmeans=showmeans, showmedians=showmedians, &
                                showextrema=showextrema)
        end do
        if (present(labels)) then
            up = .true.
            if (present(vert)) up = vert
            if (up) then
                call xticks(pos, labels)
            else
                call yticks(pos, labels)
            end if
        end if
    end subroutine violinplot_many

    subroutine add_dist_series(kd, y, position, width, color, label, wdefault)
        integer, intent(in) :: kd
        real(dp), intent(in) :: y(:), wdefault
        real(dp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: color, label
        integer :: is, i, nd

        call ensure_fig()
        if (size(y) < 1) return

        ! Unpositioned distributions line up at 1, 2, 3, ... in the order added.
        nd = 0
        do i = 1, ax(cur_i)%n_series
            if (ax(cur_i)%series(i)%kind == SERIES_BOX .or. &
                ax(cur_i)%series(i)%kind == SERIES_VIOLIN) nd = nd + 1
        end do

        is = new_shape_series(kd, y, y, color, label)
        if (is < 1) return
        call sort_in_place(ax(cur_i)%series(is)%y)
        ax(cur_i)%series(is)%pos = real(nd + 1, dp)
        if (present(position)) ax(cur_i)%series(is)%pos = position
        ax(cur_i)%series(is)%width = wdefault
        if (present(width)) ax(cur_i)%series(is)%width = width
        ! Neither kind takes a turn in the color cycle: matplotlib draws box
        ! furniture in black and every violin in the first cycle color.
        if (.not. present(color)) then
            if (kd == SERIES_BOX) then
                ax(cur_i)%series(is)%color = "#000000"
            else
                ax(cur_i)%series(is)%color = cycle_color(0)
            end if
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
    end subroutine add_dist_series

    subroutine colorbar(label, orientation, fraction, pad, shrink, aspect)
        character(len=*), intent(in), optional :: label, orientation
        real(dp), intent(in), optional :: fraction, pad, shrink, aspect
        real(dp) :: keep
        call ensure_fig()
        if (.not. ax(cur_i)%has_cmap_src) return
        if (ax(cur_i)%cbar_on) return
        ax(cur_i)%cbar_on = .true.
        if (present(label)) ax(cur_i)%cbar_label = label
        if (present(orientation)) then
            select case (lower(orientation))
            case ("vertical")
            case ("horizontal")
                ax(cur_i)%cbar_horiz = .true.
                ! matplotlib leaves more room under a horizontal bar,
                ! because its tick labels sit under it too.
                ax(cur_i)%cbar_pad = 0.15_dp
            case default
                error stop "fplot: colorbar orientation must be vertical or horizontal"
            end select
        end if
        if (present(fraction)) ax(cur_i)%cbar_frac = fraction
        if (present(pad)) ax(cur_i)%cbar_pad = pad
        if (present(shrink)) ax(cur_i)%cbar_shrink = shrink
        if (present(aspect)) ax(cur_i)%cbar_aspect = aspect

        ! The bar is cut out of the axes box, as in matplotlib: the axes
        ! keeps what is left of the width, or of the height.
        keep = 1.0_dp - ax(cur_i)%cbar_frac - ax(cur_i)%cbar_pad
        if (ax(cur_i)%cbar_horiz) then
            ax(cur_i)%bottom = ax(cur_i)%top - keep * (ax(cur_i)%top - ax(cur_i)%bottom)
        else
            ax(cur_i)%right = ax(cur_i)%left + keep * (ax(cur_i)%right - ax(cur_i)%left)
        end if
    end subroutine colorbar

    ! Vertical bars of the given heights, centred on x and drawn from y = 0.
    ! bottom stacks this series on top of another; colors gives every bar
    ! its own color, as matplotlib's list-valued color does.
    subroutine bar_num(x, height, width, color, label, alpha, bottom, colors, &
                       edgecolor, linewidth, hatch, yerr, align, tick_label, &
                       ecolor, capsize)
        real(dp), intent(in) :: x(:), height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        real(dp), intent(in), optional :: yerr(:), capsize
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:), align, ecolor
        character(len=*), intent(in), optional :: tick_label(:)
        real(dp), allocatable :: pos(:)
        integer :: is, n

        call ensure_fig()
        n = min(size(x), size(height))
        allocate (pos(n))
        pos = x(1:n)
        ! align="edge" measures from the left edge of the bar rather than
        ! from its middle, so the positions move by half a width.
        if (present(align)) then
            if (align == "edge") then
                if (present(width)) then
                    pos = pos + 0.5_dp*width
                else
                    pos = pos + 0.4_dp
                end if
            end if
        end if
        is = new_shape_series(SERIES_BAR, pos, height, color, label, alpha)
        if (is < 1) return
        if (present(width)) ax(cur_i)%series(is)%width = width
        call bar_options(is, bottom, colors, edgecolor, linewidth, hatch)
        if (present(yerr)) then
            call set_arm(ax(cur_i)%series(is)%eylo, n, yerr)
            call set_arm(ax(cur_i)%series(is)%eyhi, n, yerr)
            ! matplotlib draws bar error bars in black, with no caps unless
            ! a capsize is asked for.
            ax(cur_i)%series(is)%ecolor = "#000000"
            ax(cur_i)%series(is)%ecap = 0.0_dp
            if (present(ecolor)) ax(cur_i)%series(is)%ecolor = resolve_color(ecolor)
            if (present(capsize)) ax(cur_i)%series(is)%ecap = capsize
        end if
        if (present(tick_label)) call xticks(pos, tick_label)
    end subroutine bar_num

    subroutine bar_cat(cats, height, width, color, label, alpha, bottom, &
                       colors, edgecolor, linewidth, hatch)
        character(len=*), intent(in) :: cats(:)
        real(dp), intent(in) :: height(:)
        real(dp), intent(in), optional :: width, alpha, bottom(:), linewidth
        character(len=*), intent(in), optional :: color, label, edgecolor, hatch
        character(len=*), intent(in), optional :: colors(:)
        call bar_num(category_positions(size(cats)), height, width, color, &
                     label, alpha, bottom, colors, edgecolor, linewidth, hatch)
        call xticks(category_positions(size(cats)), cats)
    end subroutine bar_cat

    ! Categories sit at 0, 1, 2, ..., as matplotlib places them.
    pure function category_positions(n) result(v)
        integer, intent(in) :: n
        real(dp) :: v(n)
        integer :: i
        do i = 1, n
            v(i) = real(i - 1, dp)
        end do
    end function category_positions

    subroutine bar_options(is, base, colors, edgecolor, linewidth, hatch)
        integer, intent(in) :: is
        real(dp), intent(in), optional :: base(:), linewidth
        character(len=*), intent(in), optional :: colors(:), edgecolor, hatch
        integer :: n, j

        n = ax(cur_i)%series(is)%n
        if (present(base)) then
            allocate (ax(cur_i)%series(is)%y2(n))
            do j = 1, n
                ax(cur_i)%series(is)%y2(j) = base(min(j, size(base)))
            end do
        end if
        if (present(colors)) then
            allocate (ax(cur_i)%series(is)%pcolor(n))
            do j = 1, n
                ax(cur_i)%series(is)%pcolor(j) = &
                    resolve_color(colors(min(j, size(colors))))
            end do
        end if
        if (present(edgecolor)) then
            ax(cur_i)%series(is)%edgecolor = resolve_color(edgecolor)
            ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
        end if
        if (present(linewidth)) ax(cur_i)%series(is)%edgewidth = linewidth
        if (present(hatch)) ax(cur_i)%series(is)%hatch = hatch
    end subroutine bar_options

    ! Label every bar of the most recent bar series with its value. fmt is a
    ! Fortran edit descriptor such as "(f4.1)"; the default prints the value
    ! as compactly as it can.
    subroutine bar_label(fmt, padding, fontsize)
        character(len=*), intent(in), optional :: fmt
        real(dp), intent(in), optional :: padding, fontsize
        integer :: i

        call ensure_fig()
        do i = ax(cur_i)%n_series, 1, -1
            if (ax(cur_i)%series(i)%kind == SERIES_BAR .or. &
                ax(cur_i)%series(i)%kind == SERIES_BARH) then
                ax(cur_i)%series(i)%bar_labels = .true.
                ax(cur_i)%series(i)%bar_label_size = def_tick
                if (present(fmt)) ax(cur_i)%series(i)%bar_fmt = fmt
                if (present(padding)) ax(cur_i)%series(i)%bar_pad = padding
                if (present(fontsize)) ax(cur_i)%series(i)%bar_label_size = fontsize
                return
            end if
        end do
    end subroutine bar_label

    ! Histogram of x using `bins` equal-width bins over the data range.
    subroutine hist(x, bins, color, label, alpha, bin_edges, density, &
                    cumulative, histtype, weights, stacked, orientation, &
                    log, rwidth)
        real(dp), intent(in) :: x(:)
        integer, intent(in), optional :: bins
        character(len=*), intent(in), optional :: color, label, histtype
        character(len=*), intent(in), optional :: orientation
        real(dp), intent(in), optional :: alpha, bin_edges(:), weights(:), rwidth
        logical, intent(in), optional :: density, cumulative, stacked, log
        integer :: nb, i, k, n, is
        real(dp) :: lo, hi, w, tot
        real(dp), allocatable :: edges(:), centers(:), counts(:), widths(:)
        real(dp), allocatable :: sx(:), sy(:), base(:), sb(:)
        logical :: norm, cum, stk, horiz
        character(len=16) :: ht

        n = size(x)
        if (n <= 0) return
        norm = .false.
        cum = .false.
        if (present(density)) norm = density
        if (present(cumulative)) cum = cumulative
        stk = .false.
        if (present(stacked)) stk = stacked
        ht = "bar"
        if (present(histtype)) ht = histtype
        horiz = .false.
        if (present(orientation)) horiz = orientation == "horizontal"

        if (present(bin_edges)) then
            nb = size(bin_edges) - 1
            if (nb < 1) return
            allocate (edges(nb + 1))
            edges = bin_edges
        else
            nb = 10
            if (present(bins)) nb = bins
            if (nb < 1) return
            lo = minval(x)
            hi = maxval(x)
            if (hi <= lo) then
                lo = lo - 0.5_dp
                hi = hi + 0.5_dp
            end if
            w = (hi - lo) / real(nb, dp)
            allocate (edges(nb + 1))
            do i = 1, nb + 1
                edges(i) = lo + real(i - 1, dp) * w
            end do
        end if

        allocate (centers(nb), counts(nb), widths(nb))
        counts = 0.0_dp
        do i = 1, nb
            centers(i) = 0.5_dp * (edges(i) + edges(i + 1))
            widths(i) = edges(i + 1) - edges(i)
        end do
        do i = 1, n
            if (x(i) < edges(1) .or. x(i) > edges(nb + 1)) cycle
            k = bin_of(x(i), edges, nb)
            if (present(weights)) then
                counts(k) = counts(k) + weights(min(i, size(weights)))
            else
                counts(k) = counts(k) + 1.0_dp
            end if
        end do

        ! density normalises by the total area, so the bars integrate to one
        ! even when the bins are of different widths.
        if (norm) then
            tot = sum(counts)
            if (tot > 0.0_dp) counts = counts / (tot * widths)
        end if
        if (cum) then
            do i = 2, nb
                counts(i) = counts(i) + counts(i - 1)
            end do
        end if

        ! rwidth leaves a gap between the bars by shrinking each about its
        ! own centre; the bins themselves are untouched.
        if (present(rwidth)) widths = widths*rwidth

        call ensure_fig()
        ! log puts the count axis on a log scale, which is the y axis for an
        ! upright histogram and the x axis for one lying on its side.
        if (present(log)) then
            if (log) then
                if (horiz) then
                    call set_xscale("log")
                else
                    call set_yscale("log")
                end if
            end if
        end if
        ! A stacked histogram starts where the last one in these axes ended.
        allocate (base(nb))
        base = 0.0_dp
        if (stk) then
            if (allocated(ax(cur_i)%hstack)) then
                if (size(ax(cur_i)%hstack) == nb) base = ax(cur_i)%hstack
                deallocate (ax(cur_i)%hstack)
            end if
            allocate (ax(cur_i)%hstack(nb))
            ax(cur_i)%hstack = base + counts
        else if (allocated(ax(cur_i)%hstack)) then
            deallocate (ax(cur_i)%hstack)
        end if

        select case (trim(ht))
        case ("step", "stepfilled")
            ! The outline of the same bars: up at every left edge, across
            ! the top, and back down to the baseline at both ends.
            allocate (sx(2*nb + 2), sy(2*nb + 2), sb(2*nb + 2))
            sx(1) = edges(1)
            sy(1) = base(1)
            sb(1) = base(1)
            do i = 1, nb
                sx(2*i) = edges(i)
                sy(2*i) = base(i) + counts(i)
                sb(2*i) = base(i)
                sx(2*i + 1) = edges(i + 1)
                sy(2*i + 1) = base(i) + counts(i)
                sb(2*i + 1) = base(i)
            end do
            sx(2*nb + 2) = edges(nb + 1)
            sy(2*nb + 2) = base(nb)
            sb(2*nb + 2) = base(nb)
            if (trim(ht) == "stepfilled") then
                if (horiz) then
                    call fill_betweenx(sx, sy, sb, color, label, alpha)
                else
                    call fill_between(sx, sy, sb, color, label, alpha)
                end if
            else if (horiz) then
                is = new_shape_series(SERIES_LINE, sy, sx, color, label, alpha)
            else
                is = new_shape_series(SERIES_LINE, sx, sy, color, label, alpha)
            end if
        case default
            ! Horizontal bars hold the bin position in x and the count in y
            ! exactly as barh does, so nothing below has to know the
            ! difference.
            is = new_shape_series(merge(SERIES_BARH, SERIES_BAR, horiz), &
                                  centers, counts, color, label, alpha)
            if (is >= 1) then
                ! Histogram bars touch, so a contrasting edge would show up
                ! as a seam between them. Bars narrowed by rwidth stand
                ! apart, and then there is no seam to hide.
                ax(cur_i)%series(is)%edgecolor = ax(cur_i)%series(is)%color
                if (present(rwidth)) then
                    if (rwidth < 1.0_dp) ax(cur_i)%series(is)%edgewidth = 0.0_dp
                end if
                ax(cur_i)%series(is)%width = widths(1)
                allocate (ax(cur_i)%series(is)%bwidth(nb))
                ax(cur_i)%series(is)%bwidth = widths
                if (stk) then
                    allocate (ax(cur_i)%series(is)%y2(nb))
                    ax(cur_i)%series(is)%y2 = base
                end if
            end if
        end select
    end subroutine hist

    ! The bin of v, with the top edge belonging to the last bin.
    pure function bin_of(v, edges, nb) result(k)
        real(dp), intent(in) :: v, edges(:)
        integer, intent(in) :: nb
        integer :: k
        do k = 1, nb - 1
            if (v < edges(k + 1)) return
        end do
        k = nb
    end function bin_of

    ! Shade between y1 and y2 (default 0).
    ! where selects the x range to shade. matplotlib fills each run of true
    ! values as its own polygon, and so does this.
    subroutine fill_between(x, y1, y2, color, label, alpha, where, hatch, edgecolor, &
                            interpolate)
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:), interpolate
        call fill_core(.false., x, y1, y2, color, label, alpha, where, hatch, edgecolor, &
                       interpolate)
    end subroutine fill_between

    ! The same band, but between two curves in x, run along y. The stored
    ! series carries the independent coordinate in x and the two edges in
    ! y and y2 exactly as fill_between does; only `horiz` says which way
    ! round the limits and the polygon are read.
    subroutine fill_betweenx(y, x1, x2, color, label, alpha, where, hatch, edgecolor, &
                             interpolate)
        real(dp), intent(in) :: y(:), x1(:)
        real(dp), intent(in), optional :: x2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:), interpolate
        call fill_core(.true., y, x1, x2, color, label, alpha, where, hatch, edgecolor, &
                       interpolate)
    end subroutine fill_betweenx

    subroutine fill_core(horiz, x, y1, y2, color, label, alpha, where, hatch, edgecolor, &
                         interpolate)
        logical, intent(in) :: horiz
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        logical, intent(in), optional :: where(:), interpolate
        integer :: n, i, j, k, m
        character(len=7) :: col
        logical :: first, interp
        real(dp), allocatable :: xr(:), ar(:), br(:)

        call ensure_fig()
        n = min(size(x), size(y1))
        if (n < 1) return
        if (.not. present(where)) then
            call add_fill(horiz, x(1:n), y1(1:n), y2, color, label, alpha, hatch, edgecolor)
            return
        end if

        interp = .false.
        if (present(interpolate)) interp = interpolate .and. present(y2)

        ! Every run after the first reuses the color of the first, and only
        ! the first carries the label, so the group is one legend entry.
        first = .true.
        col = ""
        i = 1
        do while (i <= n)
            if (.not. where(i)) then
                i = i + 1
                cycle
            end if
            j = i
            do while (j < n)
                if (.not. where(j + 1)) exit
                j = j + 1
            end do
            if (allocated(xr)) deallocate (xr)
            if (allocated(ar)) deallocate (ar)
            if (allocated(br)) deallocate (br)
            if (interp) then
                call interp_run(x, y1, y2, i, j, n, xr, ar, br, m)
            else
                m = j - i + 1
                allocate (xr(m), ar(m), br(m))
                xr = x(i:j)
                ar = y1(i:j)
                br = 0.0_dp
                if (present(y2)) then
                    if (size(y2) >= j) br = y2(i:j)
                end if
            end if
            if (first) then
                call add_fill(horiz, xr(1:m), ar(1:m), br(1:m), color, label, alpha, hatch, edgecolor)
            else
                call add_fill(horiz, xr(1:m), ar(1:m), br(1:m), col, alpha=alpha, &
                              hatch=hatch, edgecolor=edgecolor)
            end if
            k = ax(cur_i)%n_series
            if (first .and. k >= 1) then
                ! The runs are one artist, so they take one cycle step and
                ! one legend entry between them.
                col = ax(cur_i)%series(k)%color
                first = .false.
            end if
            i = j + 1
        end do
    end subroutine fill_core

    ! matplotlib's interpolate=: the shaded run is carried out to the point
    ! where the two curves actually cross, instead of stopping at the last
    ! sample on the near side of the crossing. The crossing is found by
    ! linear interpolation of y1 - y2 across the straddling interval, which
    ! is what matplotlib does too.
    subroutine interp_run(x, y1, y2, i, j, n, xr, ar, br, m)
        real(dp), intent(in) :: x(:), y1(:), y2(:)
        integer, intent(in) :: i, j, n
        real(dp), allocatable, intent(out) :: xr(:), ar(:), br(:)
        integer, intent(out) :: m
        integer :: k
        real(dp) :: t, xc, yc
        logical :: pre, post

        pre = i > 1
        post = j < n
        m = (j - i + 1)
        allocate (xr(m + 2), ar(m + 2), br(m + 2))
        m = 0
        if (pre) then
            call crossing(x, y1, y2, i - 1, i, t, xc, yc)
            if (t >= 0.0_dp) then
                m = 1
                xr(1) = xc
                ar(1) = yc
                br(1) = yc
            end if
        end if
        do k = i, j
            m = m + 1
            xr(m) = x(k)
            ar(m) = y1(k)
            br(m) = y2(k)
        end do
        if (post) then
            call crossing(x, y1, y2, j, j + 1, t, xc, yc)
            if (t >= 0.0_dp) then
                m = m + 1
                xr(m) = xc
                ar(m) = yc
                br(m) = yc
            end if
        end if
    end subroutine interp_run

    ! Where y1 - y2 changes sign between samples p and q. t < 0 says the
    ! two never meet there and the caller should leave the run as it is.
    subroutine crossing(x, y1, y2, p, q, t, xc, yc)
        real(dp), intent(in) :: x(:), y1(:), y2(:)
        integer, intent(in) :: p, q
        real(dp), intent(out) :: t, xc, yc
        real(dp) :: d0, d1

        d0 = y1(p) - y2(p)
        d1 = y1(q) - y2(q)
        t = -1.0_dp
        xc = 0.0_dp
        yc = 0.0_dp
        if (abs(d0 - d1) <= 0.0_dp) return
        t = d0 / (d0 - d1)
        if (t < 0.0_dp .or. t > 1.0_dp) then
            t = -1.0_dp
            return
        end if
        xc = x(p) + t * (x(q) - x(p))
        yc = y1(p) + t * (y1(q) - y1(p))
    end subroutine crossing

    ! matplotlib's stackplot: every row of y is one layer, drawn as a band
    ! from the sum of the layers below it to that sum plus its own values.
    subroutine stackplot(x, y, labels, colors, alpha)
        real(dp), intent(in) :: x(:), y(:, :)
        character(len=*), intent(in), optional :: labels(:), colors(:)
        real(dp), intent(in), optional :: alpha
        real(dp), allocatable :: lo(:), hi(:)
        integer :: n, nl, k
        logical :: has_c, has_l

        call ensure_fig()
        n = min(size(x), size(y, 2))
        nl = size(y, 1)
        if (n < 2 .or. nl < 1) return
        allocate (lo(n), hi(n))
        lo = 0.0_dp
        do k = 1, nl
            hi = lo + y(k, 1:n)
            has_c = .false.
            has_l = .false.
            if (present(colors)) has_c = k <= size(colors)
            if (present(labels)) has_l = k <= size(labels)
            if (has_c .and. has_l) then
                call add_fill(.false., x(1:n), hi, lo, colors(k), labels(k), alpha)
            else if (has_c) then
                call add_fill(.false., x(1:n), hi, lo, colors(k), alpha=alpha)
            else if (has_l) then
                call add_fill(.false., x(1:n), hi, lo, label=labels(k), alpha=alpha)
            else
                call add_fill(.false., x(1:n), hi, lo, alpha=alpha)
            end if
            lo = hi
        end do
    end subroutine stackplot

    ! y2(i:j) if it is there at all, which keeps the optional optional.
    function slice(v, i, j) result(w)
        real(dp), intent(in), optional :: v(:)
        integer, intent(in) :: i, j
        real(dp), allocatable :: w(:)
        if (present(v)) then
            allocate (w(max(0, j - i + 1)))
            w = v(i:j)
        else
            allocate (w(0))
        end if
    end function slice

    subroutine add_fill(horiz, x, y1, y2, color, label, alpha, hatch, edgecolor)
        logical, intent(in) :: horiz
        real(dp), intent(in) :: x(:), y1(:)
        real(dp), intent(in), optional :: y2(:)
        character(len=*), intent(in), optional :: color, label, hatch, edgecolor
        real(dp), intent(in), optional :: alpha
        integer :: is, n

        is = new_shape_series(SERIES_FILL, x, y1, color, label, alpha)
        if (is < 1) return
        ax(cur_i)%series(is)%horiz = horiz
        if (present(hatch)) ax(cur_i)%series(is)%hatch = hatch
        if (present(edgecolor)) ax(cur_i)%series(is)%hcolor = resolve_color(edgecolor)
        n = ax(cur_i)%series(is)%n
        allocate (ax(cur_i)%series(is)%y2(n))
        ax(cur_i)%series(is)%y2(1:n) = 0.0_dp
        if (present(y2)) then
            if (size(y2) >= n) ax(cur_i)%series(is)%y2(1:n) = y2(1:n)
        end if
    end subroutine add_fill

    ! Line plot with symmetric vertical error bars.
    ! yerr/xerr are symmetric; the _lo/_hi pairs give an asymmetric error,
    ! matplotlib's 2xN array written as two arrays.
    subroutine errorbar(x, y, yerr, fmt, color, label, capsize, marker, &
                        xerr, yerr_lo, yerr_hi, xerr_lo, xerr_hi, &
                        ecolor, elinewidth, lw, alpha)
        real(dp), intent(in) :: x(:), y(:)
        real(dp), intent(in), optional :: yerr(:), xerr(:)
        real(dp), intent(in), optional :: yerr_lo(:), yerr_hi(:)
        real(dp), intent(in), optional :: xerr_lo(:), xerr_hi(:)
        character(len=*), intent(in), optional :: fmt, color, label, marker
        character(len=*), intent(in), optional :: ecolor
        real(dp), intent(in), optional :: capsize, elinewidth, lw, alpha
        integer :: is, n, mk, ls
        character(len=7) :: col

        call ensure_fig()
        is = new_shape_series(SERIES_ERRORBAR, x, y, color, label, alpha)
        if (is < 1) return
        n = ax(cur_i)%series(is)%n
        call set_arm(ax(cur_i)%series(is)%eylo, n, yerr, yerr_lo)
        call set_arm(ax(cur_i)%series(is)%eyhi, n, yerr, yerr_hi)
        call set_arm(ax(cur_i)%series(is)%exlo, n, xerr, xerr_lo)
        call set_arm(ax(cur_i)%series(is)%exhi, n, xerr, xerr_hi)

        if (present(fmt)) then
            if (len_trim(fmt) > 0) then
                call parse_fmt(trim(fmt), col, mk, ls)
                ax(cur_i)%series(is)%marker = mk
                ax(cur_i)%series(is)%linestyle = ls
                if (len_trim(col) > 0) ax(cur_i)%series(is)%color = col
            end if
        end if
        if (present(marker)) ax(cur_i)%series(is)%marker = marker_from_char(marker(1:1))
        ! width doubles as the cap half-width, in points.
        ax(cur_i)%series(is)%width = 3.0_dp
        if (present(capsize)) ax(cur_i)%series(is)%width = capsize
        if (present(ecolor)) ax(cur_i)%series(is)%ecolor = resolve_color(ecolor)
        if (present(elinewidth)) ax(cur_i)%series(is)%elw = elinewidth
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine errorbar

    ! One arm of the error bars: the asymmetric value if it was given, else
    ! the symmetric one, else nothing at all.
    subroutine set_arm(arm, n, sym, side)
        real(dp), allocatable, intent(out) :: arm(:)
        integer, intent(in) :: n
        real(dp), intent(in), optional :: sym(:), side(:)
        if (present(side)) then
            allocate (arm(n))
            arm = abs(side(1:n))
        else if (present(sym)) then
            allocate (arm(n))
            arm = abs(sym(1:n))
        end if
    end subroutine set_arm

    ! Reference line spanning the full axes.
    subroutine axhline(y, color, linestyle, lw, label)
        real(dp), intent(in) :: y
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_ref_line(SERIES_HLINE, y, color, linestyle, lw, label)
    end subroutine axhline

    subroutine axvline(x, color, linestyle, lw, label)
        real(dp), intent(in) :: x
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_ref_line(SERIES_VLINE, x, color, linestyle, lw, label)
    end subroutine axvline

    ! An endless line through xy1, either through xy2 or with the given
    ! slope. Like matplotlib's axline it is drawn to the edges of the axes
    ! and does not stretch them. slope is meaningless on a log axis, and
    ! matplotlib refuses it there; here it is simply taken literally.
    subroutine axline(xy1, xy2, slope, color, linestyle, lw, label)
        real(dp), intent(in) :: xy1(2)
        real(dp), intent(in), optional :: xy2(2), slope
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        real(dp) :: p2(2)
        integer :: is

        call ensure_fig()
        if (present(xy2)) then
            p2 = xy2
        else if (present(slope)) then
            p2 = [xy1(1) + 1.0_dp, xy1(2) + slope]
        else
            return
        end if
        is = new_shape_series(SERIES_AXLINE, [xy1(1), p2(1)], [xy1(2), p2(2)], &
                              color, label)
        if (is < 1) return
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(linestyle)) ax(cur_i)%series(is)%linestyle = linestyle_from_str(linestyle)
        ax(cur_i)%series(is)%linewidth = 1.5_dp
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine axline

    subroutine add_ref_line(kd, v, color, linestyle, lw, label)
        integer, intent(in) :: kd
        real(dp), intent(in) :: v
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        integer :: is

        call ensure_fig()
        is = new_shape_series(kd, [v], [v], color, label)
        if (is < 1) return
        ! Reference lines default to black, not to the color cycle.
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(linestyle)) ax(cur_i)%series(is)%linestyle = linestyle_from_str(linestyle)
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine add_ref_line

    ! A run of horizontal lines, each from xmin to xmax in data coordinates.
    subroutine hlines(y, xmin, xmax, color, linestyle, lw, label)
        real(dp), intent(in) :: y(:), xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_lines(SERIES_HLINES, y, xmin, xmax, color, linestyle, lw, label)
    end subroutine hlines

    subroutine vlines(x, ymin, ymax, color, linestyle, lw, label)
        real(dp), intent(in) :: x(:), ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        call add_lines(SERIES_VLINES, x, ymin, ymax, color, linestyle, lw, label)
    end subroutine vlines

    subroutine add_lines(kd, v, lo, hi, color, linestyle, lw, label)
        integer, intent(in) :: kd
        real(dp), intent(in) :: v(:), lo, hi
        character(len=*), intent(in), optional :: color, linestyle, label
        real(dp), intent(in), optional :: lw
        integer :: is, n

        call ensure_fig()
        n = size(v)
        if (n < 1) return
        is = new_shape_series(kd, spread(lo, 1, n), v, color, label)
        if (is < 1) return
        allocate (ax(cur_i)%series(is)%y2(n))
        ax(cur_i)%series(is)%y2 = hi
        if (kd == SERIES_VLINES) then
            ! x carries the positions and y the two ends for a vertical run.
            ax(cur_i)%series(is)%x = v
            ax(cur_i)%series(is)%y = lo
        end if
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = "#000000"
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
        if (present(linestyle)) ax(cur_i)%series(is)%linestyle = linestyle_from_str(linestyle)
        if (present(lw)) ax(cur_i)%series(is)%linewidth = lw
    end subroutine add_lines

    ! A shaded band. The span runs the full width (height) of the axes unless
    ! the cross-axis limits are given, and those are axes fractions, not data,
    ! exactly as in matplotlib.
    subroutine axhspan(ymin, ymax, xmin, xmax, color, alpha, label)
        real(dp), intent(in) :: ymin, ymax
        real(dp), intent(in), optional :: xmin, xmax, alpha
        character(len=*), intent(in), optional :: color, label
        call add_span(SERIES_HSPAN, ymin, ymax, xmin, xmax, color, alpha, label)
    end subroutine axhspan

    subroutine axvspan(xmin, xmax, ymin, ymax, color, alpha, label)
        real(dp), intent(in) :: xmin, xmax
        real(dp), intent(in), optional :: ymin, ymax, alpha
        character(len=*), intent(in), optional :: color, label
        call add_span(SERIES_VSPAN, xmin, xmax, ymin, ymax, color, alpha, label)
    end subroutine axvspan

    subroutine add_span(kd, lo, hi, flo, fhi, color, alpha, label)
        integer, intent(in) :: kd
        real(dp), intent(in) :: lo, hi
        real(dp), intent(in), optional :: flo, fhi, alpha
        character(len=*), intent(in), optional :: color, label
        real(dp) :: f0, f1
        integer :: is

        call ensure_fig()
        f0 = 0.0_dp
        f1 = 1.0_dp
        if (present(flo)) f0 = flo
        if (present(fhi)) f1 = fhi
        if (kd == SERIES_HSPAN) then
            is = new_shape_series(kd, [f0, f1], [lo, hi], color, label, alpha)
        else
            is = new_shape_series(kd, [lo, hi], [f0, f1], color, label, alpha)
        end if
        if (is < 1) return
        ! A patch takes matplotlib's default patch color rather than the next
        ! color of the line cycle.
        if (.not. present(color)) then
            ax(cur_i)%series(is)%color = cycle_color(0)
            ax(cur_i)%color_cycle = ax(cur_i)%color_cycle - 1
        end if
    end subroutine add_span

    pure function linestyle_from_str(s) result(ls)
        character(len=*), intent(in) :: s
        integer :: ls
        select case (trim(s))
        case ("-"); ls = LINE_SOLID
        case ("--"); ls = LINE_DASHED
        case (":"); ls = LINE_DOTTED
        case ("-."); ls = LINE_DASHDOT
        case ("None", "none", ""); ls = LINE_NONE
        case default; ls = LINE_SOLID
        end select
    end function linestyle_from_str

    ! One number out of a matplotlib style string, as in "arc3,rad=0.3"
    ! or "round,pad=0.5". The default comes back when the key is absent.
    function style_number(spec, key, dflt) result(v)
        character(len=*), intent(in) :: spec, key
        real(dp), intent(in) :: dflt
        real(dp) :: v
        integer :: i, j, ios

        v = dflt
        i = index(spec, trim(key)//"=")
        if (i == 0) return
        i = i + len_trim(key) + 1
        j = index(spec(i:), ",")
        if (j == 0) then
            j = len_trim(spec)
        else
            j = i + j - 2
        end if
        if (j < i) return
        read (spec(i:j), *, iostat=ios) v
        if (ios /= 0) v = dflt
    end function style_number

    ! Text at a point in data coordinates.
    subroutine text(x, y, s, color, fontsize, ha, fontweight, fontstyle, &
                    va, rotation, bbox_facecolor, bbox_edgecolor, bbox_alpha, &
                    bbox_pad, transform, boxstyle)
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va, bbox_facecolor, bbox_edgecolor
        character(len=*), intent(in), optional :: transform, boxstyle
        real(dp), intent(in), optional :: fontsize, rotation, bbox_alpha, bbox_pad
        call add_text(x, y, s, color, fontsize, ha, .false., 0.0_dp, 0.0_dp, &
                      fontweight, fontstyle, va, rotation, bbox_facecolor, &
                      bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                      boxstyle=boxstyle)
    end subroutine text

    ! The same, but placed in figure coordinates, so that a note can sit
    ! anywhere on the canvas rather than inside one axes.
    subroutine figtext(x, y, s, color, fontsize, ha, fontweight, fontstyle, &
                       va, rotation)
        real(dp), intent(in) :: x, y
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va
        real(dp), intent(in), optional :: fontsize, rotation
        integer :: it
        call add_text(x, y, s, color, fontsize, ha, .false., 0.0_dp, 0.0_dp, &
                      fontweight, fontstyle, va, rotation)
        it = ax(cur_i)%n_texts
        ax(cur_i)%texts(it)%in_fig = .true.
    end subroutine figtext

    ! Text at (xtext, ytext) with an arrow pointing at (x, y).
    subroutine annotate(s, x, y, xtext, ytext, color, fontsize, ha, &
                        fontweight, fontstyle, va, rotation, bbox_facecolor, &
                        bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                        arrowstyle, arrowcolor, arrowlw, shrink, &
                        connectionstyle, boxstyle)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: x, y
        real(dp), intent(in), optional :: xtext, ytext, fontsize
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va, bbox_facecolor, bbox_edgecolor
        character(len=*), intent(in), optional :: transform, arrowstyle, arrowcolor
        character(len=*), intent(in), optional :: connectionstyle, boxstyle
        real(dp), intent(in), optional :: rotation, bbox_alpha, bbox_pad
        real(dp), intent(in), optional :: arrowlw, shrink
        real(dp) :: xt, yt
        logical :: arrow

        xt = x
        yt = y
        ! Like matplotlib, the connector appears only when it is asked for.
        arrow = present(arrowstyle)
        if (present(xtext)) xt = xtext
        if (present(ytext)) yt = ytext
        ! The label sits at the text position; the arrow runs back to (x, y).
        call add_text(xt, yt, s, color, fontsize, ha, arrow, x, y, &
                      fontweight, fontstyle, va, rotation, bbox_facecolor, &
                      bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                      arrowstyle, arrowcolor, arrowlw, shrink, &
                      connectionstyle, boxstyle)
    end subroutine annotate

    subroutine add_text(x, y, s, color, fontsize, ha, arrow, xarr, yarr, &
                        fontweight, fontstyle, va, rotation, bbox_facecolor, &
                        bbox_edgecolor, bbox_alpha, bbox_pad, transform, &
                        arrowstyle, arrowcolor, arrowlw, shrink, &
                        connectionstyle, boxstyle)
        real(dp), intent(in) :: x, y, xarr, yarr
        character(len=*), intent(in) :: s
        character(len=*), intent(in), optional :: color, ha, fontweight, fontstyle
        character(len=*), intent(in), optional :: va, bbox_facecolor, bbox_edgecolor
        character(len=*), intent(in), optional :: transform, arrowstyle, arrowcolor
        character(len=*), intent(in), optional :: connectionstyle, boxstyle
        real(dp), intent(in), optional :: fontsize, rotation, bbox_alpha, bbox_pad
        real(dp), intent(in), optional :: arrowlw, shrink
        logical, intent(in) :: arrow
        integer :: it
        character(len=7) :: col

        call ensure_fig()
        call push_text(ax(cur_i), it)
        ax(cur_i)%texts(it)%x = x
        ax(cur_i)%texts(it)%y = y
        ax(cur_i)%texts(it)%s = s
        ax(cur_i)%texts(it)%has_arrow = arrow
        if (present(fontweight)) &
            ax(cur_i)%texts(it)%weight = weight_from_str(fontweight)
        if (present(fontstyle)) &
            ax(cur_i)%texts(it)%slant = slant_from_str(fontstyle)
        ax(cur_i)%texts(it)%xtail = xarr
        ax(cur_i)%texts(it)%ytail = yarr
        ax(cur_i)%texts(it)%fontsize = 10.0_dp
        ax(cur_i)%texts(it)%ha = "left"
        ax(cur_i)%texts(it)%color = rc_text_color
        if (present(fontsize)) ax(cur_i)%texts(it)%fontsize = fontsize
        if (present(ha)) ax(cur_i)%texts(it)%ha = ha
        if (present(va)) ax(cur_i)%texts(it)%va = va
        if (present(rotation)) ax(cur_i)%texts(it)%rot = rotation
        if (present(bbox_facecolor)) then
            ax(cur_i)%texts(it)%has_box = .true.
            ax(cur_i)%texts(it)%box_fc = resolve_color(bbox_facecolor)
        end if
        if (present(bbox_edgecolor)) then
            ax(cur_i)%texts(it)%has_box = .true.
            ax(cur_i)%texts(it)%box_ec = resolve_color(bbox_edgecolor)
        end if
        if (present(bbox_alpha)) ax(cur_i)%texts(it)%box_alpha = bbox_alpha
        if (present(bbox_pad)) ax(cur_i)%texts(it)%box_pad = bbox_pad
        if (present(boxstyle)) then
            ax(cur_i)%texts(it)%has_box = .true.
            ax(cur_i)%texts(it)%box_style = boxstyle
            ax(cur_i)%texts(it)%box_pad = style_number(boxstyle, "pad", &
                                                       ax(cur_i)%texts(it)%box_pad)
        end if
        if (present(connectionstyle)) &
            ax(cur_i)%texts(it)%arc_rad = style_number(connectionstyle, "rad", 0.0_dp)
        col = resolve_color(color)
        if (len_trim(col) > 0) ax(cur_i)%texts(it)%color = col
        if (present(arrowstyle)) &
            ax(cur_i)%texts(it)%arrow_head = index(arrowstyle, ">") > 0
        if (present(arrowcolor)) &
            ax(cur_i)%texts(it)%arrow_color = resolve_color(arrowcolor)
        if (present(arrowlw)) ax(cur_i)%texts(it)%arrow_lw = arrowlw
        if (present(shrink)) ax(cur_i)%texts(it)%arrow_shrink = shrink
        if (present(transform)) then
            select case (transform)
            case ("axes"); ax(cur_i)%texts(it)%in_axes = .true.
            case ("figure"); ax(cur_i)%texts(it)%in_fig = .true.
            end select
        end if
    end subroutine add_text

    subroutine add_series(ia, x, y, fmt, label, lw, color, marker, linestyle, alpha)
        integer, intent(in) :: ia
        real(dp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: fmt, label, color, marker, linestyle
        real(dp), intent(in), optional :: lw, alpha
        integer :: n, is, m, ls
        real(dp) :: ca
        character(len=7) :: col
        character(len=32) :: f
        logical :: have_fmt

        n = min(size(x), size(y))
        if (n <= 0) return
        call push_series(ax(ia), is)

        allocate (ax(ia)%series(is)%x(n), ax(ia)%series(is)%y(n))
        ax(ia)%series(is)%x(1:n) = x(1:n)
        ax(ia)%series(is)%y(1:n) = y(1:n)
        ax(ia)%series(is)%n = n
        ax(ia)%series(is)%linewidth = rc_lw
        ax(ia)%series(is)%markersize = default_markersize
        ax(ia)%series(is)%label = ""
        ax(ia)%series(is)%marker = MARKER_NONE
        ax(ia)%series(is)%linestyle = LINE_SOLID
        ax(ia)%series(is)%color = ""

        have_fmt = .false.
        f = ""
        if (present(fmt)) then
            if (len_trim(fmt) > 0) then
                f = fmt
                have_fmt = .true.
            end if
        end if

        col = ""
        m = MARKER_NONE
        ls = LINE_NONE
        if (have_fmt) then
            call parse_fmt(trim(f), col, m, ls)
            ax(ia)%series(is)%marker = m
            if (ls == LINE_NONE .and. m == MARKER_NONE) then
                ax(ia)%series(is)%linestyle = LINE_SOLID
            else if (ls == LINE_NONE .and. m /= MARKER_NONE) then
                ax(ia)%series(is)%linestyle = LINE_NONE
            else
                ax(ia)%series(is)%linestyle = ls
            end if
            if (len_trim(col) > 0) ax(ia)%series(is)%color = col
        end if

        col = resolve_color(color, ca)
        if (len_trim(col) > 0) ax(ia)%series(is)%color = col
        ! An "#rrggbbaa" spelling carries its own alpha; an explicit alpha=
        ! argument still wins, matching matplotlib.
        if (ca >= 0.0_dp) ax(ia)%series(is)%alpha = ca

        if (present(marker)) then
            select case (trim(marker))
            case ("None", "none", ""); ax(ia)%series(is)%marker = MARKER_NONE
            case default
                ax(ia)%series(is)%marker = marker_from_char(marker(1:1))
            end select
        end if

        if (present(linestyle)) &
            ax(ia)%series(is)%linestyle = linestyle_from_str(linestyle)

        if (present(lw)) ax(ia)%series(is)%linewidth = lw
        if (present(alpha)) ax(ia)%series(is)%alpha = alpha
        if (present(label)) ax(ia)%series(is)%label = label

        if (len_trim(ax(ia)%series(is)%color) == 0) then
            ax(ia)%series(is)%color = cycle_color(ax(ia)%color_cycle)
            ax(ia)%color_cycle = ax(ia)%color_cycle + 1
        end if

        ! marker-only format string => no line
        if (have_fmt .and. ax(ia)%series(is)%marker /= MARKER_NONE) then
            if (index(f, "-") == 0 .and. index(f, ":") == 0) then
                ax(ia)%series(is)%linestyle = LINE_NONE
            end if
        end if
    end subroutine add_series

    function fmt_pt(v) result(t)
        real(dp), intent(in) :: v
        character(len=64) :: t
        integer :: n
        call fmt_num(v, t, n)
        t = t(1:n)
    end function fmt_pt

    ! Per-point color when scatter mapped c values, otherwise the series color.
    ! The knobs the nonlinear norms turn. A centred norm needs a centre;
    ! without one it would be the plain linear norm anyway.
    subroutine norm_params(vcenter, gamma, linthresh)
        real(dp), intent(in), optional :: vcenter, gamma, linthresh
        if (present(vcenter)) ax(cur_i)%img_vcenter = vcenter
        if (present(gamma)) ax(cur_i)%img_gamma = gamma
        if (present(linthresh)) ax(cur_i)%img_linthresh = linthresh
    end subroutine norm_params

    ! matplotlib's Colormap.set_bad / set_under / set_over, and
    ! LinearSegmentedColormap.from_list, applied to the current axes.
    subroutine set_bad(color)
        character(len=*), intent(in) :: color
        call ensure_fig()
        ax(cur_i)%cmap_bad = resolve_color(color)
    end subroutine set_bad

    subroutine set_under(color)
        character(len=*), intent(in) :: color
        call ensure_fig()
        ax(cur_i)%cmap_under = resolve_color(color)
    end subroutine set_under

    subroutine set_over(color)
        character(len=*), intent(in) :: color
        call ensure_fig()
        ax(cur_i)%cmap_over = resolve_color(color)
    end subroutine set_over

    subroutine set_cmap_colors(colors)
        character(len=*), intent(in) :: colors(:)
        integer :: i
        call ensure_fig()
        if (allocated(ax(cur_i)%cmap_list)) deallocate (ax(cur_i)%cmap_list)
        if (size(colors) < 1) return
        allocate (ax(cur_i)%cmap_list(size(colors)))
        do i = 1, size(colors)
            ax(cur_i)%cmap_list(i) = resolve_color(colors(i))
        end do
    end subroutine set_cmap_colors

    pure function norm_from_str(s) result(k)
        character(len=*), intent(in) :: s
        integer :: k
        select case (trim(s))
        case ("log"); k = NORM_LOG
        case ("centered", "twoslope"); k = NORM_CENTER
        case ("power"); k = NORM_POWER
        case ("symlog"); k = NORM_SYMLOG
        case default; k = NORM_LINEAR
        end select
    end function norm_from_str

    ! ------------------------------------------------------------------
    ! A 3D axes. The camera lives in fplot_proj3d; what is here is what
    ! the camera is pointed at: the limits, the three back panes and their
    ! grids, the three axis lines with ticks and labels, and the data.
    !
    ! There is no depth buffer, only the painter's algorithm: faces are
    ! sorted back to front and drawn in that order. mplot3d does the same
    ! and has the same limitation, that surfaces which intersect one
    ! another come out wrong.
    ! ------------------------------------------------------------------

    ! The extension picks the backend, as it does in matplotlib. A name with
    ! no extension at all is taken as SVG.
    function file_ext(filename) result(ext)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: ext
        integer :: d, sl

        d = index(filename, ".", back=.true.)
        sl = max(index(filename, "/", back=.true.), &
                 index(filename, achar(92), back=.true.))
        if (d <= sl + 1) then
            ext = "svg"
        else
            ext = lower(filename(d + 1:len_trim(filename)))
        end if
    end function file_ext

    ! Writing SVG bytes into a .png would produce a file no viewer can open,
    ! so an unsupported extension is a hard error rather than a silent default.
    subroutine reject_ext(filename)
        character(len=*), intent(in) :: filename
        print *, "fplot: cannot write ", trim(filename)
        print *, "fplot: supported formats are .svg, .pdf, .eps and .png, not ." &
            //file_ext(filename)
        error stop "fplot: unsupported savefig format"
    end subroutine reject_ext

    ! dpi sizes the raster in the PNG backend. The vector formats ignore it,
    ! as matplotlib does: they emit the same inches*72 canvas at any dpi.
    subroutine savefig(filename, transparent, facecolor, dpi, bbox_inches, pad_inches)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        real(dp), intent(in), optional :: dpi, pad_inches
        character(len=:), allocatable :: svg
        real(dp) :: saved_dpi

        ! dpi given here applies to this file only, as it does in matplotlib;
        ! the figure's own dpi is what figure(dpi=) set and outlives the call.
        saved_dpi = fig_dpi
        if (present(dpi)) then
            if (dpi <= 0.0_dp) error stop "fplot: savefig dpi must be positive"
            fig_dpi = dpi
        end if
        select case (file_ext(filename))
        case ("svg")
            svg = render_svg(facecolor, transparent, bbox_inches, pad_inches)
        case ("pdf")
            svg = render_pdf(facecolor, transparent, bbox_inches, pad_inches)
        case ("png")
            svg = render_png(facecolor, transparent, bbox_inches, pad_inches)
        case ("eps")
            svg = render_eps(facecolor, transparent, bbox_inches, pad_inches)
        case default
            call reject_ext(filename)
            return
        end select
        fig_dpi = saved_dpi
        call write_bytes(filename, svg)
    end subroutine savefig

    subroutine write_bytes(filename, data)
        character(len=*), intent(in) :: filename, data
        integer :: u, ios

        open (newunit=u, file=trim(filename), status="replace", action="write", &
              form="unformatted", access="stream", iostat=ios)
        if (ios /= 0) then
            ! fallback formatted
            open (newunit=u, file=trim(filename), status="replace", action="write", &
                  form="formatted", iostat=ios)
            if (ios /= 0) then
                print *, "fplot: failed to open ", trim(filename)
                return
            end if
            write (u, "(A)") data
            close (u)
            return
        end if
        if (len(data) > 0) write (u) data
        close (u)
    end subroutine write_bytes

    ! ------------------------------------------------------------------
    ! Animation. matplotlib builds an Animation object around a callback;
    ! Fortran has loops, so the loop stays in the caller and each pass adds
    ! the figure as it stands to the reel.
    ! ------------------------------------------------------------------

    subroutine add_frame(facecolor, dpi)
        character(len=*), intent(in), optional :: facecolor
        real(dp), intent(in), optional :: dpi
        type(png_renderer_t) :: r
        real(dp) :: saved_dpi

        saved_dpi = fig_dpi
        if (present(dpi)) fig_dpi = dpi
        r%keep_pixels = .true.
        call r%set_dpi(fig_dpi)
        call render_figure(r, facecolor)
        fig_dpi = saved_dpi

        if (anim_n > 0) then
            if (r%pw /= anim_w .or. r%ph /= anim_h) then
                print *, "fplot: add_frame ignored, frame size changed"
                return
            end if
        end if
        anim_w = r%pw
        anim_h = r%ph
        call anim_append(r%rgb)
        anim_n = anim_n + 1
    end subroutine add_frame

    subroutine anim_append(frame)
        character(len=*), intent(in) :: frame
        character(len=:), allocatable :: bigger
        integer :: cap, need

        need = anim_len + len(frame)
        cap = 0
        if (allocated(anim_pix)) cap = len(anim_pix)
        if (cap < need) then
            cap = max(2*cap, need)
            allocate (character(len=cap) :: bigger)
            if (anim_len > 0) bigger(1:anim_len) = anim_pix(1:anim_len)
            call move_alloc(bigger, anim_pix)
        end if
        anim_pix(anim_len + 1:need) = frame
        anim_len = need
    end subroutine anim_append

    ! Writes the frames collected so far and starts a new reel, so that a
    ! program can make several animations without a reset call.
    subroutine save_animation(filename, fps, loop)
        character(len=*), intent(in) :: filename
        real(dp), intent(in), optional :: fps
        logical, intent(in), optional :: loop
        real(dp) :: rate
        logical :: rep
        integer :: delay

        if (anim_n == 0) then
            print *, "fplot: no frames; call add_frame before save_animation"
            return
        end if
        if (file_ext(filename) /= "gif") then
            print *, "fplot: save_animation writes .gif, not .", file_ext(filename)
            return
        end if
        rate = 5.0_dp
        if (present(fps)) then
            if (fps <= 0.0_dp) error stop "fplot: save_animation fps must be positive"
            rate = fps
        end if
        rep = .true.
        if (present(loop)) rep = loop

        ! GIF counts delays in hundredths of a second and viewers treat 0 as
        ! "as fast as you like", so a frame never asks for less than one.
        delay = max(1, nint(100.0_dp/rate))
        call write_bytes(filename, gif_encode(anim_w, anim_h, anim_n, &
                                              anim_pix(1:anim_len), delay, rep))
        anim_n = 0
        anim_w = 0
        anim_h = 0
        anim_len = 0
        deallocate (anim_pix)
    end subroutine save_animation

    subroutine show()
        ! File backend. In LFortran Jupyter notebooks, prefer:
        !   use lfortran_display
        !   call display_data("image/svg+xml", render_svg())
        call savefig("fplot_show.svg")
        print *, "fplot: wrote fplot_show.svg"
        print *, "fplot: for Jupyter use display_data('image/svg+xml', render_svg())"
    end subroutine show

end module fplot
