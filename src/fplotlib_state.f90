! fplotlib_state - the types and the module state the rest of fplotlib works on.
!
! fplotlib is stateful, as pylab is: there is a current figure and a current
! axes, and every drawing call adds to them. What that state is made of
! lives here, so that the parts of the library that build it and the parts
! that draw it can be read apart from one another.
module fplotlib_state
    use fplotlib_colors
    use fplotlib_style
    use fplotlib_render
    use fplotlib_ticks
    use fplotlib_cmap
    use fplotlib_scale
    implicit none
    public

    ! How the ticks of an axis are written. FMT_AUTO is matplotlib's
    ! ScalarFormatter, which is what an axis does unless it is told
    ! otherwise; the rest are its named formatters.
    ! Most rows or columns a figure's grid can be given a ratio for.
    integer, parameter :: MAX_RATIO = 32

    integer, parameter :: FMT_AUTO = 0
    integer, parameter :: FMT_PERCENT = 1
    integer, parameter :: FMT_COMMA = 2
    integer, parameter :: FMT_FIXED = 3

    ! What one axis agreed to write on its ticks: how many decimals, and
    ! the offset and power of ten factored out of every label and written
    ! once at the end of the axis instead.
    type :: tickfmt_t
        integer :: dec = 0
        real(dp) :: off = 0.0_dp
        integer :: oom = 0
        integer :: style = FMT_AUTO
        real(dp) :: whole = 100.0_dp
    end type tickfmt_t

    ! Initial slot count for the per-axes series and text arrays; both grow
    ! on demand, so this is only the allocation granularity.
    integer, parameter :: INIT_SLOTS = 8
    integer, parameter :: MAX_MINOR = 256

    ! Colorbar layout, as fractions of the axes box before it was shrunk.
    integer, parameter :: CBAR_SLICES = 64

    real(dp), parameter :: FIG_W_DEFAULT = 6.4_dp
    real(dp), parameter :: FIG_H_DEFAULT = 4.8_dp
    real(dp), parameter :: DPI_DEFAULT = 100.0_dp

    ! Default figure margins (matplotlib rcParams), in figure fractions.
    ! Tick geometry and label size, matching matplotlib's rcParams.
    real(dp), parameter :: TICK_LEN = 3.5_dp
    real(dp), parameter :: TICK_FONT = 10.0_dp
    ! Minor ticks are shorter than majors by this factor.
    real(dp), parameter :: MINOR_FRAC = 2.0_dp / 3.5_dp
    integer, parameter :: SPINE_LEFT = 1, SPINE_RIGHT = 2
    integer, parameter :: SPINE_BOTTOM = 3, SPINE_TOP = 4

    real(dp), parameter :: MARGIN_LEFT = 0.125_dp
    real(dp), parameter :: MARGIN_RIGHT = 0.9_dp
    real(dp), parameter :: MARGIN_BOTTOM = 0.11_dp
    real(dp), parameter :: MARGIN_TOP = 0.88_dp
    ! Default subplot spacing (matplotlib rcParams), relative to axes size.
    real(dp), parameter :: WSPACE = 0.2_dp
    real(dp), parameter :: HSPACE = 0.2_dp
    ! Suptitle baseline (matplotlib figure.suptitle default y), in figure
    ! fractions measured from the bottom.
    real(dp), parameter :: SUPTITLE_Y = 0.98_dp
    ! Mean advance width of DejaVu Sans at the 10 pt legend font size, used
    ! to size the legend box to its labels.
    real(dp), parameter :: LEGEND_CHAR_W = 5.6_dp
    ! Points per inch, matplotlib's font sizes, and the width of one digit
    ! in DejaVu Sans as a fraction of the font size.
    real(dp), parameter :: PT_PER_IN = 72.0_dp
    real(dp), parameter :: TITLE_FONT = 12.0_dp
    real(dp), parameter :: SUPTITLE_FONT = 12.0_dp
    real(dp), parameter :: LABEL_FONT = 11.0_dp
    real(dp), parameter :: LEGEND_FONT = 10.0_dp
    real(dp), parameter :: DIGIT_W = 0.636_dp
    ! Height of a one-line label including its leading, in font sizes.
    real(dp), parameter :: LABEL_BOX = 1.45_dp
    ! matplotlib's axes.labelpad, the gap between the tick labels and the
    ! axis label.
    real(dp), parameter :: LABEL_PAD = 4.0_dp
    real(dp), parameter :: PI = 3.141592653589793_dp

    ! What a series draws. LINE covers plot/scatter/semilog*; the rest are
    ! the shape-based plot types.
    ! How a value is turned into a place on the colormap.
    integer, parameter :: NORM_LINEAR = 0
    integer, parameter :: NORM_LOG = 1
    integer, parameter :: NORM_CENTER = 2
    integer, parameter :: NORM_POWER = 3
    integer, parameter :: NORM_SYMLOG = 4

    integer, parameter :: SERIES_LINE = 0
    integer, parameter :: SERIES_BAR = 1
    integer, parameter :: SERIES_FILL = 2
    integer, parameter :: SERIES_ERRORBAR = 3
    integer, parameter :: SERIES_HLINE = 4
    integer, parameter :: SERIES_VLINE = 5
    integer, parameter :: SERIES_BARH = 6
    integer, parameter :: SERIES_STEM = 7
    integer, parameter :: SERIES_PIE = 8
    integer, parameter :: SERIES_BOX = 9
    integer, parameter :: SERIES_VIOLIN = 10
    ! HLINES/VLINES: y (x) holds the position of each line and x, y2 its two
    ! ends. HSPAN/VSPAN: two points, one pair in data coordinates and the
    ! other in axes fractions.
    integer, parameter :: SERIES_HLINES = 11
    integer, parameter :: SERIES_VLINES = 12
    integer, parameter :: SERIES_HSPAN = 13
    integer, parameter :: SERIES_VSPAN = 14
    ! PATCH: a closed ring of vertices, filled and outlined.
    integer, parameter :: SERIES_PATCH = 16

    ! matplotlib's default zorders for the artists fplotlib draws.
    real(dp), parameter :: Z_PATCH = 1.0_dp, Z_GRID = 1.5_dp, Z_LINE = 2.0_dp
    ! QUIVER: x, y hold the tails and qu, qv the vectors.
    integer, parameter :: SERIES_QUIVER = 15
    ! ARROWHEAD: x, y hold the tips and qu, qv the direction. Drawn at a
    ! fixed size in points, which is what streamplot wants.
    integer, parameter :: SERIES_ARROWHEAD = 17
    ! 3D: LINE3D and SCATTER3D carry a z alongside x and y; SURFACE carries
    ! a grid, drawn as one quadrilateral per cell.
    integer, parameter :: SERIES_LINE3D = 18
    integer, parameter :: SERIES_SCATTER3D = 19
    integer, parameter :: SERIES_SURFACE = 20
    ! AXLINE: an endless line through the two points in x(1:2), y(1:2). It
    ! is drawn to the edges of the axes and takes no part in the limits.
    integer, parameter :: SERIES_AXLINE = 21
    ! TRISURF: points in x, y and z joined by the triangles in tri.
    integer, parameter :: SERIES_TRISURF = 22
    ! BAR3D: boxes standing on (x, y, z), dx by dy wide and z2 - z tall.
    integer, parameter :: SERIES_BAR3D = 23
    ! The most points one streamline may have.
    integer, parameter :: MAX_STREAM_PTS = 20000

    ! Working state for streamplot: the field in grid coordinates, and the
    ! coarse mask that keeps the streamlines apart by refusing a second one
    ! through any cell.
    type :: stream_t
        integer :: nx = 0, ny = 0
        real(dp), allocatable :: u(:, :), v(:, :), sp(:, :)
        integer :: mnx = 30, mny = 30
        integer, allocatable :: mask(:, :)
        real(dp) :: g2mx = 1.0_dp, g2my = 1.0_dp
        real(dp) :: m2gx = 1.0_dp, m2gy = 1.0_dp
        integer :: cx = -1, cy = -1
        integer :: nclaim = 0
        integer, allocatable :: claim(:, :)
    end type stream_t

    type :: series_t
        integer :: kind = SERIES_LINE
        integer :: n = 0
        real(dp), allocatable :: x(:)
        real(dp), allocatable :: y(:)
        ! FILL: lower edge. BAR: the baseline it is stacked on.
        real(dp), allocatable :: y2(:)
        ! ERRORBAR: the four arms, each already a distance from the point.
        real(dp), allocatable :: eylo(:), eyhi(:), exlo(:), exhi(:)
        ! PATCH: whether the ring is filled at all. Its outline is drawn
        ! only when edgecolor names one, as in matplotlib, where a patch is
        ! filled and edgeless unless asked otherwise.
        logical :: patch_fill = .true.
        ! Where the artist sits in the stack. Negative means "whatever this
        ! kind of artist gets by default", which is what series_z works out.
        real(dp) :: zorder = -1.0_dp
        ! A patch drawn from an arbitrary path carries its own verbs; a
        ! plain polygon leaves this alone and every point is a line to.
        integer, allocatable :: pverb(:)
        ! Most patches never ask for room of their own; the ones a plotting
        ! call makes for itself, such as broken_barh, do.
        logical :: patch_scales = .false.
        ! A cell of a mesh, whose seam with its neighbour must not show.
        logical :: patch_seal = .false.
        ! FILL: set by fill_betweenx, where x holds the independent
        ! coordinate and y, y2 the two edges, all with the axes swapped.
        logical :: horiz = .false.
        ! Surfaces: a colormap over z rather than one flat color, and the
        ! wireframe form, which rules the grid instead of filling it.
        integer :: scmap = -1
        logical :: wire = .false.
        ! QUIVER: the vector at each point, and how it is drawn. A negative
        ! scale or width means matplotlib's autoscale.
        real(dp), allocatable :: qu(:), qv(:)
        real(dp) :: qscale = -1.0_dp
        real(dp) :: qwidth = -1.0_dp
        ! QUIVER: the key, one arrow of length qk_u drawn to the same scale,
        ! with a label beside it. qk_coord says where (qk_x, qk_y) is
        ! measured: "axes", "data", "figure" or "inches". An empty color
        ! is the quiver's own, an empty label color the text color.
        logical :: qk_on = .false.
        real(dp) :: qk_x = 0.0_dp, qk_y = 0.0_dp, qk_u = 1.0_dp
        real(dp) :: qk_angle = 0.0_dp, qk_sep = 0.1_dp, qk_size = 10.0_dp
        character(len=8) :: qk_coord = "axes"
        character(len=1) :: qk_pos = "N"
        character(len=7) :: qk_color = "", qk_lcolor = ""
        character(len=256) :: qk_label = ""
        ! BOX/VIOLIN: the position on the category axis, and how the box
        ! is drawn: across instead of up, waisted at the median, with the
        ! mean marked, and filled rather than left as an outline.
        real(dp) :: pos = 1.0_dp
        logical :: box_vert = .true.
        logical :: box_notch = .false.
        logical :: box_mean = .false.
        logical :: box_fill = .false.
        real(dp) :: whis = 1.5_dp
        ! PIE: how far each wedge is pushed out along its own mid angle,
        ! where the first wedge starts, and which way the wedges run.
        real(dp), allocatable :: pexp(:)
        real(dp) :: pie_start = 0.0_dp
        real(dp) :: pie_radius = 1.0_dp
        logical :: pie_ccw = .true.
        character(len=7) :: color = "#1f77b4"
        integer :: marker = MARKER_NONE
        integer :: linestyle = LINE_SOLID
        real(dp) :: linewidth = 1.5_dp
        real(dp) :: markersize = 6.0_dp
        ! Marker colours, empty meaning "the colour of the line", and a
        ! stride so that only every n-th point carries a marker.
        character(len=7) :: mfc = "", mec = ""
        real(dp) :: mew = -1.0_dp
        integer :: markevery = 1
        ! A dash pattern of the caller's own, in points, which overrides
        ! the one the line style would give.
        integer :: n_dash = 0
        real(dp) :: dashes(4) = 0.0_dp
        ! Per-point overrides used by scatter; unallocated means uniform.
        real(dp), allocatable :: psize(:)
        character(len=7), allocatable :: pcolor(:)
        real(dp) :: width = 0.8_dp
        ! Per-bar width, for a histogram with uneven bins.
        real(dp), allocatable :: bwidth(:)
        real(dp) :: alpha = 1.0_dp
        ! bar_label: written at draw time, when the bar top is known in
        ! points and the padding can be honoured exactly.
        logical :: bar_labels = .false.
        real(dp) :: bar_pad = 3.0_dp
        real(dp) :: bar_label_size = 10.0_dp
        character(len=16) :: bar_fmt = ""
        ! matplotlib's hatch: the lines ruled over the fill, and the colour
        ! they are ruled in, black unless an edge colour was named.
        character(len=8) :: hatch = ""
        character(len=7) :: hcolor = ""
        character(len=7) :: edgecolor = "#ffffff"
        real(dp) :: edgewidth = 0.5_dp
        ! Error bars carry their own colour and weight, apart from whatever
        ! the line or the bars are drawn in.
        character(len=7) :: ecolor = ""
        real(dp) :: elw = 1.5_dp, ecap = 0.0_dp
        ! 3D: the third coordinate of a line or scatter, and the grid of a
        ! surface, in the row = y, column = x order the 2D grids use.
        real(dp), allocatable :: z(:)
        real(dp), allocatable :: zg(:, :)
        integer, allocatable :: tri(:, :)
        ! 3D bars: the top of each box, and the footprint they all share.
        real(dp), allocatable :: z2(:)
        real(dp) :: d3x = 1.0_dp, d3y = 1.0_dp
        ! An arrow reaches past the data it belongs to, and mplot3d leaves
        ! the limits to the points the arrows are drawn at.
        logical :: nolim = .false.
        character(len=128) :: label = ""
    end type series_t

    type :: text_t
        real(dp) :: x = 0.0_dp, y = 0.0_dp
        ! Arrow tail; only used when has_arrow is set.
        real(dp) :: xtail = 0.0_dp, ytail = 0.0_dp
        logical :: has_arrow = .false.
        ! "->" adds a head at the annotated point; the shaft is pulled back
        ! from both ends by shrink points so it never touches either.
        logical :: arrow_head = .false.
        character(len=7) :: arrow_color = "#000000"
        real(dp) :: arrow_lw = 1.0_dp, arrow_shrink = 2.0_dp
        ! matplotlib's "arc3": the shaft bows out of the straight line by
        ! this fraction of its length. Zero leaves it straight.
        real(dp) :: arc_rad = 0.0_dp
        real(dp) :: fontsize = 10.0_dp
        character(len=7) :: color = "#000000"
        character(len=8) :: ha = "left"
        ! Empty means the older placement, which is neither matplotlib's
        ! "baseline" nor any of the others but is what every plot drawn so
        ! far expects.
        character(len=8) :: va = ""
        real(dp) :: rot = 0.0_dp
        integer :: weight = WEIGHT_NORMAL, slant = SLANT_ROMAN
        ! An optional box behind the text.
        logical :: has_box = .false.
        character(len=7) :: box_fc = "#ffffff", box_ec = ""
        real(dp) :: box_alpha = 1.0_dp, box_pad = 0.3_dp
        ! "square" or "round"; round corners have matplotlib's radius of
        ! one pad.
        character(len=8) :: box_style = "square"
        ! figtext places in figure coordinates rather than data ones;
        ! transform="axes" places in fractions of the axes box, so that a
        ! note stays put when the data range changes.
        logical :: in_fig = .false.
        logical :: in_axes = .false.
        character(len=256) :: s = ""
    end type text_t

    type :: axes_t
        integer :: n_series = 0
        type(series_t), allocatable :: series(:)
        integer :: n_texts = 0
        type(text_t), allocatable :: texts(:)
        character(len=256) :: title = ""
        character(len=256) :: xlabel = ""
        character(len=256) :: ylabel = ""
        logical :: grid_on = .false.
        ! Empty means the style sheet's axes colour.
        character(len=7) :: facecolor = ""
        real(dp) :: face_alpha = 1.0_dp
        ! Empty or negative means "whatever the style sheet says".
        character(len=8) :: grid_axis = "both"
        character(len=8) :: grid_which = "major"
        character(len=16) :: grid_color = ""
        integer :: grid_ls = -1
        real(dp) :: grid_lw = -1.0_dp
        real(dp) :: grid_alpha = -1.0_dp
        logical :: legend_on = .false.
        character(len=16) :: legend_loc = "best"
        logical :: minor_ticks = .false.
        ! User-specified tick positions and optional labels.
        integer :: n_xticks = 0, n_yticks = 0
        real(dp) :: xtick_pos(MAX_TICKS), ytick_pos(MAX_TICKS)
        ! Minor ticks placed by hand, and the ends the locator is told to
        ! leave alone ("lower", "upper" or "both").
        integer :: n_xminor = 0, n_yminor = 0
        real(dp) :: xminor_pos(MAX_TICKS), yminor_pos(MAX_TICKS)
        character(len=6) :: xtick_prune = "", ytick_prune = ""
        logical :: xtick_labeled = .false., ytick_labeled = .false.
        character(len=24) :: xtick_lab(MAX_TICKS), ytick_lab(MAX_TICKS)
        ! Image (imshow). One image per axes, as in normal matplotlib use.
        ! Contour set (contour / contourf).
        logical :: frame_off = .false.
        logical :: has_cont = .false.
        logical :: cont_filled = .false.
        ! clabel: a level written into the line itself, the line broken to
        ! make room for it.
        logical :: cont_labels = .false.
        real(dp) :: clab_size = 10.0_dp
        real(dp), allocatable :: cz(:, :)
        real(dp), allocatable :: clev(:)
        integer :: cont_cmap = CMAP_VIRIDIS
        real(dp) :: cont_ext(4) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp]
        ! A contour set autoscales tight, so the data fills the frame.
        logical :: tight_lim = .false.
        logical :: has_img = .false.
        ! imshow(interpolation=): "nearest", the default, hands the samples
        ! over as they are; "bilinear" resamples them at the size the image
        ! is drawn.
        logical :: img_bilinear = .false.
        ! Set by imshow, and by a scatter that maps c values, so that
        ! colorbar() has a range and colormap to draw.
        logical :: has_cmap_src = .false.
        real(dp), allocatable :: img(:, :)
        integer :: img_cmap = CMAP_VIRIDIS
        real(dp) :: img_vmin = 0.0_dp, img_vmax = 1.0_dp
        ! How a value is placed on the colormap: linearly, or by its
        ! logarithm, matplotlib's LogNorm.
        ! How values are mapped onto the colormap.
        integer :: img_norm = NORM_LINEAR
        real(dp) :: img_vcenter = 0.0_dp, img_gamma = 1.0_dp
        real(dp) :: img_linthresh = 1.0_dp
        ! BoundaryNorm: the edges of the bands the data is sorted into. The
        ! image then takes one flat color per band rather than a ramp.
        real(dp), allocatable :: img_bounds(:)
        real(dp) :: img_ext(4) = [0.0_dp, 1.0_dp, 0.0_dp, 1.0_dp]
        logical :: img_origin_upper = .true.
        ! Colours for the values a colormap has nothing to say about, and
        ! a colormap of the caller's own making.
        character(len=7) :: cmap_bad = "", cmap_under = "", cmap_over = ""
        character(len=7), allocatable :: cmap_list(:)
        ! An image whose colours are given outright rather than through a
        ! colormap: (row, column, channel) with three or four channels.
        real(dp), allocatable :: img_rgb(:, :, :)
        logical :: has_rgb = .false.
        ! pcolormesh keeps the same samples in img, but with its own cell
        ! edges instead of an evenly divided extent.
        ! Where the axes sits in the figure's grid, zero based, and how
        ! many cells it spans. A plain subplot spans one of each.
        integer :: g_row = 0, g_col = 0
        integer :: g_rowspan = 1, g_colspan = 1
        ! An axis whose numbers are days since 1970-01-01, and so takes
        ! its ticks and labels from the calendar.
        logical :: x_date = .false., y_date = .false.
        ! Formatter and locator chosen by the user. A style of FMT_AUTO, a
        ! decimal count of -1, a base of zero and a bin count of zero all
        ! mean "whatever matplotlib would have done".
        integer :: xfmt_style = FMT_AUTO, yfmt_style = FMT_AUTO
        integer :: xfmt_dec = -1, yfmt_dec = -1
        real(dp) :: xfmt_whole = 100.0_dp, yfmt_whole = 100.0_dp
        logical :: x_use_offset = .true., y_use_offset = .true.
        integer :: x_scilo = -5, x_scihi = 6
        integer :: y_scilo = -5, y_scihi = 6
        real(dp) :: xtick_base = 0.0_dp, ytick_base = 0.0_dp
        integer :: xtick_nbins = 0, ytick_nbins = 0
        logical :: has_mesh = .false.
        real(dp), allocatable :: mesh_x(:), mesh_y(:)
        ! Data units per point in y over the same in x. Zero means auto.
        ! adjustable="box" shrinks the axes to suit; "datalim" widens the
        ! limits instead, which is what matplotlib's axis("equal") does.
        ! Tick styling. dir is +1 outward, -1 inward, 0 for both.
        real(dp) :: xtick_len = TICK_LEN, ytick_len = TICK_LEN
        real(dp) :: xtick_dir = 1.0_dp, ytick_dir = 1.0_dp
        real(dp) :: xtick_rot = 0.0_dp, ytick_rot = 0.0_dp
        real(dp) :: xtick_size = TICK_FONT, ytick_size = TICK_FONT
        real(dp) :: title_size = TITLE_FONT
        real(dp) :: xlabel_size = LABEL_FONT, ylabel_size = LABEL_FONT
        ! loc is "center", "left" or "right"; the pads are matplotlib's
        ! labelpad in points, measured from its default of LABEL_PAD.
        character(len=6) :: title_loc = "center"
        real(dp) :: xlabel_pad = LABEL_PAD, ylabel_pad = LABEL_PAD
        ! Face of the title and the two axis labels.
        integer :: title_w = WEIGHT_NORMAL, title_sl = SLANT_ROMAN
        integer :: xlabel_w = WEIGHT_NORMAL, xlabel_sl = SLANT_ROMAN
        integer :: ylabel_w = WEIGHT_NORMAL, ylabel_sl = SLANT_ROMAN
        real(dp) :: legend_size = LEGEND_FONT
        integer :: legend_ncol = 1
        logical :: legend_frame = .true.
        character(len=64) :: legend_title = ""
        real(dp) :: legend_bbox(2) = 0.0_dp
        logical :: legend_has_bbox = .false.
        logical :: spine(4) = .true.
        ! Twin axes: which axes to borrow limits from, and which side this
        ! one's own ticks and label go on. A twin also lets the axes beneath
        ! it show through, so it draws no background.
        integer :: share_x = 0, share_y = 0
        ! An axes placed by hand rather than in the grid. An inset keeps
        ! its rectangle in the fractions of the axes it sits in, so it
        ! follows that axes when the layout moves.
        ! A polar axes: x is the angle in radians and y the radius, and the
        ! box holds a circle rather than a rectangle.
        logical :: polar = .false.
        logical :: fixed_pos = .false.
        integer :: inset_of = 0
        real(dp) :: inset_rect(4) = [0.0_dp, 0.0_dp, 1.0_dp, 1.0_dp]
        ! A secondary axis carries the limits of the axes it belongs to,
        ! through v -> sec_scale*v + sec_offset.
        integer :: sec_of = 0
        logical :: sec_is_x = .true.
        real(dp) :: sec_scale = 1.0_dp, sec_offset = 0.0_dp
        ! Shared axes (subplots(sharex=)). Unlike a twin, which borrows the
        ! other axes' limits wholesale, a share group takes the union of what
        ! all its members hold, so every panel shows the same span. link_x is
        ! the index of the axes the group is named after.
        integer :: link_x = 0, link_y = 0
        logical :: xticklabels_off = .false., yticklabels_off = .false.
        logical :: y_right = .false., x_top = .false.
        ! Room a plotting call asks for beyond what it draws. eventplot
        ! keeps a whole line length clear above and below its strokes.
        logical :: yroom_set = .false.
        real(dp) :: yroom(2) = 0.0_dp
        ! A table of text below (or above) the axes.
        logical :: has_table = .false.
        character(len=32), allocatable :: tbl_cells(:, :), tbl_col(:), tbl_row(:)
        real(dp), allocatable :: tbl_w(:)
        real(dp) :: tbl_size = 10.0_dp
        character(len=16) :: tbl_loc = "bottom"
        ! The running top of a stack of histograms, for hist(stacked=.true.).
        real(dp), allocatable :: hstack(:)
        logical :: xroom_set = .false.
        real(dp) :: xroom(2) = 0.0_dp
        ! Whether that room is a sticky edge, taking no margin beyond it.
        logical :: room_sticks = .false.
        ! A 3D axes. The camera angles are matplotlib's defaults and the z
        ! limits behave like the other two, except that z takes no margin.
        logical :: is3d = .false.
        real(dp) :: elev = 30.0_dp, azim = -60.0_dp
        logical :: zlim_set = .false.
        real(dp) :: zmin_user = 0.0_dp, zmax_user = 1.0_dp
        character(len=256) :: zlabel = ""
        logical :: xaxis_off = .false., yaxis_off = .false.
        logical :: patch_off = .false.
        real(dp) :: aspect = 0.0_dp
        logical :: aspect_datalim = .false.
        ! How much room past the data each axis takes, as a fraction of the
        ! drawn length. Matplotlib's five percent unless margins() says
        ! otherwise; zero is axis("tight"), which fits the data exactly.
        real(dp) :: xmargin = 0.05_dp, ymargin = 0.05_dp
        logical :: cbar_on = .false.
        ! Which way the bar runs, and matplotlib's four numbers for where it
        ! sits: how much of the box it takes, how much space is left between
        ! it and the axes, how much of the length it uses, and how many
        ! times longer it is than it is thick.
        logical :: cbar_horiz = .false.
        real(dp) :: cbar_frac = 0.15_dp
        real(dp) :: cbar_pad = 0.05_dp
        real(dp) :: cbar_shrink = 1.0_dp
        real(dp) :: cbar_aspect = 20.0_dp
        character(len=32) :: cbar_label = ""
        type(scale_t) :: xsc
        type(scale_t) :: ysc
        logical :: x_inv = .false., y_inv = .false.
        logical :: xlim_set = .false.
        logical :: ylim_set = .false.
        real(dp) :: xmin_user = 0.0_dp, xmax_user = 1.0_dp
        real(dp) :: ymin_user = 0.0_dp, ymax_user = 1.0_dp
        integer :: color_cycle = 0
        ! Axes position in figure fractions (matplotlib convention).
        real(dp) :: left = MARGIN_LEFT, right = MARGIN_RIGHT
        real(dp) :: bottom = MARGIN_BOTTOM, top = MARGIN_TOP
    end type axes_t

    ! Figure state (pylab current figure)
    real(dp), save :: fig_w_in = FIG_W_DEFAULT
    real(dp), save :: fig_h_in = FIG_H_DEFAULT
    ! Kept for savefig backends that rasterize; SVG is resolution independent,
    ! so matplotlib emits the same inches*72 pt canvas at any dpi.
    real(dp), save :: fig_dpi = DPI_DEFAULT
    ! Figure margins and inter-subplot spacing, as matplotlib's
    ! subplots_adjust names them.
    real(dp), save :: fig_left = MARGIN_LEFT, fig_right = MARGIN_RIGHT
    real(dp), save :: fig_bottom = MARGIN_BOTTOM, fig_top = MARGIN_TOP
    real(dp), save :: fig_wspace = WSPACE, fig_hspace = HSPACE
    character(len=256), save :: fig_suptitle = ""
    ! Animation frames, packed RGB, all frames end to end. Kept as bytes
    ! rather than as figures because a figure cannot be replayed: the caller
    ! redraws and calls add_frame, exactly as a matplotlib update function
    ! redraws and returns.
    character(len=:), allocatable, save :: anim_pix
    integer, save :: anim_n = 0, anim_w = 0, anim_h = 0
    ! Filled length of anim_pix. The reel is grown by doubling and written
    ! into in place: appending with // would build a temporary as large as
    ! the whole animation on every frame.
    integer, save :: anim_len = 0
    ! Font sizes for anything not set on an individual axes. New axes take
    ! their sizes from here, so set_fontsize before or after plotting behaves
    ! the same way.
    real(dp), save :: def_title = TITLE_FONT, def_label = LABEL_FONT
    real(dp), save :: def_tick = TICK_FONT, def_legend = LEGEND_FONT
    real(dp), save :: fig_suptitle_size = SUPTITLE_FONT
    integer, save :: fig_suptitle_w = WEIGHT_NORMAL
    integer, save :: fig_suptitle_sl = SLANT_ROMAN

    ! ------------------------------------------------------------------
    ! Global defaults, matplotlib's rcParams. A style is nothing but a set
    ! of these: everything below is consulted while drawing or copied into
    ! an axes as it is made, so style_use before the first plot call is what
    ! a user expects it to be.
    ! ------------------------------------------------------------------
    integer, parameter :: MAX_CYCLE = 12
    integer, save :: rc_n_cycle = 0      ! zero means the built-in tab10
    character(len=7), save :: rc_cycle(MAX_CYCLE) = "#000000"
    character(len=7), save :: rc_fig_face = "#ffffff"
    character(len=7), save :: rc_axes_face = "#ffffff"
    character(len=7), save :: rc_grid_color = "#b0b0b0"
    character(len=7), save :: rc_text_color = "#000000"
    character(len=7), save :: rc_spine_color = "#000000"
    real(dp), save :: rc_grid_lw = 0.8_dp
    real(dp), save :: rc_spine_lw = 0.8_dp
    character(len=7), save :: rc_legend_face = "#ffffff"
    character(len=7), save :: rc_legend_edge = "#cccccc"
    real(dp), save :: rc_lw = default_linewidth
    logical, save :: rc_grid = .false.

    ! Clipping region currently in force. See set_clip.
    type(clip_t), save :: g_clip
    type(axes_t), allocatable, save :: ax(:)
    integer, save :: n_ax = 0
    integer, save :: cur_i = 0
    integer, save :: grid_m = 0, grid_n = 0
    ! Relative column widths and row heights, GridSpec's width_ratios and
    ! height_ratios. All ones, the default, is an even grid.
    real(dp), save :: fig_wratio(MAX_RATIO) = 1.0_dp
    real(dp), save :: fig_hratio(MAX_RATIO) = 1.0_dp
    ! A grid whose cells are filled in one at a time by subplot2grid,
    ! rather than all at once by subplot.
    logical, save :: grid_sparse = .false.
    logical, save :: fig_initialized = .false.
    ! Refit the margins to the decorations just before every draw.
    logical, save :: fig_constrained = .false.

    ! A parked figure. Holds exactly the module state above, so switching
    ! figures is a copy in and a copy out rather than threading a figure
    ! object through every renderer routine. Any new figure-level variable
    ! must be added here and to stash_fig/unstash_fig.
    type :: figure_t
        logical :: live = .false.
        real(dp) :: w_in, h_in, dpi
        real(dp) :: left, right, bottom, top, wspace, hspace
        character(len=256) :: suptitle
        real(dp) :: d_title, d_label, d_tick, d_legend, suptitle_size
        type(axes_t), allocatable :: ax(:)
        integer :: n_ax, cur_i, grid_m, grid_n
        logical :: sparse, constrained
        real(dp) :: wratio(MAX_RATIO), hratio(MAX_RATIO)
    end type figure_t
    type(figure_t), allocatable, save :: figs(:)
    integer, save :: cur_fig = 0

    ! Pie wedges and colorbar cells are laid out in figure geometry, not on a
    ! user scale, so they map through a plain linear one.
    type(scale_t), parameter :: linear_scale = scale_t(SCALE_LINEAR, 2.0_dp, 1.0_dp)

end module fplotlib_state
