program test_plots
    use, intrinsic :: iso_fortran_env, only: error_unit
    use fplot
    implicit none
    integer :: fig1
    type(axes), allocatable :: axs(:, :)
    type(axes) :: a1, a2, a3
    real(dp) :: td(366), yd(366)

    integer, parameter :: n = 100
    integer, parameter :: m = 20
    integer, parameter :: mk = 6
    integer, parameter :: n_marks = 11
    integer, parameter :: ns = 30
    integer, parameter :: nb = 6
    integer, parameter :: nh = 20
    integer, parameter :: ne = 8
    real(dp) :: x(n), y(n), y2(n), y3(n)
    real(dp) :: lo, hi
    real(dp) :: rgbim(8, 12, 3)
    real(dp) :: zn(16, 16)
    character(len=64) :: buf
    real(dp) :: xl(m), yl(m), yl2(m)
    real(dp) :: xm(mk), ym(mk)
    real(dp) :: xs(ns), ys(ns)
    integer, parameter :: ntc = 120
    real(dp) :: tx2(ntc), ty2(ntc), tz2(ntc)
    integer, parameter :: nts = 60
    real(dp) :: usx(nts), usy(nts), usz(nts)
    integer, parameter :: nb3 = 12
    real(dp) :: b3x(nb3), b3y(nb3), b3z(nb3), b3d(nb3)
    integer, parameter :: nq3 = 9
    real(dp) :: q3x(nq3), q3y(nq3), q3z(nq3), q3u(nq3), q3v(nq3), q3w(nq3)
    integer, parameter :: nc3 = 21
    real(dp) :: c3x(nc3), c3y(nc3), c3z(nc3, nc3)
    integer, parameter :: ntp = 24
    real(dp) :: tx(ntp), ty(ntp), tz(ntp)
    integer, parameter :: nlg = 60
    real(dp) :: lgx(nlg), lgy(nlg)
    integer, parameter :: nd = 40
    real(dp) :: dist1(nd), dist2(nd), boxmat(2, nd)
    character(len=8) :: mkeys
    type(axes) :: mos(8)
    integer, parameter :: nsym = 201
    real(dp) :: xsym(nsym)
    integer, parameter :: nsm = 2
    real(dp) :: xsm(nsm) = [0.0_dp, 1.0_dp], ysm(nsm)
    real(dp) :: xe(ne), ye(ne), ee(ne)
    real(dp), parameter :: xb(nb) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp, 6.0_dp]
    real(dp), parameter :: hb(nb) = [3.0_dp, 5.0_dp, 2.0_dp, 7.0_dp, 4.0_dp, 6.0_dp]
    real(dp), parameter :: hb2(nb) = [1.0_dp, 2.0_dp, 4.0_dp, 1.0_dp, 3.0_dp, 2.0_dp]
    character(len=8), parameter :: bar_cols(nb) = [ &
        "red     ", "green   ", "blue    ", "orange  ", "purple  ", "brown   "]
    real(dp), parameter :: xh(nh) = [ &
        0.2_dp, 0.5_dp, 0.7_dp, 1.1_dp, 1.3_dp, 1.4_dp, 1.8_dp, 2.0_dp, 2.1_dp, 2.3_dp, &
        2.4_dp, 2.6_dp, 2.9_dp, 3.0_dp, 3.2_dp, 3.3_dp, 3.7_dp, 4.0_dp, 4.4_dp, 4.9_dp]
    character(len=1), parameter :: mark_codes(n_marks) = &
        ["o", "x", ".", "s", "^", "v", "<", ">", "*", "+", "D"]
    integer :: i, j, k
    real(dp) :: xgap(40), ygap(40), yband(40), qnan, zero
    integer, parameter :: n3 = 31, nl3 = 100
    real(dp), parameter :: PI_T = 3.14159265358979323846_dp
    real(dp) :: s3x(n3), s3y(n3), s3z(n3, n3)
    real(dp) :: t3(nl3), l3x(nl3), l3y(nl3), l3z(nl3)
    real(dp) :: big(n), bigy(n)
    character(len=8), parameter :: fruit(4) = &
        ["apple ", "banana", "cherry", "date  "]
    real(dp), parameter :: hedges(5) = [-3.0_dp, -1.0_dp, 0.0_dp, 1.5_dp, 3.0_dp]
    real(dp) :: wts(nd)
    integer, parameter :: nzr = 8, nzc = 16
    real(dp) :: zimg(nzr, nzc), zlog(nzr, nzc)
    real(dp), parameter :: xedge(nzc + 1) = [ &
        0.0_dp, 0.5_dp, 1.5_dp, 3.0_dp, 5.0_dp, 8.0_dp, 9.0_dp, 10.0_dp, &
        11.0_dp, 12.0_dp, 13.0_dp, 14.0_dp, 15.0_dp, 16.0_dp, 17.0_dp, &
        18.0_dp, 19.0_dp]
    real(dp) :: qx(64), qy(64), qu(64), qv(64)
    real(dp) :: hx(500), hy(500), mz(6, 6), ev(40)
    real(dp) :: sxg(16), syg(16), su(16, 16), sv(16, 16)
    real(dp), parameter :: yedge(nzr + 1) = [ &
        0.0_dp, 1.0_dp, 2.0_dp, 4.0_dp, 6.0_dp, 6.5_dp, 7.0_dp, 8.0_dp, 10.0_dp]
    real(dp) :: svals(ns), cvals(ns)
    integer, parameter :: nst = 12
    real(dp) :: stx(nst), sty(3, nst)
    character(len=4), parameter :: stlab(3) = ["low ", "mid ", "high"]
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    logical :: test_paths_ready = .false.
    character(len=1024) :: test_repo_root = ""
    character(len=1024) :: test_output_dir = ""

    ! Shared data
    do i = 1, n
        x(i) = 2.0_dp * pi * real(i - 1, dp) / real(n - 1, dp)
        y(i) = sin(x(i))
        y2(i) = cos(x(i))
        y3(i) = 0.5_dp * sin(2.0_dp * x(i))
    end do

    do i = 1, m
        xl(i) = 10.0_dp ** (real(i - 1, dp) / real(m - 1, dp) * 2.0_dp - 1.0_dp)  ! 0.1 .. 10
        yl(i) = xl(i) ** 2
        yl2(i) = 10.0_dp * exp(-xl(i))
    end do

    ! 1) basic_line
    call clf()
    call plot(x, y, "k-", label="sin")
    call title("Basic line")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.0_dp, 2.0_dp * pi)
    call ylim(-1.2_dp, 1.2_dp)
    call save_all("basic_line")

    ! 2) multi_style
    call clf()
    call plot(x, y, "b-o", label="sin")
    call plot(x, y2, "r--", label="cos")
    call plot(x, y3, "g.", label="half sin2")
    call title("Multiple styles")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.0_dp, 2.0_dp * pi)
    call ylim(-1.5_dp, 1.5_dp)
    call save_all("multi_style")

    ! 3) markers_only
    call clf()
    call plot(x(1:n:5), y(1:n:5), "rx", label="x marks")
    call plot(x(1:n:5), y2(1:n:5), "bo", label="circles")
    call plot(x(1:n:5), y3(1:n:5), "g.", label="points")
    call title("Markers only")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call xlim(0.0_dp, 2.0_dp * pi)
    call ylim(-1.5_dp, 1.5_dp)
    call save_all("markers_only")

    ! 4) semilogx
    call clf()
    call semilogx(xl, yl, "b-", label="x^2")
    call title("semilogx")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.1_dp, 10.0_dp)
    call ylim(0.0_dp, 120.0_dp)
    call save_all("semilogx")

    ! 5) semilogy
    call clf()
    call semilogy(xl, yl2, "r-o", label="10*exp(-x)")
    call title("semilogy")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.1_dp, 10.0_dp)
    call ylim(1.0e-4_dp, 20.0_dp)
    call save_all("semilogy")

    ! 6) loglog
    call clf()
    call loglog(xl, yl, "k-", label="x^2")
    call title("loglog")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call xlim(0.1_dp, 10.0_dp)
    call ylim(0.01_dp, 100.0_dp)
    call save_all("loglog")

    ! 7) subplots_2x1 (two rows, one column; exercises subplot(m,n,i),
    !    per-axes state, and suptitle)
    call clf()
    call subplot(2, 1, 1)
    call plot(x, y, "b-", label="sin")
    call title("top: sin")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()

    call subplot(2, 1, 2)
    call plot(x, y2, "r--", label="cos")
    call title("bottom: cos")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()

    call suptitle("fplot subplots")
    call save_all("subplots_2x1")

    ! 8) subplots_2x2 (four panels; also checks per-axes log scale)
    call clf()
    call subplot(2, 2, 1)
    call plot(x, y, "b-", label="sin")
    call title("sin")
    call grid(.true.)

    call subplot(2, 2, 2)
    call plot(x, y2, "r--", label="cos")
    call title("cos")
    call grid(.true.)

    call subplot(2, 2, 3)
    call plot(x, y3, "g-", label="0.5 sin(2x)")
    call title("half sin 2x")
    call grid(.true.)

    call subplot(2, 2, 4)
    call semilogx(xl, yl, "k-", label="x^2")
    call title("semilogx panel")
    call grid(.true.)

    call suptitle("fplot 2x2 subplots")
    call save_all("subplots_2x2")

    ! 9) markers gallery (one row per marker code)
    call clf()
    do i = 1, mk
        xm(i) = real(i, dp)
        ym(i) = 1.0_dp
    end do
    do i = 1, n_marks
        call plot(xm, ym * real(i, dp), marker=mark_codes(i), linestyle="None", &
                  label=mark_codes(i))
    end do
    call title("Markers")
    call xlim(0.0_dp, real(mk + 1, dp))
    call ylim(0.0_dp, real(n_marks + 1, dp))
    call save_all("markers_gallery")

    ! 10) scatter
    call clf()
    do i = 1, ns
        xs(i) = 10.0_dp * real(i - 1, dp) / real(ns - 1, dp)
        ys(i) = sin(xs(i)) + 0.1_dp * xs(i)
    end do
    call scatter(xs, ys, label="points")
    call title("Scatter")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call save_all("scatter")

    ! 11) bar
    call clf()
    call bar(xb, hb, label="counts")
    call title("Bar")
    call xlabel("category")
    call ylabel("value")
    call legend()
    call save_all("bar")

    ! 12) hist
    call clf()
    call hist(xh, bins=8, label="samples")
    call title("Histogram")
    call xlabel("value")
    call ylabel("count")
    call legend()
    call save_all("hist")

    ! 13) fill_between
    call clf()
    call fill_between(x, y, y3, alpha=0.5_dp, label="band")
    call plot(x, y, "b-", label="sin")
    call title("Fill between")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call save_all("fill_between")

    ! 14) errorbar
    call clf()
    do i = 1, ne
        xe(i) = real(i, dp)
        ye(i) = sqrt(xe(i))
        ee(i) = 0.15_dp * ye(i)
    end do
    call errorbar(xe, ye, ee, fmt="o-", capsize=3.0_dp, label="meas")
    call title("Errorbar")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call save_all("errorbar")

    ! 15) hv_lines
    call clf()
    call plot(x, y, "b-", label="sin")
    call axhline(0.0_dp, color="k", linestyle="--")
    call axvline(pi, color="r", linestyle=":")
    call title("Reference lines")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call save_all("hv_lines")

    ! 16) text and annotate
    call clf()
    call plot(x, y, "b-", label="sin")
    call text(1.0_dp, 0.8_dp, "peak region", color="k")
    call annotate("minimum", 4.712_dp, -1.0_dp, xtext=2.2_dp, ytext=-0.55_dp, color="r")
    call title("Text and annotate")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call save_all("text_annotate")

    ! 17) custom ticks, minor ticks and legend placement
    call clf()
    call plot(x, y, "b-", label="sin")
    call plot(x, cos(x), "r--", label="cos")
    call xticks([0.0_dp, 1.5708_dp, 3.1416_dp, 4.7124_dp, 6.2832_dp], &
                ["0    ", "pi/2 ", "pi   ", "3pi/2", "2pi  "])
    call yticks([-1.0_dp, 0.0_dp, 1.0_dp])
    call minorticks_on()
    call legend(loc="lower left")
    call title("Ticks and legend placement")
    call xlabel("x")
    call ylabel("y")
    call save_all("ticks_legend")

    ! 18) a non-default figure size
    call figure(figsize=[8.0_dp, 3.0_dp])
    call plot(x, y, "b-", label="sin")
    call title("Wide figure")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call legend()
    call save_all("figsize")

    ! 19) more series than the old fixed 32-slot cap.
    ! figure(), not clf(), because clf() keeps the previous canvas size.
    call figure()
    do i = 1, 40
        call plot(x, sin(x + 0.05_dp * real(i, dp)))
    end do
    call title("Forty series")
    call xlabel("x")
    call ylabel("y")
    call save_all("many_series")

    ! 20) alpha on lines and markers
    call figure()
    call plot(x, y, "b-", label="sin", alpha=0.35_dp)
    call plot(x, cos(x), "r-o", label="cos", alpha=0.6_dp)
    call scatter(x(1:ns), y(1:ns), s=80.0_dp, c="g", label="pts", alpha=0.5_dp)
    call title("Alpha")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call save_all("alpha")

    ! 21) a non-default figure facecolor
    call figure()
    call plot(x, y, "b-")
    call title("Figure facecolor")
    call xlabel("x")
    call ylabel("y")
    call grid(.true.)
    call save_all("facecolor", facecolor="#eeeeee")

    do i = 1, nzr
        do j = 1, nzc
            zimg(i, j) = sin(0.4_dp * real(j, dp)) + cos(0.5_dp * real(i, dp))
            zlog(i, j) = 10.0_dp ** (0.5_dp * real(i + j, dp) / 4.0_dp)
        end do
    end do

    do i = 1, ns
        svals(i) = 20.0_dp + 8.0_dp * real(i, dp)
        cvals(i) = real(i, dp)
    end do

    ! 22) imshow with the default upper origin
    call figure()
    call imshow(zimg)
    call title("imshow")
    call save_all("imshow")

    ! 23) imshow with an extent, lower origin and a colorbar
    call figure()
    call imshow(zimg, cmap="plasma", extent=[0.0_dp, 4.0_dp, 0.0_dp, 2.0_dp], &
                origin="lower")
    call colorbar(label="value")
    call title("imshow with colorbar")
    call xlabel("x")
    call ylabel("y")
    call save_all("imshow_cbar")

    ! 24) scatter with per-point sizes and color-mapped values
    call figure()
    call scatter(x(1:ns), y(1:ns), sizes=svals, cvals=cvals, cmap="viridis")
    call colorbar(label="c")
    call title("Scatter with c and s arrays")
    call xlabel("x")
    call ylabel("y")
    call save_all("scatter_cmap")

    ! 25) contour lines
    call figure()
    call contour(zimg)
    call title("contour")
    call xlabel("x")
    call ylabel("y")
    call save_all("contour")

    ! 26) filled contours with a colorbar
    call figure()
    call contourf(zimg, cmap="coolwarm")
    call colorbar()
    call title("contourf")
    call xlabel("x")
    call ylabel("y")
    call save_all("contourf")

    ! 27) step
    call figure()
    call step(xb, hb, where="mid")
    call title("step")
    call save_all("step")

    ! 28) stem
    call figure()
    call stem(xb, hb)
    call title("stem")
    call save_all("stem")

    ! 29) barh
    call figure()
    call barh(xb, hb)
    call title("barh")
    call save_all("barh")

    ! 30) pie
    call figure()
    call pie(hb, labels=["a", "b", "c", "d", "e", "f"])
    call title("pie")
    call save_all("pie")

    do i = 1, nd
        dist1(i) = sin(real(i, dp)) + 0.3_dp * cos(2.7_dp * real(i, dp))
        dist2(i) = 1.0_dp + 2.0_dp * sin(0.7_dp * real(i, dp))**3
        wts(i) = 0.5_dp + 0.02_dp * real(i, dp)
    end do

    ! 31) boxplot
    call figure()
    call boxplot(dist1)
    call boxplot(dist2)
    call title("boxplot")
    call save_all("boxplot")

    ! 32) violinplot
    call figure()
    call violinplot(dist1)
    call violinplot(dist2)
    call title("violinplot")
    call save_all("violinplot")

    do i = 1, nsym
        xsym(i) = -100.0_dp + real(i - 1, dp)
    end do

    ! 33) symlog y scale
    call figure()
    call plot(xsym, xsym)
    call set_yscale("symlog")
    call title("symlog")
    call save_all("symlog")

    ! 34) axis("equal")
    call figure()
    call plot(xb, hb, marker="o")
    call axis("equal")
    call title("axis equal")
    call save_all("axis_equal")

    ! 35) tick styling and hidden spines
    call figure()
    call plot(xb, hb)
    call tick_params(direction="in", labelsize=8.0_dp)
    call tick_params(axis="x", rotation=45.0_dp)
    call spines(top=.false., right=.false.)
    call title("tick_params")
    call save_all("tick_style")

    ! 36) tight_layout with long tick labels
    call figure()
    call subplot(1, 2, 1)
    call plot(xb, hb * 1000.0_dp)
    call ylabel("value")
    call xlabel("category")
    call subplot(1, 2, 2)
    call plot(xb, hb)
    call xlabel("category")
    call tight_layout()
    call save_all("tight_layout")

    ! 37) subplots_adjust
    call figure()
    call subplot(2, 1, 1)
    call plot(xb, hb)
    call subplot(2, 1, 2)
    call plot(xb, hb)
    call subplots_adjust(left=0.2_dp, hspace=0.5_dp)
    call save_all("subplots_adjust")

    ! 38) twinx
    call figure()
    call plot(xb, hb)
    call ylabel("left")
    call xlabel("x")
    call twinx()
    call plot(xb, hb * 100.0_dp)
    call ylabel("right")
    call title("twinx")
    call save_all("twinx")

    ! 39) color spellings
    call figure()
    do i = 1, 6
        ysm = real(i, dp) + 0.0_dp * xsm
        select case (i)
        case (1); call plot(xsm, ysm, color="red", lw=3.0_dp)
        case (2); call plot(xsm, ysm, color="tab:orange", lw=3.0_dp)
        case (3); call plot(xsm, ysm, color="steelblue", lw=3.0_dp)
        case (4); call plot(xsm, ysm, color="#0f0", lw=3.0_dp)
        case (5); call plot(xsm, ysm, color="0.5", lw=3.0_dp)
        case (6); call plot(xsm, ysm, color="#8c564bcc", lw=3.0_dp)
        end select
    end do
    call title("color names")
    call save_all("colors")

    ! 40) font sizes
    call figure()
    call plot(xb, hb, label="sine")
    call title("big title", fontsize=17.0_dp)
    call xlabel("x axis", fontsize=14.0_dp)
    call ylabel("y axis", fontsize=14.0_dp)
    call tick_params(labelsize=13.0_dp)
    call legend(fontsize=12.0_dp)
    call save_all("fontsize")

    ! 41) legend options
    call figure()
    call plot(xb, hb, label="alpha")
    call plot(xb, hb * 0.5_dp, label="beta")
    call plot(xb, hb * 0.25_dp, label="gamma")
    call plot(xb, hb * 0.125_dp, label="delta")
    call legend(loc="upper right", ncol=2, title="series", frameon=.false.)
    call save_all("legend_opts")

    ! 42) savefig(bbox_inches="tight")
    call figure()
    call plot(xb, hb)
    call title("tight")
    call xlabel("x")
    call ylabel("y")
    call save_all("savefig_tight", bbox_inches="tight", dpi=200.0_dp)

    ! 43) two live figures kept apart, then closed
    call figure()
    fig1 = gcf()
    call plot(xb, hb, "r-")
    call title("figure one")
    call figure()
    call plot(xb, -hb, "b-")
    call title("figure two")
    call close()
    call figure(num=fig1)
    call save_all("figures")
    call close(all=.true.)

    ! 44) an image on a log axis, where the samples are no longer evenly
    ! spaced on the canvas and so cannot be sent as one raster
    call figure()
    call imshow(zimg, extent=[1.0_dp, 1000.0_dp, 0.0_dp, 4.0_dp], aspect="auto")
    call set_xscale("log")
    call title("imshow on a log axis")
    call save_all("imshow_log")

    ! 45) subplots with axes handles and a shared x axis
    call subplots(2, 2, axs, sharex=.true.)
    call axs(1, 1)%plot(x, y, "b-", label="sin")
    call axs(1, 1)%set_title("one")
    call axs(1, 1)%legend()
    call axs(1, 2)%scatter(xs, ys, s=18.0_dp, c="r")
    call axs(1, 2)%set_title("two")
    call axs(2, 1)%bar(xb, hb, color="g")
    call axs(2, 1)%set_xlabel("x")
    call axs(2, 2)%plot(x, y3, "m--")
    call axs(2, 2)%grid(.true.)
    call axs(2, 2)%set_xlabel("x")
    call suptitle("subplots with handles")
    call save_all("subplots_shared")

    ! 46) a style sheet
    call style_use("ggplot")
    call clf()
    call plot(x, y, label="sin")
    call plot(x, y2, label="cos")
    call title("ggplot style")
    call xlabel("x")
    call ylabel("y")
    call legend()
    call save_all("style_ggplot")
    call style_use("default")

    ! 47) spans and line runs
    call style_use("default")
    call clf()
    call plot(x, y, "b-")
    call axhspan(-0.5_dp, 0.5_dp, color="orange", alpha=0.3_dp)
    call axvspan(1.0_dp, 2.0_dp, color="green", alpha=0.2_dp)
    call hlines([-1.0_dp, 1.0_dp], 0.0_dp, 3.0_dp, color="red", linestyle="--")
    call vlines([4.0_dp, 5.0_dp], -1.0_dp, 0.0_dp, color="purple")
    call title("spans and line runs")
    call save_all("spans")

    ! 48) stacked and labelled bars
    call clf()
    call bar(xb, hb, width=0.6_dp, color="tab:blue", label="first")
    call bar(xb, hb2, width=0.6_dp, bottom=hb, color="tab:orange", label="second")
    call bar_label()
    call legend()
    call title("stacked bars")
    call save_all("bar_stacked")

    ! 49) per-bar colors and horizontal bar labels
    call clf()
    call barh(xb, hb, color="k", colors=bar_cols)
    call bar_label(padding=2.0_dp)
    call title("bars in their own colors")
    call save_all("bar_colors")

    ! 50) histogram options
    call clf()
    call hist(dist1, bins=12, density=.true., color="tab:blue", alpha=0.6_dp, &
              label="density")
    call hist(dist1, bins=12, density=.true., histtype="step", color="k", &
              label="step")
    call legend()
    call title("histogram options")
    call save_all("hist_opts")

    ! 51) uneven bins, cumulative
    call clf()
    call hist(dist1, bin_edges=hedges, cumulative=.true., color="tab:green")
    call title("uneven bins, cumulative")
    call save_all("hist_bins")

    ! 52) asymmetric errors in both directions
    call clf()
    call errorbar(xe, ye, yerr_lo=ee, yerr_hi=0.5_dp*ee, xerr=0.3_dp*ee, &
                  fmt="o", color="tab:red", capsize=4.0_dp)
    call title("asymmetric errors")
    call save_all("errorbar_xy")

    ! 53) fill_between with a condition
    call clf()
    call plot(x, y, "k-")
    call fill_between(x, y, spread(0.0_dp, 1, n), where=(y > 0.0_dp), &
                      color="tab:green", alpha=0.4_dp, label="positive")
    call fill_between(x, y, spread(0.0_dp, 1, n), where=(y < 0.0_dp), &
                      color="tab:red", alpha=0.4_dp, label="negative")
    call legend()
    call title("fill_between where")
    call save_all("fill_where")

    ! 54) a reversed colormap
    call clf()
    call imshow(zimg, cmap="RdBu_r", aspect="auto")
    call colorbar()
    call title("RdBu_r")
    call save_all("cmap_reversed")

    ! 55) a logarithmic color norm
    call clf()
    call imshow(zlog, cmap="magma", norm="log", aspect="auto")
    call colorbar()
    call title("log color norm")
    call save_all("cmap_lognorm")

    ! 56) categories instead of numbers
    call clf()
    call bar(fruit, hb(1:4), color="tab:purple")
    call ylabel("count")
    call title("categories")
    call save_all("categorical")

    ! 57) mathtext in the labels
    call clf()
    call plot(x, y)
    call xlabel("$x_{i}$ [m]")
    call ylabel("$E = mc^{2}$")
    call title("$10^{-3} < T^{2}_{n} < 10^{3}$")
    call save_all("mathtext")

    ! 58) a mesh with uneven cells
    call clf()
    call pcolormesh(xedge, yedge, zimg, cmap="viridis")
    call colorbar()
    call title("pcolormesh")
    call save_all("pcolormesh")

    ! 59) panels spanning several cells
    call clf()
    a1 = subplot2grid([3, 3], [0, 0], colspan=3)
    a2 = subplot2grid([3, 3], [1, 0], colspan=2, rowspan=2)
    a3 = subplot2grid([3, 3], [1, 2], rowspan=2)
    call a1%plot(x, y)
    call a1%set_title("wide")
    call a2%plot(x, y2)
    call a2%set_title("big")
    call a3%plot(x, y)
    call a3%set_title("tall")
    call save_all("gridspec")

    ! 60) a date axis
    call clf()
    do i = 1, 366
        td(i) = date_num(2024, 1, 1) + real(i - 1, dp)
        yd(i) = sin(2.0_dp * pi * real(i - 1, dp) / 365.0_dp)
    end do
    call plot(td, yd)
    call xaxis_date()
    call title("dates")
    call save_all("dates")

    ! 61) a vector field
    call clf()
    do i = 1, 8
        do j = 1, 8
            k = (i - 1)*8 + j
            qx(k) = real(j - 1, dp)*0.5_dp
            qy(k) = real(i - 1, dp)*0.5_dp
            qu(k) = cos(qx(k))
            qv(k) = sin(qy(k))
        end do
    end do
    call quiver(qx, qy, qu, qv)
    call title("quiver")
    call save_all("quiver")

    ! 62) labelled contour lines
    call clf()
    call contour(zimg)
    call clabel()
    call title("clabel")
    call save_all("clabel")

    ! 63) an inset and a secondary axis
    call clf()
    a1 = subplot2grid([1, 1], [0, 0])
    call a1%plot(x, y)
    call a1%set_title("inset")
    a2 = a1%secondary_xaxis(scale=2.0_dp)
    a3 = a1%inset_axes([0.6_dp, 0.6_dp, 0.35_dp, 0.3_dp])
    call a3%plot(x, y2)
    call save_all("inset")

    ! 64) plain shapes
    call clf()
    call add_rectangle([0.1_dp, 0.1_dp], 0.4_dp, 0.2_dp, &
                       facecolor="tab:orange", edgecolor="black")
    call add_circle([0.7_dp, 0.7_dp], 0.15_dp)
    call add_ellipse([0.3_dp, 0.7_dp], 0.4_dp, 0.2_dp, angle=30.0_dp, &
                     facecolor="tab:green", alpha=0.5_dp)
    call add_polygon([0.6_dp, 0.9_dp, 0.75_dp], [0.1_dp, 0.1_dp, 0.4_dp], &
                     edgecolor="tab:red", lw=2.0_dp, fill=.false.)
    call title("patches")
    call save_all("patches")

    ! 65) a polar plot
    call clf()
    do i = 1, n
        td(i) = 2.0_dp*pi*real(i - 1, dp)/real(n - 1, dp)
        yd(i) = 1.0_dp + cos(td(i))
    end do
    call polar(td(1:n), yd(1:n))
    call title("polar")
    call save_all("polar")

    ! 66) a smoothed image
    call clf()
    call imshow(zimg, interpolation="bilinear")
    call title("interp")
    call save_all("interp")

    ! 67) two dimensional histograms
    call clf()
    do i = 1, 500
        hx(i) = gauss(real(i, dp)*12.9898_dp)
        hy(i) = gauss(real(i, dp)*78.233_dp)
    end do
    call hist2d(hx, hy, bins=[12, 12])
    call title("hist2d")
    call save_all("hist2d")

    ! 68) hexagonal binning
    call clf()
    call hexbin(hx, hy, gridsize=15)
    call title("hexbin")
    call save_all("hexbin")

! 69) a matrix as an image
    call clf()
    do i = 1, 6
        do j = 1, 6
            mz(i, j) = real(i*j, dp)
        end do
    end do
    call matshow(mz)
    call save_all("matshow")

! 70) a row of events
    call clf()
    do i = 1, 40
        ev(i) = 10.0_dp*(gauss(real(i, dp)*3.7_dp) + 1.0_dp)
    end do
    call eventplot(ev, color="C0")
    call title("eventplot")
    call save_all("eventplot")

! 71) bars broken into pieces
    call clf()
    call broken_barh(reshape([1.0_dp, 4.0_dp, 7.0_dp, 2.0_dp, 2.0_dp, 3.0_dp], [3, 2]), &
                     [1.0_dp, 0.8_dp], color="C0")
    call broken_barh(reshape([2.0_dp, 6.0_dp, 3.0_dp, 3.0_dp], [2, 2]), &
                     [2.5_dp, 0.8_dp], color="C1")
    call title("broken_barh")
    call save_all("broken_barh")

! 72) streamlines of a vector field
    call clf()
    do i = 1, 16
        sxg(i) = -3.0_dp + 6.0_dp*real(i - 1, dp)/15.0_dp
    end do
    do i = 1, 16
        syg(i) = -3.0_dp + 6.0_dp*real(i - 1, dp)/15.0_dp
    end do
    do i = 1, 16
        do j = 1, 16
            su(i, j) = -1.0_dp - sxg(j)**2 + syg(i)
            sv(i, j) = 1.0_dp + sxg(j) - syg(i)**2
        end do
    end do
    call streamplot(sxg, syg, su, sv)
    call title("streamplot")
    call save_all("streamplot")

! 73) a table below the axes
    call clf()
    call table(reshape(["100", "200", "300", "400", "500", "600"], [2, 3]), &
               col_labels=["a", "b", "c"], row_labels=["x", "y"])
    call xticks([real(dp) ::])
    call yticks([real(dp) ::])
    call title("table")
    call save_all("table")

    ! 125) level lines drawn in space
    call clf()
    do i = 1, nc3
        c3x(i) = -3.0_dp + 6.0_dp*real(i - 1, dp)/real(nc3 - 1, dp)
        c3y(i) = c3x(i)
    end do
    do i = 1, nc3
        do j = 1, nc3
            c3z(i, j) = sin(c3x(j))*cos(c3y(i))
        end do
    end do
    call contour3d(c3x, c3y, c3z)
    call title("contour3d")
    call save_all("contour3d")

    ! 124) arrows in space
    call clf()
    k = 0
    do i = 1, 3
        do j = 1, 3
            k = k + 1
            q3x(k) = -0.8_dp + 0.8_dp*real(i - 1, dp)
            q3y(k) = -0.8_dp + 0.8_dp*real(j - 1, dp)
            q3z(k) = 0.0_dp
            q3u(k) = sin(PI_T*q3x(k))*cos(PI_T*q3y(k))
            q3v(k) = -cos(PI_T*q3x(k))*sin(PI_T*q3y(k))
            q3w(k) = 0.5_dp*sin(PI_T*q3x(k))
        end do
    end do
    call quiver3d(q3x, q3y, q3z, q3u, q3v, q3w, length=0.4_dp, normalize=.true.)
    call title("quiver3d")
    call save_all("quiver3d")

    ! 123) boxes standing on the xy plane
    call clf()
    do k = 1, nb3
        b3x(k) = real(mod(k - 1, 4), dp)
        b3y(k) = real((k - 1)/4, dp)
        b3z(k) = 0.0_dp
        b3d(k) = 1.0_dp + sin(real(k, dp))
    end do
    call bar3d(b3x, b3y, b3z, 0.6_dp, 0.6_dp, b3d)
    call title("bar3d")
    call save_all("bar3d")

    ! 122) a surface over scattered points
    call clf()
    do k = 1, nts
        usx(k) = 3.0_dp*sin(real(2*k, dp))
        usy(k) = 3.0_dp*cos(real(3*k, dp))
        usz(k) = sin(usx(k))*cos(usy(k))
    end do
    call plot_trisurf(usx, usy, usz, cmap="viridis")
    call title("trisurf")
    call save_all("trisurf")

! A denser scatter for the contours, matching what matplotlib is given.
    do k = 1, ntc
        tx2(k) = 3.0_dp*(1.0_dp + sin(real(7*k, dp)))
        ty2(k) = 3.0_dp*(1.0_dp + cos(real(11*k, dp)))
        tz2(k) = sin(tx2(k))*cos(ty2(k))
    end do

    ! 121) the bands between those lines, filled
    call clf()
    call tricontourf(tx2, ty2, tz2)
    call colorbar()
    call title("tricontourf")
    call save_all("tricontourf")

! 120) level lines over the triangles
    call clf()
    call tricontour(tx2, ty2, tz2)
    call title("tricontour")
    call save_all("tricontour")

! Points on a jittered grid, the same ones matplotlib is given.
    k = 0
    do i = 1, 6
        do j = 1, 4
            k = k + 1
            tx(k) = real(i - 1, dp) + 0.35_dp*sin(real(3*k, dp))
            ty(k) = real(j - 1, dp) + 0.35_dp*cos(real(5*k, dp))
            tz(k) = tx(k)*ty(k)
        end do
    end do

    ! 119) the same triangles, filled by value
    call clf()
    call tripcolor(tx, ty, tz)
    call colorbar()
    call title("tripcolor")
    call save_all("tripcolor")

! 118) scattered points, triangulated
    call clf()
    call triplot(tx, ty, marker="o")
    call title("triplot")
    call save_all("triplot")

! 117) a log axis too short to label by decades alone
    call clf()
    do i = 1, nlg
        lgx(i) = 1.0_dp + 7.0_dp*real(i - 1, dp)/real(nlg - 1, dp)
        lgy(i) = lgx(i)**1.5_dp
    end do
    call loglog(lgx, lgy)
    call xlim(1.0_dp, 8.0_dp)
    call save_all("log_minor_labels")

! 116) a bowed connector and a box with round corners
    call clf()
    call plot(x, y)
    call annotate("peak", 1.57_dp, 1.0_dp, 3.5_dp, 0.6_dp, &
                  arrowstyle="->", connectionstyle="arc3,rad=0.3", &
                  boxstyle="round,pad=0.5", bbox_facecolor="#ffff99", &
                  bbox_edgecolor="#333333", ha="center")
    call annotate("trough", 4.71_dp, -1.0_dp, 1.5_dp, -0.6_dp, &
                  arrowstyle="->", connectionstyle="arc3,rad=-0.4", ha="center")
    call save_all("annotate_curve")

! 115) margins refitted to the decorations at draw time
    call clf()
    call subplots(1, 2, axs)
    call constrained_layout(.true.)
    do j = 1, 2
        call axs(1, j)%plot(x, real(j, dp)*y)
        call axs(1, j)%set_xlabel("a long x label")
        call axs(1, j)%set_ylabel("a long y label")
        call axs(1, j)%set_title("panel")
    end do
    call save_all("constrained")

! 114) panels drawn as a picture of the figure
    call clf()
    call subplot_mosaic(["AB", "CC"], mkeys, mos)
    call mos(1)%plot(x, y)
    call mos(1)%set_title("A")
    call mos(2)%plot(x, y2, "r-")
    call mos(2)%set_title("B")
    call mos(3)%plot(x, 0.5_dp*y, "g-")
    call mos(3)%set_title("C")
    call save_all("mosaic")

! 113) names given at legend time, and a line kept out of it
    call clf()
    call plot(x, y)
    call plot(x, y2)
    call plot(x, 0.5_dp*y, label="_hidden")
    call legend(labels=["first ", "second"])
    call title("legend labels")
    call save_all("legend_labels")

! 112) violins across, with the mean and median marked
    call clf()
    boxmat(1, :) = dist1
    boxmat(2, :) = dist2
    call subplot(1, 2, 1)
    call violinplot(boxmat, labels=["one", "two"], showmeans=.true., &
                    showmedians=.true.)
    call title("means")
    call subplot(1, 2, 2)
    call violinplot(boxmat, labels=["one", "two"], vert=.false., &
                    showextrema=.false.)
    call title("across")
    call save_all("violin_opts")

! 111) boxes across, waisted, filled and with the mean marked
    call clf()
    boxmat(1, :) = dist1
    boxmat(2, :) = dist2
    call subplot(1, 2, 1)
    call boxplot(boxmat, labels=["one", "two"], notch=.true., &
                 showmeans=.true., patch_artist=.true.)
    call title("notched")
    call subplot(1, 2, 2)
    call boxplot(boxmat, labels=["one", "two"], vert=.false.)
    call title("across")
    call save_all("box_opts")

! 110) a pie with a slice pulled out and its shares written in
    call clf()
    call pie(hb, labels=["a", "b", "c", "d", "e", "f"], &
             explode=[0.0_dp, 0.1_dp, 0.0_dp, 0.0_dp, 0.0_dp, 0.0_dp], &
             startangle=90.0_dp, counterclock=.false., autopct="%.1f%%")
    call title("pie options")
    call save_all("pie_opts")

! 109) marker edges and a band carried to the crossing
    call clf()
    call subplot(1, 2, 1)
    call scatter(x(1:16), y(1:16), s=120.0_dp, c="skyblue", &
                 edgecolors="black", linewidths=1.5_dp)
    call subplot(1, 2, 2)
    call plot(x, y, "b-")
    call plot(x, y2, "r-")
    call fill_between(x, y, y2, color="green", alpha=0.4_dp, &
                      where=y > y2, interpolate=.true.)
    call save_all("scatter_edge")

! 108) one legend for the whole figure
    call clf()
    call subplot(1, 2, 1)
    call plot(x, y, "b-", label="sin")
    call subplot(1, 2, 2)
    call plot(x, y2, "r--", label="cos")
    call figlegend(loc="upper right")
    call save_all("figlegend")

! 107) minor ticks placed by hand, and a pruned locator
    call clf()
    call subplot(2, 1, 1)
    call plot(x, y, "b-")
    call xticks([0.0_dp, 2.0_dp, 4.0_dp, 6.0_dp])
    call xticks([1.0_dp, 3.0_dp, 5.0_dp], minor=.true.)
    call title("minor by hand")
    call subplot(2, 1, 2)
    call plot(x, y, "r-")
    call locator_params(axis="y", prune="both")
    call title("prune=both")
    call save_all("tick_locator")

! 106) colours for what is off the scale, and a colormap of our own
    call clf()
    zero = 0.0_dp
    qnan = zero/zero
    do i = 1, 16
        do j = 1, 16
            zn(i, j) = real(i + j, dp)
        end do
    end do
    zn(4:6, 4:6) = qnan
    call subplot(1, 2, 1)
    call imshow(zn, cmap="viridis", vmin=8.0_dp, vmax=24.0_dp)
    call set_bad("red")
    call set_under("black")
    call set_over("white")
    call title("bad/under/over")
    call subplot(1, 2, 2)
    call imshow(zn)
    call set_cmap_colors(["#000080", "#ffffff", "#800000"])
    call title("own colormap")
    call save_all("cmap_special")

! 105) three ways of spreading values over a colormap
    call clf()
    do i = 1, 16
        do j = 1, 16
            zn(i, j) = real(j - 8, dp)*real(i, dp)/4.0_dp
        end do
    end do
    call subplot(1, 3, 1)
    call imshow(zn, cmap="coolwarm", norm="centered", vcenter=0.0_dp)
    call title("centered")
    call subplot(1, 3, 2)
    call imshow(abs(zn), cmap="viridis", norm="power", gamma=0.5_dp)
    call title("power")
    call subplot(1, 3, 3)
    call imshow(zn, cmap="coolwarm", norm="symlog", linthresh=1.0_dp)
    call title("symlog")
    call save_all("norms")

! 104) an image whose colours are given outright
    call clf()
    do i = 1, 8
        do j = 1, 12
            rgbim(i, j, 1) = real(j - 1, dp)/11.0_dp
            rgbim(i, j, 2) = real(i - 1, dp)/7.0_dp
            rgbim(i, j, 3) = 0.5_dp
        end do
    end do
    call imshow(rgbim)
    call title("imshow of an RGB array")
    call save_all("imshow_rgb")

! 103) a histogram on its side, one on a log axis, and one with gaps
    call clf()
    call subplot(1, 3, 1)
    call hist(dist1, bins=12, orientation="horizontal", color="C0")
    call title("horizontal")
    call subplot(1, 3, 2)
    call hist(dist1, bins=12, log=.true., color="C1")
    call title("log")
    call subplot(1, 3, 3)
    call hist(dist1, bins=12, rwidth=0.7_dp, color="C2")
    call title("rwidth")
    call save_all("hist_orient")

! 102) a title against one end, and labels pushed further out
    call clf()
    call plot(x, y, "b-")
    call title("left title", loc="left")
    call xlabel("x", labelpad=12.0_dp)
    call ylabel("y", labelpad=16.0_dp)
    call save_all("title_loc")

! 101) bars with error bars, edge alignment and their own tick labels
    call clf()
    call subplot(1, 2, 1)
    call bar([0.0_dp, 1.0_dp, 2.0_dp], [3.0_dp, 5.0_dp, 2.0_dp], &
             yerr=[0.4_dp, 0.6_dp, 0.3_dp], capsize=4.0_dp, &
             tick_label=["a", "b", "c"])
    call title("yerr")
    call subplot(1, 2, 2)
    call bar([0.0_dp, 1.0_dp, 2.0_dp], [3.0_dp, 5.0_dp, 2.0_dp], &
             align="edge", color="g")
    call title("align=edge")
    call save_all("bar_err")

! 100) axes that count down, and limits read back out
    call clf()
    call plot(x, y, "b-")
    call invert_xaxis()
    call invert_yaxis()
    call get_xlim(lo, hi)
    write (buf, '(a,f6.2,a,f6.2)') "x from ", lo, " to ", hi
    call text(0.05_dp, 0.1_dp, trim(buf), transform="axes")
    call title("inverted axes")
    call save_all("invert_axes")

! 99) an annotation with a real arrow head
    call clf()
    call plot(x, y, "b-")
    call annotate("minimum", 4.712_dp, -1.0_dp, xtext=2.2_dp, ytext=-0.55_dp, &
                  arrowstyle="->")
    call annotate("start", 0.0_dp, 0.0_dp, xtext=1.0_dp, ytext=0.6_dp, &
                  arrowstyle="->", arrowcolor="r", arrowlw=1.5_dp)
    call title("annotate arrows")
    call save_all("annotate_arrow")

! 98) text pinned to the corners of the axes, not to the data
    call clf()
    call plot(x, y)
    call text(0.05_dp, 0.95_dp, "top left", transform="axes", va="top")
    call text(0.95_dp, 0.05_dp, "bottom right", transform="axes", ha="right")
    call title("transform=axes")
    call save_all("transform_axes")

! 97) one array is enough
    call clf()
    call plot(y)
    call plot(y*0.5_dp, "o--")
    call title("plot(y)")
    call save_all("plot_y")

! 96) a wireframe and a colormapped surface
    call clf()
    do i = 1, n3
        s3x(i) = -3.0_dp + 6.0_dp*real(i - 1, dp)/real(n3 - 1, dp)
        s3y(i) = s3x(i)
    end do
    do i = 1, n3
        do j = 1, n3
            s3z(i, j) = sin(sqrt(s3x(j)**2 + s3y(i)**2))
        end do
    end do
    call subplot(1, 2, 1)
    call plot_wireframe(s3x, s3y, s3z, color="tab:blue")
    call title("wireframe")
    call subplot(1, 2, 2)
    call plot_surface(s3x, s3y, s3z, cmap="viridis")
    call title("colormapped")
    call save_all("surface3d_extra")

! 95) fractions, roots and greek
    call clf()
    call plot(x, y)
    call xlabel("$\frac{x}{2}$")
    call ylabel("$\sqrt{y}$")
    call title("$T = 2\pi\sqrt{\frac{L}{g}}$, $\Omega = \alpha + \beta$")
    call save_all("mathtext_frac")

! 94) hatched fills
    call clf()
    call subplot(1, 2, 1)
    call bar(xb, hb, hatch="/")
    call bar(xb, hb2, bottom=hb, hatch="\\")
    call title("hatched bars")
    call subplot(1, 2, 2)
    call fill_between(x, y, hatch="x", color="tab:green", alpha=0.4_dp, &
                      edgecolor="tab:green")
    call add_rectangle([1.0_dp, -0.8_dp], 2.0_dp, 0.6_dp, facecolor="white", &
                       edgecolor="black", hatch="|||")
    call title("hatched fill")
    call save_all("hatch")

! 93) a colorbar lying on its side
    call clf()
    call subplot(1, 2, 1)
    call imshow(zimg, cmap="viridis", aspect="auto")
    call colorbar(orientation="horizontal", label="value")
    call title("horizontal")
    call subplot(1, 2, 2)
    call imshow(zimg, cmap="viridis", aspect="auto")
    call colorbar(shrink=0.6_dp, aspect=10.0_dp)
    call title("shrunk")
    call save_all("colorbar_orient")

! 92) how much room the data is given
    call clf()
    call subplot(1, 2, 1)
    call plot(x, y)
    call margins(x=0.0_dp, y=0.3_dp)
    call title("margins")
    call subplot(1, 2, 2)
    call plot(x, y)
    call autoscale(.false.)
    call plot(x, 3.0_dp * y)
    call title("autoscale off")
    call save_all("margins")

! 91) stacked bands, a band along y, and an endless line
    call clf()
    do i = 1, nst
        stx(i) = real(i - 1, dp)
        sty(1, i) = 1.0_dp + 0.5_dp * real(i - 1, dp)
        sty(2, i) = 2.0_dp + sin(0.5_dp * stx(i))
        sty(3, i) = 3.0_dp - 0.1_dp * real(i - 1, dp)
    end do
    call subplot(1, 2, 1)
    call stackplot(stx, sty, labels=stlab)
    call legend(loc="upper left")
    call title("stackplot")
    call subplot(1, 2, 2)
    call fill_betweenx(x, y - 1.5_dp, y2 + 1.5_dp, color="tab:orange", alpha=0.5_dp)
    call axline([0.0_dp, 0.0_dp], slope=1.5_dp, color="tab:red", linestyle="--")
    call title("fill_betweenx and axline")
    call save_all("fills")

! 90) a background colour for one axes only
    call clf()
    call subplots(1, 2, axs)
    call axs(1, 1)%plot(x, y)
    call axs(1, 1)%set_facecolor("#eeeeee")
    call axs(1, 1)%grid(.true., color="white")
    call axs(1, 2)%plot(x, y)
    call save_all("axes_facecolor")

! 89) marker and line detail
    call clf()
    call plot(x, y, marker="o", markevery=8, markerfacecolor="white", &
              markeredgecolor="tab:red", markersize=7.0_dp)
    call plot(x, y - 1.0_dp, color="tab:green", dashes=[8.0_dp, 2.0_dp, 2.0_dp, 2.0_dp])
    call plot(x(1:8), y(1:8) + 1.0_dp, color="tab:purple", drawstyle="steps-post")
    call save_all("marker_detail")

! 88) text options: rotation, vertical alignment, a box and two lines
    call clf()
    call plot(x, y)
    call text(1.0_dp, 0.6_dp, "rotated", rotation=30.0_dp)
    call text(4.0_dp, 0.6_dp, "top", va="top", ha="center")
    call text(4.0_dp, -0.6_dp, "boxed", ha="center", va="center", &
              bbox_facecolor="yellow", bbox_edgecolor="black")
    call text(1.0_dp, -0.4_dp, "two"//achar(10)//"lines", va="top")
    call figtext(0.02_dp, 0.02_dp, "figure corner", fontsize=8.0_dp)
    call save_all("text_options")

! 87) bold and italic
    call clf()
    call plot(x, y)
    call title("bold title", fontweight="bold")
    call xlabel("italic x", fontstyle="italic")
    call ylabel("bold italic y", fontweight="bold", fontstyle="italic")
    call text(2.0_dp, 0.5_dp, "emphasis", fontstyle="italic")
    call text(2.0_dp, -0.5_dp, "strong", fontweight="bold")
    call save_all("font_faces")

! 86) the grid: which ticks it follows and what it looks like
    call clf()
    call subplot(1, 2, 1)
    call plot(x, y)
    call grid(.true., which="both", color="0.7", linestyle=":", lw=0.6_dp)
    call minorticks_on()
    call title("both")
    call subplot(1, 2, 2)
    call plot(x, y)
    call grid(.true., axis="y", color="tab:blue", linestyle="--", alpha=0.4_dp)
    call title("y only")
    call save_all("grid_options")

! 85) the same calls, reached through an axes handle rather than the
! current axes
    call clf()
    call subplots(2, 2, axs)
    call axs(1, 1)%boxplot(dist1)
    call axs(1, 1)%set_title("box")
    call axs(1, 2)%violinplot(dist2)
    call axs(1, 2)%set_title("violin")
    call axs(2, 1)%semilogy(x + 1.0_dp, exp(x))
    call axs(2, 1)%minorticks_on()
    call axs(2, 2)%pie([3.0_dp, 1.0_dp, 2.0_dp])
    call axs(2, 2)%set_title("pie")
    call save_all("axes_handles2")

! 84) what is drawn over what: the grid rules across the bars but not
! across the line, and the band is lifted over both
    call clf()
    call bar(xb, hb, color="tab:blue")
    call fill_between(xb, hb*0.5_dp, color="tab:green", alpha=0.7_dp)
    call set_zorder(3.0_dp)
    call plot(xb, hb, "r-", lw=2.0_dp)
    call grid(.true.)
    call title("layers")
    call save_all("zorder")

! 83) missing data: the line breaks at it and the axes do not stretch
    call clf()
    ! A quiet NaN without leaning on ieee_arithmetic, which not every
    ! compiler this has to build with provides.
    zero = 0.0_dp
    qnan = zero / zero
    do i = 1, 40
        xgap(i) = real(i, dp)
        ygap(i) = sin(0.2_dp * real(i, dp))
        yband(i) = ygap(i) - 0.4_dp
    end do
    ygap(12:15) = qnan
    yband(30:32) = qnan
    call fill_between(xgap, ygap, yband, color="tab:orange", alpha=0.5_dp)
    call plot(xgap, ygap, "b-o", label="with a gap")
    call legend(loc="upper right")
    call title("missing data")
    call save_all("nan_gap")

! 82) an arrow patch and a path of cubic curves
    call clf()
    call xlim(0.0_dp, 1.0_dp)
    call ylim(0.0_dp, 1.2_dp)
    call add_arrow(0.1_dp, 0.1_dp, 0.6_dp, 0.4_dp, width=0.2_dp, &
                   facecolor="tab:blue", edgecolor="k")
    call add_path([0.1_dp, 0.3_dp, 0.6_dp, 0.9_dp], &
                  [0.8_dp, 1.1_dp, 0.5_dp, 0.8_dp], "MC", &
                  facecolor="tab:orange", edgecolor="k", lw=2.0_dp)
    call title("an arrow and a path")
    call save_all("patches_path")

! 81) bands of a discrete norm, and the qualitative maps
    call clf()
    call subplot(1, 2, 1)
    call imshow(zimg, cmap="viridis", &
                boundaries=[-2.0_dp, -1.0_dp, 0.0_dp, 1.0_dp, 2.0_dp])
    call colorbar()
    call title("bands")
    call subplot(1, 2, 2)
    call imshow(zimg, cmap="tab20")
    call title("tab20")
    call save_all("cmap_discrete")

! 80) a stacked histogram, and one whose samples carry weights
    call clf()
    call subplot(1, 2, 1)
    call hist(dist1, bin_edges=hedges, stacked=.true., label="one")
    call hist(dist2, bin_edges=hedges, stacked=.true., label="two")
    call legend()
    call title("stacked")
    call subplot(1, 2, 2)
    call hist(dist1, bin_edges=hedges, weights=wts, color="tab:purple")
    call title("weighted")
    call save_all("hist_stacked")

! 79) a grid whose columns and rows are not equal
    call clf()
    call gridspec(width_ratios=[2.0_dp, 1.0_dp], height_ratios=[1.0_dp, 2.0_dp])
    call subplot(2, 2, 1)
    call plot(x, y, "b-")
    call title("wide")
    call subplot(2, 2, 2)
    call plot(x, y2, "r-")
    call title("narrow")
    call subplot(2, 2, 3)
    call plot(x, y2, "g-")
    call subplot(2, 2, 4)
    call plot(x, y, "k-")
    call save_all("grid_ratios")

! 78) named formatters and a locator: a percentage on y, thousands on x,
!     and ticks every 250 units
    call clf()
    do i = 1, n
        big(i) = 1000.0_dp*real(i - 1, dp)
        bigy(i) = 50.0_dp + 40.0_dp*y(i)
    end do
    call plot(big, bigy)
    call tick_format("y", "percent")
    call tick_format("x", "comma")
    call tick_locator("y", base=25.0_dp)
    call title("formatters")
    call save_all("formatters")

! 77) a large y axis and an offset x axis: what the tick labels leave out
!     is written once at the end of each axis
    call clf()
    do i = 1, n
        big(i) = 1.0e5_dp + real(i - 1, dp)*3.0_dp/real(n - 1, dp)
        bigy(i) = 2.0e6_dp*y(i)
    end do
    call plot(big, bigy)
    call title("offset text")
    call save_all("offset_text")

! 75) a 3D surface
    call clf()
    do i = 1, n3
        s3x(i) = -3.0_dp + 6.0_dp*real(i - 1, dp)/real(n3 - 1, dp)
        s3y(i) = s3x(i)
    end do
    do i = 1, n3
        do j = 1, n3
            s3z(i, j) = sin(sqrt(s3x(j)**2 + s3y(i)**2))
        end do
    end do
    call plot_surface(s3x, s3y, s3z)
    call title("surface3d")
    call save_all("surface3d")

! 76) a 3D line and a 3D scatter
    call clf()
    do i = 1, nl3
        t3(i) = real(i - 1, dp)*4.0_dp*PI_T/real(nl3 - 1, dp)
        l3x(i) = cos(t3(i))
        l3y(i) = sin(t3(i))
        l3z(i) = t3(i)/(4.0_dp*PI_T)
    end do
    call plot3d(l3x, l3y, l3z)
    call scatter3d(l3x(1:nl3:10), l3y(1:nl3:10), l3z(1:nl3:10), c="r")
    call xlabel("x")
    call ylabel("y")
    call zlabel("z")
    call title("line3d")
    call save_all("line3d")

! 74) an animation: the same line redrawn at a moving phase
    do i = 1, 20
        call clf()
        call plot(x, sin(x + real(i - 1, dp)*0.3_dp), "b-")
        call ylim(-1.5_dp, 1.5_dp)
        call title("anim_sine")
        call add_frame()
    end do
    call save_animation(output_path("anim_sine", ".gif"), fps=10.0_dp)
    print *, "wrote ", trim(output_path("anim_sine", ".gif"))

    print *, "All test plots written."
    call verify_reference_hashes()

contains

    ! A repeatable stand-in for random numbers: the fractional part of a
    ! large sine, summed twice so the result piles up in the middle.
    function gauss(seed) result(g)
        real(dp), intent(in) :: seed
        real(dp) :: g, a, b
        a = sin(seed)*43758.5453_dp
        b = sin(seed + 1.0_dp)*43758.5453_dp
        g = (a - floor(a)) + (b - floor(b)) - 1.0_dp
    end function gauss

    ! Every case is written in all three formats, so the three savefig calls
    ! and the line that reports them are stated once here rather than once
    ! per case. The options are passed straight through: an absent optional
    ! stays absent, so a plain case says nothing about facecolor or dpi.
    subroutine save_all(stem, facecolor, bbox_inches, dpi)
        character(len=*), intent(in) :: stem
        character(len=*), intent(in), optional :: facecolor, bbox_inches
        real(dp), intent(in), optional :: dpi

        call savefig(output_path(stem, ".svg"), facecolor=facecolor, &
                     bbox_inches=bbox_inches, dpi=dpi)
        call savefig(output_path(stem, ".pdf"), facecolor=facecolor, &
                     bbox_inches=bbox_inches, dpi=dpi)
        call savefig(output_path(stem, ".png"), facecolor=facecolor, &
                     bbox_inches=bbox_inches, dpi=dpi)
        call savefig(output_path(stem, ".eps"), facecolor=facecolor, &
                     bbox_inches=bbox_inches, dpi=dpi)
        print *, "wrote ", trim(output_path(stem, ".svg")), ", .pdf, .png and .eps"
    end subroutine save_all

    subroutine verify_reference_hashes()
        character(len=32) :: env
        character(len=256) :: python_cmd
        character(len=1024) :: command, cmdmsg
        integer :: status, cmdstat, exitstat

        call get_environment_variable("FPLOT_SKIP_HASH_VERIFY", env, status=status)
        if (status == 0) then
            if (len_trim(env) > 0 .and. trim(env) /= "0") then
                print *, "Skipping reference hash verification (FPLOT_SKIP_HASH_VERIFY set)."
                return
            end if
        end if

        call get_environment_variable("FPLOT_TEST_PYTHON", python_cmd, status=status)
        if (status /= 0 .or. len_trim(python_cmd) == 0) python_cmd = "python"

        call ensure_test_paths(python_cmd)
        command = trim(python_cmd)//' "'//trim(join_path(trim(test_repo_root), "tests/verify_reference_hashes.py"))//'"'
        call execute_command_line(trim(command), wait=.true., exitstat=exitstat, &
                                  cmdstat=cmdstat, cmdmsg=cmdmsg)
        if (cmdstat /= 0) then
            write (error_unit, '(a)') "fplot: failed to start reference hash verifier: "//trim(cmdmsg)
            error stop 1
        end if
        if (exitstat /= 0) then
            write (error_unit, '(a,i0)') "fplot: reference hash verification failed with exit code ", exitstat
            error stop 1
        end if
    end subroutine verify_reference_hashes

    function output_path(stem, suffix) result(path)
        character(len=*), intent(in) :: stem, suffix
        character(len=1024) :: path

        call ensure_test_paths()
        path = join_path(trim(test_output_dir), trim(stem)//trim(suffix))
    end function output_path

    subroutine ensure_test_paths(python_cmd)
        character(len=*), intent(in), optional :: python_cmd
        character(len=1024) :: exe_path, probe, mkdir_cmd, cmdmsg, pycmd
        integer :: status, cmdstat, exitstat, slash
        logical :: exists

        if (test_paths_ready) return

        call get_command_argument(0, exe_path, status=status)
        if (status == 0 .and. len_trim(exe_path) > 0) then
            probe = normalize_path(trim(exe_path))
            slash = index(trim(probe), "/", back=.true.)
            if (slash > 0) then
                probe = probe(:slash - 1)
            else
                probe = "."
            end if

            do
                inquire(file=trim(join_path(trim(probe), "fpm.toml")), exist=exists)
                if (exists) then
                    test_repo_root = trim(probe)
                    exit
                end if
                slash = index(trim(probe), "/", back=.true.)
                if (slash <= 0) exit
                if (slash == 3 .and. probe(2:2) == ":") then
                    probe = probe(:slash)
                else
                    probe = probe(:slash - 1)
                end if
            end do
        end if

        if (len_trim(test_repo_root) == 0) test_repo_root = "."
        test_output_dir = join_path(trim(test_repo_root), "tests/out")

        pycmd = "python"
        if (present(python_cmd)) then
            if (len_trim(python_cmd) > 0) pycmd = trim(python_cmd)
        end if
        mkdir_cmd = trim(pycmd)//' -c "from pathlib import Path; Path(r'''//trim(test_output_dir)//''').mkdir(parents=True, exist_ok=True)"'
        call execute_command_line(trim(mkdir_cmd), wait=.true., exitstat=exitstat, &
                                  cmdstat=cmdstat, cmdmsg=cmdmsg)
        if (cmdstat /= 0 .or. exitstat /= 0) then
            write (error_unit, '(a)') "fplot: failed to create output directory: "//trim(test_output_dir)
            if (len_trim(cmdmsg) > 0) write (error_unit, '(a)') trim(cmdmsg)
            error stop 1
        end if

        test_paths_ready = .true.
    end subroutine ensure_test_paths

    function join_path(base, leaf) result(path)
        character(len=*), intent(in) :: base, leaf
        character(len=1024) :: path
        integer :: n

        path = trim(base)
        n = len_trim(path)
        if (n > 0 .and. path(n:n) /= "/") then
            path = trim(path)//"/"
        end if
        path = trim(path)//trim(leaf)
    end function join_path

    function normalize_path(raw) result(path)
        character(len=*), intent(in) :: raw
        character(len=1024) :: path
        integer :: idx

        path = trim(raw)
        do idx = 1, len_trim(path)
            if (path(idx:idx) == "\\") path(idx:idx) = "/"
        end do
    end function normalize_path

end program test_plots
