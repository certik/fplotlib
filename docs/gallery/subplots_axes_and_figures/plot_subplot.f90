! Multiple subplots
! =================
!
! Several axes in one figure. This is matplotlib's
! `subplot <https://matplotlib.org/stable/gallery/subplots_axes_and_figures/subplot.html>`_.
!
! ``subplot(m, n, i)`` splits the figure into ``m`` rows and ``n`` columns and
! makes panel ``i``, counted row by row from 1, the current axes. Everything
! that follows -- :f:subr:`plot`, :f:subr:`title`, :f:subr:`xlabel` -- applies
! to that panel.

program plot_subplot
    use fplotlib, only: axes, clf, dp, figure, legend, plot, savefig, set_facecolor, &
                        subplot, subplot2grid, suptitle, tight_layout, title
    implicit none

    integer, parameter :: n = 200
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: x(n), y1(n), y2(n)
    type(axes) :: top, bottom_left, bottom_right
    integer :: i

    do i = 1, n
        x(i) = 2.0_dp*pi*real(i - 1, dp)/real(n - 1, dp)
    end do
    y1 = sin(x)
    y2 = sin(3.0_dp*x)*exp(-0.3_dp*x)

! %%
! A 2 x 2 grid. A panel can have its own background through
! :f:subr:`set_facecolor`.

    call figure(figsize=[9.0_dp, 7.0_dp])

    call subplot(2, 2, 1)
    call plot(x, y1, "C0-")
    call title("subplot(2, 2, 1)")

    call subplot(2, 2, 2)
    call plot(x, y2, "C1-")
    call title("subplot(2, 2, 2)")

    call subplot(2, 2, 3)
    call plot(x, y1*y2, "C2-")
    call title("subplot(2, 2, 3)")

    call subplot(2, 2, 4)
    call set_facecolor("#fff3d6")
    call plot(x, y1 + y2, "C3-")
    call title("subplot(2, 2, 4), facecolor set")

    call suptitle("A 2 x 2 grid of panels")
    call tight_layout()

    call savefig("subplot_grid.svg")

! %%
! Panels do not have to be the same size. :f:subr:`subplot2grid` places an axes
! by its cell in a grid and the number of rows and columns it spans (cells are
! counted from 0), so one wide panel can sit above two narrow ones.

    call clf()
    top = subplot2grid([2, 2], [0, 0], colspan=2)
    call plot(x, y1, "C0-", label="sin(x)")
    call plot(x, y2, "C1--", label="damped")
    call legend()
    call title("spans both columns")

    bottom_left = subplot2grid([2, 2], [1, 0])
    call plot(x, y1*y2, "C2-")
    call title("one cell")

    bottom_right = subplot2grid([2, 2], [1, 1])
    call plot(x, y1 + y2, "C3-")
    call title("one cell")

    call tight_layout()

    call savefig("subplot_span.svg")

    print *, "wrote subplot_grid.svg and subplot_span.svg"

end program plot_subplot
