! Curve with error band
! =====================
!
! A parametric curve whose uncertainty is given perpendicular to the curve,
! drawn as a band around it. This is matplotlib's
! `curve error band <https://matplotlib.org/stable/gallery/statistics/curve_error_band.html>`_.
!
! :f:subr:`fill_between` only covers errors in ``y``. For a curve that turns
! back on itself the band has to follow the normal, so its outline is built by
! hand -- the curve pushed out along the normal, then back along the curve
! pushed in -- and filled with :f:subr:`add_polygon`.

program plot_curve_error_band
    use fplotlib, only: add_polygon, axis, dp, figure, plot, savefig, title, xlabel, &
                        ylabel
    implicit none

    integer, parameter :: n = 80
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: t(n), x(n), y(n), nx(n), ny(n), err(n)
    real(dp) :: dx, dy, norm
    real(dp) :: bx(2*n), by(2*n)
    integer :: i

! %%
! A circle of radius 1,
! :math:`(x, y) = (\cos t, \sin t)` for :math:`t \in [0, 2\pi]`, with an
! error :math:`\varepsilon(t) = 0.05 + 0.15 \sin^2 2t` that varies along it.

    do i = 1, n
        t(i) = 2.0_dp*pi*real(i - 1, dp)/real(n - 1, dp)
    end do
    x = cos(t)
    y = sin(t)
    err = 0.05_dp + 0.15_dp*sin(2.0_dp*t)**2

! %%
! Unit normals from the tangent :math:`(\dot{x}, \dot{y})`, as
! :math:`\mathbf{n} = (\dot{y}, -\dot{x}) / \lVert(\dot{x}, \dot{y})\rVert`. The tangent is known analytically
! here; for sampled data a finite difference would do.

    do i = 1, n
        dx = -sin(t(i))
        dy = cos(t(i))
        norm = sqrt(dx**2 + dy**2)
        nx(i) = dy/norm
        ny(i) = -dx/norm
    end do

! %%
! The outline of the band: out along one side, back along the other.

    bx(1:n) = x + err*nx
    by(1:n) = y + err*ny
    bx(n + 1:2*n) = x(n:1:-1) - err(n:1:-1)*nx(n:1:-1)
    by(n + 1:2*n) = y(n:1:-1) - err(n:1:-1)*ny(n:1:-1)

    call figure(figsize=[6.0_dp, 6.0_dp])
    call add_polygon(bx, by, facecolor="C0", edgecolor="none", alpha=0.3_dp)
    call plot(x, y, "C0-", lw=1.5_dp)

    call axis("equal")
    call title("Error band normal to the curve")
    call xlabel("x")
    call ylabel("y")

    call savefig("curve_error_band.svg")

    print *, "wrote curve_error_band.svg"

end program plot_curve_error_band
