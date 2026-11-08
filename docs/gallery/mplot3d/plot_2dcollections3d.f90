! Plot 2D data on 3D plot
! =======================
!
! Flat data can live on any plane of a 3D axes. This is matplotlib's
! `2dcollections3d <https://matplotlib.org/stable/gallery/mplot3d/2dcollections3d.html>`_.
!
! matplotlib has a ``zdir`` argument that picks the plane. fplotlib has none,
! but the same effect is one array away: :f:subr:`plot3d` and
! :f:subr:`scatter3d` take all three coordinates, so a curve on the plane
! ``z = 0`` just gets a third coordinate of zeros, and one on ``y = 0`` gets
! a second coordinate of zeros.

program plot_2dcollections3d
    use fplotlib, only: dp, figure, legend, plot3d, savefig, scatter3d, view_init, &
                        xlabel, xlim, ylabel, ylim, zlabel, zlim
    implicit none

    integer, parameter :: n = 100, m = 20
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: x(n), y(n), zero(n)
    real(dp) :: xs(m), ys(m), zs(m), zero_m(m)
    integer :: i

! %%
! A sine curve in the x-y plane (z = 0), kept inside the axis limits because
! fplotlib does not clip 3D data to them.

    do i = 1, n
        x(i) = real(i - 1, dp)/real(n - 1, dp)
    end do
    y = 0.5_dp + 0.5_dp*sin(2.0_dp*pi*x)
    zero = 0.0_dp

    call figure(figsize=[8.0_dp, 7.0_dp])
    call plot3d(x, y, zero, "C0-", lw=2.0_dp, label="on z = 0")

! %%
! Scatter points on two other planes: the x-z plane (y = 0) and the
! y-z plane (x = 0).

    do i = 1, m
        xs(i) = real(i - 1, dp)/real(m - 1, dp)
        ys(i) = 0.5_dp + 0.5_dp*sin(4.0_dp*pi*xs(i))
        zs(i) = real(i - 1, dp)/real(m - 1, dp)
    end do
    zero_m = 0.0_dp

    call scatter3d(xs, zero_m, ys, s=20.0_dp, c="C1", label="on y = 0")
    call scatter3d(zero_m, xs, zs, s=20.0_dp, c="C2", marker="^", label="on x = 0")

! %%
! Axis limits and labels, and a point of view to see all three planes.

    call xlim(0.0_dp, 1.0_dp)
    call ylim(0.0_dp, 1.0_dp)
    call zlim(0.0_dp, 1.0_dp)
    call xlabel("X")
    call ylabel("Y")
    call zlabel("Z")
    call legend()
    call view_init(elev=20.0_dp, azim=-35.0_dp)

    call savefig("2dcollections3d.svg")

    print *, "wrote 2dcollections3d.svg"

end program plot_2dcollections3d
