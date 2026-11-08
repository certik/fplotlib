! Confidence ellipse of a two-dimensional dataset
! ===============================================
!
! The covariance of a cloud of points defines an ellipse that encloses a given
! number of standard deviations. This is matplotlib's
! `confidence ellipse <https://matplotlib.org/stable/gallery/statistics/confidence_ellipse.html>`_.
!
! matplotlib builds a unit ellipse from the Pearson coefficient and then
! rotates and scales it. Here the covariance matrix
!
! .. math::
!
!    \Sigma = \begin{pmatrix} s_{xx} & s_{xy} \\ s_{xy} & s_{yy} \end{pmatrix}
!
! is diagonalised directly: its eigenvectors give the rotation and the square
! roots of its eigenvalues :math:`\lambda_{1,2}` the semi-axes, so the ellipse
! enclosing :math:`k` standard deviations has full axes
! :math:`2k\sqrt{\lambda_{1}}` and :math:`2k\sqrt{\lambda_{2}}`.
! :f:subr:`add_ellipse` draws the result.

program plot_confidence_ellipse
    use fplotlib, only: add_ellipse, axis, dp, figure, grid, plot, savefig, scatter, &
                        title, xlabel, ylabel
    implicit none

    integer, parameter :: n = 500
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: x(n), y(n), z1(n), z2(n)
    real(dp) :: mx, my, sxx, syy, sxy, half, lam1, lam2, angle
    character(len=2), parameter :: colors(3) = ["C3", "C2", "C0"]
    integer :: k, i
    integer(8) :: state

! %%
! Correlated Gaussian data with a reproducible generator (a small linear
! congruential one and the Box-Muller transform), so the picture does not change
! from run to run.

    state = 12345_8
    do i = 1, n
        z1(i) = gauss()
        z2(i) = gauss()
    end do
    x = 3.0_dp*z1 + 1.0_dp
    y = 0.8_dp*(3.0_dp*z1) + 1.2_dp*z2 - 0.5_dp

! %%
! The sample mean and covariance, and the eigenvalues of :math:`\Sigma`
!
! .. math::
!
!    \lambda_{1,2} = \frac{s_{xx} + s_{yy}}{2}
!      \pm \sqrt{\left(\frac{s_{xx} - s_{yy}}{2}\right)^2 + s_{xy}^2},
!    \qquad
!    \theta = \frac{1}{2} \operatorname{atan2}(2 s_{xy},\, s_{xx} - s_{yy}).

    mx = sum(x)/n
    my = sum(y)/n
    sxx = sum((x - mx)**2)/(n - 1)
    syy = sum((y - my)**2)/(n - 1)
    sxy = sum((x - mx)*(y - my))/(n - 1)

    half = sqrt(((sxx - syy)/2.0_dp)**2 + sxy**2)
    lam1 = (sxx + syy)/2.0_dp + half
    lam2 = (sxx + syy)/2.0_dp - half
    angle = 0.5_dp*atan2(2.0_dp*sxy, sxx - syy)*180.0_dp/pi

! %%
! One ellipse per number of standard deviations: 1, 2 and 3 sigma, drawn from
! the outside in so the smaller ones stay visible.

    call figure(figsize=[7.0_dp, 7.0_dp])
    call scatter(x, y, s=6.0_dp, c="k", alpha=0.6_dp)

    do k = 3, 1, -1
        call add_ellipse([mx, my], 2.0_dp*k*sqrt(lam1), 2.0_dp*k*sqrt(lam2), &
                         angle=angle, edgecolor=colors(k), lw=2.0_dp, fill=.false.)
    end do
    call plot([mx], [my], "ro", markersize=5.0_dp)

    call axis("equal")
    call title("Confidence ellipses: 1, 2 and 3 sigma")
    call xlabel("x")
    call ylabel("y")
    call grid(.true., alpha=0.3_dp)

    call savefig("confidence_ellipse.svg")

    print *, "wrote confidence_ellipse.svg"

contains

    real(dp) function uniform()
        state = mod(state*6364136223846793005_8 + 1442695040888963407_8, huge(1_8))
        uniform = (real(abs(state), dp) + 1.0_dp)/(real(huge(1_8), dp) + 2.0_dp)
    end function uniform

    real(dp) function gauss()
        gauss = sqrt(-2.0_dp*log(uniform()))*cos(2.0_dp*pi*uniform())
    end function gauss

end program plot_confidence_ellipse
