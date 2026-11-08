! Lorenz Attractor: 3D Visualization
! ==================================
!
! The Lorenz attractor is a classic example of chaotic dynamics. This program
! solves the Lorenz differential equations and visualizes the resulting 3D
! trajectory. The famous "butterfly" shape emerges from the chaotic behavior.
!
! The Lorenz system is
!
! .. math::
!
!    \begin{aligned}
!    \frac{dx}{dt} &= \sigma (y - x), \\
!    \frac{dy}{dt} &= x (\rho - z) - y, \\
!    \frac{dz}{dt} &= x y - \beta z,
!    \end{aligned}
!
! where :math:`\sigma`, :math:`\rho` and :math:`\beta` are parameters. The
! classic values :math:`\sigma = 10`, :math:`\rho = 28` and
! :math:`\beta = 8/3` give chaotic behaviour.

program plot_lorenz_attractor
    use fplotlib, only: dp, figure, grid, plot3d, savefig, title, xlabel, ylabel, &
                        zlabel
    implicit none

    integer, parameter :: n_steps = 10000
    real(dp) :: x(n_steps), y(n_steps), z(n_steps)
    real(dp) :: x_curr, y_curr, z_curr, dt
    integer :: i
    real(dp), parameter :: sigma = 10.0_dp, rho = 28.0_dp, beta = 8.0_dp/3.0_dp

! %%
! Set the initial conditions and the time step :math:`\Delta t` for numerical
! integration

    x_curr = 1.0_dp
    y_curr = 1.0_dp
    z_curr = 1.0_dp
    dt = 0.01_dp

! %%
! Integrate the Lorenz equations with the explicit Euler method,
! :math:`\mathbf{r}_{k+1} = \mathbf{r}_k + \Delta t \, \dot{\mathbf{r}}_k`.
! Each iteration computes the derivatives and updates the position.

    do i = 1, n_steps
        x(i) = x_curr
        y(i) = y_curr
        z(i) = z_curr

        ! Compute derivatives (Lorenz equations)
        ! We use temporary variables to avoid overwriting values needed for other derivatives
        block
            real(dp) :: dx, dy, dz
            dx = sigma * (y_curr - x_curr)
            dy = x_curr * (rho - z_curr) - y_curr
            dz = x_curr * y_curr - beta * z_curr

            ! Update positions using Euler method
            x_curr = x_curr + dt * dx
            y_curr = y_curr + dt * dy
            z_curr = z_curr + dt * dz
        end block
    end do

! %%
! Create a 3D plot of the Lorenz attractor trajectory.
! The :f:subr:`plot3d` function draws a 3D line through the points.
! The characteristic double-loop "butterfly" shape should be clearly visible.

    call figure(figsize=[10.0_dp, 8.0_dp])
    call plot3d(x, y, z, "b-", lw=0.5_dp, label="Lorenz trajectory")

! %%
! Configure the 3D view and add labels

    call xlabel("X")
    call ylabel("Y")
    call zlabel("Z")
    call title("Lorenz Attractor")
    call grid(.true.)

    call savefig("lorenz_attractor.svg")

    print *, "wrote lorenz_attractor.svg"

end program plot_lorenz_attractor
