! Multiple Subplots Demonstration
! ===============================
!
! This example shows how to create and manage multiple plots within a single
! figure using subplots. Subplots are essential for comparing related datasets
! or presenting different aspects of your analysis side by side.

program plot_subplots_demo
    use fplotlib, only: dp, figure, fill_between, grid, legend, plot, savefig, &
                        subplot, title, xlabel, ylabel, ylim
    implicit none

    integer, parameter :: n = 200
    real(dp) :: x(n), y_sin(n), y_cos(n), y_tan(n), y_exp(n)
    integer :: i
    real(dp), parameter :: pi = 3.14159265358979323846_dp

! %%
! Generate sample data: common trigonometric and exponential functions

    do i = 1, n
        x(i) = 4.0_dp * pi * real(i - 1, dp) / real(n - 1, dp)
        y_sin(i) = sin(x(i))
        y_cos(i) = cos(x(i))
        y_tan(i) = tanh(x(i))
        y_exp(i) = exp(-x(i) / (4.0_dp * pi)) * sin(x(i))
    end do

! %%
! Create a figure with a 2x2 grid of subplots

    call figure(figsize=[12.0_dp, 9.0_dp])

! %%
! First subplot: sine function

    call subplot(2, 2, 1)
    call plot(x, y_sin, "b-", lw=2.0_dp, label="sin(x)")
    call title("Sine Function")
    call xlabel("x")
    call ylabel("sin(x)")
    call grid(.true.)
    call legend()

! %%
! Second subplot: cosine function

    call subplot(2, 2, 2)
    call plot(x, y_cos, "r-", lw=2.0_dp, label="cos(x)")
    call title("Cosine Function")
    call xlabel("x")
    call ylabel("cos(x)")
    call grid(.true.)
    call legend()

! %%
! Third subplot: hyperbolic tangent function

    call subplot(2, 2, 3)
    call plot(x, y_tan, "g-", lw=2.0_dp, label="tanh(x)")
    call title("Hyperbolic Tangent")
    call xlabel("x")
    call ylabel("tanh(x)")
    call grid(.true.)
    call legend()
    call ylim(-1.2_dp, 1.2_dp)

! %%
! Fourth subplot: damped oscillation (combination of sine and exponential decay)

    call subplot(2, 2, 4)
    call plot(x, y_exp, "m-", lw=2.0_dp, label="exp(-x/(4π)) * sin(x)")
    call fill_between(x, y_exp, color="m", alpha=0.2_dp)
    call title("Damped Oscillation")
    call xlabel("x")
    call ylabel("amplitude")
    call grid(.true.)
    call legend()

    call savefig("subplots_demo.svg")

    print *, "wrote subplots_demo.svg"

end program plot_subplots_demo
