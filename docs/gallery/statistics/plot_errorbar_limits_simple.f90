! Error Bars: Simple Demonstration
! ================================
!
! This example demonstrates how to add error bars to a plot to represent
! uncertainty or variability in your measurements. Error bars are essential
! for scientific visualization as they communicate the reliability of data.

program plot_errorbar_limits_simple
    use fplotlib, only: dp, errorbar, figure, grid, legend, savefig, title, xlabel, &
                        ylabel
    implicit none

    integer, parameter :: n = 8
    real(dp) :: x(n), y(n), yerr(n), xerr(n)
    integer :: i
    real(dp), parameter :: pi = 3.14159265358979323846_dp

! %%
! Create sample data with some natural variability. We'll plot measurements
! taken at regular intervals with both positive and negative error bounds.

    do i = 1, n
        x(i) = real(i - 1, dp)
        y(i) = sin(x(i) / 2.0_dp) * 10.0_dp + 5.0_dp
        ! Symmetric errors for y-axis
        yerr(i) = 0.5_dp + 0.2_dp * real(i, dp)
        ! Symmetric errors for x-axis
        xerr(i) = 0.1_dp
    end do

! %%
! Plot the data with error bars on both x and y axes. The :f:subr:`errorbar`
! function accepts the data arrays and error magnitudes.

    call figure(figsize=[8.0_dp, 6.0_dp])
    call errorbar(x, y, yerr, xerr=xerr, fmt="o", capsize=5.0_dp, &
                  label="measurements", color="C0")

! %%
! Add labels and formatting to make the plot clear and professional.

    call xlabel("Time [s]")
    call ylabel("Amplitude [V]")
    call title("Simple Error Bar Example")
    call grid(.true., alpha=0.3_dp)
    call legend()

    call savefig("errorbar_limits_simple.svg")

    print *, "wrote errorbar_limits_simple.svg"

end program plot_errorbar_limits_simple
