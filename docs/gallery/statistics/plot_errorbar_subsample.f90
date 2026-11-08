! Error Bars: Subsampled Data
! ===========================
!
! This example shows how to plot error bars on a subset of data points when
! you have many measurements. This technique helps reduce visual clutter while
! still communicating uncertainty in your data.

program plot_errorbar_subsample
    use fplotlib, only: dp, errorbar, figure, grid, legend, plot, savefig, title, &
                        xlabel, xlim, ylabel
    implicit none

    integer, parameter :: n = 100, subsample = 5
    real(dp) :: x(n), y(n), yerr(n)
    integer :: i

! %%
! Generate a large dataset with measurements and their uncertainties

    do i = 1, n
        x(i) = 2.0_dp * 3.14159265358979323846_dp * real(i - 1, dp) / real(n - 1, dp)
        y(i) = sin(x(i)) + 0.1_dp * cos(5.0_dp * x(i))
        ! Error increases with x position
        yerr(i) = 0.1_dp + 0.05_dp * sin(x(i))**2
    end do

! %%
! Create a figure that shows all data points as a line, but only error bars
! for every Nth point to reduce visual clutter

    call figure(figsize=[10.0_dp, 6.0_dp])

! %%
! Plot the full data as a thin line to show the overall trend

    call plot(x, y, "b-", lw=1.0_dp, label="full data", alpha=0.5_dp)

! %%
! Overlay error bars on subsampled points to show uncertainty at specific locations.
! This demonstrates the data quality without cluttering the visualization.

    call errorbar(x(1:n:subsample), y(1:n:subsample), yerr(1:n:subsample), &
                  fmt="ro", capsize=4.0_dp, label="subsampled with errors")

! %%
! Add formatting and labels

    call xlabel("Angle [radians]")
    call ylabel("Amplitude")
    call title("Error Bars on Subsampled Data")
    call grid(.true., alpha=0.3_dp)
    call legend()
    call xlim(0.0_dp, 2.0_dp * 3.14159265358979323846_dp)

    call savefig("errorbar_subsample.svg")

    print *, "wrote errorbar_subsample.svg"

end program plot_errorbar_subsample
