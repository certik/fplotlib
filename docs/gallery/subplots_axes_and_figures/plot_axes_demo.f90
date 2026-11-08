! Axes Demo: Controlling Axes Properties
! ======================================
!
! This example demonstrates how to control and customize the axes in your
! plots, including setting limits, adjusting aspect ratios, and using
! different coordinate systems.

program plot_axes_demo
    use fplotlib, only: axis, dp, figure, grid, legend, plot, savefig, set_yscale, &
                        subplot, title, xlabel, xlim, ylabel, ylim
    implicit none

    integer, parameter :: n = 100
    real(dp) :: x(n), y1(n), y2(n), y3(n)
    integer :: i
    real(dp), parameter :: pi = 3.14159265358979323846_dp

! %%
! Generate sample data

    do i = 1, n
        x(i) = real(i - 1, dp) / real(n - 1, dp) * 2.0_dp * pi
        y1(i) = sin(x(i))
        y2(i) = cos(x(i))
        y3(i) = sin(2.0_dp * x(i)) * cos(3.0_dp * x(i))
    end do

! %%
! Create a figure with multiple subplots demonstrating different axes configurations

    call figure(figsize=[14.0_dp, 10.0_dp])

! %%
! Subplot 1: Default axes

    call subplot(2, 3, 1)
    call plot(x, y1, "b-", lw=2.0_dp)
    call title("Default Axes")
    call xlabel("x")
    call ylabel("sin(x)")
    call grid(.true., alpha=0.3_dp)

! %%
! Subplot 2: Custom axis limits

    call subplot(2, 3, 2)
    call plot(x, y1, "r-", lw=2.0_dp)
    call xlim(0.0_dp, pi)
    call ylim(-0.5_dp, 1.0_dp)
    call title("Custom Limits: xlim(0,π), ylim(-0.5,1)")
    call xlabel("x")
    call ylabel("sin(x)")
    call grid(.true., alpha=0.3_dp)

! %%
! Subplot 3: Aspect ratio control (equal spacing)

    call subplot(2, 3, 3)
    call plot(x, y1, "g-", lw=2.0_dp)
    call title("Equal Aspect Ratio")
    call xlabel("x")
    call ylabel("sin(x)")
    call grid(.true., alpha=0.3_dp)
    call axis("equal")

! %%
! Subplot 4: Logarithmic scale on y-axis

    call subplot(2, 3, 4)
    block
        real(dp) :: y_positive(n)
        integer :: j
        do j = 1, n
            y_positive(j) = abs(y3(j)) + 0.1_dp
        end do
        call plot(x, y_positive, "m-", lw=2.0_dp)
        call set_yscale("log")
    end block
    call title("Logarithmic Y-Axis")
    call xlabel("x")
    call ylabel("log(|sin(2x)cos(3x)| + 0.1)")
    call grid(.true., alpha=0.3_dp)

! %%
! Subplot 5: Tight layout - axes with no padding

    call subplot(2, 3, 5)
    call plot(x, y2, "c-", lw=2.0_dp)
    call title("Tight Layout")
    call xlabel("x")
    call ylabel("cos(x)")
    call grid(.true., alpha=0.3_dp)
    call axis("tight")

! %%
! Subplot 6: Multiple curves with different scales

    call subplot(2, 3, 6)
    call plot(x, y1, "b-", lw=2.0_dp, label="sin(x)")
    call plot(x, y2, "r--", lw=2.0_dp, label="cos(x)")
    call plot(x, y3 * 0.5_dp, "g:", lw=2.0_dp, label="0.5*sin(2x)cos(3x)")
    call title("Multiple Curves")
    call xlabel("x")
    call ylabel("amplitude")
    call grid(.true., alpha=0.3_dp)
    call legend(loc="upper right")

! %%
! Save the figure

    call savefig("axes_demo.svg")

    print *, "wrote axes_demo.svg"

end program plot_axes_demo
