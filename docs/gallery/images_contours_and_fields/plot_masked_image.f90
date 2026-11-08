! Masked Image Display
! ====================
!
! This example demonstrates how to display an image with masked regions,
! allowing you to hide or highlight certain areas of your data. This is useful
! for focusing on specific regions of interest or handling missing data values.
!
! The example creates a simple 2D array and displays it using :f:subr:`imshow`
! with masking capabilities.

program plot_masked_image
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fplotlib, only: colorbar, dp, figure, imshow, savefig, set_bad, title, xlabel, &
                        ylabel
    implicit none

    integer, parameter :: nx = 50, ny = 50
    real(dp) :: data(ny, nx), x(nx), y(ny)
    integer :: i, j
    real(dp), parameter :: pi = 3.14159265358979323846_dp

! %%
! Create a grid of x and y values

    do i = 1, nx
        x(i) = -5.0_dp + 10.0_dp * real(i - 1, dp) / real(nx - 1, dp)
    end do

    do j = 1, ny
        y(j) = -5.0_dp + 10.0_dp * real(j - 1, dp) / real(ny - 1, dp)
    end do

! %%
! Create a 2D data field, the Gaussian :math:`f(x, y) = e^{-(x^2 + y^2)/2}`

    do j = 1, ny
        do i = 1, nx
            data(j, i) = exp(-(x(i)**2 + y(j)**2) / 2.0_dp)
        end do
    end do

! %%
! Fortran has no masked arrays, so masked entries are NaN: fplotlib draws them
! with the "bad" colour. Here a circular region in the centre is masked out.

    do j = 1, ny
        do i = 1, nx
            if (sqrt(x(i)**2 + y(j)**2) < 1.5_dp) then
                data(j, i) = ieee_value(1.0_dp, ieee_quiet_nan)
            end if
        end do
    end do

! %%
! Display the image with a colormap. The image shows the 2D data field with
! the center region masked out, creating a "hole" in the visualization.

    call figure(figsize=[8.0_dp, 6.0_dp])
    call set_bad("lightgray")
    call imshow(data, cmap="viridis")
    call title("Masked Image: 2D Gaussian with Center Masked")
    call xlabel("X index")
    call ylabel("Y index")
    call colorbar()

    call savefig("masked_image.png")

    print *, "wrote masked_image.png"

end program plot_masked_image
