! Multiple Images Display
! =======================
!
! This example shows how to display multiple images side by side in a single
! figure using subplots. This is useful for comparing different datasets,
! showing processing stages, or presenting multiple related visualizations.

program plot_multi_image
    use fplotlib, only: colorbar, dp, figure, imshow, savefig, subplot, title
    implicit none

    integer, parameter :: nx = 40, ny = 40
    real(dp) :: data1(ny, nx), data2(ny, nx), data3(ny, nx), x(nx), y(ny)
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
! Create three different 2D data fields: the Gaussian
! :math:`e^{-(x^2 + y^2)/2}`, the product :math:`\sin x \cos y`, and the
! radial distance :math:`\sqrt{x^2 + y^2}`

    ! Gaussian
    do j = 1, ny
        do i = 1, nx
            data1(j, i) = exp(-(x(i)**2 + y(j)**2) / 2.0_dp)
        end do
    end do

    ! Product of sinusoids
    do j = 1, ny
        do i = 1, nx
            data2(j, i) = sin(x(i)) * cos(y(j))
        end do
    end do

    ! Radial distance
    do j = 1, ny
        do i = 1, nx
            data3(j, i) = sqrt(x(i)**2 + y(j)**2)
        end do
    end do

! %%
! Create a 1x3 subplot layout to display the three images side by side

    call figure(figsize=[15.0_dp, 4.0_dp])

    call subplot(1, 3, 1)
    call imshow(data1, cmap="viridis")
    call title("Gaussian")
    call colorbar()

    call subplot(1, 3, 2)
    call imshow(data2, cmap="plasma")
    call title("Sin(x) * Cos(y)")
    call colorbar()

    call subplot(1, 3, 3)
    call imshow(data3, cmap="cool")
    call title("Radial Distance")
    call colorbar()

    call savefig("multi_image.png")

    print *, "wrote multi_image.png"

end program plot_multi_image
