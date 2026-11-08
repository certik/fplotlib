! 3D Vector Field: Quiver Plot
! ============================
!
! This example demonstrates how to visualize a 3D vector field using the
! :f:subr:`quiver3d` function. Vector fields are useful for visualizing
! gradients, flow patterns, and other vector-valued functions in 3D space.

program plot_quiver3d
    use fplotlib, only: dp, figure, grid, quiver3d, savefig, title, xlabel, ylabel, &
                        zlabel
    implicit none

    integer, parameter :: nx = 5, ny = 5, nz = 5
    integer :: i, j, k, idx
    real(dp) :: x(nx), y(ny), z(nz), dx, dy, dz
    real(dp), allocatable :: pos_x(:), pos_y(:), pos_z(:), vec_x(:), vec_y(:), vec_z(:)
    integer :: n_arrows

! %%
! Create a grid of points in 3D space

    do i = 1, nx
        x(i) = -2.0_dp + 4.0_dp * real(i - 1, dp) / real(nx - 1, dp)
    end do
    do j = 1, ny
        y(j) = -2.0_dp + 4.0_dp * real(j - 1, dp) / real(ny - 1, dp)
    end do
    do k = 1, nz
        z(k) = -2.0_dp + 4.0_dp * real(k - 1, dp) / real(nz - 1, dp)
    end do

! %%
! Allocate storage for the arrow positions and vectors

    n_arrows = nx * ny * nz
    allocate (pos_x(n_arrows), pos_y(n_arrows), pos_z(n_arrows))
    allocate (vec_x(n_arrows), vec_y(n_arrows), vec_z(n_arrows))

! %%
! Define a 3D vector field: a multiple of the position vector,
! :math:`\mathbf{F}(\mathbf{r}) = 0.3\,\mathbf{r}`, so the field is radial and
! points away from the origin.
! In a real application, you might compute the gradient of a scalar field,
! or the curl of a velocity field.

    idx = 1
    do k = 1, nz
        do j = 1, ny
            do i = 1, nx
                pos_x(idx) = x(i)
                pos_y(idx) = y(j)
                pos_z(idx) = z(k)

                ! Define the vector field: in this case, we use a simple radial field
                ! proportional to the position vector
                vec_x(idx) = x(i) * 0.3_dp
                vec_y(idx) = y(j) * 0.3_dp
                vec_z(idx) = z(k) * 0.3_dp

                idx = idx + 1
            end do
        end do
    end do

! %%
! Create the 3D quiver plot

    call figure(figsize=[10.0_dp, 8.0_dp])
    call quiver3d(pos_x, pos_y, pos_z, vec_x, vec_y, vec_z, color="C0")

! %%
! Configure the plot with labels and title

    call xlabel("X")
    call ylabel("Y")
    call zlabel("Z")
    call title("3D Vector Field (Radial)")
    call grid(.true.)

    call savefig("quiver3d.svg")

    print *, "wrote quiver3d.svg"

    deallocate (pos_x, pos_y, pos_z, vec_x, vec_y, vec_z)

end program plot_quiver3d
