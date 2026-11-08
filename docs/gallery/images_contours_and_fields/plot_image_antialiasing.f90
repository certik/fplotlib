! Image resampling
! ================
!
! An image with more pixels than the figure has to show them must be
! downsampled. Drawn with ``interpolation="nearest"`` each screen pixel simply
! picks one source pixel, which turns fine structure into moire patterns.
! ``interpolation="bilinear"`` averages neighbouring pixels instead.
!
! This follows matplotlib's
! `image antialiasing <https://matplotlib.org/stable/gallery/images_contours_and_fields/image_antialiasing.html>`_.
! fplotlib has two interpolation modes, ``"nearest"`` and ``"bilinear"``;
! matplotlib's other filters (``"hanning"``, ``"lanczos"``, ...) are not
! available.

program plot_image_antialiasing
    use fplotlib, only: dp, figure, imshow, savefig, subplot, title
    implicit none

    integer, parameter :: n = 600
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp), allocatable :: a(:, :)
    real(dp) :: x, y
    integer :: i, j

! %%
! A chirp, :math:`a(x, y) = \sin\left(2\pi \cdot 60\,(x^2 + y^2)\right)` on
! the unit square: the spatial frequency grows with the distance from the
! corner, so somewhere the pattern is finer than the screen can resolve.

    allocate (a(n, n))
    do j = 1, n
        do i = 1, n
            x = real(j - 1, dp)/real(n - 1, dp)
            y = real(i - 1, dp)/real(n - 1, dp)
            a(i, j) = sin(2.0_dp*pi*60.0_dp*(x**2 + y**2))
        end do
    end do

! %%
! The same array twice. The left panel shows the moire rings of nearest
! neighbour sampling; the right one averages neighbouring pixels, so the fine
! structure is blurred rather than dropped and the spurious pattern changes.

    call figure(figsize=[10.0_dp, 5.0_dp])

    call subplot(1, 2, 1)
    call imshow(a, cmap="RdBu", interpolation="nearest")
    call title("interpolation = nearest")

    call subplot(1, 2, 2)
    call imshow(a, cmap="RdBu", interpolation="bilinear")
    call title("interpolation = bilinear")

    call savefig("image_antialiasing.png")

    print *, "wrote image_antialiasing.png"

end program plot_image_antialiasing
