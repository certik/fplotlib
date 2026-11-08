! Blend transparency with color
! =============================
!
! Show one field in colour and let a second one decide how opaque it is, so
! that only the interesting part stands out from a grey background. This is
! matplotlib's
! `image transparency blend <https://matplotlib.org/stable/gallery/images_contours_and_fields/image_transparency_blend.html>`_.
!
! :f:subr:`imshow` has no per-pixel ``alpha`` array, so the blend is done in
! Fortran instead: the colormap is evaluated with ``cmap_rgb`` from
! ``fplotlib_cmap`` (which returns 0-255 integers), mixed with grey, and the resulting RGB array goes to
! ``imshow``.

program plot_image_transparency_blend
    use fplotlib, only: colorbar, dp, figure, imshow, savefig, subplot, title
    use fplotlib_cmap, only: cmap_from_str, cmap_rgb
    implicit none

    integer, parameter :: n = 200
    real(dp), allocatable :: field(:, :), weight(:, :), gray(:, :, :), blend(:, :, :)
    real(dp) :: x, y, t, alpha, fmin, fmax
    integer :: i, j, cmap, r, g, b

! %%
! ``field`` is the quantity to colour, and ``weight`` is the significance
! that will set the opacity: a broad bump centred to the right of the middle.

    allocate (field(n, n), weight(n, n), blend(n, n, 3))
    do j = 1, n
        do i = 1, n
            x = -3.0_dp + 6.0_dp*real(j - 1, dp)/real(n - 1, dp)
            y = -3.0_dp + 6.0_dp*real(i - 1, dp)/real(n - 1, dp)
            field(i, j) = sin(2.0_dp*x)*cos(2.0_dp*y) + 0.5_dp*x
            weight(i, j) = exp(-((x - 1.0_dp)**2 + y**2)/1.5_dp)
        end do
    end do
    fmin = minval(field)
    fmax = maxval(field)

! %%
! Blend pixel by pixel: ``alpha`` of the colormap colour over the grey that
! stands for "not significant",
!
! .. math::
!
!    \mathrm{RGB} = \alpha \, \mathrm{RGB}_\mathrm{cmap}
!                  + (1 - \alpha) \, \mathrm{RGB}_\mathrm{grey},
!
! with :math:`\alpha` the weight.

    cmap = cmap_from_str("viridis")
    do j = 1, n
        do i = 1, n
            t = (field(i, j) - fmin)/(fmax - fmin)
            call cmap_rgb(cmap, t, r, g, b)
            alpha = weight(i, j)
            blend(i, j, :) = alpha*real([r, g, b], dp)/255.0_dp + (1.0_dp - alpha)*0.5_dp
        end do
    end do

! %%
! Three panels: the plain colour map, the weight on its own, and the blend.

    call figure(figsize=[13.0_dp, 4.5_dp])

    call subplot(1, 3, 1)
    call imshow(field, cmap="viridis")
    call title("field")
    call colorbar()

    call subplot(1, 3, 2)
    call imshow(weight, cmap="gray")
    call title("weight (opacity)")
    call colorbar()

    call subplot(1, 3, 3)
    call imshow(blend)
    call title("field blended by weight")

    call savefig("image_transparency_blend.png")

    print *, "wrote image_transparency_blend.png"

end program plot_image_transparency_blend
