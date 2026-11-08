! A rotating Möbius strip as an animated PNG
! ============================================
!
! A Möbius strip has a half twist: following either edge once around the
! strip brings you back on the other edge. Here a parametric wireframe turns
! about the vertical axis, and :f:subr:`save_animation` writes the frames as
! a full-colour APNG. The ``.png`` extension lets the gallery embed the
! animation while also showing its first frame in viewers without APNG support.

program plot_mobius_apng
    use fplotlib, only: add_frame, clf, dp, figure, grid, plot3d, save_animation, &
                        title, view_init, xlabel, xlim, ylabel, ylim, zlabel, zlim
    implicit none

    integer, parameter :: nu = 121, nv = 9, n_frames = 36
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp), parameter :: radius = 1.0_dp, half_width = 0.4_dp
    real(dp) :: x(nu, nv), y(nu, nv), z(nu, nv)
    real(dp) :: u, v, angle, r
    integer :: frame, i, j

    call figure(figsize=[5.0_dp, 5.0_dp], dpi=80.0_dp)

! %%
! For angle :math:`u` around the centre and width coordinate :math:`v`, use
! :math:`(x,y,z) = ((R+v\cos(u/2))\cos u,\,
! (R+v\cos(u/2))\sin u,\,v\sin(u/2))`. At :math:`u=2\pi`, the
! point at :math:`v` meets the point at :math:`-v` at :math:`u=0`.
! Rotating the x-y coordinates leaves the twist attached to the strip.
!
! The grid is drawn with :f:subr:`plot3d` because :f:subr:`plot_surface`
! accepts a height field, not a general parametric surface. Fixed limits,
! figure size and camera keep the frames steady. The final phase is omitted
! because it would duplicate the first frame when the animation loops.

    do frame = 1, n_frames
        angle = 2.0_dp*pi*real(frame - 1, dp)/real(n_frames, dp)
        do j = 1, nv
            v = half_width*(2.0_dp*real(j - 1, dp)/real(nv - 1, dp) - 1.0_dp)
            do i = 1, nu
                u = 2.0_dp*pi*real(i - 1, dp)/real(nu - 1, dp)
                r = radius + v*cos(u/2.0_dp)
                x(i, j) = r*cos(u + angle)
                y(i, j) = r*sin(u + angle)
                z(i, j) = v*sin(u/2.0_dp)
            end do
        end do

        call clf()
        do j = 1, nv
            call plot3d(x(:, j), y(:, j), z(:, j), color="C0", lw=1.2_dp)
        end do
        do i = 1, nu - 1, 5
            call plot3d(x(i, :), y(i, :), z(i, :), color="C1", lw=0.8_dp)
        end do
        call xlim(-1.5_dp, 1.5_dp)
        call ylim(-1.5_dp, 1.5_dp)
        call zlim(-0.5_dp, 0.5_dp)
        call view_init(elev=25.0_dp, azim=-55.0_dp)
        call grid(.false.)
        call title("Rotating Mobius strip")
        call xlabel("x")
        call ylabel("y")
        call zlabel("z")
        call add_frame()
    end do

! %%
! A ``.png`` filename selects the APNG encoder; ``fps`` controls the playback
! rate and the animation loops by default.

    call save_animation("mobius.png", fps=18.0_dp)

    print *, "wrote mobius.png from", n_frames, "frames"

end program plot_mobius_apng
