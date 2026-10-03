! Travelling wave, saved as an animated GIF
! =========================================
!
! Animation in fplotlib is deliberately plain: draw a figure, call
! :f:subr:`add_frame` to keep it, clear the figure, and repeat. When the loop
! is done, :f:subr:`save_animation` encodes everything collected so far into a
! GIF -- the encoder is part of the library, so no external tool is involved.
!
! The picture below is the finished GIF, playing in the page.

program plot_wave_animation
    use fplotlib
    implicit none

    integer, parameter :: n = 240, n_frames = 40
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp), parameter :: k = 2.0_dp, decay = 0.15_dp
    real(dp) :: x(n), y(n), envelope(n), phase
    integer :: frame, i

    do i = 1, n
        x(i) = 10.0_dp*real(i - 1, dp)/real(n - 1, dp)
    end do
    envelope = exp(-decay*x)

! %%
! One frame per phase step. The axis limits are set explicitly inside the
! loop: without them every frame would autoscale to its own data and the
! animation would jitter.
!
! Frames must all come out the same size, which they do as long as the figure
! size and DPI stay put -- so keep anything that changes the layout, such as a
! longer title, out of the loop.

    do frame = 1, n_frames
        phase = 2.0_dp*pi*real(frame - 1, dp)/real(n_frames, dp)
        y = sin(k*x - phase)*envelope

        call clf()
        call plot(x, y, "b-", lw=2.0_dp)
        call fill_between(x, y, color="b", alpha=0.2_dp)
        call xlim(0.0_dp, 10.0_dp)
        call ylim(-1.1_dp, 1.1_dp)
        call title("Travelling wave")
        call xlabel("x")
        call ylabel("displacement")
        call grid(.true.)

        call add_frame()
    end do

! %%
! :f:subr:`save_animation` writes the GIF and resets the frame buffer, so one
! program can build several animations in turn. ``fps`` sets the playback
! rate; pass ``loop=.false.`` for an animation that plays only once.

    call save_animation("wave.gif", fps=20.0_dp)

    print *, "wrote wave.gif from", n_frames, "frames"

end program plot_wave_animation
