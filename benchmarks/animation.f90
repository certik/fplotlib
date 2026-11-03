program bench_animation
    use fplotlib
    implicit none
    integer, parameter :: n = 240, n_frames = 40
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp) :: x(n), y(n), envelope(n), phase
    integer :: frame, i

    do i = 1, n
        x(i) = 10.0_dp*real(i - 1, dp)/real(n - 1, dp)
    end do
    envelope = exp(-0.15_dp*x)

    do frame = 1, n_frames
        phase = 2.0_dp*pi*real(frame - 1, dp)/real(n_frames, dp)
        y = sin(2.0_dp*x - phase)*envelope
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
    call save_animation("animation_f.gif", fps=20.0_dp)
end program bench_animation
