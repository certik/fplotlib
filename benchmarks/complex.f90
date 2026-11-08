program bench_complex
    use fplotlib
    implicit none
    integer, parameter :: n = 5000
    real(dp) :: x(n), y1(n), y2(n), y3(n)
    integer :: i
    character(len=:), allocatable :: ext
    character(len=16) :: arg

    ext = "png"
    if (command_argument_count() > 0) then
        call get_command_argument(1, arg)
        ext = trim(arg)
    end if
    do i = 1, n
        x(i) = 20.0_dp*real(i - 1, dp)/real(n - 1, dp)
        y1(i) = sin(x(i))*exp(-0.1_dp*x(i))
        y2(i) = cos(2.0_dp*x(i))*exp(-0.05_dp*x(i))
        y3(i) = 0.5_dp*sin(0.5_dp*x(i))
    end do
    call plot(x, y1, "b-", label="damped sin", lw=2.0_dp)
    call plot(x, y2, "r--", label="damped cos")
    call plot(x, y3, "g-", label="slow sin")
    call fill_between(x, y1, y2, alpha=0.2_dp)
    call title("Complex benchmark")
    call xlabel("time")
    call ylabel("amplitude")
    call grid(.true.)
    call legend()
    call savefig("complex_f." // ext)
end program bench_complex
