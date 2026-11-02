program bench_simple
    use fplotlib
    implicit none
    integer, parameter :: n = 200
    real(dp) :: x(n), y(n)
    integer :: i
    character(len=:), allocatable :: ext
    character(len=16) :: arg

    ext = "png"
    if (command_argument_count() > 0) then
        call get_command_argument(1, arg)
        ext = trim(arg)
    end if
    do i = 1, n
        x(i) = 4.0_dp*3.14159265358979_dp*real(i - 1, dp)/real(n - 1, dp)
        y(i) = sin(x(i))*exp(-0.1_dp*x(i))
    end do
    call plot(x, y, "b-")
    call savefig("simple_f." // ext)
end program bench_simple
