! Damped oscillation, saved as SVG
! ================================
!
! A first taste of the pylab-style API. Two damped sinusoids go into one
! figure, the decaying envelope is shaded behind them, and the result is
! written as SVG -- so the picture below is vector graphics, and stays sharp
! at any zoom level.
!
! Every example in this gallery is a plain Fortran program: it starts with
! ``use fplotlib`` and ends with a ``savefig`` call.

program plot_damped_oscillation
    use fplotlib
    implicit none

    integer, parameter :: n = 400
    real(dp), parameter :: t_max = 12.0_dp, decay = 0.25_dp, omega = 3.0_dp
    real(dp) :: t(n), envelope(n), y1(n), y2(n)
    integer :: i

    do i = 1, n
        t(i) = t_max*real(i - 1, dp)/real(n - 1, dp)
    end do

    envelope = exp(-decay*t)
    y1 = sin(omega*t)*envelope
    y2 = cos(omega*t)*envelope

! %%
! The envelope first, so the curves are drawn on top of it. :f:subr:`fill_between`
! takes the band between two curves; ``alpha`` makes it translucent.

    call fill_between(t, -envelope, envelope, color="gray", alpha=0.25_dp, &
                      label="envelope")

    call plot(t, y1, "b-", label="sin", lw=2.0_dp)
    call plot(t, y2, "r--", label="cos")
    call axhline(0.0_dp, color="k", lw=0.8_dp)

! %%
! Labels, a grid and a legend, exactly as in matplotlib.

    call title("Damped oscillation")
    call xlabel("time [s]")
    call ylabel("amplitude")
    call grid(.true.)
    call legend(loc="upper right")

! %%
! :f:subr:`savefig` picks the backend from the file extension. Ask for ``.svg`` and
! fplotlib writes SVG itself -- there is no external graphics library
! underneath. The same program would produce a bitmap by writing ``.png``, or
! print-ready output with ``.pdf`` or ``.eps``.

    call savefig("damped_oscillation.svg")

    print *, "wrote damped_oscillation.svg"

end program plot_damped_oscillation
