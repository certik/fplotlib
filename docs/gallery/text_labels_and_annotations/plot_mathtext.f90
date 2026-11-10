! Math in labels
! ==============
!
! Any text fplotlib draws can hold a math fragment between ``$`` signs:
! :f:subr:`title`, :f:subr:`suptitle`, :f:subr:`xlabel`, :f:subr:`ylabel`,
! a legend entry, :f:subr:`text` and :f:subr:`annotate`. Characters outside
! the dollars are ordinary text, so a unit or a word sits next to a symbol
! in the same string.
!
! A log axis writes each decade as ``$10^{n}$`` on its own, and a linear
! tick that lands on a power of ten at ``1e4`` or above, or at ``1e-3`` or
! below, does the same. The second panel is only there to show that.
!
! The constructs themselves, and the commands the layout accepts, are in
! ``plot_mathtext_examples.f90``.

program plot_mathtext
    use fplotlib, only: annotate, constrained_layout, dp, figure, grid, legend, &
                        plot, savefig, semilogy, subplot, suptitle, text, title, &
                        xlabel, ylabel
    implicit none

    integer, parameter :: n = 400
    real(dp), parameter :: pi = 3.14159265358979323846_dp
    real(dp), parameter :: gamma = 0.6_dp, omega = 5.0_dp
    real(dp) :: t(n), env(n), y(n), tl(n), yl(n)
    real(dp) :: tp, yp
    integer :: i

    do i = 1, n
        t(i) = 4.0_dp*real(i - 1, dp)/real(n - 1, dp)
        env(i) = exp(-gamma*t(i))
        y(i) = env(i)*sin(omega*t(i))
        tl(i) = -1.0_dp + 4.0_dp*real(i - 1, dp)/real(n - 1, dp)
        yl(i) = 10.0_dp**tl(i)
    end do

    call figure(figsize=[9.0_dp, 8.0_dp])
    call constrained_layout(.true.)
    call suptitle("mathtext in titles, labels and annotations", fontsize=13.0_dp)

! %%
! The title, the axis labels and the legend entries are plain Fortran
! strings. ``\gamma`` and ``\omega`` are Greek letters, ``^`` and ``_``
! raise and lower the next character, and the words stay outside the
! dollars.

    call subplot(2, 1, 1)
    call plot(t, y, "C0-", lw=2.0_dp, label="$e^{-\gamma t}\sin\omega t$")
    call plot(t, env, "C1--", label="$e^{-\gamma t}$")
    call plot(t, -env, "C1--")
    call title("$A(t)$ for $\omega = 5$, $\gamma = 0.6$")
    call xlabel("time $t$ [s]")
    call ylabel("amplitude $A$")
    call grid(.true., alpha=0.3_dp)
    call legend(loc="upper right")

    tp = pi/(2.0_dp*omega)
    yp = exp(-gamma*tp)
    call annotate("$t = \frac{\pi}{2\omega}$", tp, yp, xtext=1.2_dp, ytext=0.72_dp, &
                  arrowstyle="->", fontsize=12.0_dp)
    call text(2.15_dp, -0.55_dp, "envelope $e^{-\gamma t}$", fontsize=11.0_dp)

! %%
! Decades on a log axis are mathtext too. The label is written by the
! tick formatter, so the program never builds the ``$10^{n}$`` string.

    call subplot(2, 1, 2)
    call semilogy(tl, yl, "C0-", lw=2.0_dp, label="$y = 10^{t}$")
    call title("a log axis labels the decades as $10^{n}$")
    call xlabel("$t$")
    call ylabel("$y$")
    call grid(.true., which="both", alpha=0.3_dp)
    call legend(loc="upper left")

    call savefig("mathtext.svg")
    print *, "wrote mathtext.svg"

end program plot_mathtext
