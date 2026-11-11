! Mathematical expressions
! ========================
!
! Every construct fplotlib's mathtext lays out. A pair of ``$`` signs
! switches into math and back, and text outside them is copied through
! unchanged. Letters in math slope; digits and punctuation stay
! upright. The glyphs are the ordinary text font.
!
! These commands have their own layout:
!
! * ``\frac{a}{b}`` draws a fraction, each half a script smaller.
! * ``\sqrt{x}`` draws a root over a braced argument.
! * ``\mathrm{text}`` sets its argument upright.
! * ``\alpha`` through ``\omega``, and ``\Gamma`` through ``\Omega``,
!   are the Greek letters. Lowercase slopes, uppercase stands upright.
!   ``\varepsilon``, ``\vartheta``, ``\varrho`` and ``\varphi`` are
!   accepted and drawn with the same glyph as the letter they abbreviate.
!   ``\varsigma`` is the final sigma, and it does differ from ``\sigma``.
! * ``\times`` is the multiplication sign.
! * ``\,`` inserts a thin space and ``\;`` a thick one. A backslash
!   followed by a space inserts a word space. A space typed between
!   symbols is dropped, and the layout puts its own space back around
!   an operator, so ``$a b$`` is the same as ``$ab$``.
!
! ``^`` and ``_`` raise and lower the next character, or the next group
! in braces. Several scripts in a row share one base, so ``x_{i}^{2}``
! hangs both of them on ``x``.
!
! Any other command is drawn as its own letters, upright, which is why
! ``\sin`` reads as the word sin and the math after it is unchanged. A
! braced group after such a command is set as math and the name is left
! out, so ``\hat{x}`` is the same as ``x``. Every ``$`` opens or closes
! a fragment.
!
! The backslash in these strings is a real character. That is the default
! for gfortran, flang and ifx. A compiler that treats ``\`` as an escape
! wants ``\\`` in the literal, or the character ``achar(92)``.

program plot_mathtext_examples
    use fplotlib, only: axis, dp, figure, savefig, text
    implicit none

    real(dp) :: y

    call figure(figsize=[12.0_dp, 11.0_dp])
    call axis("off")
    y = 0.97_dp

! %%
! Scripts and groups. The exponent and the index of one base can be
! written in either order.

    call sheet(y, "mixed with text", &
               "energy $E = mc^{2}$ at $T = 300\,\mathrm{K}$")
    call sheet(y, "scripts", &
               "$x^{2}$, $x_{i}$, $x_{i}^{2}$, $10^{-3} < T^{2}_{n} < 10^{3}$")
    call sheet(y, "groups", &
               "$x_{i+1}$, $(a+b)^{2}$, $e^{x^{2}}$, $e^{-\gamma t}$")

! %%
! Fractions and roots take their arguments in braces. A fraction nests,
! and a root can hold a fraction.

    call sheet(y, "fractions", &
               "$\frac{3}{4}$, $\frac{1}{2\pi}$, $\frac{a}{1+\frac{b}{c}}$, $e^{-\frac{t}{\tau}}$")
    call sheet(y, "roots", &
               "$\sqrt{2}$, $\sqrt{x^{2}+y^{2}}$, $2\pi\sqrt{\frac{L}{g}}$")

! %%
! Greek, by its TeX name. The second form of epsilon, theta, phi and rho
! is the same glyph; final sigma is not.

    call sheet(y, "lowercase Greek", &
               "$\alpha\ \beta\ \gamma\ \delta\ \epsilon\ \zeta\ \eta\ \theta\ \iota\ \kappa\ \lambda\ \mu$")
    call sheet(y, "", &
               "$\nu\ \xi\ \pi\ \rho\ \sigma\ \tau\ \upsilon\ \phi\ \chi\ \psi\ \omega$")
    call sheet(y, "uppercase Greek", &
               "$\Gamma\ \Delta\ \Theta\ \Lambda\ \Xi\ \Pi\ \Sigma\ \Upsilon\ \Phi\ \Psi\ \Omega$")
    call sheet(y, "variant names", &
               "$\epsilon=\varepsilon$, $\theta=\vartheta$, $\phi=\varphi$, $\rho=\varrho$, $\sigma\ \varsigma$")

! %%
! ``\mathrm`` sets the letters in its braces upright. ``\times`` and the
! three space commands are the other commands with their own drawing.
! ``\sin`` is not one of them: the word is drawn upright and what follows
! stays in math.

    call sheet(y, "roman", "$\mathrm{upright}$ and $sloped$")
    call sheet(y, "times", "$a \times b = c$")
    call sheet(y, "spacing", "$ab$, $a b$, $a\,b$, $a\;b$, $a\ b$")
    call sheet(y, "unknown command", "$\sin(2\pi f t)$, $\cos\theta$, $\log x$")

    call savefig("mathtext_examples.svg")
    print *, "wrote mathtext_examples.svg"

contains

    subroutine sheet(y, caption, formula)
        real(dp), intent(inout) :: y
        character(len=*), intent(in) :: caption, formula

        if (len_trim(caption) > 0) then
            call text(0.02_dp, y, caption, fontsize=12.0_dp, ha="left", va="center", &
                      transform="axes", color="dimgray")
        end if
        call text(0.30_dp, y, formula, fontsize=15.0_dp, ha="left", va="center", &
                  transform="axes")
        y = y - 0.068_dp
    end subroutine sheet

end program plot_mathtext_examples
