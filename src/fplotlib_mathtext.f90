! Mathtext layout.
!
! matplotlib lets any label contain a TeX fragment between dollar signs,
! and that is by far the most used piece of it: exponents in "$10^{3}$"
! and indices in "$x_i$". The layout is done here, once, and turned into
! a list of plain text runs with an offset and a size each. Every backend
! then draws the runs it already knows how to draw, so none of them needs
! to know what mathtext is.
!
! Only the layout is TeX. The glyphs are the ordinary text font, sloped
! for the letters of a math fragment and upright for everything else,
! which is TeX's own rule and matplotlib's.
module fplotlib_mathtext
    use fplotlib_glyphs, only: EM, XHEIGHT, glyph_advance, glyph_points
    use fplotlib_style, only: utf8_next, utf8_char
    implicit none
    private

    integer, parameter :: dp = kind(0.0d0)

    public :: mrun_t, math_layout, math_width, math_is

    ! One run of plain text: draw s(1:n) at size, offset by (dx, dy) from
    ! the origin of the whole string. dy grows downwards, as the canvas does.
    !
    ! A run with line = .true. is not text at all but a straight stroke of
    ! width lw from (dx, dy) to (x2, y2): the bar of a fraction and the
    ! radical of a root are drawn, not set, and there is no sensible glyph
    ! to borrow for either.
    integer, parameter, public :: MAX_RUNS = 64
    type :: mrun_t
        character(len=64) :: s = ""
        integer :: n = 0
        real(dp) :: dx = 0.0_dp
        real(dp) :: dy = 0.0_dp
        real(dp) :: size = 10.0_dp
        logical :: italic = .false.
        logical :: line = .false.
        real(dp) :: x2 = 0.0_dp
        real(dp) :: y2 = 0.0_dp
        real(dp) :: lw = 0.0_dp
    end type mrun_t

    ! matplotlib's mathtext constants: a script is 0.7 of its parent, a
    ! superscript sits 0.44 em above the baseline and a subscript 0.2 em
    ! below it, both measured in the parent's size.
    real(dp), parameter :: SCRIPT = 0.7_dp
    real(dp), parameter :: SUP_RISE = 0.44_dp
    real(dp), parameter :: SUB_DROP = 0.20_dp
    real(dp), parameter :: MIN_SIZE = 0.4_dp
    ! Fractions and roots, measured off matplotlib's own output at 20 pt
    ! and written here as fractions of the size. The bar of a fraction is
    ! RULE thick and sits AXIS above the baseline, which is TeX's axis
    ! height; the two halves are set a script smaller, PAD clear of the
    ! ends of the bar.
    real(dp), parameter :: RULE = 0.0625_dp
    real(dp), parameter :: AXIS = 0.2544_dp
    real(dp), parameter :: NUM_RISE = 0.3581_dp
    real(dp), parameter :: DEN_DROP = 0.3712_dp
    real(dp), parameter :: PAD = 0.0625_dp
    ! The root sign: how wide it is, how far it reaches above and below
    ! the baseline, and the gap it leaves around what is under it.
    real(dp), parameter :: ROOT_W = 0.62_dp
    real(dp), parameter :: ROOT_TOP = 1.08_dp
    real(dp), parameter :: ROOT_BOT = 0.22_dp
    real(dp), parameter :: ROOT_PAD = 0.125_dp
    ! TeX's math glue, in em. A mu is 1/18 em: thin is 3 mu, medium 4 mu
    ! and thick 5 mu. \, and \; are the same two widths written by hand.
    ! The medium space is what a binary operator takes, the thick space
    ! what a relation takes, and both are dropped in a script.
    real(dp), parameter :: THIN = 0.167_dp
    real(dp), parameter :: MED = 4.0_dp/18.0_dp
    real(dp), parameter :: THICK = 0.278_dp
    ! Matplotlib's DejaVu constants for where a script sits sideways.
    ! delta and script_space are fractions of the x-height; delta_slanted
    ! is a fraction of how far the last letter rises above two thirds of it.
    real(dp), parameter :: DELTA = 0.025_dp
    real(dp), parameter :: DELTA_SLANT = 0.2_dp
    real(dp), parameter :: SCRIPT_GAP = 0.05_dp
    ! Atom classes, in the order of TeX's spacing table. AT_NONE is "no
    ! atom yet", which is not a class: a binary operator in that position
    ! is set as ordinary, the way a leading minus is.
    integer, parameter :: AT_NONE = 0
    integer, parameter :: AT_ORD = 1
    integer, parameter :: AT_OP = 2
    integer, parameter :: AT_BIN = 3
    integer, parameter :: AT_REL = 4
    integer, parameter :: AT_OPEN = 5
    integer, parameter :: AT_CLOSE = 6
    integer, parameter :: AT_PUNCT = 7
    integer, parameter :: AT_INNER = 8
    ! Spelled this way because a backslash inside a string literal is not
    ! portable: some compilers read it as the start of an escape.
    character, parameter :: BS = achar(92)

    ! The atom the next one is spaced against, and the last character of
    ! the nucleus a script would attach to.
    type :: math_st
        integer :: atom = AT_NONE
        integer :: code = 0
        logical :: slanted = .false.
        ! 0 is text style. 1 and 2 are script and scriptscript, where TeX
        ! drops the spaces that its table writes in parentheses.
        integer :: style = 0
    end type math_st

contains

    ! Is there anything in this string for the mathtext engine to do?
    pure function math_is(s) result(yes)
        character(len=*), intent(in) :: s
        logical :: yes
        integer :: i
        yes = .false.
        do i = 1, len(s)
            if (s(i:i) == "$") then
                yes = .true.
                return
            end if
        end do
    end function math_is

    ! Width of a string in points, mathtext or not.
    function math_width(s, size) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        real(dp) :: w
        type(mrun_t) :: runs(MAX_RUNS)
        integer :: nr
        call math_layout(s, size, runs, nr, w)
    end function math_width

    ! Lay a string out into runs. Text outside the dollar signs is copied
    ! through unchanged, so a string with no mathtext in it comes back as
    ! a single run and costs nothing.
    subroutine math_layout(s, size, runs, nr, width)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        type(mrun_t), intent(out) :: runs(MAX_RUNS)
        integer, intent(out) :: nr
        real(dp), intent(out) :: width
        integer :: i, j
        real(dp) :: pen, xnew

        nr = 0
        pen = 0.0_dp
        i = 1
        do while (i <= len(s))
            if (s(i:i) == "$") then
                j = i + 1
                do while (j <= len(s))
                    if (s(j:j) == "$") exit
                    j = j + 1
                end do
                if (j > i + 1) then
                    call layout_math(s(i + 1:j - 1), size, pen, 0.0_dp, &
                                     runs, nr, xnew)
                    pen = xnew
                end if
                i = j + 1
            else
                j = i
                do while (j <= len(s))
                    if (s(j:j) == "$") exit
                    j = j + 1
                end do
                call emit(s(i:j - 1), size, pen, 0.0_dp, runs, nr, xnew)
                pen = xnew
                i = j
            end if
        end do
        width = pen
    end subroutine math_layout

    ! ------------------------------------------------------------------
    ! The math mode itself
    ! ------------------------------------------------------------------

    ! Lay out one math fragment starting at pen x = x0, baseline y = y0.
    ! xend comes back as the pen position after the fragment.
    recursive subroutine layout_math(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        ! \mathrm and its relatives turn the sloping off for their group.
        logical, intent(in), optional :: math
        type(math_st) :: st
        logical :: mm

        mm = .true.
        if (present(math)) mm = math
        call layout_list(s, size, x0, y0, runs, nr, xend, mm, st)
    end subroutine layout_math

    ! The list itself. Spaces are dropped, each symbol is an atom, and
    ! the glue between two atoms is TeX's: thick around a relation, medium
    ! around a binary operator, thin around an operator name and after a
    ! punctuation mark. A braced group is one ordinary atom, whatever is
    ! inside it.
    recursive subroutine layout_list(s, size, x0, y0, runs, nr, xend, math, st)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: math
        type(math_st), intent(inout) :: st
        type(math_st) :: inner
        real(dp) :: pen, smax, sx, ssize, superk, subk
        integer :: i, j, code, nb, nstyle
        logical :: placed

        pen = x0
        i = 1
        do while (i <= len(s))
            select case (s(i:i))
            case (" ", achar(9))
                ! TeX throws a typed space away and puts its own back
                ! from the atom classes. Keeping the typed one is what
                ! made "E = mc^2" depend on where the space happened to sit.
                i = i + 1
            case ("^", "_")
                ! The scripts share the nucleus. A slanted letter pushes
                ! the superscript out by its italic correction and leaves
                ! the subscript on the advance; an upright nucleus shifts
                ! both by a hair. The following atom starts past both,
                ! plus the small gap matplotlib leaves after a script.
                ssize = max(size*SCRIPT, MIN_SIZE*size)
                call script_kern(st%code, st%slanted, size, superk, subk)
                nstyle = min(st%style + 1, 2)
                smax = pen
                placed = .false.
                do
                    call unit_end(s, i + 1, j)
                    if (j > i) then
                        if (s(i:i) == "^") then
                            call layout_unit(s(i + 1:j), ssize, pen + superk, &
                                             y0 - SUP_RISE*size, runs, nr, sx, &
                                             math, nstyle)
                        else
                            call layout_unit(s(i + 1:j), ssize, pen + subk, &
                                             y0 + SUB_DROP*size, runs, nr, sx, &
                                             math, nstyle)
                        end if
                        smax = max(smax, sx)
                        placed = .true.
                    end if
                    i = j + 1
                    if (i > len(s)) exit
                    if (s(i:i) /= "^" .and. s(i:i) /= "_") exit
                end do
                if (placed) pen = smax + SCRIPT_GAP*(XHEIGHT/EM)*size
            case ("{")
                call group_end(s, i, j)
                call begin_atom(AT_ORD, size, pen, st)
                call fresh_state(inner, st%style)
                call layout_list(s(i + 1:j - 1), size, pen, y0, runs, nr, sx, &
                                 math, inner)
                pen = sx
                st%code = inner%code
                st%slanted = inner%slanted
                i = j + 1
            case (BS)
                call command_end(s, i, j)
                call layout_command(s(i:j), size, pen, y0, runs, nr, math, st)
                i = j + 1
            case ("}")
                i = i + 1
            case default
                call utf8_next(s, i, code, nb)
                call begin_atom(atom_kind(code), size, pen, st)
                call emit_run(s(i:i + nb - 1), size, pen, y0, runs, nr, sx, &
                              is_slanted(code, math))
                pen = sx
                st%code = code
                st%slanted = is_slanted(code, math)
                i = i + nb
            end select
        end do
        xend = pen
    end subroutine layout_list

    ! One unit: a group, a command, or a single character. A script is a
    ! fresh list in the next style down, so its atoms do not see the nucleus.
    recursive subroutine layout_unit(s, size, x0, y0, runs, nr, xend, math, style)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: math
        integer, intent(in) :: style
        type(math_st) :: inner
        integer :: j

        call fresh_state(inner, style)
        if (len(s) == 0) then
            xend = x0
        else if (s(1:1) == "{") then
            call group_end(s, 1, j)
            call layout_list(s(2:j - 1), size, x0, y0, runs, nr, xend, math, inner)
        else
            call layout_list(s, size, x0, y0, runs, nr, xend, math, inner)
        end if
    end subroutine layout_unit

    ! The handful of commands worth having without a math font: the
    ! spaces, and the wrappers that only change the style of a group.
    recursive subroutine layout_command(s, size, pen, y0, runs, nr, math, st)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, y0
        real(dp), intent(inout) :: pen
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        logical, intent(in) :: math
        type(math_st), intent(inout) :: st
        type(math_st) :: inner
        real(dp) :: sx
        integer :: j, name

        if (len(s) < 2) return
        ! An explicit skip is not an atom: the next symbol is still spaced
        ! against whatever came before the skip.
        select case (s(2:2))
        case (",")
            pen = pen + THIN*size
            return
        case (";")
            pen = pen + THICK*size
            return
        case (" ")
            pen = pen + 0.333_dp*size
            return
        case ("$")
            call begin_atom(AT_ORD, size, pen, st)
            call emit_run("$", size, pen, y0, runs, nr, sx, .false.)
            pen = sx
            st%code = iachar("$")
            st%slanted = .false.
            return
        end select

        name = 0
        if (cmd_name(s) == "times") name = 215
        if (name == 0) name = greek_code(cmd_name(s))
        if (name > 0) then
            ! \times is a binary operator. A greek letter is ordinary:
            ! lowercase slopes and uppercase stands upright.
            if (name == 215) then
                call begin_atom(AT_BIN, size, pen, st)
            else
                call begin_atom(AT_ORD, size, pen, st)
            end if
            call emit_run(utf8_char(name), size, pen, y0, runs, nr, sx, &
                          name >= 945 .and. name /= 215)
            pen = sx
            st%code = name
            st%slanted = name >= 945 .and. name /= 215
            return
        end if
        if (cmd_name(s) == "frac") then
            ! A fraction is an inner atom, so text style puts a thin space
            ! between it and an ordinary symbol beside it.
            call begin_atom(AT_INNER, size, pen, st)
            call layout_frac(s, size, pen, y0, runs, nr, sx, math, st%style)
            pen = sx
            st%code = 0
            st%slanted = .false.
            return
        end if
        if (cmd_name(s) == "sqrt") then
            ! A radical is spaced as an ordinary atom.
            call begin_atom(AT_ORD, size, pen, st)
            call layout_sqrt(s, size, pen, y0, runs, nr, sx, math, st%style)
            pen = sx
            st%code = 0
            st%slanted = .false.
            return
        end if

        ! \mathrm{...} sets its group upright. Any other command with a
        ! group drops the name and sets the group, so \hat{x} is x.
        ! A name with no group is an operator: \sin, \cos, \log.
        j = index(s, "{")
        if (j > 0) then
            call begin_atom(AT_ORD, size, pen, st)
            call fresh_state(inner, st%style)
            call layout_list(s(j + 1:len(s) - 1), size, pen, y0, runs, nr, sx, &
                             math .and. s(2:j - 1) /= "mathrm", inner)
            pen = sx
            st%code = inner%code
            st%slanted = inner%slanted
        else
            call begin_atom(AT_OP, size, pen, st)
            call emit_run(s(2:), size, pen, y0, runs, nr, sx, .false.)
            pen = sx
            st%code = iachar(s(len(s):len(s)))
            st%slanted = .false.
        end if
    end subroutine layout_command

    ! \frac{a}{b}: the two halves a script smaller, each centred over the
    ! bar. They are laid out at the left edge and then slid across, which
    ! is the only way to centre something whose width is not known until
    ! it has been laid out.
    recursive subroutine layout_frac(s, size, x0, y0, runs, nr, xend, math, style)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: math
        integer, intent(in) :: style
        type(math_st) :: inner
        integer :: n0, n1, d0, d1, kn, kd, nnum
        real(dp) :: ssize, xl, xn, xd, wn, wd, w, yb
        real(dp) :: ytop, ybot, gap, need, edge
        logical :: ink

        call cmd_arg(s, 1, n0, n1)
        call cmd_arg(s, 2, d0, d1)
        ssize = max(size*SCRIPT, MIN_SIZE*size)
        xl = x0 + PAD*size
        ! The halves are one style down, so a plus in a denominator of a
        ! text-style fraction is in script style and takes no medium space.
        call fresh_state(inner, min(style + 1, 2))

        kn = nr + 1
        call layout_list(s(n0:n1), ssize, xl, y0 - NUM_RISE*size, runs, nr, xn, &
                         math, inner)
        wn = xn - xl
        nnum = nr
        call fresh_state(inner, min(style + 1, 2))
        kd = nr + 1
        call layout_list(s(d0:d1), ssize, xl, y0 + DEN_DROP*size, runs, nr, xd, &
                         math, inner)
        wd = xd - xl

        w = max(wn, wd)
        call shift_runs(runs, kn, nnum, 0.5_dp*(w - wn))
        call shift_runs(runs, kd, nr, 0.5_dp*(w - wd))

        ! The fixed rise and drop were measured for a single character.
        ! A nested fraction is taller than that, so open the gap until the
        ! ink clears the bar by twice the rule, and leave a short fraction
        ! where the fixed position already put it.
        yb = y0 - AXIS*size
        need = 2.0_dp*RULE*size
        if (nnum >= kn) then
            call runs_ink(runs, kn, nnum, ytop, ybot, ink)
            edge = yb - 0.5_dp*RULE*size
            if (ink) then
                gap = edge - ybot
                if (gap < need) call shift_y(runs, kn, nnum, gap - need)
            end if
        end if
        if (nr >= kd) then
            call runs_ink(runs, kd, nr, ytop, ybot, ink)
            edge = yb + 0.5_dp*RULE*size
            if (ink) then
                gap = ytop - edge
                if (gap < need) call shift_y(runs, kd, nr, need - gap)
            end if
        end if

        call emit_line(xl, yb, xl + w, yb, RULE*size, runs, nr)
        xend = xl + w + PAD*size
    end subroutine layout_frac

    ! \sqrt{a}: the sign is drawn rather than set, since the text font has
    ! no radical in it, and the bar over it is one more stroke.
    recursive subroutine layout_sqrt(s, size, x0, y0, runs, nr, xend, math, style)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: math
        integer, intent(in) :: style
        type(math_st) :: inner
        integer :: a0, a1
        real(dp) :: top, bot, wr, th, xb, xe

        call cmd_arg(s, 1, a0, a1)
        th = RULE*size
        wr = ROOT_W*size
        top = y0 - ROOT_TOP*size
        bot = y0 + ROOT_BOT*size
        xb = x0 + wr + ROOT_PAD*size
        ! The radicand stays in the surrounding style: \sqrt{a+b} keeps
        ! the medium space around the plus.
        call fresh_state(inner, style)
        call layout_list(s(a0:a1), size, xb, y0, runs, nr, xe, math, inner)

        ! The sign itself: a short rise to the foot, a long rise to the
        ! top, then the bar across whatever is under the root.
        call emit_line(x0, y0 - 0.35_dp*size, x0 + 0.24_dp*wr, bot, th, runs, nr)
        call emit_line(x0 + 0.24_dp*wr, bot, x0 + wr, top, th, runs, nr)
        call emit_line(x0 + wr, top, xe + ROOT_PAD*size, top, th, runs, nr)
        xend = xe + ROOT_PAD*size
    end subroutine layout_sqrt

    ! Slide runs k0..k1 across by dx, once their width is known.
    pure subroutine shift_runs(runs, k0, k1, dx)
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(in) :: k0, k1
        real(dp), intent(in) :: dx
        integer :: k
        do k = max(k0, 1), min(k1, MAX_RUNS)
            runs(k)%dx = runs(k)%dx + dx
            runs(k)%x2 = runs(k)%x2 + dx
        end do
    end subroutine shift_runs

    ! Slide runs k0..k1 down by dy. dy grows downwards, with the canvas.
    pure subroutine shift_y(runs, k0, k1, dy)
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(in) :: k0, k1
        real(dp), intent(in) :: dy
        integer :: k
        do k = max(k0, 1), min(k1, MAX_RUNS)
            runs(k)%dy = runs(k)%dy + dy
            runs(k)%y2 = runs(k)%y2 + dy
        end do
    end subroutine shift_y

    pure subroutine fresh_state(st, style)
        type(math_st), intent(out) :: st
        integer, intent(in) :: style
        st%atom = AT_NONE
        st%code = 0
        st%slanted = .false.
        st%style = style
    end subroutine fresh_state

    ! Glue before an atom, and the class it leaves behind. A binary
    ! operator with nothing, or with an operator, relation, opening or
    ! punctuation before it, is ordinary: that is the unary minus.
    pure subroutine begin_atom(kind, size, pen, st)
        integer, intent(in) :: kind
        real(dp), intent(in) :: size
        real(dp), intent(inout) :: pen
        type(math_st), intent(inout) :: st
        integer :: k

        k = kind
        if (k == AT_BIN) then
            if (st%atom == AT_NONE .or. st%atom == AT_BIN .or. st%atom == AT_OP &
                .or. st%atom == AT_REL .or. st%atom == AT_OPEN &
                .or. st%atom == AT_PUNCT) k = AT_ORD
        end if
        if (st%atom /= AT_NONE) pen = pen + muskip(st%atom, k, st%style)*size
        st%atom = k
    end subroutine begin_atom

    ! TeX's spacing table. The parenthesised entries are the ones that
    ! exist only in text style; a script returns zero for those.
    pure function muskip(left, right, style) result(w)
        integer, intent(in) :: left, right, style
        real(dp) :: w
        integer :: kind
        logical :: text_only

        w = 0.0_dp
        kind = 0
        text_only = .false.
        select case (left)
        case (AT_ORD)
            select case (right)
            case (AT_OP); kind = 1
            case (AT_BIN); kind = 2; text_only = .true.
            case (AT_REL); kind = 3; text_only = .true.
            case (AT_INNER); kind = 1; text_only = .true.
            end select
        case (AT_OP)
            select case (right)
            case (AT_ORD, AT_OP); kind = 1
            case (AT_REL); kind = 3; text_only = .true.
            case (AT_INNER); kind = 1; text_only = .true.
            end select
        case (AT_BIN)
            select case (right)
            case (AT_ORD, AT_OP, AT_OPEN, AT_INNER)
                kind = 2
                text_only = .true.
            end select
        case (AT_REL)
            select case (right)
            case (AT_ORD, AT_OP, AT_OPEN, AT_INNER)
                kind = 3
                text_only = .true.
            end select
        case (AT_CLOSE)
            select case (right)
            case (AT_OP); kind = 1
            case (AT_BIN); kind = 2; text_only = .true.
            case (AT_REL); kind = 3; text_only = .true.
            case (AT_INNER); kind = 1; text_only = .true.
            end select
        case (AT_PUNCT)
            kind = 1
            text_only = .true.
        case (AT_INNER)
            select case (right)
            case (AT_OP); kind = 1
            case (AT_ORD, AT_OPEN, AT_PUNCT, AT_INNER)
                kind = 1
                text_only = .true.
            case (AT_BIN); kind = 2; text_only = .true.
            case (AT_REL); kind = 3; text_only = .true.
            end select
        end select
        if (kind == 0) return
        if (text_only .and. style > 0) return
        if (kind == 1) w = THIN
        if (kind == 2) w = MED
        if (kind == 3) w = THICK
    end function muskip

    pure integer function atom_kind(code) result(k)
        integer, intent(in) :: code
        select case (code)
        case (iachar("+"), iachar("-"), iachar("*"), 215, 8722)
            k = AT_BIN
        case (iachar("="), iachar("<"), iachar(">"), iachar(":"))
            k = AT_REL
        case (iachar("("), iachar("["))
            k = AT_OPEN
        case (iachar(")"), iachar("]"))
            k = AT_CLOSE
        case (iachar(","), iachar(";"))
            k = AT_PUNCT
        case default
            k = AT_ORD
        end select
    end function atom_kind

    ! ASCII letters and lowercase greek slope. Digits, uppercase greek
    ! and the multiplication sign stay upright.
    pure logical function is_slanted(code, math) result(yes)
        integer, intent(in) :: code
        logical, intent(in) :: math
        yes = math .and. ((code >= iachar("A") .and. code <= iachar("Z")) .or. &
                          (code >= iachar("a") .and. code <= iachar("z")) .or. &
                          (code >= 945 .and. code <= 969))
    end function is_slanted

    ! Horizontal offset of a superscript and a subscript from the
    ! nucleus advance. Matplotlib's rule for DejaVu, which has no
    ! italic-correction field of its own: an upright nucleus moves both
    ! scripts by delta times the x-height, and a slanted one moves only
    ! the superscript, by twice that plus a share of the letter's height.
    subroutine script_kern(code, slanted, size, superk, subk)
        integer, intent(in) :: code
        logical, intent(in) :: slanted
        real(dp), intent(in) :: size
        real(dp), intent(out) :: superk, subk
        real(dp) :: xh, h

        xh = XHEIGHT/EM*size
        superk = DELTA*xh
        subk = DELTA*xh
        if (.not. slanted) return
        h = xh
        if (code > 0) h = glyph_ascent(code)/EM*size
        superk = superk + DELTA*xh + DELTA_SLANT*(h - xh*2.0_dp/3.0_dp)
        if (superk < 0.0_dp) superk = 0.0_dp
        subk = 0.0_dp
    end subroutine script_kern

    ! How far a glyph rises above the baseline, in font units.
    function glyph_ascent(code) result(h)
        integer, intent(in) :: code
        real(dp) :: h
        real(dp), allocatable :: x(:), y(:)
        integer :: n

        call glyph_points(code, x, y, n)
        if (n <= 0) then
            h = XHEIGHT
        else
            h = maxval(y(1:n))
            if (h <= 0.0_dp) h = XHEIGHT
        end if
    end function glyph_ascent

    ! Ink of runs k0..k1, in canvas coordinates (y grows downwards).
    subroutine runs_ink(runs, k0, k1, ytop, ybot, any)
        type(mrun_t), intent(in) :: runs(MAX_RUNS)
        integer, intent(in) :: k0, k1
        real(dp), intent(out) :: ytop, ybot
        logical, intent(out) :: any
        integer :: k
        real(dp) :: t, b
        logical :: one

        any = .false.
        ytop = 0.0_dp
        ybot = 0.0_dp
        do k = max(k0, 1), min(k1, MAX_RUNS)
            call run_ink(runs(k), t, b, one)
            if (.not. one) cycle
            if (.not. any) then
                ytop = t
                ybot = b
                any = .true.
            else
                ytop = min(ytop, t)
                ybot = max(ybot, b)
            end if
        end do
    end subroutine runs_ink

    subroutine run_ink(run, ytop, ybot, any)
        type(mrun_t), intent(in) :: run
        real(dp), intent(out) :: ytop, ybot
        logical, intent(out) :: any
        real(dp), allocatable :: x(:), y(:)
        integer :: i, n, code, nb
        real(dp) :: sc, ymin, ymax

        ymin = 0.0_dp
        ymax = 0.0_dp
        any = .false.
        ytop = run%dy
        ybot = run%dy
        if (run%line) then
            ytop = min(run%dy, run%y2) - 0.5_dp*run%lw
            ybot = max(run%dy, run%y2) + 0.5_dp*run%lw
            any = .true.
            return
        end if
        if (run%n <= 0) return
        sc = run%size/EM
        i = 1
        do while (i <= run%n)
            call utf8_next(run%s, i, code, nb)
            i = i + nb
            call glyph_points(code, x, y, n)
            if (n <= 0) cycle
            if (.not. any) then
                ymin = minval(y(1:n))
                ymax = maxval(y(1:n))
                any = .true.
            else
                ymin = min(ymin, minval(y(1:n)))
                ymax = max(ymax, maxval(y(1:n)))
            end if
        end do
        if (.not. any) return
        ytop = run%dy - ymax*sc
        ybot = run%dy - ymin*sc
    end subroutine run_ink

    subroutine emit_line(x1, y1, x2, y2, lw, runs, nr)
        real(dp), intent(in) :: x1, y1, x2, y2, lw
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        if (nr >= MAX_RUNS) return
        nr = nr + 1
        runs(nr)%n = 0
        runs(nr)%line = .true.
        runs(nr)%dx = x1
        runs(nr)%dy = y1
        runs(nr)%x2 = x2
        runs(nr)%y2 = y2
        runs(nr)%lw = lw
    end subroutine emit_line


    ! The letters of the command that s starts with, without the backslash.
    pure function cmd_name(s) result(name)
        character(len=*), intent(in) :: s
        character(len=16) :: name
        integer :: j
        name = ""
        j = 2
        do while (j <= len(s))
            if (.not. is_alpha(s(j:j))) exit
            j = j + 1
        end do
        if (j > 2 .and. j - 2 <= len(name)) name = s(2:j - 1)
    end function cmd_name

    ! The k-th braced argument of a command, empty when there is no such
    ! argument. i0:i1 is the text inside the braces.
    pure subroutine cmd_arg(s, k, i0, i1)
        character(len=*), intent(in) :: s
        integer, intent(in) :: k
        integer, intent(out) :: i0, i1
        integer :: i, j, seen
        i0 = 1
        i1 = 0
        seen = 0
        i = 2
        do while (i <= len(s))
            if (s(i:i) == "{") then
                call group_end(s, i, j)
                seen = seen + 1
                if (seen == k) then
                    i0 = i + 1
                    i1 = j - 1
                    return
                end if
                i = j + 1
            else
                i = i + 1
            end if
        end do
    end subroutine cmd_arg

    ! End of the group that starts at s(i:i) == "{".
    pure subroutine group_end(s, i, j)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        integer, intent(out) :: j
        integer :: depth
        depth = 0
        do j = i, len(s)
            if (s(j:j) == "{") depth = depth + 1
            if (s(j:j) == "}") then
                depth = depth - 1
                if (depth == 0) return
            end if
        end do
        j = len(s)
    end subroutine group_end

    ! End of the command that starts at s(i:i) == "\". A command is the
    ! backslash plus its letters, plus a braced argument if there is one.
    pure subroutine command_end(s, i, j)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        integer, intent(out) :: j
        integer :: k, a, nargs
        if (i + 1 > len(s)) then
            j = i
            return
        end if
        if (.not. is_alpha(s(i + 1:i + 1))) then
            j = i + 1
            return
        end if
        j = i + 1
        do while (j < len(s))
            if (.not. is_alpha(s(j + 1:j + 1))) exit
            j = j + 1
        end do
        ! One braced argument, or two for the commands that take two.
        nargs = 1
        if (s(i + 1:j) == "frac") nargs = 2
        do a = 1, nargs
            if (j >= len(s)) exit
            if (s(j + 1:j + 1) /= "{") exit
            call group_end(s, j + 1, k)
            j = k
        end do
    end subroutine command_end

    ! End of the unit that starts at index i, for a script argument.
    pure subroutine unit_end(s, i, j)
        character(len=*), intent(in) :: s
        integer, intent(in) :: i
        integer, intent(out) :: j
        if (i > len(s)) then
            j = i - 1
        else if (s(i:i) == "{") then
            call group_end(s, i, j)
        else if (s(i:i) == BS) then
            call command_end(s, i, j)
        else
            j = i
        end if
    end subroutine unit_end

    pure function is_alpha(c) result(yes)
        character, intent(in) :: c
        logical :: yes
        yes = (c >= "a" .and. c <= "z") .or. (c >= "A" .and. c <= "Z")
    end function is_alpha

    ! The greek letters, by their TeX names. Everything else is left to
    ! degrade to its own name, which at least shows what was asked for.
    pure function greek_code(name) result(code)
        character(len=*), intent(in) :: name
        integer :: code
        select case (name)
        case ("alpha"); code = 945
        case ("beta"); code = 946
        case ("gamma"); code = 947
        case ("delta"); code = 948
        case ("epsilon", "varepsilon"); code = 949
        case ("zeta"); code = 950
        case ("eta"); code = 951
        case ("theta", "vartheta"); code = 952
        case ("iota"); code = 953
        case ("kappa"); code = 954
        case ("lambda"); code = 955
        case ("mu"); code = 956
        case ("nu"); code = 957
        case ("xi"); code = 958
        case ("pi"); code = 960
        case ("rho", "varrho"); code = 961
        case ("varsigma"); code = 962
        case ("sigma"); code = 963
        case ("tau"); code = 964
        case ("upsilon"); code = 965
        case ("phi", "varphi"); code = 966
        case ("chi"); code = 967
        case ("psi"); code = 968
        case ("omega"); code = 969
        case ("Gamma"); code = 915
        case ("Delta"); code = 916
        case ("Theta"); code = 920
        case ("Lambda"); code = 923
        case ("Xi"); code = 926
        case ("Pi"); code = 928
        case ("Sigma"); code = 931
        case ("Upsilon"); code = 933
        case ("Phi"); code = 934
        case ("Psi"); code = 936
        case ("Omega"); code = 937
        ! The multiplication sign, which log tick labels are written with.
        case ("times"); code = 215
        case default; code = 0
        end select
    end function greek_code

    ! ------------------------------------------------------------------
    ! Runs
    ! ------------------------------------------------------------------

    ! In math mode the letters slope and everything else stays upright, so
    ! the text is broken at every change of class and each piece emitted on
    ! its own. Outside math mode it is one run, as it always was.
    subroutine emit(s, size, x0, y0, runs, nr, xend, math)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in), optional :: math
        logical :: mm
        real(dp) :: pen, nxt
        integer :: i, j

        mm = .false.
        if (present(math)) mm = math
        if (.not. mm) then
            call emit_run(s, size, x0, y0, runs, nr, xend, .false.)
            return
        end if
        pen = x0
        i = 1
        do while (i <= len(s))
            j = i
            do while (j < len(s))
                if (is_alpha(s(j + 1:j + 1)) .neqv. is_alpha(s(i:i))) exit
                j = j + 1
            end do
            call emit_run(s(i:j), size, pen, y0, runs, nr, nxt, is_alpha(s(i:i)))
            pen = nxt
            i = j + 1
        end do
        xend = pen
    end subroutine emit

    subroutine emit_run(s, size, x0, y0, runs, nr, xend, italic)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size, x0, y0
        type(mrun_t), intent(inout) :: runs(MAX_RUNS)
        integer, intent(inout) :: nr
        real(dp), intent(out) :: xend
        logical, intent(in) :: italic
        integer :: n

        xend = x0 + run_width(s, size)
        n = min(len(s), 64)
        if (n <= 0) return
        ! Letters of one atom class sit against each other, so they can
        ! share a run. A glue gap leaves the pen short of x0 and starts
        ! a new one. The tests are nested because .and. does not
        ! short-circuit, and runs(nr) is not there when nr is 0.
        if (nr > 0) then
            if (.not. runs(nr)%line .and. runs(nr)%italic .eqv. italic &
                .and. abs(runs(nr)%dy - y0) <= 1.0e-9_dp &
                .and. abs(runs(nr)%size - size) <= 1.0e-9_dp &
                .and. runs(nr)%n + n <= 64) then
                if (abs(runs(nr)%dx + run_width(runs(nr)%s(1:runs(nr)%n), size) &
                        - x0) <= 1.0e-4_dp) then
                    runs(nr)%s(runs(nr)%n + 1:runs(nr)%n + n) = s(1:n)
                    runs(nr)%n = runs(nr)%n + n
                    return
                end if
            end if
        end if
        if (nr >= MAX_RUNS) return
        nr = nr + 1
        runs(nr)%s = s(1:n)
        runs(nr)%n = n
        runs(nr)%dx = x0
        runs(nr)%dy = y0
        runs(nr)%size = size
        runs(nr)%italic = italic
        runs(nr)%line = .false.
    end subroutine emit_run

    function run_width(s, size) result(w)
        character(len=*), intent(in) :: s
        real(dp), intent(in) :: size
        real(dp) :: w
        integer :: i, code, nb
        w = 0.0_dp
        i = 1
        do while (i <= len(s))
            call utf8_next(s, i, code, nb)
            w = w + glyph_advance(code)
            i = i + nb
        end do
        w = w*size/EM
    end function run_width

end module fplotlib_mathtext
