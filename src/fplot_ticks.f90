! Nice tick generation for linear and log axes.
module fplot_ticks
    use fplot_style, only: dp
    implicit none
    private

    integer, parameter, public :: MAX_TICKS = 32

    public :: linear_ticks
    public :: nice_number
    public :: log_ticks
    public :: symlog_ticks
    public :: format_tick_to, format_log_minor_to, log_minor_labelled
    public :: tick_decimals
    public :: format_tick_fixed
    public :: tick_offset, tick_decimals_at, format_offset_text
    public :: multiple_ticks, format_percent, format_grouped

contains

    pure function nice_number(x, round_up) result(nice)
        real(dp), intent(in) :: x
        logical, intent(in) :: round_up
        real(dp) :: nice
        real(dp) :: expn, f, nf

        if (x <= 0.0_dp) then
            nice = 1.0_dp
            return
        end if

        expn = floor(log10(x))
        f = x / (10.0_dp ** expn)

        if (round_up) then
            if (f < 1.5_dp) then
                nf = 1.0_dp
            else if (f < 3.0_dp) then
                nf = 2.0_dp
            else if (f < 7.0_dp) then
                nf = 5.0_dp
            else
                nf = 10.0_dp
            end if
        else
            if (f <= 1.0_dp) then
                nf = 1.0_dp
            else if (f <= 2.0_dp) then
                nf = 2.0_dp
            else if (f <= 5.0_dp) then
                nf = 5.0_dp
            else
                nf = 10.0_dp
            end if
        end if

        nice = nf * (10.0_dp ** expn)
    end function nice_number

    ! matplotlib's MaxNLocator, which is what an axis gets by default.
    ! The steps it will accept are 1, 2, 2.5, 5 and 10 times a power of
    ! ten; of those it takes the smallest that covers the range in nbins
    ! intervals, and then backs off to a larger one only if the smaller
    ! leaves fewer than two ticks inside the view.
    subroutine linear_ticks(vmin, vmax, nbins, ticks, n_ticks)
        real(dp), intent(in) :: vmin, vmax
        integer, intent(in) :: nbins
        real(dp), intent(out) :: ticks(MAX_TICKS)
        integer, intent(out) :: n_ticks
        ! The steps, one decade below and one above, as matplotlib's
        ! _staircase builds them.
        real(dp), parameter :: STEPS(10) = [ &
            0.1_dp, 0.2_dp, 0.25_dp, 0.5_dp, &
            1.0_dp, 2.0_dp, 2.5_dp, 5.0_dp, 10.0_dp, 20.0_dp]
        real(dp) :: lo, hi, scal, off, raw, step, base, t
        real(dp) :: v0, v1
        integer :: nb, i, k, first, low, high, n, inside

        lo = min(vmin, vmax)
        hi = max(vmin, vmax)
        if (abs(hi - lo) < 1.0e-30_dp*max(1.0_dp, abs(lo), abs(hi))) then
            lo = lo - 1.0_dp
            hi = hi + 1.0_dp
        end if
        nb = max(1, min(nbins, MAX_TICKS - 1))

        call scale_range(lo, hi, nb, scal, off)
        v0 = lo - off
        v1 = hi - off
        raw = (v1 - v0)/real(nb, dp)

        first = size(STEPS)
        do i = 1, size(STEPS)
            if (STEPS(i)*scal >= raw) then
                first = i
                exit
            end if
        end do

        n = 0
        do k = first, 1, -1
            step = STEPS(k)*scal
            base = floor(v0/step)*step
            low = edge_le((v0 - base)/step)
            high = edge_ge((v1 - base)/step)
            n = 0
            inside = 0
            do i = low, high
                t = real(i, dp)*step + base + off
                if (n >= MAX_TICKS) exit
                n = n + 1
                ticks(n) = t
                if (t >= lo - 1.0e-10_dp*abs(step) .and. &
                    t <= hi + 1.0e-10_dp*abs(step)) inside = inside + 1
            end do
            ! matplotlib's _min_n_ticks, which is two.
            if (inside >= 2) exit
        end do

        ! The locator deliberately runs one tick past each end so that the
        ! round-numbers limit mode has something to snap to; an axis that
        ! does not autoscale its limits has no use for those.
        k = 0
        do i = 1, n
            if (ticks(i) >= lo - 1.0e-10_dp*(hi - lo) .and. &
                ticks(i) <= hi + 1.0e-10_dp*(hi - lo)) then
                k = k + 1
                ticks(k) = ticks(i)
            end if
        end do
        if (k == 0) then
            k = 2
            ticks(1) = lo
            ticks(2) = hi
        end if
        n_ticks = k
    end subroutine linear_ticks

    ! matplotlib's scale_range: the power of ten one step is near, and an
    ! offset for data that sits far from zero relative to its own spread.
    pure subroutine scale_range(vmin, vmax, n, scal, off)
        real(dp), intent(in) :: vmin, vmax
        integer, intent(in) :: n
        real(dp), intent(out) :: scal, off
        real(dp) :: dv, maxabs, meanv

        dv = abs(vmax - vmin)
        maxabs = max(abs(vmin), abs(vmax))
        if (maxabs == 0.0_dp .or. dv/maxabs < 1.0e-12_dp) then
            scal = 1.0_dp
            off = 0.0_dp
            return
        end if
        meanv = 0.5_dp*(vmax + vmin)
        if (abs(meanv)/dv < 100.0_dp) then
            off = 0.0_dp
        else
            off = sign(10.0_dp**floor(log10(abs(meanv))), meanv)
        end if
        scal = 10.0_dp**floor(log10(dv/real(n, dp)))
    end subroutine scale_range

    ! floor and ceiling in step units, snapping to the integer when the
    ! value is within rounding distance of it.
    pure function edge_le(x) result(k)
        real(dp), intent(in) :: x
        integer :: k
        real(dp) :: frac
        k = floor(x)
        frac = x - real(k, dp)
        if (frac > 1.0_dp - 1.0e-10_dp) k = k + 1
    end function edge_le

    pure function edge_ge(x) result(k)
        real(dp), intent(in) :: x
        integer :: k
        real(dp) :: frac
        k = floor(x)
        frac = x - real(k, dp)
        if (frac > 1.0e-10_dp) k = k + 1
    end function edge_ge

    ! Decades either side of zero, plus zero itself, thinned by a whole
    ! stride when there are more decades than will fit.
    subroutine symlog_ticks(vmin, vmax, ticks, n_ticks)
        real(dp), intent(in) :: vmin, vmax
        real(dp), intent(out) :: ticks(MAX_TICKS)
        integer, intent(out) :: n_ticks
        real(dp) :: lo, hi, m, v
        integer :: nd, stride, k, n

        lo = min(vmin, vmax)
        hi = max(vmin, vmax)
        m = max(abs(lo), abs(hi))
        if (m <= 0.0_dp) m = 1.0_dp

        nd = max(0, ceiling(log10(m)))
        stride = 1
        do while (2 * (nd / stride) + 1 > 9)
            stride = stride + 1
        end do

        n = 0
        do k = nd - mod(nd, stride), 0, -stride
            v = -10.0_dp ** k
            if (v >= lo .and. v <= hi) then
                n = n + 1
                ticks(n) = v
            end if
        end do
        if (0.0_dp >= lo .and. 0.0_dp <= hi .and. n < MAX_TICKS) then
            n = n + 1
            ticks(n) = 0.0_dp
        end if
        do k = 0, nd, stride
            v = 10.0_dp ** k
            if (v >= lo .and. v <= hi .and. n < MAX_TICKS) then
                n = n + 1
                ticks(n) = v
            end if
        end do
        n_ticks = n
    end subroutine symlog_ticks

    subroutine log_ticks(vmin, vmax, ticks, n_ticks)
        real(dp), intent(in) :: vmin, vmax
        real(dp), intent(out) :: ticks(MAX_TICKS)
        integer, intent(out) :: n_ticks
        real(dp) :: lo, hi, t
        integer :: i0, i1, i, n

        lo = min(vmin, vmax)
        hi = max(vmin, vmax)
        if (lo <= 0.0_dp) lo = tiny(1.0_dp)
        if (hi <= 0.0_dp) hi = 1.0_dp

        i0 = int(floor(log10(lo)))
        i1 = int(ceiling(log10(hi)))

        n = 0
        do i = i0, i1
            t = 10.0_dp ** real(i, dp)
            if (t >= lo * (1.0_dp - 1.0e-12_dp) .and. t <= hi * (1.0_dp + 1.0e-12_dp)) then
                if (n < MAX_TICKS) then
                    n = n + 1
                    ticks(n) = t
                end if
            end if
        end do

        if (n == 0) then
            n = 2
            ticks(1) = lo
            ticks(2) = hi
        end if
        n_ticks = n
    end subroutine log_ticks

    ! A minor tick on a log axis, which matplotlib writes as the multiple
    ! and the decade: 3x10^-1 and so on.
    subroutine format_log_minor_to(v, s, n)
        real(dp), intent(in) :: v
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        integer :: expn, k
        character(len=32) :: tmp

        n = 0
        if (v <= 0.0_dp) return
        expn = floor(log10(v) + 1.0e-9_dp)
        k = nint(v/10.0_dp**expn)
        write (tmp, '("$",I0,"\times10^{",I0,"}$")') k, expn
        n = len_trim(tmp)
        s(1:n) = tmp(1:n)
    end subroutine format_log_minor_to

    ! Which multiples of a decade matplotlib labels, given how many decades
    ! the axis spans: none at all over a decade, the sparse set down to a
    ! fifth of one, and every multiple below that.
    pure function log_minor_labelled(k, decades) result(yes)
        integer, intent(in) :: k
        real(dp), intent(in) :: decades
        logical :: yes
        yes = .false.
        if (decades > 1.0_dp) return
        if (decades > 0.4_dp) then
            yes = k == 2 .or. k == 3 .or. k == 4 .or. k == 6
        else
            yes = k >= 2 .and. k <= 9
        end if
    end function log_minor_labelled

    subroutine format_tick_to(v, is_log, s, n)
        real(dp), intent(in) :: v
        logical, intent(in) :: is_log
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        real(dp) :: av
        integer :: expn
        character(len=32) :: tmp
        integer :: i, j, k, dot

        av = abs(v)
        if (is_log .or. (av > 0.0_dp .and. (av >= 1.0e4_dp .or. av < 1.0e-3_dp))) then
            if (av < 1.0e-30_dp) then
                s = "0"
                n = 1
                return
            end if
            expn = nint(log10(av))
            if (abs(av / (10.0_dp ** expn) - 1.0_dp) < 1.0e-8_dp) then
                ! matplotlib's LogFormatterSciNotation writes every decade
                ! as a power in mathtext, ten to the nought included.
                if (v < 0.0_dp) then
                    write (tmp, '("$-10^{",I0,"}$")') expn
                else
                    write (tmp, '("$10^{",I0,"}$")') expn
                end if
                n = len_trim(tmp)
                s(1:n) = tmp(1:n)
                return
            end if
        end if

        if (abs(v - real(nint(v), dp)) < 1.0e-8_dp * max(1.0_dp, av)) then
            write (tmp, "(I0)") nint(v)
            n = len_trim(tmp)
            s(1:n) = tmp(1:n)
            return
        end if

        write (tmp, "(F20.4)") v
        j = 1
        do while (j < 20 .and. tmp(j:j) == " ")
            j = j + 1
        end do
        k = 0
        do i = j, 20
            if (tmp(i:i) == " ") exit
            k = k + 1
            s(k:k) = tmp(i:i)
        end do
        dot = 0
        do i = 1, k
            if (s(i:i) == ".") then
                dot = i
                exit
            end if
        end do
        if (dot > 0) then
            do while (k > dot .and. s(k:k) == "0")
                k = k - 1
            end do
            if (k == dot) k = k - 1
        end if
        if (k <= 0) then
            s = "0"
            n = 1
        else
            n = k
        end if
    end subroutine format_tick_to

    ! matplotlib's ScalarFormatter labels every tick on an axis with the
    ! same number of decimals: as few as still tell the ticks apart. That
    ! is why an axis running -1 to 1 in quarters reads 1.00 and not 1.
    pure function tick_decimals(locs, n) result(d)
        real(dp), intent(in) :: locs(:)
        integer, intent(in) :: n
        integer :: d
        real(dp) :: lo, hi, rng, thresh, err
        integer :: oom, i

        d = 0
        if (n < 1) return
        lo = minval(locs(1:n))
        hi = maxval(locs(1:n))
        rng = hi - lo
        if (rng <= 0.0_dp) rng = maxval(abs(locs(1:n)))
        if (rng <= 0.0_dp) return

        oom = floor(log10(rng))
        thresh = 1.0e-3_dp*10.0_dp**oom
        d = max(0, 3 - oom)
        do while (d >= 0)
            err = 0.0_dp
            do i = 1, n
                err = max(err, abs(locs(i) - round_to(locs(i), d)))
            end do
            if (err < thresh) then
                d = d - 1
            else
                exit
            end if
        end do
        d = d + 1
    end function tick_decimals

    pure function round_to(v, d) result(r)
        real(dp), intent(in) :: v
        integer, intent(in) :: d
        real(dp) :: r, p
        p = 10.0_dp**d
        r = anint(v*p)/p
    end function round_to

    ! One tick label with a decimal count fixed for the whole axis. Values
    ! far from one still fall back to the powers of ten, as matplotlib's
    ! formatter does when it switches to scientific notation.
    subroutine format_tick_fixed(v, dec, s, n)
        real(dp), intent(in) :: v
        integer, intent(in) :: dec
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        character(len=32) :: tmp
        character(len=16) :: fmt
        real(dp) :: av, vv
        integer :: i

        ! matplotlib's ScalarFormatter writes anything within 1e-8 of zero as
        ! zero. The locator makes a tick as i*step + base, which for the tick
        ! meant to be at 0 is exactly 0 on one build and 1e-17 on another;
        ! without this the second falls to the power-of-ten branch and reads
        ! "0" where the rest of the axis reads "0.0".
        vv = v
        if (abs(vv) < 1.0e-8_dp) vv = 0.0_dp
        av = abs(vv)
        if (dec < 0 .or. (av > 0.0_dp .and. (av >= 1.0e4_dp .or. av < 1.0e-3_dp))) then
            call format_tick_to(vv, .false., s, n)
            return
        end if
        if (dec == 0) then
            write (tmp, "(I0)") nint(vv)
        else
            write (fmt, "(A,I0,A)") "(F24.", dec, ")"
            write (tmp, fmt) vv
        end if
        tmp = adjustl(tmp)
        n = len_trim(tmp)
        ! A value below one prints as .5 on some compilers and 0.5 on
        ! others; matplotlib writes the leading zero, so put it back.
        if (tmp(1:1) == ".") then
            s(1:1) = "0"
            s(2:n + 1) = tmp(1:n)
            n = n + 1
        else if (n > 1 .and. tmp(1:2) == "-.") then
            s(1:2) = "-0"
            s(3:n + 1) = tmp(2:n)
            n = n + 1
        else
            s(1:n) = tmp(1:n)
        end if
        ! Rounding can turn a small negative into "-0.00".
        do i = 1, n
            if (s(i:i) /= "-" .and. s(i:i) /= "0" .and. s(i:i) /= ".") return
        end do
        if (s(1:1) == "-") then
            s(1:n - 1) = s(2:n)
            n = n - 1
        end if
    end subroutine format_tick_fixed


    ! ------------------------------------------------------------------
    ! An axis whose numbers are large, small, or nearly equal is unreadable
    ! if every tick spells itself out. matplotlib factors out two things and
    ! writes them once at the end of the axis: an offset, when every tick
    ! shares its leading digits, and a power of ten, when the numbers are
    ! far enough from one. What is left on each tick is the difference.
    ! ------------------------------------------------------------------

    ! floor(v / 10**k), which is what Python's // does for the positive
    ! values this is asked about.
    pure function decade_floor(v, k) result(r)
        real(dp), intent(in) :: v
        integer, intent(in) :: k
        real(dp) :: r
        r = floor(v/10.0_dp**k)
    end function decade_floor

    ! use_off and the power limits are what ticklabel_format sets; their
    ! defaults are the rcParams ones, axes.formatter.useoffset and .limits.
    pure subroutine tick_offset(locs, n, vmin, vmax, off, oom, use_off, plo, phi)
        real(dp), intent(in) :: locs(:), vmin, vmax
        integer, intent(in) :: n
        real(dp), intent(out) :: off
        integer, intent(out) :: oom
        logical, intent(in), optional :: use_off
        integer, intent(in), optional :: plo, phi
        ! rcParams axes.formatter.offset_threshold.
        integer, parameter :: THRESHOLD = 4
        real(dp) :: lmin, lmax, amin, amax, sgn, span
        integer :: k, kmax, lo_lim, hi_lim
        logical :: want_off

        off = 0.0_dp
        oom = 0
        want_off = .true.
        if (present(use_off)) want_off = use_off
        lo_lim = -5
        hi_lim = 6
        if (present(plo)) lo_lim = plo
        if (present(phi)) hi_lim = phi
        if (n < 1) return
        lmin = minval(locs(1:n))
        lmax = maxval(locs(1:n))

        ! An offset is only worth it when the ticks all have the same sign,
        ! and only when it saves at least four digits.
        if (want_off .and. lmin /= lmax .and. &
            .not. (lmin <= 0.0_dp .and. lmax >= 0.0_dp)) then
            amin = min(abs(lmin), abs(lmax))
            amax = max(abs(lmin), abs(lmax))
            sgn = sign(1.0_dp, lmin)
            kmax = ceiling(log10(amax))
            ! The smallest power of ten at which the two ends still agree.
            k = kmax
            do while (k > kmax - 32)
                if (decade_floor(amin, k) /= decade_floor(amax, k)) exit
                k = k - 1
            end do
            k = k + 1
            if ((amax - amin)/10.0_dp**k <= 1.0e-2_dp) then
                ! The ticks straddle a multiple of a large power of ten, so
                ! the digits they agree on are not the ones just counted.
                k = kmax
                do while (k > kmax - 32)
                    if (decade_floor(amax, k) - decade_floor(amin, k) > 1.0_dp) exit
                    k = k - 1
                end do
                k = k + 1
            end if
            if (decade_floor(amax, k) >= 10.0_dp**(THRESHOLD - 1)) &
                off = sgn*decade_floor(amax, k)*10.0_dp**k
        end if

        ! The power of ten is measured on what is left once the offset is
        ! taken away, which is the span of the axis rather than its values.
        if (off /= 0.0_dp) then
            span = abs(vmax - vmin)
            if (span > 0.0_dp) oom = floor(log10(span))
        else
            amax = maxval(abs(locs(1:n)))
            if (amax > 0.0_dp) oom = floor(log10(amax))
        end if
        if (oom > lo_lim .and. oom < hi_lim) oom = 0
    end subroutine tick_offset

    ! The decimals the labels need once the offset and the power of ten
    ! have been taken out of them.
    pure function tick_decimals_at(locs, n, off, oom) result(d)
        real(dp), intent(in) :: locs(:), off
        integer, intent(in) :: n, oom
        integer :: d
        real(dp) :: s(MAX_TICKS)
        integer :: m

        m = min(n, MAX_TICKS)
        if (m < 1) then
            d = 0
            return
        end if
        s(1:m) = (locs(1:m) - off)/10.0_dp**oom
        d = tick_decimals(s, m)
    end function tick_decimals_at

    ! One value as a significand and an exponent, the shortest way round:
    ! 100000 is "1e5" and 250000 is "2.5e5".
    pure subroutine format_significand(v, s, n)
        real(dp), intent(in) :: v
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        character(len=32) :: tmp, ex
        real(dp) :: m
        integer :: e

        e = floor(log10(abs(v)))
        m = anint(v/10.0_dp**e*1.0e6_dp)/1.0e6_dp
        if (m == anint(m)) then
            write (tmp, "(I0)") nint(m)
        else
            write (tmp, "(F0.6)") m
            n = len_trim(tmp)
            do while (n > 1 .and. tmp(n:n) == "0")
                n = n - 1
            end do
            if (tmp(n:n) == ".") n = n - 1
            tmp = tmp(1:n)
        end if
        if (e == 0) then
            s = trim(tmp)
        else
            write (ex, "(I0)") e
            s = trim(tmp)//"e"//trim(ex)
        end if
        n = len_trim(s)
    end subroutine format_significand

    ! What is written at the end of the axis: the power of ten, then the
    ! offset with its sign, either of which may be absent.
    pure subroutine format_offset_text(off, oom, s, n)
        real(dp), intent(in) :: off
        integer, intent(in) :: oom
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        character(len=32) :: tmp, sig
        integer :: m

        s = ""
        n = 0
        if (oom /= 0) then
            write (tmp, "(A,I0)") "1e", oom
            s = trim(tmp)
        end if
        if (off /= 0.0_dp) then
            call format_significand(abs(off), sig, m)
            s = trim(s)//merge("+", "-", off > 0.0_dp)//sig(1:m)
        end if
        n = len_trim(s)
    end subroutine format_offset_text


    ! Ticks every base units, which is matplotlib's MultipleLocator.
    pure subroutine multiple_ticks(vmin, vmax, base, t, n)
        real(dp), intent(in) :: vmin, vmax, base
        real(dp), intent(out) :: t(MAX_TICKS)
        integer, intent(out) :: n
        integer :: k, k0, k1

        n = 0
        if (base <= 0.0_dp) return
        k0 = ceiling(min(vmin, vmax)/base - 1.0e-9_dp)
        k1 = floor(max(vmin, vmax)/base + 1.0e-9_dp)
        do k = k0, k1
            if (n >= MAX_TICKS) exit
            n = n + 1
            t(n) = real(k, dp)*base
        end do
    end subroutine multiple_ticks

    ! A percentage, as matplotlib's PercentFormatter writes it: the value
    ! is a fraction of whole, and whole is 100 when the data already is a
    ! percentage.
    pure subroutine format_percent(v, whole, dec, s, n)
        real(dp), intent(in) :: v, whole
        integer, intent(in) :: dec
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        character(len=32) :: tmp
        character(len=16) :: fmt

        if (dec <= 0) then
            write (tmp, "(I0)") nint(v/whole*100.0_dp)
        else
            write (fmt, "(A,I0,A)") "(F24.", dec, ")"
            write (tmp, fmt) v/whole*100.0_dp
        end if
        s = trim(adjustl(tmp))//"%"
        n = len_trim(s)
    end subroutine format_percent

    ! The integer part in groups of three, which is what a reader wants of
    ! an axis counting people or dollars.
    pure subroutine format_grouped(v, dec, s, n)
        real(dp), intent(in) :: v
        integer, intent(in) :: dec
        character(len=*), intent(out) :: s
        integer, intent(out) :: n
        character(len=48) :: tmp, out
        character(len=16) :: fmt
        integer :: i, k, dot, first, d

        d = max(0, dec)
        if (d == 0) then
            write (tmp, "(I0)") nint(abs(v))
        else
            write (fmt, "(A,I0,A)") "(F32.", d, ")"
            write (tmp, fmt) abs(v)
        end if
        tmp = adjustl(tmp)
        dot = index(tmp, ".")
        if (dot == 0) dot = len_trim(tmp) + 1

        out = ""
        k = 0
        first = dot - 1
        do i = first, 1, -1
            out = tmp(i:i)//trim(out)
            k = k + 1
            if (mod(k, 3) == 0 .and. i > 1) out = ","//trim(out)
        end do
        if (dot <= len_trim(tmp)) out = trim(out)//tmp(dot:len_trim(tmp))
        if (v < 0.0_dp) out = "-"//trim(out)
        s = trim(out)
        n = len_trim(s)
    end subroutine format_grouped

end module fplot_ticks
