! Spy: sparsity patterns
! ======================
!
! The nonzero structure of a matrix, as matplotlib's
! `spy <https://matplotlib.org/stable/api/_as_gen/matplotlib.pyplot.spy.html>`_
! draws it: one square marker per stored entry, row 1 at the top and the
! column numbers along the top edge.
!
! :f:subr:`spy` takes either a dense matrix, whose entries above ``precision``
! in magnitude are marked, or the COO indices of a sparse one as a
! ``(2, nnz)`` integer array of rows and columns. Indices start at 1.

program plot_spy
    use fplotlib, only: dp, figure, savefig, spy, subplot, title
    implicit none

    integer, parameter :: n = 30
    real(dp) :: a(n, n)
    integer, allocatable :: ij(:, :)
    integer :: i, k

! %%
! A dense matrix: the tridiagonal of a 1D Laplacian, plus two corner entries
! and an anti-diagonal of tiny values that ``precision`` filters out.

    a = 0.0_dp
    do i = 1, n
        a(i, i) = 2.0_dp
        if (i > 1) a(i, i - 1) = -1.0_dp
        if (i < n) a(i, i + 1) = -1.0_dp
        a(i, n + 1 - i) = a(i, n + 1 - i) + 1.0e-9_dp
    end do
    a(1, n) = -1.0_dp
    a(n, 1) = -1.0_dp

    call figure(figsize=[10.0_dp, 5.0_dp])
    call subplot(1, 2, 1)
    call spy(a, precision=1.0e-6_dp, markersize=4.0_dp)
    call title("dense, precision = 1e-6")

! %%
! A sparse matrix given only by its COO indices: a 5-point stencil on a
! 6 x 6 grid. The shape ``m``, ``n`` is passed so that empty trailing rows or
! columns still show.

    k = 0
    allocate (ij(2, 5 * 36))
    block
        integer :: gx, gy, r
        do gy = 1, 6
            do gx = 1, 6
                r = (gy - 1) * 6 + gx
                k = k + 1; ij(:, k) = [r, r]
                if (gx > 1) then; k = k + 1; ij(:, k) = [r, r - 1]; end if
                if (gx < 6) then; k = k + 1; ij(:, k) = [r, r + 1]; end if
                if (gy > 1) then; k = k + 1; ij(:, k) = [r, r - 6]; end if
                if (gy < 6) then; k = k + 1; ij(:, k) = [r, r + 6]; end if
            end do
        end do
    end block

    call subplot(1, 2, 2)
    call spy(ij(:, 1:k), m=36, n=36, markersize=5.0_dp, color="C1")
    call title("COO indices")

    call savefig("spy.svg")

    print *, "wrote spy.svg"

end program plot_spy
