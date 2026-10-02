! Quiver simple demo
! ==================
!
! A vector field drawn as arrows, with a key that says how long an arrow of a
! given size is. This is matplotlib's
! `quiver simple demo <https://matplotlib.org/stable/gallery/images_contours_and_fields/quiver_simple_demo.html>`_,
! line for line.
!
! The grid comes from the Fortran standard library, much as the Python version
! gets it from numpy: ``stdlib_math`` has both ``arange`` and ``meshgrid``. To
! build this example on its own, add ``stdlib = "*"`` to the
! ``[dependencies]`` of your ``fpm.toml``.

program plot_quiver_simple_demo
    use fplotlib
    use stdlib_math, only: arange, meshgrid
    implicit none

    real(dp), allocatable :: x(:), y(:), u(:, :), v(:, :)

! %%
! ``arange`` includes its end point, so the numpy ``arange(-10, 10, 1)`` is
! ``arange(-10, 9, 1)`` here. ``meshgrid`` uses numpy's default "xy"
! indexing: ``u(i, j)`` is ``x(j)`` and ``v(i, j)`` is ``y(i)``.

    x = arange(-10.0_dp, 9.0_dp, 1.0_dp)
    y = arange(-10.0_dp, 9.0_dp, 1.0_dp)
    allocate (u(size(y), size(x)), v(size(y), size(x)))
    call meshgrid(x, y, u, v)

! %%
! ``quiver`` takes one entry per arrow, so the grid is flattened. The
! positions and the vectors are the same arrays: each arrow points away from
! the origin, and is the longer the farther out it sits.
!
! ``quiverkey`` adds a reference arrow of length 10, drawn to the same scale
! as the field. Its position is in axes coordinates, so ``y = 1.1`` puts it
! just above the plot; with ``labelpos="E"`` the arrow's head sits at that
! point and the label runs to its right.

    call quiver(reshape(u, [size(u)]), reshape(v, [size(v)]), &
                reshape(u, [size(u)]), reshape(v, [size(v)]))
    call quiverkey(0.3_dp, 1.1_dp, 10.0_dp, "Quiver key, length = 10", labelpos="E")

    call savefig("quiver_simple_demo.svg")

    print *, "wrote quiver_simple_demo.svg"

end program plot_quiver_simple_demo
