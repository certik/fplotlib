(gallery)=
# Gallery

The fplotlib gallery contains a collection of examples organized by topic,
inspired by the [matplotlib gallery](https://matplotlib.org/stable/gallery/index.html)
but adapted to fplotlib's native functionality and Fortran idioms.

Every example is a complete Fortran program from `docs/gallery/` or its
subdirectories. The documentation build compiles each one against this
checkout of fplotlib with [fpm](https://fpm.fortran-lang.org), runs it,
and embeds the pictures it wrote -- so what you see on the page is what
the code in front of you produces, in whichever format it asked for.

Use the download link at the bottom of an example to get the `.f90` source,
then build it with:

```sh
fpm run --example <name>
```

Some examples also use the [Fortran standard library](https://stdlib.fortran-lang.org),
the way a Python one would use numpy. For those, add `stdlib = "*"` to the
`[dependencies]` of your `fpm.toml`.

## Gallery Sections

The gallery is organized into the following topic areas:

* **{doc}`Images, Contours and Fields </auto_gallery/images_contours_and_fields/index>`** —
  Visualization of multi-dimensional data, including images, contours, and
  vector fields.

* **{doc}`Statistics </auto_gallery/statistics/index>`** —
  Statistical plotting techniques including error bars, confidence intervals,
  and data uncertainty visualization.

* **{doc}`3D Plotting </auto_gallery/mplot3d/index>`** —
  Three-dimensional visualization including surface plots, scatter plots, and
  vector fields in 3D.

* **{doc}`Subplots, Axes and Figures </auto_gallery/subplots_axes_and_figures/index>`** —
  Layout management, figure sizing, and axes control.

* **{doc}`Animation </auto_gallery/animation/index>`** —
  Animated GIFs built frame by frame.
