# fplot

[![CI](https://github.com/certik/fplot/actions/workflows/ci.yml/badge.svg)](https://github.com/certik/fplot/actions/workflows/ci.yml)

Pure Fortran plotting library with a pylab-style API, writing SVG, PNG, PDF and EPS.
No external graphics library: the PNG rasterizer and its font are part of fplot.

```fortran
use fplot
call plot(x, y, "k-", label="sin")
call title("My plot")
call xlabel("x")
call ylabel("y")
call grid(.true.)
call legend()
call savefig("out.svg")   ! or "out.png", "out.pdf", "out.eps"
call show()               ! Jupyter (LFortran) or writes fplot_show.svg

! subplots
call clf()
call subplot(2, 1, 1)
call plot(x, y, "b-", label="sin")
call title("top")
call subplot(2, 1, 2)
call plot(x, y2, "r--", label="cos")
call title("bottom")
call suptitle("figure title")
```

## Features

- `plot`, `scatter`, `bar`, `hist`, `fill_between`, `errorbar`
- `bar` takes `yerr=`, `capsize=`, `align="edge"` and `tick_label=`;
  `errorbar` takes `ecolor=`, `elinewidth=`, `lw=` and `alpha=`
- `plot(y)` with a single array numbers the points `0, 1, 2, ...` for x
- `xlim`/`ylim` to set, `get_xlim`/`get_ylim` to read back what autoscaling
  chose, and `invert_xaxis`/`invert_yaxis` to make an axis count down
- `scatter` also takes per-point `sizes=` and color-mapped `cvals=`
  (separate keywords because Fortran cannot overload one dummy as
  scalar-or-array the way matplotlib's `s=` and `c=` do)
- `step`, `stem`, `barh`, `pie`, `boxplot`, `violinplot`
- `boxplot(y)` for one dataset or `boxplot(y(:,:), labels=)` for a row
  each, with `vert=`, `notch=`, `showmeans=`, `patch_artist=` and `whis=`
- `violinplot` the same way, with `vert=`, `showmeans=`, `showmedians=`
  and `showextrema=`
- `pie(explode=, startangle=, counterclock=, autopct=, pctdistance=,
  labeldistance=, radius=, colors=, edgecolor=, linewidth=)`
- `semilogx`, `semilogy`, `loglog`, `axhline`, `axvline`
- `fill_betweenx` for a band along y, `stackplot` for layers summed on top
  of each other, and `axline` for an endless line through two points or
  through one with a `slope=`
- `text` and `annotate` (`arrowstyle="->"` draws a leader with a head at the
  annotated point, with `arrowcolor=`, `arrowlw=` and `shrink=`), with
  `rotation=`, `va=`, a `bbox_facecolor=` box behind them and lines broken
  at `achar(10)`; `figtext` places on the canvas instead of in the axes, and
  `transform="axes"` places in fractions of the axes box.
  `connectionstyle="arc3,rad=0.3"` bows the leader out of the straight
  line, and `boxstyle="round,pad=0.5"` rounds the corners of the box
- `imshow` of an (row, column, channel) array paints the colours given,
  three channels for RGB or four for RGBA
- `imshow` with `cmap`, `extent`, `origin`, `vmin`/`vmax` and square-pixel
  `aspect`, plus `colorbar(label=, orientation=, fraction=, pad=, shrink=,
  aspect=)`, upright or lying on its side
- `contour` and `contourf` with automatic or explicit `levels`
- Colormaps: `viridis`, `plasma`, `inferno`, `magma`, `gray`, `coolwarm`
- Format strings: colors `bgrcmykw` / `C0`–`C9`, linestyles `-` `--` `:` `-.`,
  markers `o x . s ^ v < > * + D`
- Optional `label=`, `lw=`, `color=`, `marker=`, `alpha=`
- `hatch=` on bars, fills and patches: `/`, `\`, `|`, `-`, `+` and `x`,
  repeated to pack the lines closer, ruled across the shape and clipped
  to it (matplotlib's dotted hatches are not drawn)
- Marker and line detail on `plot`: `markersize=`, `markerfacecolor=`,
  `markeredgecolor=`, `markeredgewidth=`, `markevery=`, `drawstyle=` and
  a `dashes=` pattern of your own
- Title, axis labels, grid, `xlim` / `ylim`, `clf` / `figure(figsize=, dpi=)`
- `set_facecolor(color, alpha=)` for the background of one axes
- `grid(on, axis=, which=, color=, linestyle=, lw=, alpha=)` for one axis
  only, for the minor ticks, or in a colour of your own; log axes carry
  minor ticks without being asked, as matplotlib's do
- `savefig(file, transparent=, facecolor=)`; the extension picks the format,
  one of `.svg`, `.png`, `.pdf` or `.eps`, and `dpi=` sizes the PNG raster
- `axis("on"|"off"|"equal"|"scaled"|"tight"|"auto")` and `set_aspect`
- `margins(m)` or `margins(x=, y=)` for the room left past the data, and
  `autoscale(enable, axis=, tight=)`; `autoscale(.false.)` pins the limits
  where the data has put them so far, so nothing drawn later moves them
- `set_xscale` / `set_yscale` with `"linear"`, `"log"` or `"symlog"`
- `tick_params(axis=, direction=, length=, labelsize=, rotation=)` and `spines`
- `legend(loc=, fontsize=, ncol=, frameon=, title=, bbox_to_anchor=,
  labels=)`, placed where the data leaves most room when `loc` is left
  out, as matplotlib's `"best"` does; a label starting with `_` keeps
  its artist out of the legend,
  `figlegend(loc=, fontsize=, ncol=, frameon=, title=)` for one legend
  covering every panel,
  `xticks` / `yticks` with optional labels, `minorticks_on`,
  `xticks(vals, minor=.true.)` to place minor ticks by hand, and
  `locator_params(axis=, nbins=, prune=)`
- Formatters and locators: `tick_format(axis, "percent"/"comma"/"fixed")`,
  `tick_locator(axis, base=, nbins=)` and `ticklabel_format(style=,
  useoffset=, scilimits=)`. An axis whose labels would otherwise repeat
  themselves factors the shared offset and power of ten out into a single
  label at its end, as matplotlib's ScalarFormatter does
- Font sizes: `fontsize=` on `title` / `xlabel` / `ylabel` / `suptitle`, and
  `set_fontsize(size=, title=, labels=, ticks=, legend=)` to set them globally
- `title(loc="left"|"right")` and `xlabel`/`ylabel(labelpad=)`
- `savefig(dpi=, bbox_inches="tight", pad_inches=)` to crop to the drawing
- Several live figures at once: `figure(num=)`, `gcf()`, `close(num=, all=)`
- `subplots(m, n, axs, sharex=, sharey=)` hands back axes handles, so
  `call axs(1,2)%plot(x, y)` works alongside the stateful `subplot` style;
  shared axes span the union of the group and drop their inner tick labels.
  Every call the stateful interface offers is also a method on the handle
- Subplots: `subplot(m, n, i)` and figure-level `suptitle`; per-axes state
  (series, labels, grid, legend, scale, limits) with matplotlib's default
  subplot spacing
- `subplots_adjust` and `tight_layout`
- `constrained_layout(.true.)`: the same fit, but made afresh at draw
  time, so labels added after the plotting still get their room. Ours
  keeps one margin for the whole grid where matplotlib works row by row
- `twinx` and `twiny` for a second y or x axis on the same plot
- Bars: `bar`/`barh` with `bottom=`/`left=` for stacks, `colors=` for a
  color per bar, `edgecolor=`/`linewidth=`, and `bar_label(fmt=, padding=)`
- `add_axes` for an axes placed by hand, `inset_axes` for a small axes
  inside another, and `secondary_xaxis`/`secondary_yaxis` for a second
  scale along an edge
- `table` for a block of text below (or above) the axes
- `streamplot` for the streamlines of a vector field
- `add_frame` / `save_animation(file, fps=, loop=)` to write an animated GIF
- `matshow`, `eventplot` and `broken_barh`
- `hist2d` and `hexbin` for counting points into square or hexagonal bins
- `imshow(interpolation="bilinear")` to smooth an image instead of
  showing it as blocks
- 3D axes: `plot3d`, `scatter3d`, `plot_surface` (flat or `cmap=`) and
  `plot_wireframe`, with `view_init`, `zlabel` and `zlim`, drawn with
  mplot3d's camera, panes and lighting
- Polar axes: `polar(theta, r)`, or `set_polar()` on an axes, with the
  angular grid, the degree labels and the radial labels along 22.5°
- Layered like matplotlib: images below patches, the grid above the
  patches and below the lines, text on top, with `set_zorder` to lift
  the artist just drawn out of that order
- Missing data: a NaN or an infinity breaks the line and the band there
  and takes no part in the limits, as it does in matplotlib
- Patches: `add_rectangle`, `add_circle`, `add_ellipse`, `add_polygon`,
  `add_arrow` and `add_path` for an arbitrary path of lines and cubics,
  filled and outlined in data coordinates
- `clabel` to write the level into each contour line, breaking the line
  at its straightest stretch to make room
- `quiver` for a field of arrows, sized as matplotlib sizes them
- Date axes: `date_num(y, m, d)` for the numbers and `xaxis_date()` to
  have the ticks land on round dates and read as dates
- `subplot2grid`: panels that span several cells of a grid, so a wide
  plot can sit over two narrow ones
- `subplot_mosaic(["AB", "CC"], keys, axs)`: the panels drawn as a small
  picture of the figure, one character per cell, a panel per character
- `gridspec(width_ratios=, height_ratios=)`: columns and rows of unequal
  size, for a wide panel beside a narrow one
- `pcolormesh` and `pcolor`: a grid of cells with edges of your own
  choosing, which is what an unevenly sampled field needs
- `triplot` and `tripcolor`: scattered points joined into triangles by
  the same Delaunay triangulation matplotlib gets from Qhull, drawn as
  edges or filled by value
- `tricontour` and `tricontourf`: level lines and filled bands over those
  same triangles, with matplotlib's rounded levels and a colorbar
- `plot_trisurf`: those triangles lifted into three dimensions and lit by
  the same light source `plot_surface` uses
- `bar3d`: boxes standing on the xy plane, six lit faces each, painted
  back to front with everything else in the axes
- `quiver3d`: arrows in space, a shaft and two barbs turned fifteen
  degrees off it, with the limits taken from where the arrows start
- `contour3d`: level lines of a grid drawn in space, each at the height
  of its own level
- matplotlib's tick locator and formatter: the same 1/2/2.5/5 steps, the
  same number of ticks for the space available, and one decimal count
  shared by the whole axis. A log axis writes its decades as powers, and
  when it spans a decade or less it labels the multiples between them
- Mathtext: `$10^{-3}$`, `$x_i$`, `$E = mc^2$`, `$\frac{a}{b}$`,
  `$\sqrt{x}$` and the greek letters in any label, laid out once and
  drawn by every backend, with the letters sloped and the rest upright as
  TeX sets them. Text is UTF-8; PDF and EPS fill the greek as outlines,
  since no core font has it
- `fontweight=` and `fontstyle=` on `title`, `suptitle`, `xlabel`,
  `ylabel`, `text` and `annotate`: real bold and oblique faces, not a
  smeared or sheared regular one
- Categorical axes: `plot`, `bar` and `barh` take a list of names in place
  of the numbers, and place them at 0, 1, 2, ... with the names as ticks
- 49 matplotlib colormaps, plus the qualitative `tab10`, `tab20` and
  `Set1`, any of them reversed with a `_r` suffix
- `imshow(norm="log")` for a logarithmic color scale, and
  `imshow(boundaries=)` for matplotlib's `BoundaryNorm`: one flat color
  per band, and a colorbar of blocks to match
- `norm="centered"` with `vcenter=`, `norm="power"` with `gamma=` and
  `norm="symlog"` with `linthresh=`, on `imshow` and `pcolormesh`
- `set_bad`, `set_under` and `set_over` for the samples a colormap has
  nothing to say about, and `set_cmap_colors` to build one from a list of
  stops; missing samples are left clear unless `set_bad` says otherwise
- `errorbar` with `xerr=` and asymmetric `yerr_lo=`/`yerr_hi=` arms
- `fill_between(where=, interpolate=)` to shade only where a condition
  holds, carried out to the crossing point
- `scatter(edgecolors=, linewidths=)` to outline the markers
- `hist(bins=, bin_edges=, density=, cumulative=, histtype="step"/"stepfilled",
  weights=, stacked=, orientation="horizontal", log=, rwidth=)`
- `axhspan`/`axvspan` shaded bands and `hlines`/`vlines` line runs
- `style_use("ggplot")` and friends (`seaborn`, `fivethirtyeight`,
  `dark_background`, `grayscale`, `default`), or `rc(figsize=, dpi=,
  fontsize=, linewidth=, grid=, facecolor=, axes_facecolor=, grid_color=,
  text_color=, color_cycle=)` for one setting at a time
- Colors as CSS4/X11 names, `tab:` names, `#rgb`, `#rrggbb`, `#rrggbbaa`,
  single letters, `C0`-`C9`, or a greyscale fraction such as `"0.5"`
- SVG defaults aligned with matplotlib (6.4×4.8 in, tab10 colors, subplot margins)

## Using fplot in your project

fplot is an [fpm](https://fpm.fortran-lang.org) package with no dependencies
outside the Fortran standard library, so a project picks it up with one entry
in its `fpm.toml`:

```toml
[dependencies]
fplot = { git = "https://github.com/certik/fplot" }
```

and then `use fplot` and call `plot`, `title`, `savefig` as the examples below
do. It builds with both flang and LFortran:

```bash
fpm build --compiler flang
fpm build --compiler lfortran
```

The pixi tasks below are for developing fplot itself, where the build scripts
give finer control over compile and link order.

## Build

Requires [pixi](https://pixi.sh). LFortran is installed by pixi on all platforms.
Flang is installed by pixi on Linux via conda-forge `flang_linux-64` (compiler
plus `libflang-rt`); on macOS provide a system Flang on `PATH`.

```bash
pixi install

# Flang (development)
pixi run build-flang
pixi run test-flang
pixi run demo-flang

# LFortran
pixi run build-lfortran
pixi run test-lfortran

# fpm uses the committed reference hashes too
fpm test

# Refresh the committed hashes after intentionally changing plot output
python tests/update_reference_hashes.py

# Optional: compare against matplotlib references when auditing fidelity
pixi run compare       # SVG, structurally
pixi run compare-png   # PNG, pixel by pixel
pixi run compare-pdf   # PDF, rasterized by pdftoppm
pixi run compare-eps   # EPS, rasterized by ghostscript
pixi run compare-gif   # GIF, frame by frame
```

`fpm test` now generates all reference artifacts and verifies them against the
committed hashes in `tests/reference_hashes.json`. When a plotting change is
intentional, regenerate `tests/out` with `fpm test`, refresh the manifest with
`python tests/update_reference_hashes.py`, and commit the updated JSON.

The SVG comparison is structural because a viewer, not fplot, decides what an
SVG looks like. PNG, PDF and EPS are compared as pixels, since there fplot
decides.

CI (Linux) runs `fpm test` across the compiler matrix, so hash verification is
the main gate. The matplotlib comparison scripts remain available for deeper
fidelity audits.

## Layout

```
src/           library modules
               fplot_state.f90   shared types and the figure state
               fplot_artist.f90  drawing primitives (strokes, fills, markers)
               fplot_draw.f90    layout, axes decoration, one renderer per plot type
               fplot.f90         the plotting API
examples/      demo.f90
tests/         Fortran test plots, hash manifest, matplotlib refs, compare scripts
scripts/       build_flang.sh, build_lfortran.sh
```

## License

MIT, in `LICENSE`.

The compiled-in glyph outlines in `src/fplot_glyphs_data.f90` are derived
from DejaVu Sans, whose licence is in `LICENSE_DEJAVU`: the Bitstream Vera fonts
are copyright Bitstream, and the DejaVu changes are in the public domain. The
colormap tables in `src/fplot_cmap.f90` are sampled from matplotlib, which is
BSD-licensed, and the perceptual maps (viridis, magma, inferno, plasma) are
released by their authors under CC0.

## Jupyter (LFortran)

With LFortran as a Jupyter kernel, display the SVG interactively:

```fortran
use fplot
use lfortran_display
! ... plot / title / grid / legend ...
call display_data("image/svg+xml", render_svg())
```

`show()` always writes `fplot_show.svg` (works with Flang and LFortran offline).

## Fidelity to matplotlib

fplot is measured against matplotlib rather than described as similar to it:
every feature has a case in `tests/test_plots.f90` and a matplotlib reference
in `tests/gen_mpl_refs.py`. The committed hash manifest in
`tests/reference_hashes.json` locks in the accepted native output for those
cases, while the comparison scripts above still put a number on the remaining
difference from matplotlib. As of this writing, over 100 cases:

| format | cases | mean difference |
| --- | --- | --- |
| PNG | 124 | 1.50/255 |
| PDF | 123 | 2.05/255 |
| EPS | 124 | 3.57/255 |
| GIF | 20 frames | 0.60/255 |

What is left is mostly antialiasing along edges, and text: fplot draws with
its own compiled-in DejaVu Sans metrics and glyph outlines, so a stem lands
on the same pixel column as matplotlib's but not always with the same
coverage. The SVG is not matplotlib's SVG byte for byte, and is not meant to
be: matplotlib writes every glyph as a path and attaches its own metadata,
while fplot writes `<text>` and leaves the glyphs to the viewer.
