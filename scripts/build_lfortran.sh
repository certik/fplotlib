#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="$ROOT/build/lfortran"
mkdir -p "$BUILD" "$ROOT/tests/out"

LFORTRAN="${LFORTRAN:-lfortran}"
echo "Using compiler: $LFORTRAN"

# LFortran can compile modules and link; use -c for objects when supported.
cd "$BUILD"

# Prefer a single shared-module build
$LFORTRAN -c "$ROOT/src/fplot_colors.f90"
$LFORTRAN -c "$ROOT/src/fplot_style.f90"
$LFORTRAN -c "$ROOT/src/fplot_render.f90"
$LFORTRAN -c "$ROOT/src/fplot_scale.f90"
$LFORTRAN -c "$ROOT/src/fplot_cmap.f90"
$LFORTRAN -c "$ROOT/src/fplot_ticks.f90"
$LFORTRAN -c "$ROOT/src/fplot_contour.f90"
$LFORTRAN -c "$ROOT/src/fplot_tri.f90"
$LFORTRAN -c "$ROOT/src/fplot_svg.f90"
$LFORTRAN -c "$ROOT/src/fplot_png.f90"
$LFORTRAN -c "$ROOT/src/fplot_backend_svg.f90"
$LFORTRAN -c "$ROOT/src/fplot_glyphs_data.f90"
$LFORTRAN -c "$ROOT/src/fplot_glyphs.f90"
$LFORTRAN -c "$ROOT/src/fplot_textpath.f90"
$LFORTRAN -c "$ROOT/src/fplot_raster.f90"
$LFORTRAN -c "$ROOT/src/fplot_mathtext.f90"
$LFORTRAN -c "$ROOT/src/fplot_dates.f90"
$LFORTRAN -c "$ROOT/src/fplot_state.f90"
$LFORTRAN -c "$ROOT/src/fplot_artist.f90"
$LFORTRAN -c "$ROOT/src/fplot_backend_pdf.f90"
$LFORTRAN -c "$ROOT/src/fplot_backend_eps.f90"
$LFORTRAN -c "$ROOT/src/fplot_backend_png.f90"
$LFORTRAN -c "$ROOT/src/fplot_gif.f90"
$LFORTRAN -c "$ROOT/src/fplot_proj3d.f90"
$LFORTRAN -c "$ROOT/src/fplot_draw.f90"
$LFORTRAN -c "$ROOT/src/fplot.f90"

$LFORTRAN -o "$BUILD/test_plots" \
    "$ROOT/tests/test_plots.f90" \
    fplot_colors.o fplot_style.o fplot_render.o fplot_scale.o fplot_cmap.o fplot_ticks.o fplot_contour.o fplot_tri.o fplot_svg.o fplot_backend_svg.o fplot_backend_pdf.o fplot_backend_eps.o fplot_glyphs_data.o fplot_glyphs.o fplot_png.o fplot_textpath.o fplot_raster.o fplot_backend_png.o fplot_gif.o fplot_proj3d.o fplot_mathtext.o fplot_dates.o fplot_state.o fplot_artist.o fplot_draw.o fplot.o

$LFORTRAN -o "$BUILD/demo" \
    "$ROOT/examples/demo.f90" \
    fplot_colors.o fplot_style.o fplot_render.o fplot_scale.o fplot_cmap.o fplot_ticks.o fplot_contour.o fplot_tri.o fplot_svg.o fplot_backend_svg.o fplot_backend_pdf.o fplot_backend_eps.o fplot_glyphs_data.o fplot_glyphs.o fplot_png.o fplot_textpath.o fplot_raster.o fplot_backend_png.o fplot_gif.o fplot_proj3d.o fplot_mathtext.o fplot_dates.o fplot_state.o fplot_artist.o fplot_draw.o fplot.o

echo "Built: $BUILD/test_plots $BUILD/demo"
echo "Jupyter: use display_data('image/svg+xml', render_svg()) with lfortran_display"
