#!/usr/bin/env bash
set -euo pipefail
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BUILD="$ROOT/build/flang"
mkdir -p "$BUILD" "$ROOT/tests/out"

# Use the `flang` frontend on PATH. Do not honor $FC: conda-forge's
# flang_linux-64 activation sets FC="${CHOST}-flang", which becomes
# "-flang" under pixi because CHOST is unset.
case "${FLANG:-}" in
    ""|-*) FLANG=flang ;;
esac
echo "Using compiler: $FLANG"

cd "$BUILD"
# Compile modules in dependency order
$FLANG -c "$ROOT/src/fplot_colors.f90"
$FLANG -c "$ROOT/src/fplot_style.f90"
$FLANG -c "$ROOT/src/fplot_render.f90"
$FLANG -c "$ROOT/src/fplot_scale.f90"
$FLANG -c "$ROOT/src/fplot_cmap.f90"
$FLANG -c "$ROOT/src/fplot_ticks.f90"
$FLANG -c "$ROOT/src/fplot_contour.f90"
$FLANG -c "$ROOT/src/fplot_tri.f90"
$FLANG -c "$ROOT/src/fplot_svg.f90"
$FLANG -c "$ROOT/src/fplot_png.f90"
$FLANG -c "$ROOT/src/fplot_backend_svg.f90"
$FLANG -c "$ROOT/src/fplot_glyphs_data.f90"
$FLANG -c "$ROOT/src/fplot_glyphs.f90"
$FLANG -c "$ROOT/src/fplot_raster.f90"
$FLANG -c "$ROOT/src/fplot_textpath.f90"
$FLANG -c "$ROOT/src/fplot_mathtext.f90"
$FLANG -c "$ROOT/src/fplot_dates.f90"
$FLANG -c "$ROOT/src/fplot_state.f90"
$FLANG -c "$ROOT/src/fplot_artist.f90"
$FLANG -c "$ROOT/src/fplot_backend_pdf.f90"
$FLANG -c "$ROOT/src/fplot_backend_eps.f90"
$FLANG -c "$ROOT/src/fplot_backend_png.f90"
$FLANG -c "$ROOT/src/fplot_gif.f90"
$FLANG -c "$ROOT/src/fplot_proj3d.f90"
$FLANG -c "$ROOT/src/fplot_draw.f90"
$FLANG -c "$ROOT/src/fplot.f90"

# Test program
$FLANG -o "$BUILD/test_plots" \
    "$ROOT/tests/test_plots.f90" \
    fplot_colors.o fplot_style.o fplot_render.o fplot_scale.o fplot_cmap.o fplot_ticks.o fplot_contour.o fplot_tri.o fplot_svg.o fplot_backend_svg.o fplot_backend_pdf.o fplot_backend_eps.o fplot_glyphs_data.o fplot_glyphs.o fplot_png.o fplot_textpath.o fplot_raster.o fplot_backend_png.o fplot_gif.o fplot_proj3d.o fplot_mathtext.o fplot_dates.o fplot_state.o fplot_artist.o fplot_draw.o fplot.o

# Demo
$FLANG -o "$BUILD/demo" \
    "$ROOT/examples/demo.f90" \
    fplot_colors.o fplot_style.o fplot_render.o fplot_scale.o fplot_cmap.o fplot_ticks.o fplot_contour.o fplot_tri.o fplot_svg.o fplot_backend_svg.o fplot_backend_pdf.o fplot_backend_eps.o fplot_glyphs_data.o fplot_glyphs.o fplot_png.o fplot_textpath.o fplot_raster.o fplot_backend_png.o fplot_gif.o fplot_proj3d.o fplot_mathtext.o fplot_dates.o fplot_state.o fplot_artist.o fplot_draw.o fplot.o

echo "Built: $BUILD/test_plots $BUILD/demo"
