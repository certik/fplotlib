#!/usr/bin/env python3
"""Generate matplotlib reference SVGs matching fplotlib test cases."""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.ticker as mticker
from matplotlib.colors import LogNorm, BoundaryNorm
import matplotlib.dates as mdates
from matplotlib.patches import Rectangle, Circle, Ellipse, Polygon, Arrow, PathPatch
from matplotlib.path import Path as MPath
import datetime
import io

import numpy as np
from PIL import Image

ROOT = Path(__file__).resolve().parent
REF = ROOT / "refs"
OUT_NAMES = [
    "quiverkey",
    "contour3d",
    "quiver3d",
    "bar3d",
    "trisurf",
    "tricontourf",
    "tricontour",
    "tripcolor",
    "triplot",
    "log_minor_labels",
    "annotate_curve",
    "constrained",
    "mosaic",
    "legend_labels",
    "violin_opts",
    "box_opts",
    "pie_opts",
    "scatter_edge",
    "figlegend",
    "tick_locator",
    "cmap_special",
    "norms",
    "imshow_rgb",
    "hist_orient",
    "title_loc",
    "bar_err",
    "invert_axes",
    "annotate_arrow",
    "transform_axes",
    "plot_y",
    "basic_line",
    "multi_style",
    "markers_only",
    "semilogx",
    "semilogy",
    "loglog",
    "subplots_2x1",
    "subplots_2x2",
    "markers_gallery",
    "scatter",
    "bar",
    "hist",
    "fill_between",
    "errorbar",
    "hv_lines",
    "text_annotate",
    "ticks_legend",
    "figsize",
    "many_series",
    "alpha",
    "facecolor",
    "imshow",
    "imshow_cbar",
    "imshow_log",
    "subplots_shared",
    "style_ggplot",
    "spans",
    "bar_stacked",
    "bar_colors",
    "hist_opts",
    "hist_bins",
    "errorbar_xy",
    "fill_where",
    "cmap_reversed",
    "cmap_lognorm",
    "categorical",
    "mathtext",
    "pcolormesh",
    "gridspec",
    "dates",
    "quiver",
    "clabel",
    "inset",
    "patches",
    "polar",
    "interp",
    "hist2d",
    "hexbin",
    "matshow",
    "eventplot",
    "broken_barh",
    "streamplot",
    "table",
    "surface3d_extra",
    "mathtext_frac",
    "hatch",
    "colorbar_orient",
    "margins",
    "fills",
    "axes_facecolor",
    "marker_detail",
    "text_options",
    "font_faces",
    "grid_options",
    "axes_handles2",
    "zorder",
    "nan_gap",
    "patches_path",
    "cmap_discrete",
    "hist_stacked",
    "grid_ratios",
    "formatters",
    "offset_text",
    "surface3d",
    "line3d",
    "scatter_cmap",
    "contour",
    "contourf",
    "step",
    "stem",
    "barh",
    "pie",
    "boxplot",
    "violinplot",
    "symlog",
    "axis_equal",
    "tick_style",
    "tight_layout",
    "subplots_adjust",
    "twinx",
    "colors",
    "fontsize",
    "legend_opts",
    "savefig_tight",
    "figures",
]


def setup_fig():
    fig, ax = plt.subplots(figsize=(6.4, 4.8))
    return fig, ax


def save(fig, name: str, **kw) -> None:
    REF.mkdir(parents=True, exist_ok=True)
    path = REF / f"{name}.svg"
    fig.savefig(path, format="svg", **kw)
    # A PNG reference too: the raster backend has to be measured against
    # matplotlib's own pixels, not against a rasterization of its SVG, which
    # would only tell us how the SVG renderer differs.
    fig.savefig(REF / f"{name}.png", format="png", **kw)
    fig.savefig(REF / f"{name}.pdf", format="pdf", **kw)
    fig.savefig(REF / f"{name}.eps", format="eps", **kw)
    plt.close(fig)
    print(f"wrote {path}")


def main() -> None:
    n = 100
    m = 20
    x = np.linspace(0, 2 * np.pi, n)
    y = np.sin(x)
    y2 = np.cos(x)
    y3 = 0.5 * np.sin(2 * x)

    xl = np.logspace(-1, 1, m)
    yl = xl**2
    yl2 = 10.0 * np.exp(-xl)

    # 1 basic_line
    fig, ax = setup_fig()
    ax.plot(x, y, "k-", label="sin")
    ax.set_title("Basic line")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0, 2 * np.pi)
    ax.set_ylim(-1.2, 1.2)
    save(fig, "basic_line")

    # 2 multi_style
    fig, ax = setup_fig()
    ax.plot(x, y, "b-o", label="sin")
    ax.plot(x, y2, "r--", label="cos")
    ax.plot(x, y3, "g.", label="half sin2")
    ax.set_title("Multiple styles")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0, 2 * np.pi)
    ax.set_ylim(-1.5, 1.5)
    save(fig, "multi_style")

    # 3 markers_only
    fig, ax = setup_fig()
    xs, ys, ys2, ys3 = x[::5], y[::5], y2[::5], y3[::5]
    ax.plot(xs, ys, "rx", label="x marks")
    ax.plot(xs, ys2, "bo", label="circles")
    ax.plot(xs, ys3, "g.", label="points")
    ax.set_title("Markers only")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.legend()
    ax.set_xlim(0, 2 * np.pi)
    ax.set_ylim(-1.5, 1.5)
    save(fig, "markers_only")

    # 4 semilogx
    fig, ax = setup_fig()
    ax.semilogx(xl, yl, "b-", label="x^2")
    ax.set_title("semilogx")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0.1, 10.0)
    ax.set_ylim(0.0, 120.0)
    save(fig, "semilogx")

    # 5 semilogy
    fig, ax = setup_fig()
    ax.semilogy(xl, yl2, "r-o", label="10*exp(-x)")
    ax.set_title("semilogy")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0.1, 10.0)
    ax.set_ylim(1e-4, 20.0)
    save(fig, "semilogy")

    # 6 loglog
    fig, ax = setup_fig()
    ax.loglog(xl, yl, "k-", label="x^2")
    ax.set_title("loglog")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    ax.set_xlim(0.1, 10.0)
    ax.set_ylim(0.01, 100.0)
    save(fig, "loglog")

    # 7 subplots_2x1
    fig, (ax_top, ax_bot) = plt.subplots(2, 1, figsize=(6.4, 4.8))
    ax_top.plot(x, y, "b-", label="sin")
    ax_top.set_title("top: sin")
    ax_top.set_xlabel("x")
    ax_top.set_ylabel("y")
    ax_top.grid(True)
    ax_top.legend()

    ax_bot.plot(x, y2, "r--", label="cos")
    ax_bot.set_title("bottom: cos")
    ax_bot.set_xlabel("x")
    ax_bot.set_ylabel("y")
    ax_bot.grid(True)
    ax_bot.legend()

    fig.suptitle("fplotlib subplots")
    save(fig, "subplots_2x1")

    # 8 subplots_2x2
    fig, axs = plt.subplots(2, 2, figsize=(6.4, 4.8))
    axs[0, 0].plot(x, y, "b-", label="sin")
    axs[0, 0].set_title("sin")
    axs[0, 0].grid(True)

    axs[0, 1].plot(x, y2, "r--", label="cos")
    axs[0, 1].set_title("cos")
    axs[0, 1].grid(True)

    axs[1, 0].plot(x, y3, "g-", label="0.5 sin(2x)")
    axs[1, 0].set_title("half sin 2x")
    axs[1, 0].grid(True)

    axs[1, 1].semilogx(xl, yl, "k-", label="x^2")
    axs[1, 1].set_title("semilogx panel")
    axs[1, 1].grid(True)

    fig.suptitle("fplotlib 2x2 subplots")
    save(fig, "subplots_2x2")

    # 9 markers_gallery
    codes = ["o", "x", ".", "s", "^", "v", "<", ">", "*", "+", "D"]
    xm = np.arange(1.0, 7.0)
    fig, ax = setup_fig()
    for i, code in enumerate(codes, start=1):
        ax.plot(xm, np.full_like(xm, float(i)), marker=code, linestyle="None",
                label=code)
    ax.set_title("Markers")
    ax.set_xlim(0.0, 7.0)
    ax.set_ylim(0.0, len(codes) + 1.0)
    save(fig, "markers_gallery")

    # 10 scatter
    xs = np.linspace(0.0, 10.0, 30)
    ys = np.sin(xs) + 0.1 * xs
    fig, ax = setup_fig()
    ax.scatter(xs, ys, label="points")
    ax.set_title("Scatter")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "scatter")

    # 11 bar
    xb = np.arange(1.0, 7.0)
    hb = np.array([3.0, 5.0, 2.0, 7.0, 4.0, 6.0])
    hb2 = np.array([1.0, 2.0, 4.0, 1.0, 3.0, 2.0])
    fig, ax = setup_fig()
    ax.bar(xb, hb, label="counts")
    ax.set_title("Bar")
    ax.set_xlabel("category")
    ax.set_ylabel("value")
    ax.legend()
    save(fig, "bar")

    # 12 hist
    xh = np.array([
        0.2, 0.5, 0.7, 1.1, 1.3, 1.4, 1.8, 2.0, 2.1, 2.3,
        2.4, 2.6, 2.9, 3.0, 3.2, 3.3, 3.7, 4.0, 4.4, 4.9,
    ])
    fig, ax = setup_fig()
    ax.hist(xh, bins=8, label="samples")
    ax.set_title("Histogram")
    ax.set_xlabel("value")
    ax.set_ylabel("count")
    ax.legend()
    save(fig, "hist")

    # 13 fill_between
    fig, ax = setup_fig()
    ax.fill_between(x, y, y3, alpha=0.5, label="band")
    ax.plot(x, y, "b-", label="sin")
    ax.set_title("Fill between")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "fill_between")

    # 14 errorbar
    xe = np.arange(1.0, 9.0)
    ye = np.sqrt(xe)
    ee = 0.15 * ye
    fig, ax = setup_fig()
    ax.errorbar(xe, ye, yerr=ee, fmt="o-", capsize=3.0, label="meas")
    ax.set_title("Errorbar")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "errorbar")

    # 15 hv_lines
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin")
    ax.axhline(0.0, color="k", linestyle="--")
    ax.axvline(np.pi, color="r", linestyle=":")
    ax.set_title("Reference lines")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "hv_lines")

    # 16 text_annotate
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin")
    ax.text(1.0, 0.8, "peak region", color="k")
    ax.annotate("minimum", xy=(4.712, -1.0), xytext=(2.2, -0.55), color="r")
    ax.set_title("Text and annotate")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    save(fig, "text_annotate")

    # 17 ticks_legend
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin")
    ax.plot(x, np.cos(x), "r--", label="cos")
    ax.set_xticks([0.0, 1.5708, 3.1416, 4.7124, 6.2832],
                  ["0", "pi/2", "pi", "3pi/2", "2pi"])
    ax.set_yticks([-1.0, 0.0, 1.0])
    ax.minorticks_on()
    ax.legend(loc="lower left")
    ax.set_title("Ticks and legend placement")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "ticks_legend")

    # 18 figsize
    fig, ax = plt.subplots(figsize=(8.0, 3.0))
    ax.plot(x, y, "b-", label="sin")
    ax.set_title("Wide figure")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    ax.legend()
    save(fig, "figsize")

    # 19 many_series
    fig, ax = setup_fig()
    for i in range(1, 41):
        ax.plot(x, np.sin(x + 0.05 * i))
    ax.set_title("Forty series")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "many_series")

    # 20 alpha
    fig, ax = setup_fig()
    ax.plot(x, y, "b-", label="sin", alpha=0.35)
    ax.plot(x, np.cos(x), "r-o", label="cos", alpha=0.6)
    ax.scatter(x[:30], y[:30], s=80.0, c="g", label="pts", alpha=0.5)
    ax.set_title("Alpha")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.legend()
    save(fig, "alpha")

    # 21 facecolor
    fig, ax = setup_fig()
    ax.plot(x, y, "b-")
    ax.set_title("Figure facecolor")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.grid(True)
    save(fig, "facecolor", facecolor="#eeeeee")

    # 22/23 imshow
    nzr, nzc = 8, 16
    zimg = np.array([[np.sin(0.4 * (j + 1)) + np.cos(0.5 * (i + 1))
                      for j in range(nzc)] for i in range(nzr)])
    zlog = np.array([[10.0 ** (0.5 * (i + j + 2) / 4.0)
                      for j in range(nzc)] for i in range(nzr)])

    fig, ax = setup_fig()
    ax.imshow(zimg)
    ax.set_title("imshow")
    save(fig, "imshow")

    fig, ax = setup_fig()
    im = ax.imshow(zimg, cmap="plasma", extent=(0.0, 4.0, 0.0, 2.0), origin="lower")
    cb = fig.colorbar(im)
    cb.set_label("value")
    ax.set_title("imshow with colorbar")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "imshow_cbar")

    # 24 scatter_cmap
    ns = 30
    svals = np.array([20.0 + 8.0 * (i + 1) for i in range(ns)])
    cvals = np.array([float(i + 1) for i in range(ns)])
    fig, ax = setup_fig()
    sc = ax.scatter(x[:ns], y[:ns], s=svals, c=cvals, cmap="viridis")
    cb = fig.colorbar(sc)
    cb.set_label("c")
    ax.set_title("Scatter with c and s arrays")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "scatter_cmap")

    # 25/26 contour
    fig, ax = setup_fig()
    ax.contour(zimg)
    ax.set_title("contour")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "contour")

    fig, ax = setup_fig()
    cf = ax.contourf(zimg, cmap="coolwarm")
    fig.colorbar(cf)
    ax.set_title("contourf")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "contourf")

    # 27-30 step / stem / barh / pie
    fig, ax = setup_fig()
    ax.step(xb, hb, where="mid")
    ax.set_title("step")
    save(fig, "step")

    fig, ax = setup_fig()
    ax.stem(xb, hb)
    ax.set_title("stem")
    save(fig, "stem")

    fig, ax = setup_fig()
    ax.barh(xb, hb)
    ax.set_title("barh")
    save(fig, "barh")

    fig, ax = setup_fig()
    ax.pie(hb, labels=["a", "b", "c", "d", "e", "f"])
    ax.set_title("pie")
    save(fig, "pie")

    # 31-32 boxplot / violinplot
    k = np.arange(1, 41, dtype=float)
    dist1 = np.sin(k) + 0.3 * np.cos(2.7 * k)
    dist2 = 1.0 + 2.0 * np.sin(0.7 * k) ** 3
    wts = 0.5 + 0.02 * k
    hedges = [-3.0, -1.0, 0.0, 1.5, 3.0]

    fig, ax = setup_fig()
    ax.boxplot([dist1, dist2])
    ax.set_title("boxplot")
    save(fig, "boxplot")

    fig, ax = setup_fig()
    ax.violinplot([dist1, dist2])
    ax.set_title("violinplot")
    save(fig, "violinplot")

    # 33 symlog
    xsym = np.linspace(-100.0, 100.0, 201)
    fig, ax = setup_fig()
    ax.plot(xsym, xsym)
    ax.set_yscale("symlog")
    ax.set_title("symlog")
    save(fig, "symlog")

    # 34 axis equal
    fig, ax = setup_fig()
    ax.plot(xb, hb, marker="o")
    ax.axis("equal")
    ax.set_title("axis equal")
    save(fig, "axis_equal")

    # 35 tick styling
    fig, ax = setup_fig()
    ax.plot(xb, hb)
    ax.tick_params(direction="in", labelsize=8.0)
    ax.tick_params(axis="x", rotation=45.0)
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)
    ax.set_title("tick_params")
    save(fig, "tick_style")

    # 36 tight_layout
    fig, axs = plt.subplots(1, 2)
    axs[0].plot(xb, hb * 1e3)
    axs[0].set_ylabel("value")
    axs[0].set_xlabel("category")
    axs[1].plot(xb, hb)
    axs[1].set_xlabel("category")
    fig.tight_layout()
    save(fig, "tight_layout")

    # 37 subplots_adjust
    fig, axs = plt.subplots(2, 1)
    axs[0].plot(xb, hb)
    axs[1].plot(xb, hb)
    fig.subplots_adjust(left=0.2, hspace=0.5)
    save(fig, "subplots_adjust")

    # 38 twinx
    fig, ax = setup_fig()
    ax.plot(xb, hb)
    ax.set_ylabel("left")
    ax.set_xlabel("x")
    ax2 = ax.twinx()
    ax2.plot(xb, hb * 100.0, color="C1")
    ax2.set_ylabel("right")
    ax.set_title("twinx")
    save(fig, "twinx")

    # 39 color spellings
    fig, ax = setup_fig()
    xsm = [0.0, 1.0]
    for i, c in enumerate(
        ["red", "tab:orange", "steelblue", "#0f0", "0.5", "#8c564bcc"], start=1
    ):
        ax.plot(xsm, [float(i)] * 2, color=c, lw=3.0)
    ax.set_title("color names")
    save(fig, "colors")

    # 40 font sizes
    fig, ax = setup_fig()
    ax.plot(xb, hb, label="sine")
    ax.set_title("big title", fontsize=17.0)
    ax.set_xlabel("x axis", fontsize=14.0)
    ax.set_ylabel("y axis", fontsize=14.0)
    ax.tick_params(labelsize=13.0)
    ax.legend(fontsize=12.0)
    save(fig, "fontsize")

    # 41 legend options
    fig, ax = setup_fig()
    ax.plot(xb, hb, label="alpha")
    ax.plot(xb, hb * 0.5, label="beta")
    ax.plot(xb, hb * 0.25, label="gamma")
    ax.plot(xb, hb * 0.125, label="delta")
    ax.legend(loc="upper right", ncol=2, title="series", frameon=False)
    save(fig, "legend_opts")

    # 42 savefig bbox_inches="tight"
    fig, ax = setup_fig()
    ax.plot(xb, hb)
    ax.set_title("tight")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    save(fig, "savefig_tight", bbox_inches="tight", dpi=200)

    # 43 two live figures kept apart, then closed
    f1, a1 = setup_fig()
    a1.plot(xb, hb, "r-")
    a1.set_title("figure one")
    f2, a2 = setup_fig()
    a2.plot(xb, -hb, "b-")
    a2.set_title("figure two")
    plt.close(f2)
    save(f1, "figures")

    # 44 an image on a log axis
    fig, ax = setup_fig()
    ax.imshow(zimg, extent=(1.0, 1000.0, 0.0, 4.0), aspect="auto")
    ax.set_xscale("log")
    ax.set_title("imshow on a log axis")
    save(fig, "imshow_log")

    # 45 subplots with axes handles and a shared x axis
    fig, axs = plt.subplots(2, 2, sharex=True, figsize=(6.4, 4.8))
    axs[0, 0].plot(x, y, "b-", label="sin")
    axs[0, 0].set_title("one")
    axs[0, 0].legend()
    axs[0, 1].scatter(xs, ys, s=18.0, c="r")
    axs[0, 1].set_title("two")
    axs[1, 0].bar(xb, hb, color="g")
    axs[1, 0].set_xlabel("x")
    axs[1, 1].plot(x, y3, "m--")
    axs[1, 1].grid(True)
    axs[1, 1].set_xlabel("x")
    fig.suptitle("subplots with handles")
    save(fig, "subplots_shared")

    # 46 a style sheet
    with plt.style.context("ggplot"):
        fig, ax = plt.subplots(figsize=(6.4, 4.8))
        ax.plot(x, y, label="sin")
        ax.plot(x, y2, label="cos")
        ax.set_title("ggplot style")
        ax.set_xlabel("x")
        ax.set_ylabel("y")
        ax.legend()
        save(fig, "style_ggplot")

    # 47 spans and line runs
    fig, ax = setup_fig()
    ax.plot(x, y, "b-")
    ax.axhspan(-0.5, 0.5, color="orange", alpha=0.3)
    ax.axvspan(1.0, 2.0, color="green", alpha=0.2)
    ax.hlines([-1.0, 1.0], 0.0, 3.0, color="red", linestyle="--")
    ax.vlines([4.0, 5.0], -1.0, 0.0, color="purple")
    ax.set_title("spans and line runs")
    save(fig, "spans")

    # 48 stacked and labelled bars
    fig, ax = setup_fig()
    ax.bar(xb, hb, width=0.6, color="tab:blue", label="first")
    ax.bar(xb, hb2, width=0.6, bottom=hb, color="tab:orange", label="second")
    ax.bar_label(ax.containers[-1], padding=3)
    ax.legend()
    ax.set_title("stacked bars")
    save(fig, "bar_stacked")

    # 49 per-bar colors and horizontal bar labels
    fig, ax = setup_fig()
    ax.barh(xb, hb, color=["red", "green", "blue", "orange", "purple", "brown"])
    ax.bar_label(ax.containers[-1], padding=2)
    ax.set_title("bars in their own colors")
    save(fig, "bar_colors")

    # 50 histogram options
    fig, ax = setup_fig()
    ax.hist(dist1, bins=12, density=True, color="tab:blue", alpha=0.6,
            label="density")
    ax.hist(dist1, bins=12, density=True, histtype="step", color="k",
            label="step")
    ax.legend()
    ax.set_title("histogram options")
    save(fig, "hist_opts")

    # 51 uneven bins, cumulative
    fig, ax = setup_fig()
    ax.hist(dist1, bins=[-3.0, -1.0, 0.0, 1.5, 3.0], cumulative=True,
            color="tab:green")
    ax.set_title("uneven bins, cumulative")
    save(fig, "hist_bins")

    # 52 asymmetric errors in both directions
    fig, ax = setup_fig()
    ax.errorbar(xe, ye, yerr=[ee, 0.5 * ee], xerr=0.3 * ee, fmt="o",
                color="tab:red", capsize=4.0)
    ax.set_title("asymmetric errors")
    save(fig, "errorbar_xy")

    # 53 fill_between with a condition
    fig, ax = setup_fig()
    ax.plot(x, y, "k-")
    ax.fill_between(x, y, 0.0, where=(y > 0.0), color="tab:green", alpha=0.4,
                    label="positive")
    ax.fill_between(x, y, 0.0, where=(y < 0.0), color="tab:red", alpha=0.4,
                    label="negative")
    ax.legend()
    ax.set_title("fill_between where")
    save(fig, "fill_where")

    # 54 a reversed colormap
    fig, ax = setup_fig()
    im = ax.imshow(zimg, cmap="RdBu_r", aspect="auto")
    fig.colorbar(im, ax=ax)
    ax.set_title("RdBu_r")
    save(fig, "cmap_reversed")

    # 55 a logarithmic color norm
    fig, ax = setup_fig()
    im = ax.imshow(zlog, cmap="magma", norm=LogNorm(), aspect="auto")
    fig.colorbar(im, ax=ax)
    ax.set_title("log color norm")
    save(fig, "cmap_lognorm")

    # 56 categories instead of numbers
    fig, ax = setup_fig()
    ax.bar(["apple", "banana", "cherry", "date"], hb[:4], color="tab:purple")
    ax.set_ylabel("count")
    ax.set_title("categories")
    save(fig, "categorical")

    # 57 mathtext in the labels
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.set_xlabel(r"$x_{i}$ [m]")
    ax.set_ylabel(r"$E = mc^{2}$")
    ax.set_title(r"$10^{-3} < T^{2}_{n} < 10^{3}$")
    save(fig, "mathtext")

    # 58 a mesh with uneven cells
    fig, ax = setup_fig()
    xe = np.array([0.0, 0.5, 1.5, 3.0, 5.0, 8.0, 9.0, 10.0, 11.0, 12.0,
                   13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0])
    ye = np.array([0.0, 1.0, 2.0, 4.0, 6.0, 6.5, 7.0, 8.0, 10.0])
    m = ax.pcolormesh(xe, ye, zimg, cmap="viridis")
    fig.colorbar(m, ax=ax)
    ax.set_title("pcolormesh")
    save(fig, "pcolormesh")

    # 59 panels spanning several cells
    fig = plt.figure(figsize=(6.4, 4.8), dpi=100)
    a1 = plt.subplot2grid((3, 3), (0, 0), colspan=3)
    a2 = plt.subplot2grid((3, 3), (1, 0), colspan=2, rowspan=2)
    a3 = plt.subplot2grid((3, 3), (1, 2), rowspan=2)
    a1.plot(x, y)
    a1.set_title("wide")
    a2.plot(x, y2)
    a2.set_title("big")
    a3.plot(x, y)
    a3.set_title("tall")
    save(fig, "gridspec")

    # 60 a date axis
    fig, ax = setup_fig()
    t0 = mdates.date2num(datetime.datetime(2024, 1, 1))
    td = t0 + np.arange(0.0, 366.0, 1.0)
    ax.plot(td, np.sin(2.0 * np.pi * np.arange(366) / 365.0))
    ax.xaxis_date()
    ax.set_title("dates")
    save(fig, "dates")

    # 61 a vector field
    fig, ax = setup_fig()
    qy, qx = np.meshgrid(np.arange(8) * 0.5, np.arange(8) * 0.5, indexing="ij")
    ax.quiver(qx, qy, np.cos(qx), np.sin(qy))
    ax.set_title("quiver")
    save(fig, "quiver")

    # 62 labelled contour lines
    fig, ax = setup_fig()
    cs = ax.contour(zimg)
    ax.clabel(cs)
    ax.set_title("clabel")
    save(fig, "clabel")

    # 63 an inset and a secondary axis
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.set_title("inset")
    ax.secondary_xaxis("top", functions=(lambda v: 2.0 * v, lambda v: v / 2.0))
    axi = ax.inset_axes([0.6, 0.6, 0.35, 0.3])
    axi.plot(x, y2)
    save(fig, "inset")

    # 64 plain shapes
    fig, ax = setup_fig()
    ax.add_patch(Rectangle((0.1, 0.1), 0.4, 0.2, facecolor="tab:orange",
                           edgecolor="black"))
    ax.add_patch(Circle((0.7, 0.7), 0.15))
    ax.add_patch(Ellipse((0.3, 0.7), 0.4, 0.2, angle=30.0,
                         facecolor="tab:green", alpha=0.5))
    ax.add_patch(Polygon([[0.6, 0.1], [0.9, 0.1], [0.75, 0.4]],
                         edgecolor="tab:red", lw=2.0, fill=False))
    ax.set_title("patches")
    save(fig, "patches")

    # 65 a polar plot
    fig = plt.figure(figsize=(6.4, 4.8), dpi=100)
    ax = fig.add_subplot(projection="polar")
    th = np.linspace(0.0, 2.0 * np.pi, 100)
    ax.plot(th, 1.0 + np.cos(th))
    ax.set_title("polar")
    save(fig, "polar")

    # 66 a smoothed image
    fig, ax = setup_fig()
    ax.imshow(zimg, interpolation="bilinear")
    ax.set_title("interp")
    save(fig, "interp")

    # 67/68 two dimensional histograms
    seed = np.arange(1, 501, dtype=float)
    ha = np.sin(seed * 12.9898) * 43758.5453
    hb = np.sin(seed * 12.9898 + 1.0) * 43758.5453
    hxx = (ha - np.floor(ha)) + (hb - np.floor(hb)) - 1.0
    ha = np.sin(seed * 78.233) * 43758.5453
    hb = np.sin(seed * 78.233 + 1.0) * 43758.5453
    hyy = (ha - np.floor(ha)) + (hb - np.floor(hb)) - 1.0

    fig, ax = setup_fig()
    ax.hist2d(hxx, hyy, bins=[12, 12])
    ax.set_title("hist2d")
    save(fig, "hist2d")

    fig, ax = setup_fig()
    ax.hexbin(hxx, hyy, gridsize=15)
    ax.set_title("hexbin")
    save(fig, "hexbin")

    # 69 a matrix as an image
    fig, ax = setup_fig()
    zm = np.array([[float(i * j) for j in range(1, 7)] for i in range(1, 7)])
    ax.matshow(zm)
    save(fig, "matshow")

    # 70 a row of events
    fig, ax = setup_fig()
    seed = np.arange(1, 41, dtype=float) * 3.7
    ea = np.sin(seed) * 43758.5453
    eb = np.sin(seed + 1.0) * 43758.5453
    ev = 10.0 * ((ea - np.floor(ea)) + (eb - np.floor(eb)))
    ax.eventplot(ev, color="C0")
    ax.set_title("eventplot")
    save(fig, "eventplot")

    # 71 bars broken into pieces
    fig, ax = setup_fig()
    ax.broken_barh([(1.0, 2.0), (4.0, 2.0), (7.0, 3.0)], (1.0, 0.8), color="C0")
    ax.broken_barh([(2.0, 3.0), (6.0, 3.0)], (2.5, 0.8), color="C1")
    ax.set_title("broken_barh")
    save(fig, "broken_barh")

    # 72 streamlines of a vector field
    fig, ax = setup_fig()
    sg = np.linspace(-3.0, 3.0, 16)
    sxx, syy = np.meshgrid(sg, sg)
    ax.streamplot(sg, sg, -1.0 - sxx**2 + syy, 1.0 + sxx - syy**2)
    ax.set_title("streamplot")
    save(fig, "streamplot")

    # 73 a table below the axes
    fig, ax = setup_fig()
    ax.table(cellText=[["100", "300", "500"], ["200", "400", "600"]],
             colLabels=["a", "b", "c"], rowLabels=["x", "y"])
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_title("table")
    save(fig, "table")

    tpx, tpy, tpz = [], [], []
    k = 0
    for i in range(6):
        for j in range(4):
            k += 1
            tpx.append(i + 0.35 * np.sin(3.0 * k))
            tpy.append(j + 0.35 * np.cos(5.0 * k))
            tpz.append(tpx[-1] * tpy[-1])
    tx, ty, tz = np.array(tpx), np.array(tpy), np.array(tpz)

    # 126 a vector field with a key above the axes
    fig, ax = setup_fig()
    kx = np.arange(-10, 10, 1)
    ku, kv = np.meshgrid(kx, kx)
    q = ax.quiver(kx, kx, ku, kv)
    ax.quiverkey(q, X=0.3, Y=1.1, U=10, label="Quiver key, length = 10", labelpos="E")
    save(fig, "quiverkey")

    # 125 level lines drawn in space
    c3 = np.linspace(-3, 3, 21)
    c3x, c3y = np.meshgrid(c3, c3)
    c3z = np.sin(c3x) * np.cos(c3y)
    fig = plt.figure(figsize=(6.4, 4.8))
    a = fig.add_subplot(projection="3d")
    a.contour(c3x, c3y, c3z)
    a.set_title("contour3d")
    save(fig, "contour3d")

    # 124 arrows in space
    q3x, q3y, q3z, q3u, q3v, q3w = [], [], [], [], [], []
    for i in range(3):
        for j in range(3):
            qx = -0.8 + 0.8 * i
            qy = -0.8 + 0.8 * j
            q3x.append(qx)
            q3y.append(qy)
            q3z.append(0.0)
            q3u.append(np.sin(np.pi * qx) * np.cos(np.pi * qy))
            q3v.append(-np.cos(np.pi * qx) * np.sin(np.pi * qy))
            q3w.append(0.5 * np.sin(np.pi * qx))
    fig = plt.figure(figsize=(6.4, 4.8))
    a = fig.add_subplot(projection="3d")
    a.quiver(q3x, q3y, q3z, q3u, q3v, q3w, length=0.4, normalize=True)
    a.set_title("quiver3d")
    save(fig, "quiver3d")

    # 123 boxes standing on the xy plane
    kb = np.arange(1, 13)
    b3x = ((kb - 1) % 4).astype(float)
    b3y = ((kb - 1) // 4).astype(float)
    b3z = np.zeros(12)
    b3d = 1.0 + np.sin(kb.astype(float))
    fig = plt.figure(figsize=(6.4, 4.8))
    a = fig.add_subplot(projection="3d")
    a.bar3d(b3x, b3y, b3z, 0.6, 0.6, b3d)
    a.set_title("bar3d")
    save(fig, "bar3d")

    # 122 a surface over scattered points
    ks = np.arange(1, 61)
    usx = 3.0 * np.sin(2.0 * ks)
    usy = 3.0 * np.cos(3.0 * ks)
    usz = np.sin(usx) * np.cos(usy)
    fig = plt.figure(figsize=(6.4, 4.8))
    a = fig.add_subplot(projection="3d")
    a.plot_trisurf(usx, usy, usz, cmap="viridis")
    a.set_title("trisurf")
    save(fig, "trisurf")

    kk = np.arange(1, 121)
    tx2 = 3.0 * (1.0 + np.sin(7.0 * kk))
    ty2 = 3.0 * (1.0 + np.cos(11.0 * kk))
    tz2 = np.sin(tx2) * np.cos(ty2)

    # 121 the bands between those lines, filled
    fig, a = plt.subplots(figsize=(6.4, 4.8))
    tcf = a.tricontourf(tx2, ty2, tz2)
    fig.colorbar(tcf, ax=a)
    a.set_title("tricontourf")
    save(fig, "tricontourf")

    # 120 level lines over the triangles
    fig, a = plt.subplots(figsize=(6.4, 4.8))
    a.tricontour(tx2, ty2, tz2)
    a.set_title("tricontour")
    save(fig, "tricontour")

    # 119 the same triangles, filled by value
    fig, a = plt.subplots(figsize=(6.4, 4.8))
    tpc = a.tripcolor(tx, ty, tz)
    fig.colorbar(tpc, ax=a)
    a.set_title("tripcolor")
    save(fig, "tripcolor")

    # 118 scattered points, triangulated
    fig, a = plt.subplots(figsize=(6.4, 4.8))
    a.triplot(tx, ty, "o-")
    a.set_title("triplot")
    save(fig, "triplot")

    # 117 a log axis too short to label by decades alone
    fig, a = plt.subplots(figsize=(6.4, 4.8))
    lx = np.linspace(1.0, 8.0, 60)
    a.loglog(lx, lx**1.5)
    a.set_xlim(1.0, 8.0)
    save(fig, "log_minor_labels")

    # 116 a bowed connector and a box with round corners
    fig, a = plt.subplots(figsize=(6.4, 4.8))
    a.plot(x, y)
    a.annotate("peak", xy=(1.57, 1.0), xytext=(3.5, 0.6), ha="center",
               arrowprops=dict(arrowstyle="->",
                               connectionstyle="arc3,rad=0.3"),
               bbox=dict(boxstyle="round,pad=0.5", fc="#ffff99",
                         ec="#333333"))
    a.annotate("trough", xy=(4.71, -1.0), xytext=(1.5, -0.6), ha="center",
               arrowprops=dict(arrowstyle="->",
                               connectionstyle="arc3,rad=-0.4"))
    save(fig, "annotate_curve")

    # 115 margins refitted to the decorations at draw time
    fig, cax = plt.subplots(1, 2, figsize=(6.4, 4.8), layout="constrained")
    for i, a in enumerate(cax.ravel()):
        a.plot(x, (i + 1) * y)
        a.set_xlabel("a long x label")
        a.set_ylabel("a long y label")
        a.set_title("panel")
    save(fig, "constrained")

    # 114 panels drawn as a picture of the figure
    fig = plt.figure(figsize=(6.4, 4.8))
    mos = fig.subplot_mosaic("AB\nCC")
    mos["A"].plot(x, y)
    mos["A"].set_title("A")
    mos["B"].plot(x, y2, "r-")
    mos["B"].set_title("B")
    mos["C"].plot(x, 0.5 * y, "g-")
    mos["C"].set_title("C")
    save(fig, "mosaic")

    # 113 names given at legend time, and a line kept out of it
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.plot(x, y2)
    ax.plot(x, 0.5 * y, label="_hidden")
    ax.legend(labels=["first", "second"])
    ax.set_title("legend labels")
    save(fig, "legend_labels")

    # 112 violins across, with the mean and median marked
    boxmat = np.vstack([dist1, dist2])
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].violinplot([boxmat[0], boxmat[1]], showmeans=True, showmedians=True)
    axs[0].set_xticks([1, 2], ["one", "two"])
    axs[0].set_title("means")
    axs[1].violinplot([boxmat[0], boxmat[1]], vert=False, showextrema=False)
    axs[1].set_yticks([1, 2], ["one", "two"])
    axs[1].set_title("across")
    save(fig, "violin_opts")

    # 111 boxes across, waisted, filled and with the mean marked
    boxmat = np.vstack([dist1, dist2])
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].boxplot(boxmat.T, tick_labels=["one", "two"], notch=True,
                   showmeans=True, patch_artist=True)
    axs[0].set_title("notched")
    axs[1].boxplot(boxmat.T, tick_labels=["one", "two"], vert=False)
    axs[1].set_title("across")
    save(fig, "box_opts")

    # 110 a pie with a slice pulled out and its shares written in
    fig, ax = plt.subplots(figsize=(6.4, 4.8))
    pievals = np.array([3.0, 5.0, 2.0, 7.0, 4.0, 6.0])
    ax.pie(pievals, labels=["a", "b", "c", "d", "e", "f"],
           explode=[0.0, 0.1, 0.0, 0.0, 0.0, 0.0],
           startangle=90.0, counterclock=False, autopct="%.1f%%")
    ax.set_title("pie options")
    save(fig, "pie_opts")

    # 109 marker edges and a band carried to the crossing
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].scatter(x[:16], y[:16], s=120.0, c="skyblue",
                   edgecolors="black", linewidths=1.5)
    axs[1].plot(x, y, "b-")
    axs[1].plot(x, y2, "r-")
    axs[1].fill_between(x, y, y2, color="green", alpha=0.4,
                        where=y > y2, interpolate=True)
    save(fig, "scatter_edge")

    # 108 one legend for the whole figure
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].plot(x, y, "b-", label="sin")
    axs[1].plot(x, y2, "r--", label="cos")
    fig.legend(loc="upper right")
    save(fig, "figlegend")

    # 107 minor ticks placed by hand, and a pruned locator
    fig, axs = plt.subplots(2, 1, figsize=(6.4, 4.8))
    axs[0].plot(x, y, "b-")
    axs[0].set_xticks([0.0, 2.0, 4.0, 6.0])
    axs[0].set_xticks([1.0, 3.0, 5.0], minor=True)
    axs[0].set_title("minor by hand")
    axs[1].plot(x, y, "r-")
    axs[1].locator_params(axis="y", prune="both")
    axs[1].set_title("prune=both")
    save(fig, "tick_locator")

    # 106 colours for what is off the scale, and a colormap of our own
    from matplotlib.colors import LinearSegmentedColormap
    zn2 = np.zeros((16, 16))
    for i in range(16):
        for j in range(16):
            zn2[i, j] = (i + 1) + (j + 1)
    zn2[3:6, 3:6] = np.nan
    cm = plt.get_cmap("viridis").copy()
    cm.set_bad("red")
    cm.set_under("black")
    cm.set_over("white")
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].imshow(zn2, cmap=cm, vmin=8, vmax=24)
    axs[0].set_title("bad/under/over")
    own = LinearSegmentedColormap.from_list("own", ["#000080", "#ffffff", "#800000"])
    axs[1].imshow(zn2, cmap=own)
    axs[1].set_title("own colormap")
    save(fig, "cmap_special")

    # 105 three ways of spreading values over a colormap
    from matplotlib.colors import TwoSlopeNorm, PowerNorm, SymLogNorm
    zn = np.zeros((16, 16))
    for i in range(16):
        for j in range(16):
            zn[i, j] = (j - 7) * (i + 1) / 4.0
    fig, axs = plt.subplots(1, 3, figsize=(6.4, 4.8))
    axs[0].imshow(zn, cmap="coolwarm",
                  norm=TwoSlopeNorm(vcenter=0.0, vmin=zn.min(), vmax=zn.max()))
    axs[0].set_title("centered")
    axs[1].imshow(np.abs(zn), cmap="viridis", norm=PowerNorm(gamma=0.5))
    axs[1].set_title("power")
    axs[2].imshow(zn, cmap="coolwarm",
                  norm=SymLogNorm(linthresh=1.0, base=10,
                                  vmin=zn.min(), vmax=zn.max()))
    axs[2].set_title("symlog")
    save(fig, "norms")

    # 104 an image whose colours are given outright
    rgbim = np.zeros((8, 12, 3))
    for i in range(8):
        for j in range(12):
            rgbim[i, j, 0] = j / 11.0
            rgbim[i, j, 1] = i / 7.0
            rgbim[i, j, 2] = 0.5
    fig, ax = setup_fig()
    ax.imshow(rgbim)
    ax.set_title("imshow of an RGB array")
    save(fig, "imshow_rgb")

    # 103 a histogram on its side, one on a log axis, and one with gaps
    fig, axs = plt.subplots(1, 3, figsize=(6.4, 4.8))
    axs[0].hist(dist1, bins=12, orientation="horizontal", color="C0")
    axs[0].set_title("horizontal")
    axs[1].hist(dist1, bins=12, log=True, color="C1")
    axs[1].set_title("log")
    axs[2].hist(dist1, bins=12, rwidth=0.7, color="C2")
    axs[2].set_title("rwidth")
    save(fig, "hist_orient")

    # 102 a title against one end, and labels pushed further out
    fig, ax = setup_fig()
    ax.plot(x, y, "b-")
    ax.set_title("left title", loc="left")
    ax.set_xlabel("x", labelpad=12)
    ax.set_ylabel("y", labelpad=16)
    save(fig, "title_loc")

    # 101 bars with error bars, edge alignment and their own tick labels
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].bar([0, 1, 2], [3, 5, 2], yerr=[0.4, 0.6, 0.3], capsize=4,
               tick_label=["a", "b", "c"])
    axs[0].set_title("yerr")
    axs[1].bar([0, 1, 2], [3, 5, 2], align="edge", color="g")
    axs[1].set_title("align=edge")
    save(fig, "bar_err")

    # 100 axes that count down, and limits read back out
    fig, ax = setup_fig()
    ax.plot(x, y, "b-")
    ax.invert_xaxis()
    ax.invert_yaxis()
    lo, hi = ax.get_xlim()
    ax.text(0.05, 0.1, "x from %6.2f to %6.2f" % (lo, hi), transform=ax.transAxes)
    ax.set_title("inverted axes")
    save(fig, "invert_axes")

    # 99 an annotation with a real arrow head
    fig, ax = setup_fig()
    ax.plot(x, y, "b-")
    ax.annotate("minimum", xy=(4.712, -1.0), xytext=(2.2, -0.55),
                arrowprops=dict(arrowstyle="->"))
    ax.annotate("start", xy=(0.0, 0.0), xytext=(1.0, 0.6),
                arrowprops=dict(arrowstyle="->", color="r", linewidth=1.5))
    ax.set_title("annotate arrows")
    save(fig, "annotate_arrow")

    # 98 text pinned to the corners of the axes, not to the data
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.text(0.05, 0.95, "top left", transform=ax.transAxes, va="top")
    ax.text(0.95, 0.05, "bottom right", transform=ax.transAxes, ha="right")
    ax.set_title("transform=axes")
    save(fig, "transform_axes")

    # 97 one array is enough
    fig, ax = setup_fig()
    ax.plot(y)
    ax.plot(y * 0.5, "o--")
    ax.set_title("plot(y)")
    save(fig, "plot_y")

    # 96 a wireframe and a colormapped surface
    fig = plt.figure(figsize=(6.4, 4.8))
    s3 = np.linspace(-3, 3, 31)
    sx, sy = np.meshgrid(s3, s3)
    sz = np.sin(np.sqrt(sx ** 2 + sy ** 2))
    ax = fig.add_subplot(1, 2, 1, projection="3d")
    ax.plot_wireframe(sx, sy, sz, color="tab:blue")
    ax.set_title("wireframe")
    ax = fig.add_subplot(1, 2, 2, projection="3d")
    ax.plot_surface(sx, sy, sz, cmap="viridis")
    ax.set_title("colormapped")
    save(fig, "surface3d_extra")

    # 95 fractions, roots and greek
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.set_xlabel(r"$\frac{x}{2}$")
    ax.set_ylabel(r"$\sqrt{y}$")
    ax.set_title(r"$T = 2\pi\sqrt{\frac{L}{g}}$, $\Omega = \alpha + \beta$")
    save(fig, "mathtext_frac")

    # 94 hatched fills
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    hx6 = np.arange(1.0, 7.0)
    hh1 = np.array([3.0, 5.0, 2.0, 7.0, 4.0, 6.0])
    hh2 = np.array([1.0, 2.0, 4.0, 1.0, 3.0, 2.0])
    axs[0].bar(hx6, hh1, hatch="/")
    axs[0].bar(hx6, hh2, bottom=hh1, hatch="\\\\")
    axs[0].set_title("hatched bars")
    axs[1].fill_between(x, y, hatch="x", color="tab:green", alpha=0.4,
                        edgecolor="tab:green")
    axs[1].add_patch(Rectangle((1.0, -0.8), 2.0, 0.6,
                     facecolor="white", edgecolor="black", hatch="|||"))
    axs[1].set_title("hatched fill")
    save(fig, "hatch")

    # 93 a colorbar lying on its side
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    im = axs[0].imshow(zimg, cmap="viridis", aspect="auto")
    fig.colorbar(im, ax=axs[0], orientation="horizontal", label="value")
    axs[0].set_title("horizontal")
    im = axs[1].imshow(zimg, cmap="viridis", aspect="auto")
    fig.colorbar(im, ax=axs[1], shrink=0.6, aspect=10.0)
    axs[1].set_title("shrunk")
    save(fig, "colorbar_orient")

    # 92 how much room the data is given
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].plot(x, y)
    axs[0].margins(x=0.0, y=0.3)
    axs[0].set_title("margins")
    axs[1].plot(x, y)
    # matplotlib defers autoscaling to the draw, so the limits only exist
    # once something has drawn them; fplotlib computes them on demand, so the
    # two agree only after a draw is forced here.
    fig.canvas.draw()
    axs[1].autoscale(False)
    axs[1].plot(x, 3.0 * y)
    axs[1].set_title("autoscale off")
    save(fig, "margins")

    # 91 stacked bands, a band along y, and an endless line
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    stx = np.arange(12.0)
    sty = np.vstack([1.0 + 0.5 * stx, 2.0 + np.sin(0.5 * stx), 3.0 - 0.1 * stx])
    axs[0].stackplot(stx, sty, labels=["low", "mid", "high"])
    axs[0].legend(loc="upper left")
    axs[0].set_title("stackplot")
    axs[1].fill_betweenx(x, y - 1.5, np.cos(x) + 1.5, color="tab:orange", alpha=0.5)
    axs[1].axline((0.0, 0.0), slope=1.5, color="tab:red", linestyle="--")
    axs[1].set_title("fill_betweenx and axline")
    save(fig, "fills")

    # 90 a background colour for one axes only
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].plot(x, y)
    axs[0].set_facecolor("#eeeeee")
    axs[0].grid(True, color="white")
    axs[1].plot(x, y)
    save(fig, "axes_facecolor")

    # 89 marker and line detail
    fig, ax = setup_fig()
    ax.plot(x, y, marker="o", markevery=8, markerfacecolor="white",
            markeredgecolor="tab:red", markersize=7.0)
    ax.plot(x, y - 1.0, color="tab:green", dashes=[8.0, 2.0, 2.0, 2.0])
    ax.plot(x[:8], y[:8] + 1.0, color="tab:purple", drawstyle="steps-post")
    save(fig, "marker_detail")

    # 88 text options: rotation, vertical alignment, a box and two lines
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.text(1.0, 0.6, "rotated", rotation=30.0)
    ax.text(4.0, 0.6, "top", va="top", ha="center")
    ax.text(4.0, -0.6, "boxed", ha="center", va="center",
            bbox=dict(boxstyle="square,pad=0.3", facecolor="yellow",
                      edgecolor="black"))
    ax.text(1.0, -0.4, "two\nlines", va="top")
    fig.text(0.02, 0.02, "figure corner", fontsize=8.0)
    save(fig, "text_options")

    # 87 bold and italic
    fig, ax = setup_fig()
    ax.plot(x, y)
    ax.set_title("bold title", fontweight="bold")
    ax.set_xlabel("italic x", fontstyle="italic")
    ax.set_ylabel("bold italic y", fontweight="bold", fontstyle="italic")
    ax.text(2.0, 0.5, "emphasis", fontstyle="italic")
    ax.text(2.0, -0.5, "strong", fontweight="bold")
    save(fig, "font_faces")

    # 86 the grid: which ticks it follows and what it looks like
    fig, axs = plt.subplots(1, 2, figsize=(6.4, 4.8))
    axs[0].plot(x, y)
    axs[0].grid(True, which="both", color="0.7", linestyle=":", linewidth=0.6)
    axs[0].minorticks_on()
    axs[0].set_title("both")
    axs[1].plot(x, y)
    axs[1].grid(True, axis="y", color="tab:blue", linestyle="--", alpha=0.4)
    axs[1].set_title("y only")
    save(fig, "grid_options")

    # 85 the same calls, reached through an axes handle
    fig, axs = plt.subplots(2, 2, figsize=(6.4, 4.8))
    axs[0, 0].boxplot(dist1)
    axs[0, 0].set_title("box")
    axs[0, 1].violinplot(dist2)
    axs[0, 1].set_title("violin")
    axs[1, 0].semilogy(x + 1.0, np.exp(x))
    axs[1, 0].minorticks_on()
    axs[1, 1].pie([3.0, 1.0, 2.0])
    axs[1, 1].set_title("pie")
    save(fig, "axes_handles2")

    # 84 what is drawn over what
    fig, ax = setup_fig()
    zx = np.arange(1.0, 7.0)
    zh = np.array([3.0, 5.0, 2.0, 7.0, 4.0, 6.0])
    ax.bar(zx, zh, color="tab:blue")
    ax.fill_between(zx, zh * 0.5, color="tab:green", alpha=0.7, zorder=3)
    ax.plot(zx, zh, "r-", lw=2.0)
    ax.grid(True)
    ax.set_title("layers")
    save(fig, "zorder")

    # 83 missing data: the line breaks at it and the axes do not stretch
    fig, ax = setup_fig()
    xgap = np.arange(1, 41, dtype=float)
    ygap = np.sin(0.2 * xgap)
    yband = ygap - 0.4
    ygap[11:15] = np.nan
    yband[29:32] = np.nan
    ax.fill_between(xgap, ygap, yband, color="tab:orange", alpha=0.5)
    ax.plot(xgap, ygap, "b-o", label="with a gap")
    ax.legend(loc="upper right")
    ax.set_title("missing data")
    save(fig, "nan_gap")

    # 82 an arrow patch and a path of cubic curves
    fig, ax = setup_fig()
    ax.set_xlim(0.0, 1.0)
    ax.set_ylim(0.0, 1.2)
    ax.add_patch(Arrow(0.1, 0.1, 0.6, 0.4, width=0.2,
                       facecolor="tab:blue", edgecolor="k"))
    pp = MPath([(0.1, 0.8), (0.3, 1.1), (0.6, 0.5), (0.9, 0.8)],
               [MPath.MOVETO, MPath.CURVE4, MPath.CURVE4, MPath.CURVE4])
    ax.add_patch(PathPatch(pp, facecolor="tab:orange", edgecolor="k", lw=2.0))
    ax.set_title("an arrow and a path")
    save(fig, "patches_path")

    # 81 bands of a discrete norm, and the qualitative maps
    fig = plt.figure(figsize=(6.4, 4.8))
    a1 = fig.add_subplot(1, 2, 1)
    bn = BoundaryNorm([-2.0, -1.0, 0.0, 1.0, 2.0], 256)
    im = a1.imshow(zimg, cmap="viridis", norm=bn)
    fig.colorbar(im, ax=a1)
    a1.set_title("bands")
    a2 = fig.add_subplot(1, 2, 2)
    a2.imshow(zimg, cmap="tab20")
    a2.set_title("tab20")
    save(fig, "cmap_discrete")

    # 80 a stacked histogram, and one whose samples carry weights
    fig = plt.figure(figsize=(6.4, 4.8))
    a1 = fig.add_subplot(1, 2, 1)
    a1.hist([dist1, dist2], bins=hedges, stacked=True, label=["one", "two"])
    a1.legend()
    a1.set_title("stacked")
    a2 = fig.add_subplot(1, 2, 2)
    a2.hist(dist1, bins=hedges, weights=wts, color="tab:purple")
    a2.set_title("weighted")
    save(fig, "hist_stacked")

    # 79 a grid whose columns and rows are not equal
    fig = plt.figure(figsize=(6.4, 4.8))
    gs = fig.add_gridspec(2, 2, width_ratios=[2, 1], height_ratios=[1, 2])
    a1 = fig.add_subplot(gs[0, 0]); a1.plot(x, y, "b-"); a1.set_title("wide")
    a2 = fig.add_subplot(gs[0, 1]); a2.plot(x, y2, "r-"); a2.set_title("narrow")
    a3 = fig.add_subplot(gs[1, 0]); a3.plot(x, y2, "g-")
    a4 = fig.add_subplot(gs[1, 1]); a4.plot(x, y, "k-")
    save(fig, "grid_ratios")

    # 78 named formatters and a locator
    fig, ax = setup_fig()
    ax.plot(1000 * np.arange(len(x)), 50 + 40 * y)
    ax.yaxis.set_major_formatter(mticker.PercentFormatter())
    ax.xaxis.set_major_formatter(mticker.StrMethodFormatter("{x:,.0f}"))
    ax.yaxis.set_major_locator(mticker.MultipleLocator(25))
    ax.set_title("formatters")
    save(fig, "formatters")

    # 77 a large y axis and an offset x axis
    fig, ax = setup_fig()
    ax.plot(1e5 + np.linspace(0, 3, len(x)), 2e6 * y)
    ax.set_title("offset text")
    save(fig, "offset_text")

    # 75 a 3D surface
    fig = plt.figure(figsize=(6.4, 4.8))
    ax = fig.add_subplot(projection="3d")
    s3 = np.linspace(-3, 3, 31)
    sx, sy = np.meshgrid(s3, s3)
    ax.plot_surface(sx, sy, np.sin(np.sqrt(sx ** 2 + sy ** 2)))
    ax.set_title("surface3d")
    save(fig, "surface3d")

    # 76 a 3D line and a 3D scatter
    fig = plt.figure(figsize=(6.4, 4.8))
    ax = fig.add_subplot(projection="3d")
    t3 = np.linspace(0, 4 * np.pi, 100)
    ax.plot(np.cos(t3), np.sin(t3), t3 / (4 * np.pi))
    ax.scatter(np.cos(t3[::10]), np.sin(t3[::10]), t3[::10] / (4 * np.pi), c="r")
    ax.set_xlabel("x")
    ax.set_ylabel("y")
    ax.set_zlabel("z")
    ax.set_title("line3d")
    save(fig, "line3d")

    # 74 an animation. matplotlib's own PillowWriter is exactly this: every
    # frame saved as a PNG and handed to Pillow, which quantizes and writes
    # the GIF.
    frames = []
    for k in range(20):
        fig, ax = setup_fig()
        ax.plot(x, np.sin(x + k * 0.3), "b-")
        ax.set_ylim(-1.5, 1.5)
        ax.set_title("anim_sine")
        buf = io.BytesIO()
        fig.savefig(buf, format="png")
        plt.close(fig)
        frames.append(Image.open(buf).convert("RGB"))
    frames[0].save(
        REF / "anim_sine.gif",
        save_all=True,
        append_images=frames[1:],
        duration=100,
        loop=0,
    )
    print("wrote tests/refs/anim_sine.gif")

    print("All matplotlib references written.")


if __name__ == "__main__":
    main()
