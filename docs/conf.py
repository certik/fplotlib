# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html
import os
import sys
sys.path.insert(0, os.path.abspath('..'))
sys.path.insert(0, os.path.abspath('../src'))
# fplotlib_gallery, the Sphinx-Gallery glue that builds, runs and scrapes the
# Fortran examples in docs/gallery, is the sphinx-fplotlib-gallery package in
# ../sphinx-fplotlib-gallery. Install it from there so the docs always use the
# in-tree version: pip install ./sphinx-fplotlib-gallery

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = 'fplotlib'
copyright = '2025, fplotlib maintainers'
author = 'fortran-lang community'
with open(os.path.join("..", "VERSION")) as f:
    release = f.read().strip()

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = [
    'sphinx.ext.autodoc',
    'sphinx.ext.autosummary',
    'sphinx.ext.mathjax',
    'sphinx.ext.viewcode',
    'myst_parser',
    'sphinx_fortran_domain',
    'sphinx_gallery.gen_gallery',
    'fplotlib_gallery',
]

templates_path = ['_templates']
# 'gallery' holds the gallery *sources*; Sphinx-Gallery copies them into
# 'auto_gallery', so leaving them visible here would give every label and
# document in them a duplicate.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store', 'gallery', '_generated']

# Select a lexer (built-in: "regex")
fortran_lexer = "ford"

# Doc comment markers to recognize (comment-only lines)
fortran_doc_chars = ["!", ">"]

fortran_sources = [
    "../src",
    "../examples",
]

# -- Gallery -----------------------------------------------------------------
# The examples in docs/gallery are Fortran programs. fplotlib_gallery builds
# them with fpm against this checkout, runs each one in a scratch directory and
# feeds whatever pictures it wrote to fplotlib_scraper, which embeds them under
# the code block that saved them.
#
# Pass -D plot_gallery=0 to sphinx-build to list the examples without a Fortran
# toolchain: they are still parsed and highlighted, just not run.

sphinx_gallery_conf = {
    "examples_dirs": ["gallery"],
    "gallery_dirs": ["auto_gallery"],
    "filename_pattern": r"[\\/]plot_",
    "image_scrapers": ("fplotlib_gallery.fplotlib_scraper",),
    # Nothing Python runs here, so there are no modules to reset between
    # examples and matplotlib is not needed to build the docs.
    "reset_modules": (),
    # Backreferences and repr capture are Python concepts; skip both.
    "inspect_global_variables": False,
    "within_subsection_order": "FileNameSortKey",
    "remove_config_comments": True,
}

# fplotlib_gallery settings; the defaults suit an in-tree build, where the
# examples are built against this checkout. See sphinx-fplotlib-gallery's
# README for the full list.
fplotlib_gallery_compiler = os.environ.get("FPM_FC")
# The examples may use the Fortran standard library, as a Python one would use
# numpy (stdlib_math's meshgrid, for one). "*" is fpm's metapackage for it.
fplotlib_gallery_dependencies = {"stdlib": '"*"'}

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'pydata_sphinx_theme'
# html_static_path = ['_static']
# html_logo = "_static/images/logo.png"

# Keep the top-left navbar title concise.
html_title = f"{project} {release}"

# If true, the current module name will be prepended to all description
# unit titles (such as .. function::).
add_module_names = False

# -- Benchmarks --------------------------------------------------------------
# docs/benchmarks.md includes a page rendered from benchmarks/results/*.json.
sys.path.insert(0, os.path.abspath('../benchmarks'))
import render_results  # noqa: E402

os.makedirs('_generated', exist_ok=True)
with open(os.path.join('_generated', 'benchmarks_results.md'), 'w') as f:
    f.write(render_results.render())
