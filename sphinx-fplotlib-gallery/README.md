# sphinx-fplotlib-gallery

A [Sphinx](https://www.sphinx-doc.org) extension that lets
[Sphinx-Gallery](https://sphinx-gallery.github.io) build, run and show Fortran
examples that draw with [fplotlib](https://github.com/fortran-lang/fplotlib).

Each example is a Fortran program. The extension builds all of them with
[fpm](https://fpm.fortran-lang.org), runs each one in a scratch directory, and
embeds every picture it saved under the code block that saved it, the same way
Sphinx-Gallery shows matplotlib figures.

## Install

```sh
pip install sphinx-fplotlib-gallery
```

Building the gallery also needs `fpm` and a Fortran compiler on `PATH`.
Without them, pass `-D plot_gallery=0` to `sphinx-build`: the examples are then
rendered and highlighted but not run.

## Configure

In `conf.py`:

```python
extensions = [
    "sphinx_gallery.gen_gallery",
    "fplotlib_gallery",
]

sphinx_gallery_conf = {
    "examples_dirs": ["gallery"],        # where the .f90 examples live
    "gallery_dirs": ["auto_gallery"],    # where the generated pages go
    "filename_pattern": r"[\/]plot_",   # which examples to run
    "image_scrapers": ("fplotlib_gallery.fplotlib_scraper",),
    "reset_modules": (),
    "inspect_global_variables": False,
}
```

The Fortran suffixes are registered with Sphinx-Gallery automatically.

### Where fplotlib comes from

`fplotlib_gallery_root` is the fpm package the documentation belongs to. It
defaults to the parent of the Sphinx source directory.

- **Documenting fplotlib itself:** the examples build against the checkout.
- **Documenting another fpm package:** the examples depend on that package by
  path, and on fplotlib too unless the package already depends on it.
- **No `fpm.toml` there:** the examples depend on fplotlib alone.

In the last two cases fplotlib comes from `fplotlib_gallery_fplotlib`, an fpm
dependency spec written as a TOML inline value. It defaults to the `main`
branch on GitHub. Pin it for reproducible documentation:

```python
fplotlib_gallery_fplotlib = '{ git = "https://github.com/fortran-lang/fplotlib", tag = "v0.1.0" }'
```

### Other settings

| Setting | Default | Meaning |
| --- | --- | --- |
| `fplotlib_gallery_dependencies` | `{}` | Extra fpm dependencies, as `{name: TOML value}`, e.g. `{"stdlib": '"*"'}` |
| `fplotlib_gallery_compiler` | fpm's choice | Value passed to `fpm --compiler` |
| `fplotlib_gallery_fpm_flags` | autodetected | Value passed to `fpm --flag` |
| `fplotlib_gallery_fpm` | `"fpm"` | The fpm executable |
| `fplotlib_gallery_build_dir` | `<srcdir>/_build/fplotlib-gallery` | Where the generated fpm project is built |
| `fplotlib_gallery_timeout` | `600` | Seconds allowed per fpm call and per example |
| `fplotlib_gallery_save_functions` | `["savefig", "save_animation"]` | Calls whose filename ties a picture to a code block |

## Write an example

Name the file to match `filename_pattern`, for example `plot_sine.f90`. Start it
with a `!` comment block holding the page title and introduction. Later prose
starts with `! %%` and splits the code into blocks.

```fortran
! Sine wave
! =========
!
! A first plot.
program plot_sine
    use fplotlib
    implicit none
    integer :: i
    real(8) :: x(100)
    x = [(0.1d0 * i, i = 0, 99)]

    ! %%
    ! Each picture appears under the block that saved it.

    call plot(x, sin(x), "b-")
    call savefig("sine.svg")
end program plot_sine
```

Every format fplotlib writes works, including animated GIF from
`save_animation`. Each example runs in an empty directory of its own.

## The gallery header

Sphinx-Gallery introduces each gallery with a `GALLERY_HEADER` file in the
examples directory. Besides `GALLERY_HEADER.rst`, this extension accepts
`GALLERY_HEADER.md` written in MyST Markdown, provided `myst_parser` is in
`extensions`. Give the gallery a label the MyST way:

```markdown
(gallery)=
# Gallery

Every example below is a complete Fortran program.
```

## Troubleshooting

On Windows, fpm can fail with `Cannot open file '...o.digest'` when the docs
live in a deep directory, because its build paths exceed the 260-character
limit. Point `fplotlib_gallery_build_dir` at a short path such as
`C:/tmp/fplotlib-gallery`, or enable long paths in Windows.

## License

MIT
