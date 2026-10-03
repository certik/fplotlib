"""Sphinx-Gallery support for Fortran examples that draw with fplotlib.

Sphinx-Gallery splits and highlights non-Python examples out of the box -- its
``BlockParser`` already recognises Fortran comments, so a ``.f90`` file whose
header is a ``!`` comment block becomes a gallery page with a title, prose and
syntax-highlighted code.  What it does not do is *run* them: every code block is
handed to :func:`compile`, because upstream only ever executes Python.

A Fortran example is not a script anyway.  It is a single compilation unit that
has to be built and run as a whole, so this extension replaces Sphinx-Gallery's
per-block executor for Fortran sources with one that

1. generates a throw-away fpm project that depends on fplotlib itself, plus
   whatever ``fplotlib_gallery_dependencies`` adds, and whose examples are the
   gallery sources,
2. builds it once per documentation build,
3. runs each example in a private working directory, and
4. hands the pictures it wrote to :func:`fplotlib_scraper`, which moves them
   into Sphinx-Gallery's image directory and emits the ``image-sg`` directives.

Pictures are attributed to the code block that saved them: the scraper reads the
``savefig`` and ``save_animation`` calls out of the block's own source, so a
figure appears under the code that produced it, exactly as it does for
matplotlib examples.  Any picture the example wrote without a literal filename
-- a name assembled at run time, say -- is attached to the last code block,
along with everything the program printed.

Because any format fplotlib writes is just a file on disk, SVG and animated GIF
need no special handling: Sphinx-Gallery already embeds and thumbnails both.

Usage in ``conf.py``::

    extensions = [..., "sphinx_gallery.gen_gallery", "fplotlib_gallery"]

    sphinx_gallery_conf = {
        "examples_dirs": "gallery",
        "gallery_dirs": "auto_gallery",
        "image_scrapers": ("fplotlib_gallery.fplotlib_scraper",),
        "reset_modules": (),
    }

``example_extensions`` and ``filetype_parsers`` are filled in automatically for
the Fortran suffixes; everything else is ordinary Sphinx-Gallery configuration.

Writing an example
------------------

Drop a Fortran program into the gallery directory, named to match
``filename_pattern`` (``plot_*.f90`` by default).  Its first lines are a ``!``
comment block holding the page's reST: a title, its underline, and as much
prose as it deserves.  Sphinx-Gallery's parser ends a text block at the first
blank line, so keep the header contiguous and write ``!`` on its own for the
blank lines inside it.

Later prose starts with ``! %%`` and continues on ``!`` lines, which also splits
the code either side of it into separate blocks::

    ! %%
    ! Now the interesting part.

    call plot(x, y, "b-")
    call savefig("picture.svg")

Nothing else is required: ``savefig("name.svg")`` puts that picture under this
block, and any format fplotlib writes will do.

Two things to know when building animations.  Frames have to come out the same
size, so keep anything that changes the layout out of the frame loop, and
``save_animation`` only writes GIF.

Finally, the example runs in an empty scratch directory of its own, so it may
write whatever it likes and should refer to its own files by plain relative
names.  ``sphinx-build -D plot_gallery=0`` renders the pages without a Fortran
toolchain: the examples are still parsed and highlighted, just not run.

A note on the timings
---------------------

The seconds Sphinx-Gallery prints for an example -- on its page and in
``sg_execution_times`` -- are the time the *program* took, plus the scraping of
its pictures.  Compiling is not included.  One fpm call builds the library and
every example together, so there is no honest way to split that cost per
example: charging it to whichever example ran first would make that one look
slow and would swing by a factor of five with the state of fpm's cache.  The
build is timed as a whole and logged instead, as ``[fplotlib] fpm finished in
...``.
"""

from __future__ import annotations

import os
import re
import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path, PurePath
from textwrap import indent
from time import time

from sphinx.errors import ExtensionError
from sphinx.util import logging
from sphinx.util.console import blue, bold, red

logger = logging.getLogger(__name__)

__version__ = "0.1.0"

#: Source suffixes treated as Fortran gallery examples (compared lower-cased).
FORTRAN_SUFFIXES = (".f90", ".f95", ".f03", ".f08", ".f")

#: Picture formats Sphinx-Gallery knows how to embed and thumbnail.  Kept in
#: step with ``sphinx_gallery.scrapers._KNOWN_IMG_EXTS``.
IMAGE_SUFFIXES = (".png", ".svg", ".jpg", ".gif", ".webp")

#: Sphinx-Gallery's own warning namespace, so a ``suppress_warnings`` entry that
#: already covers the gallery keeps covering these too.
_WARNING_TYPE = "sphx_glr"

_EXE_SUFFIX = ".exe" if os.name == "nt" else ""

_ANSI = re.compile(r"\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])")

_MANIFEST = """\
# Generated by docs/_ext/fplotlib_gallery.py -- do not edit by hand.
#
# A throw-away project whose examples are the gallery sources and whose
# dependencies are the fplotlib checkout these docs are built from and those
# fplotlib_gallery_dependencies names.
name = "fplotlib-gallery"
version = "0.0.0"

[build]
auto-executables = false
auto-examples = false
auto-tests = false

[dependencies]
fplotlib = {{ path = "{root}" }}
{dependencies}{examples}"""

# Every example gets a source directory to itself: fpm compiles all the sources
# under an example's `source-dir`, so sharing one would make each example fail
# to build whenever any other example does.
_MANIFEST_EXAMPLE = """
[[example]]
name = "{name}"
source-dir = "example/{name}"
main = "{name}.f90"
"""


class FortranExampleError(RuntimeError):
    """An example could not be built, or did not run to completion."""


@dataclass
class _Example:
    """A gallery source and the fpm example name it is built under."""

    name: str
    source: Path


@dataclass
class _Run:
    """What one example produced: its output and the pictures it wrote."""

    stdout: str
    #: Base name -> path in the working directory, for filename lookups.
    by_name: dict[str, Path]
    #: Every picture in the order it was written, for the trailing sweep.
    in_order: list[Path]
    #: Seconds the program itself took, excluding everything fpm did.
    duration: float
    #: Pictures already emitted, so no two blocks show the same figure.
    claimed: set[Path] = field(default_factory=set)


def _default_flags(compiler: str | None) -> str:
    """Return extra fpm flags for `compiler`, or an empty string.

    A conda-forge gfortran on Windows links against DLLs that anything else on
    ``PATH`` can shadow, and the example then dies with
    ``STATUS_ENTRYPOINT_NOT_FOUND`` before it draws anything.  Linking the
    runtime statically costs nothing for examples this small and makes the build
    independent of whatever else happens to be installed.
    """
    env = os.environ.get("FPLOTLIB_GALLERY_FPM_FLAGS")
    if env is not None:
        return env
    is_gfortran = compiler is None or "gfortran" in Path(compiler).name.lower()
    if is_gfortran and os.name == "nt":
        return "-static-libgfortran -static-libgcc"
    return ""


class FortranGallery:
    """Builds and runs the Fortran examples of one Sphinx project."""

    def __init__(self, app) -> None:
        config = app.config
        self.srcdir = Path(app.srcdir)
        self.fpm = config.fplotlib_gallery_fpm
        self.compiler = config.fplotlib_gallery_compiler
        self.timeout = config.fplotlib_gallery_timeout
        self.dependencies = dict(config.fplotlib_gallery_dependencies or {})
        flags = config.fplotlib_gallery_fpm_flags
        self.flags = _default_flags(self.compiler) if flags is None else flags

        root = config.fplotlib_gallery_root or self.srcdir.parent
        self.root = Path(root)
        if not self.root.is_absolute():
            self.root = (self.srcdir / self.root).resolve()
        if not (self.root / "fpm.toml").is_file():
            raise ExtensionError(
                f"fplotlib_gallery: no fpm.toml under {self.root}. Set "
                f"'fplotlib_gallery_root' in conf.py to the fplotlib checkout."
            )

        # Deliberately not under the output directory -- that is published as-is
        # -- but kept between builds all the same, so fpm keeps its cache and
        # rebuilding unchanged examples costs a second or two.
        build_dir = config.fplotlib_gallery_build_dir
        if build_dir is None:
            build_dir = self.srcdir / "_build" / "fplotlib-gallery"
        self.build_dir = Path(build_dir)
        if not self.build_dir.is_absolute():
            self.build_dir = self.srcdir / self.build_dir

        self._examples: dict[Path, _Example] | None = None
        self._runs: dict[Path, _Run] = {}
        self._prepared = False
        self._batch_built = False
        self._exe_dir: Path | None = None
        self._gallery_conf = config.sphinx_gallery_conf

    # -- discovery ---------------------------------------------------------

    @property
    def examples(self) -> dict[Path, _Example]:
        """Every Fortran example under the configured ``examples_dirs``."""
        if self._examples is None:
            self._examples = self._discover()
        return self._examples

    def _discover(self) -> dict[Path, _Example]:
        dirs = self._gallery_conf.get("examples_dirs", [])
        if isinstance(dirs, (str, Path)):
            dirs = [dirs]
        found: dict[Path, _Example] = {}
        names: dict[str, Path] = {}
        for entry in dirs:
            base = Path(entry)
            if not base.is_absolute():
                base = self.srcdir / base
            base = base.resolve()
            if not base.is_dir():
                continue
            for source in sorted(base.rglob("*")):
                if source.suffix.lower() not in FORTRAN_SUFFIXES:
                    continue
                source = source.resolve()
                stem = source.relative_to(base).with_suffix("").as_posix()
                name = re.sub(r"[^0-9A-Za-z_]", "_", stem)
                clash = names.get(name)
                if clash is not None and clash != source:
                    raise ExtensionError(
                        f"fplotlib_gallery: {source} and {clash} both map to the "
                        f"fpm example name '{name}'. Rename one of them."
                    )
                names[name] = source
                found[source] = _Example(name=name, source=source)
        return found

    # -- building ----------------------------------------------------------

    def _fpm(self, *args: str, example: str | None = None):
        command = [self.fpm, *args]
        if example is not None:
            command += ["--example", example]
        if self.compiler:
            command += ["--compiler", self.compiler]
        if self.flags:
            command += ["--flag", self.flags]
        logger.debug("[fplotlib_gallery] %s (in %s)", " ".join(command), self.build_dir)
        try:
            return subprocess.run(
                command,
                cwd=self.build_dir,
                capture_output=True,
                text=True,
                errors="replace",
                timeout=self.timeout,
            )
        except FileNotFoundError as exc:
            raise ExtensionError(
                f"fplotlib_gallery: could not run '{self.fpm}'. Install the Fortran "
                f"package manager (https://fpm.fortran-lang.org) or point "
                f"'fplotlib_gallery_fpm' at it."
            ) from exc
        except subprocess.TimeoutExpired as exc:
            raise FortranExampleError(
                f"'{' '.join(command)}' did not finish within {self.timeout}s "
                f"(fplotlib_gallery_timeout)."
            ) from exc

    def _prepare(self) -> None:
        """Write the runner project and build every example, once per run."""
        if self._prepared:
            return
        self._prepared = True
        examples = self.examples
        if not examples:
            return

        source_dir = self.build_dir / "example"
        source_dir.mkdir(parents=True, exist_ok=True)

        # Copy only what changed, so fpm's per-file cache stays warm.
        wanted = set()
        for example in examples.values():
            target = source_dir / example.name / f"{example.name}.f90"
            wanted.add(example.name)
            target.parent.mkdir(exist_ok=True)
            text = example.source.read_text(encoding="utf-8")
            if not target.is_file() or target.read_text(encoding="utf-8") != text:
                target.write_text(text, encoding="utf-8")
        for stale in source_dir.iterdir():
            if stale.name in wanted:
                continue
            if stale.is_dir():
                shutil.rmtree(stale, ignore_errors=True)
            else:
                stale.unlink()

        self._write_manifest([example.name for example in examples.values()])

        count = len(examples)
        plural = "s" if count != 1 else ""
        logger.info(
            bold("[fplotlib] ") + f"building {count} Fortran example{plural} with fpm"
        )
        # One build for the whole gallery, which is the fast path: the library is
        # compiled once and every example links against it.  A failure is not
        # fatal, because fpm builds every target it knows about before running
        # any of them -- so one example that does not compile would otherwise be
        # reported against all of them.  _executable() then retries the examples
        # one at a time, and only the broken one is reported as failing.
        started = time()
        process = self._fpm("build")
        self._batch_built = process.returncode == 0
        # Reported here because it is not in any example's time: it is shared,
        # and charging it to whichever example happened to run first would say
        # more about the state of fpm's cache than about the example.
        logger.info(
            bold("[fplotlib] ") + f"fpm finished in {time() - started:.1f}s"
        )
        if not self._batch_built:
            logger.debug(
                "[fplotlib_gallery] the batch build failed; falling back to "
                "building each example on its own:\n%s",
                _clean(process.stdout, process.stderr),
            )

    def _write_manifest(self, names) -> None:
        """Write the runner manifest declaring exactly `names` as examples."""
        relative_root = Path(os.path.relpath(self.root, self.build_dir)).as_posix()
        manifest = _MANIFEST.format(
            root=relative_root,
            dependencies="".join(
                f"{name} = {spec}\n" for name, spec in self.dependencies.items()
            ),
            examples="".join(_MANIFEST_EXAMPLE.format(name=name) for name in names),
        )
        manifest_path = self.build_dir / "fpm.toml"
        if (
            not manifest_path.is_file()
            or manifest_path.read_text(encoding="utf-8") != manifest
        ):
            manifest_path.write_text(manifest, encoding="utf-8")

    def _executable(self, example: _Example) -> Path:
        """Return the binary fpm built for `example`."""
        if self._batch_built and self._exe_dir is not None:
            candidate = self._exe_dir / f"{example.name}{_EXE_SUFFIX}"
            if candidate.is_file():
                return candidate
        if not self._batch_built:
            # Narrow the manifest to this one example so that a sibling that does
            # not compile cannot fail it too.  fpm keys its cache on the sources,
            # not the manifest, so the library stays built and this costs a
            # recompile of the example alone.
            self._write_manifest([example.name])
        # `--runner echo` makes fpm print the path it would execute, which beats
        # guessing at the name of its hashed build directory.
        process = self._fpm("run", "--runner", "echo", example=example.name)
        if process.returncode == 0:
            for line in reversed(process.stdout.splitlines()):
                candidate = self.build_dir / line.strip().strip('"')
                if candidate.is_file():
                    self._exe_dir = candidate.parent
                    return candidate
        raise FortranExampleError(
            f"fpm could not build '{example.name}':\n\n"
            + indent(_clean(process.stdout, process.stderr), "    ")
        )

    # -- running -----------------------------------------------------------

    def run(self, src_file) -> _Run:
        """Build if needed, then run `src_file` and collect its pictures."""
        source = Path(src_file).resolve()
        if source in self._runs:
            return self._runs[source]

        self._prepare()
        example = self.examples.get(source)
        if example is None:
            raise FortranExampleError(
                f"{source} is not under any of the gallery 'examples_dirs'."
            )

        # A private, empty directory per example: whatever is in it afterwards is
        # exactly what the program drew.
        work_dir = self.build_dir / "run" / example.name
        if work_dir.exists():
            shutil.rmtree(work_dir)
        work_dir.mkdir(parents=True)

        executable = self._executable(example)
        # Timed around the program alone. Compiling is deliberately left out:
        # one fpm call builds the whole gallery, so whichever example ran first
        # would otherwise be charged for the library and for all of its
        # siblings, and the figure would swing with the state of fpm's cache.
        started = time()
        try:
            process = subprocess.run(
                [str(executable)],
                cwd=work_dir,
                capture_output=True,
                text=True,
                errors="replace",
                timeout=self.timeout,
            )
        except subprocess.TimeoutExpired as exc:
            raise FortranExampleError(
                f"{example.name} did not finish within {self.timeout}s "
                f"(fplotlib_gallery_timeout)."
            ) from exc
        duration = time() - started
        if process.returncode != 0:
            raise FortranExampleError(
                f"{example.name} exited with code {process.returncode}:\n\n"
                + indent(_clean(process.stdout, process.stderr), "    ")
            )

        pictures = [
            path
            for path in work_dir.rglob("*")
            if path.is_file() and path.suffix.lower() in IMAGE_SUFFIXES
        ]
        pictures.sort(key=lambda path: (path.stat().st_mtime_ns, path.name))
        by_name: dict[str, Path] = {}
        for path in pictures:
            by_name.setdefault(path.name, path)

        run = _Run(
            stdout=_clean(process.stdout, process.stderr),
            by_name=by_name,
            in_order=pictures,
            duration=duration,
        )
        self._runs[source] = run
        return run


def _clean(*streams: str) -> str:
    """Join captured streams, drop ANSI colouring and trailing blank lines."""
    text = "\n".join(stream for stream in streams if stream and stream.strip())
    return _ANSI.sub("", text.expandtabs()).rstrip()


# -- the scraper -----------------------------------------------------------


def _compile_save_call(names) -> re.Pattern:
    """Build the regex matching ``call savefig("...")`` and friends."""
    alternatives = "|".join(re.escape(name) for name in names)
    return re.compile(
        rf"""\b(?:{alternatives})\s*\(\s*(?:filename\s*=\s*)?"""
        r"""(?P<quote>['"])(?P<path>[^'"]+)(?P=quote)""",
        re.IGNORECASE,
    )


#: Replaced from ``fplotlib_gallery_save_functions`` when the config is read.
_SAVE_CALL = _compile_save_call(["savefig", "save_animation"])


def _saved_in(content: str) -> list[str]:
    """Return the base names a code block saves, in source order."""
    names: list[str] = []
    for match in _SAVE_CALL.finditer(content):
        name = PurePath(match.group("path").replace("\\", "/")).name
        if name and name not in names:
            names.append(name)
    return names


def fplotlib_scraper(block, block_vars, gallery_conf) -> str:
    """Sphinx-Gallery image scraper for fplotlib's Fortran examples.

    Returns the reST for the pictures belonging to `block`: the ones it saved
    under a literal filename, plus -- on the last code block -- anything the
    program wrote that no earlier block claimed.  Returns an empty string for
    Python examples, so it is safe to list alongside the matplotlib scraper.
    """
    run = block_vars.get("fplotlib_run")
    if run is None:
        return ""

    selected: list[Path] = []
    for name in _saved_in(block.content):
        path = run.by_name.get(name)
        if path is not None and path not in run.claimed:
            run.claimed.add(path)
            selected.append(path)
    if block_vars.get("fplotlib_last_block"):
        for path in run.in_order:
            if path not in run.claimed:
                run.claimed.add(path)
                selected.append(path)
    if not selected:
        return ""

    from sphinx_gallery.scrapers import figure_rst

    image_paths = block_vars["image_path_iterator"]
    written: list[str] = []
    for path in selected:
        # The iterator hands out '..._001.png'; keep its numbering but the real
        # suffix. Sphinx-Gallery finds the file again by probing the known
        # extensions, so the mismatch with the template is expected.
        target = Path(next(image_paths)).with_suffix(path.suffix.lower())
        target.parent.mkdir(parents=True, exist_ok=True)
        # A picture whose format changed between builds would otherwise leave a
        # stale sibling that the probing picks up in preference to this one.
        for suffix in IMAGE_SUFFIXES:
            stale = target.with_suffix(suffix)
            if stale != target and stale.is_file():
                stale.unlink()
        shutil.copyfile(path, target)
        written.append(str(target))

    return figure_rst(written, gallery_conf["src_dir"])


# -- replacing Sphinx-Gallery's executor -----------------------------------

_GALLERY: FortranGallery | None = None
_ORIGINAL_EXECUTE_SCRIPT = None


def _is_fortran(src_file) -> bool:
    return Path(src_file).suffix.lower() in FORTRAN_SUFFIXES


def _execute_script(script_blocks, script_vars, gallery_conf, file_conf):
    """Stand in for ``gen_rst.execute_script`` on Fortran examples."""
    if not _is_fortran(script_vars.get("src_file", "")) or _GALLERY is None:
        return _ORIGINAL_EXECUTE_SCRIPT(
            script_blocks, script_vars, gallery_conf, file_conf
        )

    from sphinx_gallery.gen_rst import CODE_OUTPUT, get_md5sum
    from sphinx_gallery.scrapers import save_figures

    # Bookkeeping the rest of Sphinx-Gallery expects of an executor.
    script_vars["memory_delta"] = 0.0
    script_vars.setdefault("example_globals", {})

    output_blocks = [""] * len(script_blocks)
    code_blocks = [i for i, block in enumerate(script_blocks) if block.type == "code"]
    if not script_vars["execute_script"] or not code_blocks:
        return output_blocks, 0.0

    try:
        run = _GALLERY.run(script_vars["src_file"])
    except FortranExampleError as exc:
        output_blocks[code_blocks[-1]] = _report_failure(exc, script_vars, gallery_conf)
        # An example that never ran costs nothing, as upstream reports for one
        # that was not executed.  What a failure did cost was compiling, and
        # that is reported against the gallery rather than against one example.
        return output_blocks, 0.0

    script_vars["fplotlib_run"] = run
    last = code_blocks[-1]
    started = time()
    for index in code_blocks:
        script_vars["fplotlib_last_block"] = index == last
        images_rst = save_figures(script_blocks[index], script_vars, gallery_conf)
        captured = ""
        if index == last and run.stdout.strip():
            captured = CODE_OUTPUT.format(indent(run.stdout, " " * 4))
        output_blocks[index] = f"\n{images_rst}\n\n{captured}\n\n\n"
    # The program plus the scraping it caused: the work that is this example's
    # own.  Building is excluded; see FortranGallery.run.
    elapsed = run.duration + (time() - started)

    # Mirrors the upstream executor: the md5 lets the next build skip this
    # example, so it is only written once the example has actually run.
    with open(script_vars["target_file"] + ".md5", "w") as handle:
        handle.write(get_md5sum(script_vars["target_file"], mode="t"))
    script_vars["passing"] = True
    return output_blocks, elapsed


def _report_failure(exc: FortranExampleError, script_vars, gallery_conf) -> str:
    """Log a failed example and render its diagnostics, as upstream does.

    Deliberately not routed through ``gen_rst.handle_exception``: that formats a
    Python traceback, and what is worth showing here is the compiler's or the
    program's own output.
    """
    from sphinx_gallery.gen_gallery import _expected_failing_examples
    from sphinx_gallery.gen_rst import codestr2rst

    message = str(exc)
    src_file = str(script_vars["src_file"])
    relative = os.path.relpath(src_file, gallery_conf["src_dir"])
    expected = _expected_failing_examples(gallery_conf)
    if {src_file, str(Path(src_file).resolve())} & expected:
        logger.info(
            "\n%s expectedly failed to execute correctly:\n\n%s",
            bold(blue(relative)),
            blue(indent(message, "    ")),
        )
    else:
        logger.warning(
            "\n%s unexpectedly failed to execute correctly:\n\n%s",
            bold(red(relative)),
            red(indent(message, "    ")),
            type=_WARNING_TYPE,
            subtype="example_error",
        )

    script_vars["formatted_exception"] = message
    script_vars["execute_script"] = False
    if gallery_conf["abort_on_example_error"]:
        raise ExtensionError(f"{relative} failed to run:\n\n{indent(message, '    ')}")
    return (
        "\n.. rst-class:: sphx-glr-script-out\n\n"
        + codestr2rst(message, lang="none")
        + "\n\n"
    )


def _patch_sphinx_gallery() -> None:
    """Route Fortran examples through our executor, once."""
    global _ORIGINAL_EXECUTE_SCRIPT
    if _ORIGINAL_EXECUTE_SCRIPT is not None:
        return
    from sphinx_gallery import gen_rst

    _ORIGINAL_EXECUTE_SCRIPT = gen_rst.execute_script
    gen_rst.execute_script = _execute_script


def _on_config_inited(app, config) -> None:
    global _GALLERY, _SAVE_CALL

    gallery_conf = getattr(config, "sphinx_gallery_conf", None)
    if gallery_conf is None:
        raise ExtensionError(
            "fplotlib_gallery needs 'sphinx_gallery.gen_gallery' in 'extensions' "
            "and a 'sphinx_gallery_conf' setting."
        )
    if gallery_conf.get("parallel"):
        raise ExtensionError(
            "fplotlib_gallery cannot run examples in parallel; remove 'parallel' "
            "from sphinx_gallery_conf."
        )

    # Teach Sphinx-Gallery about Fortran so that conf.py does not have to.
    extensions = set(gallery_conf.get("example_extensions", {".py"}))
    parsers = dict(gallery_conf.get("filetype_parsers", {}))
    for suffix in FORTRAN_SUFFIXES:
        extensions.add(suffix)
        parsers.setdefault(suffix, "Fortran")
    gallery_conf["example_extensions"] = extensions
    gallery_conf["filetype_parsers"] = parsers

    _SAVE_CALL = _compile_save_call(config.fplotlib_gallery_save_functions)
    _GALLERY = FortranGallery(app)
    _patch_sphinx_gallery()


def setup(app):
    """Register the extension."""
    app.setup_extension("sphinx_gallery.gen_gallery")

    app.add_config_value("fplotlib_gallery_root", None, "env")
    app.add_config_value("fplotlib_gallery_fpm", "fpm", "env")
    app.add_config_value("fplotlib_gallery_build_dir", None, "env")
    app.add_config_value("fplotlib_gallery_compiler", None, "env")
    app.add_config_value("fplotlib_gallery_fpm_flags", None, "env")
    app.add_config_value("fplotlib_gallery_timeout", 600, "env")
    app.add_config_value("fplotlib_gallery_dependencies", {}, "env")
    app.add_config_value(
        "fplotlib_gallery_save_functions", ["savefig", "save_animation"], "env"
    )

    app.connect("config-inited", _on_config_inited)

    return {
        "version": __version__,
        # Examples are built and run by one fpm project in this process.
        "parallel_read_safe": False,
        "parallel_write_safe": True,
    }
