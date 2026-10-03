.. _gallery:

Gallery
=======

Every example below is a complete Fortran program from ``docs/gallery/``. The
documentation build compiles each one against this checkout of fplotlib with
`fpm <https://fpm.fortran-lang.org>`_, runs it, and embeds the pictures it
wrote -- so what you see on the page is what the code in front of you produces,
in whichever format it asked for.

Use the download link at the bottom of an example to get the ``.f90`` source,
then build it with::

    fpm run --example <name>

Some examples also use the `Fortran standard library
<https://stdlib.fortran-lang.org>`_, the way a Python one would use numpy. For
those, add ``stdlib = "*"`` to the ``[dependencies]`` of your ``fpm.toml``.
