# Benchmarks

fplotlib is compared against Matplotlib. The code lives in `benchmarks/` and is run with:

```
pip install -r benchmarks/requirements.txt
python benchmarks/run_benchmarks.py --repeats 5 --save
```

Measured items:

- **Compilation**: building fplotlib as a library (`fpm build`, `-O3`).
- **Import vs. compile**: Python imports (`matplotlib`, `numpy`) versus
  compiling the Fortran program to a binary.
- **Runtime**: generating a PNG and a PDF from a *simple* plot (one line) and a
  *complex* plot (3 curves, fill, legend, grid, labels, 5000 points), Python
  script versus Fortran binary.
- **Animation**: a 40-frame, 240-point travelling wave saved as a GIF
  (`clf`/`plot`/`add_frame`/`save_animation` versus Matplotlib
  `FuncAnimation` with `PillowWriter`).

## Results

Each entry below is one run on one machine; click it to open or close the
table. Timings are medians and are only comparable within a single entry.
Matplotlib run times exclude imports, which are measured separately (look for the "Import" lines).

To add your machine, run the command above and propose the new
`benchmarks/results/*.json` file in a pull request. The `github-actions` entry
is produced by the docs workflow on a GitHub runner at build time.

```{include} _generated/benchmarks_results.md
```
