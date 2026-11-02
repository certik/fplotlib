# Benchmarks

fplotlib is compared against Matplotlib (see issue #35). The code lives in
`benchmarks/` and is run with:

```
pip install -r benchmarks/requirements.txt
python benchmarks/run_benchmarks.py --repeats 5 --out benchmarks/results.md
```

Measured items:

- **Compilation**: building fplotlib as a library (`fpm build`, `-O3`).
- **Import vs. compile**: Python imports (`matplotlib`, `numpy`) versus
  compiling the Fortran program to a binary.
- **Runtime**: generating a PNG and a PDF from a *simple* plot (one line) and a
  *complex* plot (3 curves, fill, legend, grid, labels, 5000 points), Python
  script versus Fortran binary.

Results (machine dependent) are written to `benchmarks/results.md`; paste them
here once measured on reference hardware.
