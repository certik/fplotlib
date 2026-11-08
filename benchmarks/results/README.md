# Benchmark results

One JSON file per machine and run. Each records the CPU and its clock speed, the OS (Windows, Linux or macOS), Fortran compiler
and flags, Python, Matplotlib and NumPy versions, and the timings.

To contribute a result:

```
pip install -r benchmarks/requirements.txt
python benchmarks/run_benchmarks.py --repeats 5 --save
```

This needs `fpm` and `gfortran` on the PATH. It writes
`benchmarks/results/<date>-<cpu>.json`; commit that file and open a pull
request. The docs list every file here as a collapsible section.

`github-actions.json` is produced by the docs workflow at build time and is
not committed.
