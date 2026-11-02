"""Benchmark fplotlib against Matplotlib (issue #35).

Measures: library compile time, import time (Python) vs program compile time
(Fortran), and run time of simple/complex plots for PNG and PDF.
Usage: python benchmarks/run_benchmarks.py [--repeats N] [--out results.md]
"""
import argparse, os, shutil, statistics, subprocess, sys, tempfile, time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent
FFLAGS = ["-O3", "-march=native"]


def timed(cmd, cwd=None):
    t = time.perf_counter()
    subprocess.run(cmd, cwd=cwd, check=True, stdout=subprocess.DEVNULL)
    return time.perf_counter() - t


def median(f, n):
    return statistics.median(f() for _ in range(n))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repeats", type=int, default=5)
    ap.add_argument("--out", default=None)
    a = ap.parse_args()
    rows = []
    work = Path(tempfile.mkdtemp())
    build = work / "build"

    # Library compile (full, from scratch)
    t = timed(["fpm", "build", "--flag", " ".join(FFLAGS), "--builddir", str(build)], cwd=ROOT)
    rows.append(("Compile library", "fplotlib (fpm build, -O3)", t))

    libdirs = [p for p in build.rglob("libfplotlib.a")]
    moddirs = {p.parent for p in build.rglob("fplotlib.mod")}
    lib, mod = libdirs[0], next(iter(moddirs))

    for name in ("simple", "complex"):
        exe = work / f"{name}_f"
        cmd = ["gfortran", *FFLAGS, "-I", str(mod), str(HERE / f"{name}.f90"),
               str(lib), "-o", str(exe)]
        t = median(lambda: timed(cmd), a.repeats)
        rows.append((f"Compile {name} program", "Fortran compile to binary", t))
        t = median(lambda: timed([sys.executable, "-c",
                   "import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot, numpy"]), a.repeats)
        rows.append((f"Import {name} script", "Python imports (matplotlib, numpy)", t))
        for ext in ("png", "pdf"):
            tf = median(lambda: timed([str(exe), ext], cwd=work), a.repeats)
            tp = median(lambda: timed([sys.executable, str(HERE / f"{name}.py"), ext], cwd=work), a.repeats)
            rows.append((f"Run {name} -> {ext}", "fplotlib", tf))
            rows.append((f"Run {name} -> {ext}", "matplotlib (incl. imports)", tp))

    out = "| Benchmark | Variant | Time (s) |\n|---|---|---|\n"
    out += "\n".join(f"| {a_} | {b} | {c:.4f} |" for a_, b, c in rows) + "\n"
    print(out)
    if a.out:
        Path(a.out).write_text(out)
    shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    main()
