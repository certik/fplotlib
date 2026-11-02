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


def py_run(script, ext, cwd):
    """Run time of main() alone, as reported by the script after its imports."""
    r = subprocess.run([sys.executable, str(script), ext], cwd=cwd, check=True,
                       capture_output=True, text=True)
    for line in r.stdout.splitlines():
        if line.startswith("RUN_SECONDS"):
            return float(line.split()[1])
    raise RuntimeError(f"no RUN_SECONDS in output of {script}")


def check(path):
    if not path.is_file() or path.stat().st_size == 0:
        sys.exit(f"missing or empty output: {path}")


def median(f, n):
    return statistics.median(f() for _ in range(n))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repeats", type=int, default=5)
    ap.add_argument("--out", default=None)
    a = ap.parse_args()
    rows = []
    work = Path(tempfile.mkdtemp())
    build = ROOT / "build"
    shutil.rmtree(build, ignore_errors=True)

    # Library compile (full, from scratch)
    t = timed(["fpm", "build", "--flag", " ".join(FFLAGS)], cwd=ROOT)
    rows.append(("Compile library", "fplotlib (fpm build, -O3)", t))

    libdirs = [p for p in build.rglob("libfplotlib.a")]
    moddirs = {p.parent for p in build.rglob("fplotlib.mod")}
    if not libdirs or not moddirs:
        sys.exit(f"fpm build produced no library under {build}")
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
            tp = median(lambda: py_run(HERE / f"{name}.py", ext, work), a.repeats)
            check(work / f"{name}_f.{ext}")
            check(work / f"{name}_py.{ext}")
            rows.append((f"Run {name} -> {ext}", "fplotlib", tf))
            rows.append((f"Run {name} -> {ext}", "matplotlib (excl. imports)", tp))

    out = "| Benchmark | Variant | Time (s) |\n|---|---|---|\n"
    out += "\n".join(f"| {a_} | {b} | {c:.4f} |" for a_, b, c in rows) + "\n"
    print(out)
    if a.out:
        Path(a.out).write_text(out)
    shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    main()
