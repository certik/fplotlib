"""Benchmark fplotlib against Matplotlib (issue #35).

Measures: library compile time, import time (Python) vs program compile time
(Fortran), and run time of simple/complex plots for PNG and PDF and a GIF animation.
Usage: python benchmarks/run_benchmarks.py [--repeats N] [--save [NAME]]

--save writes benchmarks/results/NAME.json with the machine description and
the timings, ready to be proposed in a pull request.
"""
import argparse, datetime, json, os, platform, re, shutil, statistics, subprocess, sys, tempfile, time
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


def first_line(cmd):
    try:
        r = subprocess.run(cmd, capture_output=True, text=True, check=True)
        return (r.stdout or r.stderr).splitlines()[0].strip()
    except (OSError, subprocess.CalledProcessError, IndexError):
        return "unknown"


def cpu_name():
    try:
        for line in Path("/proc/cpuinfo").read_text().splitlines():
            if line.startswith("model name"):
                return line.split(":", 1)[1].strip()
    except OSError:
        pass
    if platform.system() == "Darwin":
        return first_line(["sysctl", "-n", "machdep.cpu.brand_string"])
    return platform.processor() or platform.machine()


def cpu_ghz():
    """Maximum clock speed in GHz when the OS reports it, else None."""
    mhz = None
    system = platform.system()
    try:
        if system == "Linux":
            for line in first_text(["lscpu"]).splitlines():
                if line.strip().startswith("CPU max MHz"):
                    mhz = float(line.split(":", 1)[1])
            if mhz is None:
                for line in Path("/proc/cpuinfo").read_text().splitlines():
                    if line.startswith("cpu MHz"):
                        mhz = float(line.split(":", 1)[1])
                        break
        elif system == "Darwin":
            mhz = float(first_line(["sysctl", "-n", "hw.cpufrequency_max"])) / 1e6
        elif system == "Windows":
            out = first_text(["powershell", "-NoProfile", "-Command",
                              "(Get-CimInstance Win32_Processor).MaxClockSpeed"])
            mhz = float(out.split()[0])
    except (OSError, ValueError, IndexError):
        pass
    return round(mhz / 1000, 2) if mhz else None


def first_text(cmd):
    try:
        return subprocess.run(cmd, capture_output=True, text=True, check=True).stdout
    except (OSError, subprocess.CalledProcessError):
        return ""


def os_name():
    return {"Darwin": "macOS"}.get(platform.system(), platform.system())


def machine_info():
    import matplotlib, numpy
    return {
        "cpu": cpu_name(),
        "cpu_ghz": cpu_ghz(),
        "os": os_name(),
        "fortran_compiler": first_line(["gfortran", "--version"]),
        "fortran_flags": " ".join(FFLAGS),
        "fpm": first_line(["fpm", "--version"]),
        "python": f"{platform.python_implementation()} {platform.python_version()}",
        "matplotlib": matplotlib.__version__,
        "numpy": numpy.__version__,
    }


def median(f, n):
    return statistics.median(f() for _ in range(n))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--repeats", type=int, default=5)
    ap.add_argument("--save", nargs="?", const="", metavar="NAME", default=None,
                    help="write benchmarks/results/NAME.json (default name: "
                         "date and CPU)")
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

    for name, exts in (("simple", ("png", "pdf")), ("complex", ("png", "pdf")),
                       ("animation", ("gif",))):
        exe = work / f"{name}_f"
        cmd = ["gfortran", *FFLAGS, "-I", str(mod), str(HERE / f"{name}.f90"),
               str(lib), "-o", str(exe)]
        t = median(lambda: timed(cmd), a.repeats)
        rows.append((f"Compile {name} program", "Fortran compile to binary", t))
        t = median(lambda: timed([sys.executable, "-c",
                   "import matplotlib; matplotlib.use('Agg'); import matplotlib.pyplot, matplotlib.animation, numpy"]), a.repeats)
        rows.append((f"Import {name} script", "Python imports (matplotlib, numpy)", t))
        for ext in exts:
            tf = median(lambda: timed([str(exe), ext], cwd=work), a.repeats)
            tp = median(lambda: py_run(HERE / f"{name}.py", ext, work), a.repeats)
            check(work / f"{name}_f.{ext}")
            check(work / f"{name}_py.{ext}")
            rows.append((f"Run {name} -> {ext}", "fplotlib", tf))
            rows.append((f"Run {name} -> {ext}", "matplotlib (excl. imports)", tp))

    print(f"{'Benchmark':32} {'Variant':36} Time (s)")
    for b, v, t in rows:
        print(f"{b:32} {v:36} {t:.4f}")
    if a.save is not None:
        info = machine_info()
        name = a.save or "{}-{}".format(
            datetime.date.today().isoformat(),
            re.sub(r"[^a-z0-9]+", "-", info["cpu"].lower()).strip("-"))
        data = {"date": datetime.date.today().isoformat(), "repeats": a.repeats,
                "machine": info,
                "results": [{"benchmark": b, "variant": v, "seconds": round(t, 6)}
                            for b, v, t in rows]}
        dest = HERE / "results" / f"{name}.json"
        dest.parent.mkdir(exist_ok=True)
        dest.write_text(json.dumps(data, indent=2) + "\n")
        print(f"\nSaved {dest}")
    shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    main()
