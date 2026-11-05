"""Turn benchmarks/results/*.json into a Markdown page with collapsible tables.

Used by docs/conf.py, so the docs always show whatever is in the results
folder. Usage: python benchmarks/render_results.py OUTPUT.md
"""
import html
import json
import sys
from pathlib import Path

RESULTS = Path(__file__).resolve().parent / "results"
MACHINE_LABELS = [
    ("cpu", "CPU"), ("cpu_ghz", "Clock speed (GHz)"), ("os", "OS"),
    ("fortran_compiler", "Fortran compiler"), ("fortran_flags", "Fortran flags"),
    ("fpm", "fpm"), ("python", "Python"), ("matplotlib", "Matplotlib"),
    ("numpy", "NumPy"),
]


def e(x):
    return html.escape(str(x))


def section(path, data):
    m = data["machine"]
    out = [f"<details><summary><b>{e(m.get('cpu', path.stem))}</b> &mdash; "
           f"{e(m.get('os', ''))} &mdash; {e(data.get('date', ''))} "
           f"(<code>{e(path.name)}</code>)</summary>", "<h4>Machine</h4>", "<table>"]
    for key, label in MACHINE_LABELS:
        if m.get(key) is not None:
            out.append(f"<tr><th>{label}</th><td>{e(m[key])}</td></tr>")
    out.append("</table>")
    out.append(f"<h4>Results (median of {e(data.get('repeats', '?'))} runs)</h4>")
    out.append("<table><tr><th>Benchmark</th><th>Variant</th>"
               "<th>Time (s)</th></tr>")
    for r in data["results"]:
        out.append(f"<tr><td>{e(r['benchmark'])}</td><td>{e(r['variant'])}</td>"
                   f"<td>{r['seconds']:.4f}</td></tr>")
    out.append("</table></details>")
    return "\n".join(out)


def render():
    blocks = []
    for path in sorted(RESULTS.glob("*.json")):
        blocks.append("```{raw} html\n" + section(path, json.loads(path.read_text()))
                      + "\n```\n")
    if not blocks:
        return "*No results have been recorded yet.*\n"
    return "\n".join(blocks)


if __name__ == "__main__":
    Path(sys.argv[1]).write_text(render())
