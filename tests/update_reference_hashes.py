#!/usr/bin/env python3
"""Refresh the committed reference hash manifest from tests/out."""

from __future__ import annotations

import hashlib
import json
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent
MANIFEST = ROOT / "reference_hashes.json"
OUT = ROOT / "out"
TEXT_SUFFIXES = {".svg", ".eps"}


def normalize_bytes(path: Path) -> bytes:
    data = path.read_bytes()
    if path.suffix.lower() in TEXT_SUFFIXES:
        return data.replace(b"\r\n", b"\n")
    return data


def sha256(path: Path) -> str:
    return hashlib.sha256(normalize_bytes(path)).hexdigest()


def main() -> int:
    if not OUT.exists():
        print(f"output directory not found: {OUT}")
        print("run: fpm test")
        return 1

    files = sorted(path for path in OUT.iterdir() if path.is_file())
    if not files:
        print(f"no output files found in {OUT}")
        print("run: fpm test")
        return 1

    payload = {
        "version": 1,
        "generated_from": str(OUT.relative_to(ROOT.parent)).replace("\\", "/"),
        "files": [
            {
                "path": path.name,
                "sha256": sha256(path),
                "size": len(normalize_bytes(path)),
            }
            for path in files
        ],
    }

    MANIFEST.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"updated {MANIFEST} with {len(files)} file hashes")
    return 0


if __name__ == "__main__":
    sys.exit(main())