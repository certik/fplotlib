#!/usr/bin/env python3
"""Verify committed reference hashes for the files emitted by tests/test_plots.f90."""

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


def load_manifest() -> dict[str, dict[str, object]]:
    payload = json.loads(MANIFEST.read_text(encoding="utf-8"))
    return {entry["path"]: entry for entry in payload["files"]}


def main() -> int:
    if not MANIFEST.exists():
        print(f"reference hash manifest not found: {MANIFEST}")
        print("run: python tests/update_reference_hashes.py")
        return 1
    if not OUT.exists():
        print(f"output directory not found: {OUT}")
        print("run: fpm test")
        return 1

    expected = load_manifest()
    actual_files = sorted(path for path in OUT.iterdir() if path.is_file())
    actual_names = {path.name for path in actual_files}
    expected_names = set(expected)

    missing = sorted(expected_names - actual_names)
    extra = sorted(actual_names - expected_names)
    mismatched: list[tuple[str, str, str]] = []

    for path in actual_files:
        name = path.name
        if name not in expected:
            continue
        digest = sha256(path)
        wanted = str(expected[name]["sha256"])
        if digest != wanted:
            mismatched.append((name, wanted, digest))

    if missing:
        print("missing output files:")
        for name in missing:
            print(f"  {name}")
    if extra:
        print("unexpected output files:")
        for name in extra:
            print(f"  {name}")
    if mismatched:
        print("hash mismatches:")
        for name, wanted, got in mismatched[:20]:
            print(f"  {name}")
            print(f"    expected {wanted}")
            print(f"    got      {got}")
        if len(mismatched) > 20:
            print(f"  ... {len(mismatched) - 20} more mismatch(es)")

    if missing or extra or mismatched:
        print("\nreference hash verification failed")
        print("update the manifest intentionally with: python tests/update_reference_hashes.py")
        return 1

    print(f"reference hash verification passed for {len(actual_files)} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())