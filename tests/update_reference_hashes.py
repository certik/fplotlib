#!/usr/bin/env python3
"""Refresh the committed reference hash manifest from tests/out."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import platform
import re
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


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--profile", help="Reference hash profile to update")
    return parser.parse_args()


def existing_profiles() -> list[str]:
    if not MANIFEST.exists():
        return []

    payload = json.loads(MANIFEST.read_text(encoding="utf-8"))
    version = int(payload.get("version", 1))
    if version == 1:
        return ["default"]
    profiles = payload.get("profiles")
    if not isinstance(profiles, dict):
        return []
    return sorted(profiles)


def infer_profile_name() -> str:
    profile = os.environ.get("FPLOT_HASH_PROFILE", "").strip()
    if profile:
        return profile

    system_name = platform.system().lower() or "unknown"
    matches = [name for name in existing_profiles() if name.startswith(f"{system_name}-")]
    if len(matches) == 1:
        return matches[0]

    compiler = "unknown"
    for key in ("FPLOT_HASH_COMPILER", "FPM_FC", "FC"):
        value = os.environ.get(key, "").strip()
        if value:
            compiler = Path(value).stem.lower()
            break
    compiler = re.sub(r"[^0-9A-Za-z._-]+", "-", compiler).strip("-") or "unknown"
    return f"{system_name}-{compiler}"


def load_profiles() -> dict[str, dict[str, object]]:
    if not MANIFEST.exists():
        return {}

    payload = json.loads(MANIFEST.read_text(encoding="utf-8"))
    version = int(payload.get("version", 1))
    if version == 1:
        return {
            "default": {
                "generated_from": str(payload.get("generated_from", "tests/out")),
                "files": payload["files"],
            }
        }

    profiles = payload.get("profiles")
    if not isinstance(profiles, dict):
        raise ValueError(f"reference hash manifest has no profiles: {MANIFEST}")
    return profiles


def main() -> int:
    args = parse_args()
    profile = args.profile or infer_profile_name()

    if not OUT.exists():
        print(f"output directory not found: {OUT}")
        print("run: fpm test")
        return 1

    files = sorted(path for path in OUT.iterdir() if path.is_file())
    if not files:
        print(f"no output files found in {OUT}")
        print("run: fpm test")
        return 1

    profiles = load_profiles()
    profiles[profile] = {
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

    payload = {
        "version": 2,
        "profiles": {name: profiles[name] for name in sorted(profiles)},
    }

    MANIFEST.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    print(f"updated {MANIFEST} profile {profile} with {len(files)} file hashes")
    return 0


if __name__ == "__main__":
    sys.exit(main())