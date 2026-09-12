#!/usr/bin/env python3
"""Verify committed reference hashes for the files emitted by tests/test_plots.f90."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
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


def sanitize_profile(profile: str) -> str:
    return re.sub(r"[^0-9A-Za-z._-]+", "-", profile.strip()).strip("-") or "actual"


def load_manifest() -> dict[str, dict[str, dict[str, object]]]:
    payload = json.loads(MANIFEST.read_text(encoding="utf-8"))
    version = int(payload.get("version", 1))

    if version == 1:
        generated_from = str(payload.get("generated_from", "tests/out"))
        files = {entry["path"]: entry for entry in payload["files"]}
        return {"default": {"generated_from": generated_from, "files": files}}

    profiles = payload.get("profiles")
    if not isinstance(profiles, dict) or not profiles:
        raise ValueError(f"reference hash manifest has no profiles: {MANIFEST}")

    normalized: dict[str, dict[str, dict[str, object]]] = {}
    for profile, entry in profiles.items():
        if isinstance(entry, dict) and "files" in entry:
            files = entry["files"]
            generated_from = str(entry.get("generated_from", "tests/out"))
        else:
            files = entry
            generated_from = "tests/out"
        normalized[profile] = {
            "generated_from": generated_from,
            "files": {item["path"]: item for item in files},
        }
    return normalized


def actual_entries() -> list[dict[str, object]]:
    return [
        {
            "path": path.name,
            "sha256": sha256(path),
            "size": len(normalize_bytes(path)),
        }
        for path in sorted(path for path in OUT.iterdir() if path.is_file())
    ]


def write_candidate_manifest(profile: str, files: list[dict[str, object]]) -> Path:
    candidate = ROOT / f"reference_hashes.{sanitize_profile(profile)}.candidate.json"
    payload = {
        "version": 2,
        "profiles": {
            profile: {
                "generated_from": str(OUT.relative_to(ROOT.parent)).replace("\\", "/"),
                "files": files,
            }
        },
    }
    candidate.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    return candidate


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--profile", help="Reference hash profile to verify")
    return parser.parse_args()


def compare_profile(
    expected: dict[str, dict[str, object]], actual_files: list[dict[str, object]]
) -> tuple[list[str], list[str], list[tuple[str, str, str]]]:
    actual_names = {entry["path"] for entry in actual_files}
    expected_names = set(expected)

    missing = sorted(expected_names - actual_names)
    extra = sorted(actual_names - expected_names)
    mismatched: list[tuple[str, str, str]] = []

    for entry in actual_files:
        name = str(entry["path"])
        if name not in expected:
            continue
        digest = str(entry["sha256"])
        wanted = str(expected[name]["sha256"])
        if digest != wanted:
            mismatched.append((name, wanted, digest))

    return missing, extra, mismatched


def main() -> int:
    args = parse_args()
    requested_profile = args.profile or os.environ.get("FPLOT_HASH_PROFILE", "").strip()

    if not MANIFEST.exists():
        print(f"reference hash manifest not found: {MANIFEST}")
        print("run: python tests/update_reference_hashes.py")
        return 1
    if not OUT.exists():
        print(f"output directory not found: {OUT}")
        print("run: fpm test")
        return 1

    manifests = load_manifest()
    actual_files = actual_entries()

    if requested_profile:
        if requested_profile not in manifests:
            print(f"reference hash profile not found: {requested_profile}")
            print("available profiles:")
            for profile in sorted(manifests):
                print(f"  {profile}")
            candidate = write_candidate_manifest(requested_profile, actual_files)
            print(f"\nwrote candidate manifest for this run to: {candidate}")
            return 1
        profiles = [requested_profile]
    else:
        profiles = sorted(manifests)

    failures: list[tuple[str, list[str], list[str], list[tuple[str, str, str]]]] = []
    for profile in profiles:
        expected = manifests[profile]["files"]
        missing, extra, mismatched = compare_profile(expected, actual_files)
        if not missing and not extra and not mismatched:
            print(f"reference hash verification passed for {len(actual_files)} files ({profile})")
            return 0
        failures.append((profile, missing, extra, mismatched))

    profile, missing, extra, mismatched = min(
        failures,
        key=lambda item: (len(item[1]) + len(item[2]) + len(item[3]), item[0]),
    )
    print(f"reference hash verification failed against profile: {profile}")
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

    candidate = write_candidate_manifest(requested_profile or profile, actual_files)
    print("\nreference hash verification failed")
    print(f"wrote candidate manifest to: {candidate}")
    print("update the manifest intentionally with: python tests/update_reference_hashes.py")
    return 1


if __name__ == "__main__":
    sys.exit(main())