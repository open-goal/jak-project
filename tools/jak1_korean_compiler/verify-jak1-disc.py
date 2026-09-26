"""(AI-assisted) Verify the supplied SCPS-56003 extraction against the repository DB."""

import json
from pathlib import Path, PurePosixPath
import struct

import pycdlib
import xxhash

root = Path(__file__).resolve().parents[2]
destination = root / "iso_data/jak1"
isos = list(destination.glob("*.iso"))
if len(isos) != 1:
    raise RuntimeError("Expected exactly one ISO in iso_data/jak1")
image = pycdlib.PyCdlib()
image.open(str(isos[0]))
combined = 0
count = 0
try:
    for directory, _, names in image.walk(iso_path="/"):
        for name in names:
            relative = PurePosixPath(directory.lstrip("/")) / name.split(";")[0]
            path = destination.joinpath(*relative.parts).resolve()
            if not path.is_relative_to(destination.resolve()):
                raise ValueError("Unsafe ISO entry")
            hasher = xxhash.xxh64()
            with path.open("rb") as stream:
                for chunk in iter(lambda: stream.read(1024 * 1024), b""):
                    hasher.update(chunk)
            combined ^= hasher.intdigest()
            count += 1
finally:
    image.close()

# Values from decompiler/extractor/extractor_util.cpp, SCPS-56003 entry.
contents_hash = xxhash.xxh64(struct.pack("<Q", combined)).intdigest()
elf_hash = xxhash.xxh64((destination / "SCPS_560.03").read_bytes()).intdigest()
result = {"serial": "SCPS-56003", "elf_hash": elf_hash,
          "file_count": count, "contents_hash": contents_hash}
print(json.dumps(result, indent=2))
if (count, contents_hash, elf_hash) != (338, 13924540661438229398, 7280758013604870207):
    raise RuntimeError("Extracted disc does not match the repository's SCPS-56003 entry")
(root / ".tools/iso-validation.json").write_text(json.dumps(result, indent=2), encoding="utf-8")
(destination / "buildinfo.json").write_text(
    json.dumps([{"serial": result["serial"], "elf_hash": elf_hash}], indent=2), encoding="utf-8")
print("PASS: all 338 extracted files match the known disc contents hash.")
