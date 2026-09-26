"""(AI-assisted) Extract ISO9660 files without mounting or modifying the ISO."""

import argparse
from pathlib import Path, PurePosixPath
import pycdlib


def main():
    parser = argparse.ArgumentParser(__doc__)
    parser.add_argument("iso", type=Path)
    parser.add_argument("destination", type=Path)
    parser.add_argument("--list", action="store_true")
    args = parser.parse_args()
    root = args.destination.resolve()
    image = pycdlib.PyCdlib()
    image.open(str(args.iso.resolve()))
    try:
        entries = []
        for directory, _, names in image.walk(iso_path="/"):
            for name in names:
                source = str(PurePosixPath(directory) / name)
                relative = PurePosixPath(source.lstrip("/")).with_name(name.split(";")[0])
                destination = root.joinpath(*relative.parts).resolve()
                if not destination.is_relative_to(root):
                    raise ValueError(f"Unsafe ISO path: {source}")
                entries.append((source, destination))
        if args.list:
            for source, _ in entries:
                print(source)
            return
        # Never silently overwrite an existing extraction or user file.
        existing = [str(dst) for _, dst in entries if dst.exists()]
        if existing:
            raise FileExistsError(f"Destination files already exist: {existing[:5]}")
        for index, (source, destination) in enumerate(entries, 1):
            destination.parent.mkdir(parents=True, exist_ok=True)
            with destination.open("xb") as output:
                image.get_file_from_iso_fp(output, iso_path=source)
            if index % 50 == 0 or index == len(entries):
                print(f"Extracted {index}/{len(entries)} files", flush=True)
    finally:
        image.close()


if __name__ == "__main__":
    main()
