# Windows doesn't have the GNU toolchain by default, making it hard to write Taskfiles that are cross-platform
# This "solves" that

import argparse
import glob
parser = argparse.ArgumentParser()
parser.add_argument("--src")
parser.add_argument("--dest")
args = parser.parse_args()

import shutil
import os

def copy_with_glob(source_glob, destination):
    # Expand the glob pattern
    paths = glob.glob(source_glob, recursive=True)

    for path in paths:
        # Get the destination path by joining the destination directory with the relative path
        relative_path = os.path.relpath(path, os.path.dirname(source_glob))
        dest_path = os.path.join(destination, relative_path)

        # Check if the path is a file or a directory
        if os.path.isfile(path):
            # Copy the file
            shutil.copy2(path, dest_path)
        elif os.path.isdir(path):
            # Copy the directory and its contents recursively
            shutil.copytree(path, dest_path, dirs_exist_ok=True)

copy_with_glob(args.src, args.dest)
