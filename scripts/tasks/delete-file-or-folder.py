# Windows doesn't have the GNU toolchain by default, making it hard to write Taskfiles that are cross-platform
# This "solves" that

# Ensures it only will delete from the current directory

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--path")
args = parser.parse_args()

from os import path
import shutil

if path.exists("./{}".format(args.path)):
  shutil.rmtree("./{}".format(args.path))
