# Updates files in gsrc if they are modified in the reference test folder
# Uses git
import subprocess
from git import Repo

repo = Repo("./")

import argparse
import os
import glob

parser = argparse.ArgumentParser("update-gsrc-via-refs")
parser.add_argument("--game", help="The name of the game", type=str)
parser.add_argument("--decompiler", help="The path to the decompiler", type=str)
parser.add_argument("--decompiler_config", help="The decomp config", type=str)
parser.add_argument("--version", help="The decomp config version", type=str)
parser.add_argument("--file_pattern", help="Provide a glob pattern to find files, instead of using git status. Relative to the reference test folder", type=str)
args = parser.parse_args()

def get_files_via_git():
    file_names = set()
    for item in repo.index.diff(None):
        path = item.b_rawpath.decode("utf-8")
        if args.game in path and "_REF" in path:
            file_names.add(os.path.basename(path).replace("_REF.gc", ""))

    for item in repo.untracked_files:
        path = item
        if args.game in path and "_REF" in path:
            file_names.add(os.path.basename(path).replace("_REF.gc", ""))
    return file_names

def get_files_via_glob():
    file_names = set()
    for file in glob.glob("./test/decompiler/reference/{}/{}".format(args.game, args.file_pattern), recursive=True):
        file_names.add(os.path.basename(file).replace("_REF.gc", ""))
    return file_names

# Get a list of changed files, as well as new files
file_names = []
if args.file_pattern:
    file_names = get_files_via_glob()
else:
    file_names = get_files_via_git()

all_names = str(file_names).replace("'", "\"").replace("{", "[").replace("}", "]");
print("Decompiling - {}".format(all_names))
# Decompile file
subprocess.run(
    [
        args.decompiler,
        "./decompiler/config/{}".format(args.decompiler_config),
        "./iso_data",
        "./decompiler_out",
        "--version",
        args.version,
        "--config-override",
        '{{"levels_extract": false, "process_art_groups": false, "decompile_code": true, "allowed_objects": {}}}'.format(all_names),
    ]
)

for file_name in file_names:
    print("Updating - {}".format(file_name))
    # Update gsrc
    os.system(
        "python ./scripts/gsrc/update-from-decomp.py --game {} --file {}".format(
            args.game, file_name
        )
    )
