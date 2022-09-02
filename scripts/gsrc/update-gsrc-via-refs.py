# Updates files in gsrc if they are modified in the reference test folder
# Uses git
# TODO - get new untracked files as well
import subprocess
from git import Repo

repo = Repo("./")

import argparse
import os

parser = argparse.ArgumentParser("update-gsrc-via-refs")
parser.add_argument("--game", help="The name of the game", type=str)
parser.add_argument("--decompiler", help="The path to the decompiler", type=str)
parser.add_argument("--decompiler_config", help="The decomp config", type=str)
args = parser.parse_args()

for item in repo.index.diff(None):
    path = item.b_rawpath.decode("utf-8")
    if args.game in path and "_REF" in path:
        file_name = os.path.basename(path).replace("_REF.gc", "")
        print("Decompiling - {}".format(file_name))
        # Decompile file
        subprocess.run(
            [
                args.decompiler,
                "./decompiler/config/{}".format(args.decompiler_config),
                "./iso_data",
                "./decompiler_out",
                "--config-override",
                '{{"allowed_objects": ["{}"]}}'.format(file_name),
            ]
        )
        print("Updating - {}".format(file_name))
        # Update gsrc
        os.system(
            "python ./scripts/gsrc/update-from-decomp.py --game {} --file {}".format(
                args.game, file_name
            )
        )
