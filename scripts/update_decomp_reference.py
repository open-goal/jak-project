import os
import glob
import argparse
import shutil

from gsrc.utils import get_ref_path_from_filename

## Script to update failing _REF.gc files
## Instructions:
##  run offline-test with the `--dump_current_output` flag set. This generates a "failures" folder.
## update reference like this
##    python3 ../scripts/update_decomp_reference.py ./failures ../test/decompiler/reference --game [jak1|jak2]

def get_failures(root_dir):
    return [
        f
        for file in os.walk(root_dir)
        for f in glob.glob(os.path.join(file[0], "*.gc"))
    ]

# removesuffix only added in python 3.9....
def removesuffix(self: str, suffix: str, /) -> str:
    if self.endswith(suffix):
        return self[:-len(suffix)]
    else:
        return self[:]

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(dest="diff", help="the failures folder")
    parser.add_argument(dest="reference", help="the test/decompiler/reference folder")
    parser.add_argument("--game", help="The name of the game (jak1/jak2)", type=str)
    args = parser.parse_args()

    for replacement in get_failures(args.diff):
        obj_name = removesuffix(os.path.basename(replacement), ".gc").replace("_REF", "")

        # Find gsrc path, given game-name
        ref_path = get_ref_path_from_filename(args.game, obj_name, args.reference)

        print("replace {} with {}".format(ref_path, replacement))
        shutil.copyfile(replacement, ref_path)

if __name__ == "__main__":
    main()
