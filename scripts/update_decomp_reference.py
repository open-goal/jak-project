import os
import glob
import argparse
import shutil

## Script to update failing _REF.gc files
## Instructions:
##  run offline-test with the `--dump-mode` flag set. This generates a "failures" folder.
## update reference like this
##    python3 ../scripts/update_decomp_reference.py ./failures ../test/decompiler/reference

## TODO - this has a bug and isn't properly game specific

def get_goal_files(root_dir):
	return [f for file in os.walk(root_dir) for f in glob.glob(os.path.join(file[0], '*.gc'))]

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(dest='diff', help='the failures folder')
    parser.add_argument(dest='reference', help='the test/decompiler/reference folder')
    args = parser.parse_args()

    location_map = {os.path.basename(x) : x for x in get_goal_files(args.reference)}

    for replacement in get_goal_files(args.diff):
    	base = os.path.basename(replacement)
    	if base not in location_map:
    		print("Could not find file {}".format(base))
    		exit(-1)
    	print("replace {} with {}".format(location_map[base], replacement))
    	shutil.copyfile(replacement, location_map[base])



if __name__ == "__main__":
    main()
