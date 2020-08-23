#!/usr/bin/env python3

import argparse
import glob
import os

# Create a dgo_names = ["...."] json config entry text for a folder of DGOs.
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(dest='folder', help='folder containing dgos')
    args = parser.parse_args()
    files = sorted([os.path.basename(x) for x in glob.glob(os.path.join(args.folder, "*.*GO"))])
    dgo_names = "\"dgo_names\":["
    count = 0
    for file in files:
        dgo_names += "\"" + file + "\", "
        count += 1
        if count == 8:
            count = 0
            dgo_names += "\n             "

    dgo_names = dgo_names[:-2] # remove last ", "
    dgo_names += "]\n"
    print(dgo_names)


if __name__ == "__main__":
    main()