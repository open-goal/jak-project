# Decompiles and adds the file to the reference test folder
from jak1_file_list import file_list

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--file")
args = parser.parse_args()

if not args.file:
  print("No --file argument provided!")
  exit(1)

import os

# TODO - make customizable
if not os.path.exists("./decompiler_out/jak1/{}_disasm.gc".format(args.file)):
  print("File not already decompiled!")
  exit(1)

src_path = ""
for f in file_list:
  if f[2] != 3:
    continue
  if f[0] == args.file:
    src_path = f[4]
    break

if not os.path.exists("./test/decompiler/reference/{}".format(src_path)):
  os.makedirs("./test/decompiler/reference/{}".format(src_path))

import shutil

shutil.copy("./decompiler_out/jak1/{}_disasm.gc".format(args.file), "./test/decompiler/reference/{}/{}_REF.gc".format(src_path, args.file))
print("Copied!")
