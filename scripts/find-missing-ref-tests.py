# Find files that are added to gsrc, but not to ref tests
from jak1_file_list import file_list
import os

src_path = ""
for f in file_list:
  if f[2] != 3:
    continue
  if f[4] == "kernel":
    continue
  src_path = f[4]
  gsrc_exists = False
  gsrc_path = "./goal_src/{}/{}.gc".format(src_path, f[0])
  if os.path.exists(gsrc_path):
    with open(gsrc_path, "r") as gsrc_file:
      lines = gsrc_file.readlines()
      line_count = len(lines)
      if line_count > 100:
        gsrc_exists = True

  # now check if there is a ref test file
  if gsrc_exists:
    ref_path = "./test/decompiler/reference/{}/{}_REF.gc".format(src_path, f[0])
    if not os.path.exists(ref_path):
      print("No Ref Test for - {}".format(f[0]))
      print(gsrc_path)
      print(ref_path)
