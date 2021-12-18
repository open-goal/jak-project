import re
from jak1_file_list import file_list
import argparse
import os

parser = argparse.ArgumentParser()
parser.add_argument("--files")
args = parser.parse_args()

files = args.files.split(",")

throw_error = False

method_split_pattern = re.compile('t9-\d+\s\(method-of-object')
function_split_pattern = re.compile('\(t9-\d+\)')
missing_res_tag_pattern = re.compile('(sv-\d{2,} int)')
decompiler_error_pattern = re.compile(';; ERROR')

for file in files:
  src_path = ""
  for f in file_list:
    if f[2] != 3:
      continue
    if f[0] == file:
      src_path = f[4]
      break

  if not os.path.exists("./goal_src/{}".format(src_path)):
    print("{} couldn't find in /goal_src!".format(file))
    throw_error = True
    continue

  file_path = "./goal_src/{}/{}.gc".format(src_path, file)
  with open(file_path) as f:
    for lineno, line in enumerate(f):
      method_split_match = method_split_pattern.search(line)
      if method_split_match:
        print("method_split - {}:{}".format(file_path, lineno + 1))
        throw_error = True
        continue
      function_split_match = function_split_pattern.search(line)
      if function_split_match:
        print("function_split - {}:{}".format(file_path, lineno + 1))
        throw_error = True
        continue
      missing_res_tag_match = missing_res_tag_pattern.search(line)
      if missing_res_tag_match:
        print("missing_res_tag - {}:{}".format(file_path, lineno + 1))
        throw_error = True
        continue
      decompiler_error_match = decompiler_error_pattern.search(line)
      if decompiler_error_match:
        print("decompiler_error - {}:{}".format(file_path, lineno + 1))
        throw_error = True
        continue

if throw_error:
  print("found potential problems!")
  exit(1)
else:
  print("looks good!")
