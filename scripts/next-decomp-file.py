import sys
from jak1_file_list import file_list

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--skip", type=int)
parser.add_argument("--file")
parser.add_argument("--list", type=int)
args = parser.parse_args()

skip_count = 0
if args.skip:
  skip_count = args.skip

def update_file(file):
  new_file_contents = []
  print("Next file to decompile is - " + file[0])
  print("Target is - " + "goal_src/" + file[4] + "/" + file[0] + ".gc")
  print("Uses the following CGO / DGO - " + str(file[3]))
  # TODO, update the CGO/DGO
  with open("decompiler\config\jak1_ntsc_black_label.jsonc", "r") as config_file:
    cfg_lines = config_file.readlines()
    for line in cfg_lines:
      if "allowed_objects" in line:
        line = "  \"allowed_objects\": [\"" + file[0] + "\"],\n"
      new_file_contents.append(line)
  if len(new_file_contents) > 0:
    with open("decompiler\config\jak1_ntsc_black_label.jsonc", "w") as f:
      f.writelines(new_file_contents)
      print("Allowed objects list updated")

if args.file:
  for file in file_list:
    if file[0] == args.file:
      update_file(file)
else:
  list_of_eligible = []
  for file in file_list:
    with open("goal_src/" + file[4] + "/" + file[0] + ".gc") as f:
      lines = f.readlines()
      if skip_count <= 0 and len(lines) <= 7:
        if args.list:
          list_of_eligible.append(file[0])
          if len(list_of_eligible) >= args.list:
            break
        else:
          update_file(file)
          break
      elif not args.list and len(lines) <= 7:
        skip_count = skip_count - 1
  if len(list_of_eligible) > 0:
    print("The next 10 files that need to be decompiled:")
    print(*list_of_eligible, sep = "\n")
