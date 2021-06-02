import sys
from jak1_file_list import file_list

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--file")
parser.add_argument("--list", type=int)
args = parser.parse_args()

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
  list_eligible = []
  for file in file_list:
    if file[2] != 3:
      continue
    if len(list_eligible) > args.list:
      break
    src_path = "goal_src/" + file[4] + "/" + file[0] + ".gc"
    with open(src_path) as f:
      lines = f.readlines()
      if len(lines) <= 7:
        list_eligible.append("{} - Empty".format(file[0]))
      else:
        # Check for TODOs
        count_todos = 0
        for line in lines:
          if "TODO" in line:
            count_todos = count_todos + 1
        if count_todos > 0:
          list_eligible.append("{} - {} TODOs - {}".format(file[0], count_todos, src_path))
  if len(list_eligible) > 0:
    print("The next {} files that need to be addressed:".format(args.list))
    print(*list_eligible, sep = "\n")
