import sys
from jak1_file_list import file_list

import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--files")
parser.add_argument("--list", type=int)
args = parser.parse_args()

def update_json_file(decomp_list):
  new_file_contents = []
  print("Decompiling - " + ','.join(decomp_list))
  # TODO, update the CGO/DGO
  with open("decompiler\config\jak1_ntsc_black_label.jsonc", "r") as config_file:
    cfg_lines = config_file.readlines()
    for line in cfg_lines:
      if "allowed_objects" in line:
        line = "  \"allowed_objects\": [\"" + ','.join(decomp_list) + "\"],\n"
      new_file_contents.append(line)
  if len(new_file_contents) > 0:
    with open("decompiler\config\jak1_ntsc_black_label.jsonc", "w") as f:
      f.writelines(new_file_contents)
      print("Allowed objects list updated")

if args.files:
  input_list = args.files.split(",")
  decomp_list = []
  for inFile in input_list:
    for file in file_list:
      if file[0] == inFile:
        decomp_list.append(file[0])
        break
      elif file[1] == inFile:
        # NOTE - kinda a hack, assumes -ag files always come after
        decomp_list.append(file[1])
        break
  if len(decomp_list) > 0:
    update_json_file(decomp_list)
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
