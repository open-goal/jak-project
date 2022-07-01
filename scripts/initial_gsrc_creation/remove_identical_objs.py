# Just removes every duplicate file (if it has a DGO in the name)

jak2_files = None

import json
from pathlib import Path

with open("../../goal_src/jak2/build/all_objs.json", "r") as f:
  jak2_files = json.load(f)

dgos = set()

jak2_files_cleaned = []

for file in jak2_files:
  for dgo in file[3]:
    dgos.add(dgo.lower())

ignore_list = [
  "dir-tpages-ART",
  "dir-tpages-GAME",
  "traffic-engine",
  "onin-game"
]

removed_files = {}

def basename_in_file_list(name):
  for file in jak2_files:
    if file[0] == name:
      return True
  return False

for file in jak2_files:
  if file[0] in ignore_list:
    jak2_files_cleaned.append(file)
    continue
  name = Path(file[0]).stem
  base_name = None
  duplicate = False
  for dgo in dgos:
    if name.lower().endswith("-{}".format(dgo)):
      # some files have multiple dgos in the name
      base_name_parts = []
      for token in name.split("-"):
        if token.lower() not in dgos:
          base_name_parts.append(token)
      base_name = "-".join(base_name_parts)
      duplicate = True
  if not duplicate:
    jak2_files_cleaned.append(file)
  else:
    print("Removing: {}".format(file[0]))
    if not basename_in_file_list(base_name):
      new_entry = file
      new_entry[0] = base_name
      jak2_files_cleaned.append(new_entry)
    if base_name not in removed_files:
      removed_files[base_name] = set(file[3])
    else:
      for dgo in file[3]:
        removed_files[base_name].add(dgo)

# Update DGO sets on the files that remain
encountered_files = []
for file in jak2_files_cleaned:
  if file[0] in removed_files:
    dgos = removed_files[file[0]]
    for dgo in file[3]:
      dgos.add(dgo)
    file[3] = dgos
    encountered_files.append(removed_files)

with open('../../goal_src/jak2/build/all_objs.json', 'w') as json_file:
  # hard-coded to maintain spacing to only diff lines that actually change
  longest_name = 44
  longest_name_in_dgo = 28
  # Actually write things out
  json_file.write("[\n")
  i = 0
  for jak2_file in jak2_files_cleaned:
    name = '{: <{}}'.format("\"{}\",".format(jak2_file[0]), longest_name + 2)
    name_in_dgo = '{: <{}}'.format("\"{}\",".format(jak2_file[1]), longest_name_in_dgo + 2)
    dgo_set = "["
    for dgo in jak2_file[3]:
      dgo_set += "\"{}\", ".format(dgo)
    dgo_set = dgo_set.removesuffix(", ")
    dgo_set += "]"
    if i == (len(jak2_files_cleaned) - 1):
      json_file.write("[{}{}{}, {}, \"{}\"]\n".format(name, name_in_dgo, jak2_file[2], dgo_set, jak2_file[4]))
    else:
      json_file.write("[{}{}{}, {}, \"{}\"],\n".format(name, name_in_dgo, jak2_file[2], dgo_set, jak2_file[4]))
    i = i + 1
  json_file.write("]\n")

