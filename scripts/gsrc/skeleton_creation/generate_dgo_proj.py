# Generates the `(cgo-file...` lines for the game.gp file
# Attempts to put DGOs in the correct order based on the file order in `all_objs`

import json

common_deps = '("$OUT/obj/cty-guard-turret-button.o")'

ignored_dgos = ["ENGINE", "KERNEL", "ART", "COMMON", "GAME", "NO-XGO"]

dgos_encountered = set()
dgos_handled = set()

jak2_files = None
with open("./goal_src/jak2/build/all_objs.json", "r") as f:
  jak2_files = json.load(f)

# Enumerate the files, order is dictated by code files (version 3)
# At the end we will fill in any dgos that weren't considerd "required"
lines = []
for file in jak2_files:
  file_name = file[0]
  version = file[2]
  dgo_list = file[3]
  for dgo in dgo_list:
    dgos_encountered.add(dgo)
  if version == 3:
    dgo = dgo_list[0]
    if dgo.lower() not in dgos_handled and dgo not in ignored_dgos:
      dgos_handled.add(dgo.lower())
      lines.append('(cgo-file "{}.gd" {})'.format(dgo.lower(), common_deps))
for dgo in dgos_encountered:
  if dgo.lower() not in dgos_handled and dgo not in ignored_dgos:
    lines.append('(cgo-file "{}.gd" {})'.format(dgo.lower(), common_deps))

for line in lines:
  print(line)
