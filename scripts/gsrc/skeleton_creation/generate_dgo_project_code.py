# Generates a file with all `game.gp` code taking the `*.gd` files as input

# example - python .\scripts\gsrc\skeleton_creation\generate_dgo_project_code.py --game jak2

import argparse
import glob
from pathlib import Path
import json
import os

# TODO - this is duplication code, but i don't feel like dealing with python's module system right now, its out of `utils.py`

jak1_files = None
jak2_files = None

with open('./goal_src/jak1/build/all_objs.json', 'r') as f:
  jak1_files = json.load(f)
with open('./goal_src/jak2/build/all_objs.json', 'r') as f:
  jak2_files = json.load(f)

def get_file_list(game_name):
  if game_name == "jak1":
    return jak1_files
  else:
    return jak2_files

def get_project_path_from_filename(game_name, file_name):
  file_list = get_file_list(game_name)
  src_path = ""
  for f in file_list:
    if f[2] != 3:
      continue
    if f[0] == file_name:
      src_path = f[4]
      break
  path = "./goal_src/{}/{}/{}.gc".format(game_name, src_path, file_name)
  if not os.path.exists(path):
    print("{} couldn't find in /goal_src/{}!".format(file_name, game_name))
    exit(1)
  return "{}/{}.gc".format(src_path, file_name)


parser = argparse.ArgumentParser("generate_dgo_project_code")
parser.add_argument("--game", help="The name of the game", type=str)
# TODO - it isn't really established what this should actually be, for example, at the time of writing
# jak 2's other DGOs depend on a `los-control` file which is a random file near the end (but not quite) in GAME.CGO
#
# Until a proper system is figured out, this is basically a best-guess so there's no point trying to be accurate at this stage
parser.add_argument("--dep", help="Arbitrary file to make all the DGOs depend upon", type=str)
args = parser.parse_args()

dgos_to_skip = ["common.gd", "engine.gd", "game.gd", "vi1.gd", "kernel.gd", "cta.gd", "lwidea.gd", "cwi.gd", "pri.gd"]
output = []
file_locations = None

for name in glob.glob('./goal_src/{}/dgos/*.gd'.format(args.game)):
  skip_it = False
  for to_skip in dgos_to_skip:
    if to_skip in name:
      skip_it = True
      break
  if skip_it:
    continue
  # read the file
  dgo_name = Path(name).stem.upper()
  # first print a comment header
  output.append(";;;;;;;;;;;;;;;;;;;;;\n")
  output.append(";; {}\n".format(dgo_name))
  output.append(";;;;;;;;;;;;;;;;;;;;;\n")
  output.append("\n")
  # print cgo call
  output.append("(cgo \"{}.DGO\" \"{}.gd\")\n".format(dgo_name, dgo_name.lower()))
  output.append("\n")

  object_paths = []
  texture_ids = []
  # anything that ends with .go
  copy_names = []

  with open(name, "r") as f:
    dgo_lines = f.readlines()[1:] # skip the first line, dont care about it
    for line in dgo_lines:
      # gross parsing, as usual without a nice data format
      cleaned_line = line.strip().replace("\"", "").replace("(", "").replace(")", "")
      if cleaned_line == "":
        continue
      object_name = cleaned_line.split(" ")[0]
      object_name_no_ext = object_name.split(".")[0]
      if ".o" in object_name:
        object_paths.append(get_project_path_from_filename(args.game, object_name_no_ext))
      elif ".go" in object_name:
        # tpages are different
        if "tpage" in object_name:
          texture_ids.append(object_name_no_ext.split("-")[1])
        else:
          copy_names.append(object_name_no_ext)

  # Construct the rest of the file
  if len(object_paths) > 0:
    output.append("(goal-src-sequence\n  \"\"\n  :deps (\"$OUT/obj/{}.o\")\n".format(args.dep))
    for path in object_paths:
      output.append("  \"{}\"\n".format(path))
    output.append("  )\n\n")

  if len(texture_ids) > 0:
    output.append("(copy-textures {})\n\n".format(" ".join(texture_ids)))

  if len(copy_names) > 0:
    output.append("(copy-gos\n")
    for name in copy_names:
      output.append("  \"{}\"\n".format(name))
    output.append("  )\n\n")

with open('dgo-proj-code.gp', 'w') as out_file:
  out_file.writelines(output)
