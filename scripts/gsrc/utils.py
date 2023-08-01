import json
import os

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

def get_gsrc_path_from_filename(game_name, file_name):
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
  return path

def get_alltypes_path_from_game(game_name):
  return "./decompiler/config/{}/all-types.gc".format(game_name)

def get_ref_path_from_filename(game_name, file_name, ref_folder):
  file_list = get_file_list(game_name)
  src_path = ""
  for f in file_list:
    if f[2] != 3:
      continue
    if f[0] == file_name:
      src_path = f[4]
      break
  if src_path == "":
    print("couldn't determine ref path for {}:{}!".format(game_name, file_name))
    exit(1)
  path = os.path.join(ref_folder, game_name, src_path, "{}_REF.gc".format(file_name))
  return path
