import json
import os
import subprocess

jak1_files = None
jak2_files = None
jak3_files = None

with open('./goal_src/jak1/build/all_objs.json', 'r') as f:
  jak1_files = json.load(f)
with open('./goal_src/jak2/build/all_objs.json', 'r') as f:
  jak2_files = json.load(f)
with open('./goal_src/jak3/build/all_objs.json', 'r') as f:
  jak3_files = json.load(f)

def get_file_list(game_name):
  match game_name:
    case "jak1":
      return jak1_files
    case "jak2":
      return jak2_files
    case "jak3":
      return jak3_files

def is_file_in_game(game_name, file_name):
  file_list = get_file_list(game_name)
  for f in file_list:
    if f[2] != 3 and f[2] != 5:
      continue
    if f[0] == file_name:
      return True
  return False

def get_gsrc_path_from_filename(game_name, file_name):
  file_list = get_file_list(game_name)
  src_path = ""
  for f in file_list:
    if f[2] != 3 and f[2] != 5:
      continue
    if f[0] == file_name:
      src_path = f[4]
      break
  path = "./goal_src/{}/{}/{}.gc".format(game_name, src_path, file_name)
  if not os.path.exists(path):
    print("couldn't find {} in /goal_src/{}!".format(file_name, game_name))
    exit(1)
  return path

def get_alltypes_path_from_game(game_name):
  return "./decompiler/config/{}/all-types.gc".format(game_name)

def get_ref_path_from_filename(game_name, file_name, ref_folder):
  file_list = get_file_list(game_name)
  src_path = ""
  for f in file_list:
    if f[2] != 3 and f[2] != 5:
      continue
    if f[0] == file_name:
      src_path = f[4]
      break
  if src_path == "":
    print("couldn't determine ref path for {}:{}!".format(game_name, file_name))
    exit(1)
  path = os.path.join(ref_folder, game_name, src_path, "{}_REF.gc".format(file_name))
  return path

def decompile_file(decompiler_path, decompiler_config, game_version, file_names, omit_var_casts=False):
  decompiler_args = '{{"levels_extract": false, "process_art_groups": false, "decompile_code": true, "allowed_objects": {}}}'.format(file_names)
  if omit_var_casts:
    decompiler_args = '{{"levels_extract": false, "process_art_groups": false, "decompile_code": true, "ignore_var_name_casts": true, "allowed_objects": {}}}'.format(file_names)
  subprocess.run(
    [
        decompiler_path,
        "./decompiler/config/{}".format(decompiler_config),
        "./iso_data",
        "./decompiler_out",
        "--version",
        game_version,
        "--config-override",
        decompiler_args,
    ],
    stdout = subprocess.DEVNULL
)
