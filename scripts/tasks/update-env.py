import argparse
import os
import pprint
import sys

parser = argparse.ArgumentParser("update-env")
parser.add_argument("--game", help="The name of the game", type=str)
parser.add_argument("--decomp_config", help="The decompiler config file", type=str)
args = parser.parse_args()

# TODO - read from defaults
file = {
  "GAME": "jak1",
  "DECOMP_CONFIG": "jak1_ntsc_black_label.jsonc"
}

env_path = os.path.join(os.path.dirname(os.path.realpath(__file__)), ".env")

if not os.path.exists(env_path):
  with open(env_path, 'w') as env_file:
    for item in file.items():
      env_file.write("{}={}\n".format(item[0], item[1]))

with open(env_path, 'r') as env_file:
  flags = env_file.readlines()
  for flag in flags:
    tokens = flag.split("=")
    if tokens[0] in file:
      file[tokens[0]] = tokens[1].strip()

valid_games = ["jak1", "jak2"]

decomp_config_map = {
  "jak1": {
    "ntscv1": "jak1_ntsc_black_label.jsonc",
    "ntscv2": "jak1_us2.jsonc",
    "pal": "jak1_pal.jsonc",
    "ntscjp": "jak1_jp.jsonc"
  },
  "jak2": {
    "ntscv1": "jak2_ntsc_v1.jsonc"
  }
}

default_config_map = {
  "jak1": "jak1_ntsc_black_label.jsonc",
  "jak2": "jak2_ntsc_v1.jsonc"
}

if args.game:
  if args.game not in valid_games:
    print("Unsupported game '{}'".format(args.game))
    sys.exit(1)
  curr = file["GAME"]
  file["GAME"] = args.game
  if (curr != file["GAME"]) or file["DECOMP_CONFIG"] not in decomp_config_map[file["GAME"]]:
    file["DECOMP_CONFIG"] = default_config_map[file["GAME"]]
if args.decomp_config:
  if args.decomp_config not in decomp_config_map[file["GAME"]]:
    print("Unsupported decomp config '{}' for game '{}'".format(args.decomp_config, file["GAME"]))
    sys.exit(1)
  file["DECOMP_CONFIG"] = decomp_config_map[file["GAME"]][args.decomp_config]

with open(env_path, 'w') as env_file:
  for item in file.items():
    env_file.write("{}={}\n".format(item[0], item[1]))

print("Task settings updated")
pp = pprint.PrettyPrinter(indent=2)
pp.pprint(file)
