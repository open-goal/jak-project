import argparse
import os
import pprint
import sys

parser = argparse.ArgumentParser("update-env")
parser.add_argument("--game", help="The name of the game", type=str)
parser.add_argument("--decomp_config", help="The decompiler config file", type=str)
parser.add_argument("--info", help="Just print out current settings", action='store_true')
args = parser.parse_args()

# TODO - read from defaults
file = {
  "GAME": "jak1",
  "DECOMP_CONFIG": "jak1/jak1_config.jsonc",
  "DECOMP_CONFIG_VERSION": "ntsc_v1"
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

if args.info:
  print(file)
  sys.exit(0)

valid_games = ["jak1", "jak2", "jak3"]

decomp_config_map = {
  "jak1": "jak1/jak1_config.jsonc",
  "jak2": "jak2/jak2_config.jsonc",
  "jak3": "jak3/jak3_config.jsonc"
}

decomp_config_version_map = {
  "jak1": {
    "ntscv1": "ntsc_v1",
    "ntscv2": "ntsc_v2",
    "pal": "pal",
    "ntscjp": "jp"
  },
  "jak2": {
    "ntscv1": "ntsc_v1",
    "ntscv2": "ntsc_v2",
    "pal": "pal",
    "ntscjp": "jp",
    "ntscko": "kor"
  },
  # TODO other versions
  "jak3": {
    "ntscv1": "ntsc_v1",
    "pal": "pal"
  }
}

default_config_version_map = {
  "jak1": "ntsc_v1",
  "jak2": "ntsc_v1",
  "jak3": "ntsc_v1"
}

type_consistency_filter_map = {
  "jak1": "Jak1TypeConsistency",
  "jak2": "Jak2TypeConsistency",
  "jak3": "Jak3TypeConsistency"
}

if args.game:
  if args.game not in valid_games:
    print("Unsupported game '{}'".format(args.game))
    sys.exit(1)
  curr = file["GAME"]
  file["GAME"] = args.game
  if (curr != file["GAME"]) or file["DECOMP_CONFIG_VERSION"] not in decomp_config_version_map[file["GAME"]]:
    file["DECOMP_CONFIG"] = decomp_config_map[file["GAME"]]
    file["DECOMP_CONFIG_VERSION"] = default_config_version_map[file["GAME"]]
    file["TYPE_CONSISTENCY_TEST_FILTER"] = type_consistency_filter_map[file["GAME"]]
if args.decomp_config:
  if args.decomp_config not in decomp_config_version_map[file["GAME"]]:
    print("Unsupported decomp config '{}' for game '{}'".format(args.decomp_config, file["GAME"]))
    sys.exit(1)
  file["DECOMP_CONFIG"] = decomp_config_map[file["GAME"]]
  file["DECOMP_CONFIG_VERSION"] = decomp_config_version_map[file["GAME"]][args.decomp_config]
  file["TYPE_CONSISTENCY_TEST_FILTER"] = type_consistency_filter_map[file["GAME"]]

with open(env_path, 'w') as env_file:
  for item in file.items():
    env_file.write("{}={}\n".format(item[0], item[1]))

print("Task settings updated")
pp = pprint.PrettyPrinter(indent=2)
pp.pprint(file)
