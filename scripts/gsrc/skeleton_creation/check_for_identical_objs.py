jak2_files = None

import json
import hashlib

def get_md5_from_file(filename):
  file_bytes = open(filename, "rb").read()
  # Remove the last 16bytes of the data as it's 16-byte aligned
  # if this is the only difference in the entire file, it is very likely identical
  # but this data is potentially garbage
  length_bytes = bytes([file_bytes[12], file_bytes[13], file_bytes[14], file_bytes[15]])
  offset_to_truncate = int.from_bytes(length_bytes, "little")
  bytes_chopped = len(file_bytes) - offset_to_truncate
  file_bytes = file_bytes[:offset_to_truncate]

  return [hashlib.md5(file_bytes).hexdigest(), bytes_chopped]

# print(get_md5_from_file("../../decompiler_out/jak2/raw_obj/air-train-ag-CAP.go"))

with open("../../goal_src/jak2/build/all_objs.json", "r") as f:
  jak2_files = json.load(f)

dgos = set()

for file in jak2_files:
  for dgo in file[3]:
    dgos.add(dgo.lower())

import glob
from pathlib import Path

obj_files = glob.glob("../../decompiler_out/jak2/raw_obj/*")

print(len(obj_files))

# Find all the files that have the same name, but a different prefix
potentially_identical_files = {}

print("WARNING: Be aware of the NOTE below, double check the output that the assumption has not changed")

for file in obj_files:
  name = Path(file).stem
  base_name = None
  duplicate = False
  for dgo in dgos:
    if name.lower().endswith("-{}".format(dgo)):
      # some files have multiple dgos in the name
      base_name_parts = []
      for token in name.split("-"):
        # NOTE - this is still a little fragile
        # some files begin with a dgo name, for example `oracle-door-ag-ORACLE`
        # so the easy hack is to skip tokens only if they are DGOs _and_ uppercase
        if token.isupper() and token.lower() in dgos:
          continue
        base_name_parts.append(token)
      base_name = "-".join(base_name_parts)
      duplicate = True
  if duplicate:
    # Calculate the hash of the file
    hash = get_md5_from_file(file)
    if base_name not in potentially_identical_files:
      potentially_identical_files[base_name] = [{"path": file, "hash": hash[0], "bytes_dropped": hash[1]}]
    else:
      potentially_identical_files[base_name].append({"path": file, "hash": hash[0], "bytes_dropped": hash[1]})

# Log any that actually differ
for base_file in potentially_identical_files:
  common_hash = potentially_identical_files[base_file][0]["hash"]
  for file in potentially_identical_files[base_file]:
    if common_hash != file["hash"]:
      print("{} is different!".format(base_name))
      break

with open('./potentially-identical-files.json', 'w') as json_file:
  json.dump(potentially_identical_files, json_file)
