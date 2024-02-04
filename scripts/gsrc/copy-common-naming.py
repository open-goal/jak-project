# The goal of this script is fairly straight forward
# Given a object file name, decompile it for both jak 2 and jak 3 WITHOUT variable casts
# Get all function definitions and compare the function bodies.
# - if the function bodies are the same, copy the variables from whichever game has them defined to the other
#   - also, if it's a function and a docstring exists on one side but not the other, copy the docstring to the other side's all-types file

# TODO - run without caring about docstrings, just var_names for all files in reference tests folder

import argparse
import json

from utils import decompile_file, is_file_in_game


parser = argparse.ArgumentParser("copy-common-naming")
parser.add_argument("--file", help="The name of the file", type=str)
parser.add_argument("--decompiler", help="The path to the decompiler", type=str)
parser.add_argument("--decompiler_config", help="The decomp config", type=str)
parser.add_argument("--version", help="The decomp config version", type=str)
args = parser.parse_args()

# Check if the file exists in both games
if not is_file_in_game("jak3", args.file) or not is_file_in_game("jak2", args.file):
  print("File not found in both games")
  exit(1)

# Decompile the file for both games
decompile_file(args.decompiler, "jak3/jak3_config.jsonc", "ntsc_v1", "[\"{}\"]".format(args.file), True)
decompile_file(args.decompiler, "jak2/jak2_config.jsonc", "ntsc_v1", "[\"{}\"]".format(args.file), True)

# Go grab the contents of each file
jak2_file_contents = open("./decompiler_out/jak2/{}_ir2.asm".format(args.file), "r").readlines()
jak3_file_contents = open("./decompiler_out/jak3/{}_ir2.asm".format(args.file), "r").readlines()

def find_all_function_defs(lines):
  store = {}
  in_function_def = False
  in_docstring = False
  passed_potential_docstring = False
  current_function_name = None
  for line in lines:
    if line.startswith("; .function") and "top-level" not in line:
      current_function_name = line.split(".function")[1].strip()
      store[current_function_name] = {
        "docstring": [],
        "definition": [],
      }
      passed_potential_docstring = False
      in_docstring = False
      continue
    if current_function_name is not None and line.startswith(";;-*-OpenGOAL-Start-*-"):
      in_function_def = True
      continue
    if current_function_name is not None and line.startswith(";;-*-OpenGOAL-End-*-"):
      in_function_def = False
      continue
    if line.strip() == "":
      continue
    if in_function_def:
      if not passed_potential_docstring and line.strip().startswith("\""):
        in_docstring = True
      if in_docstring:
        store[current_function_name]["docstring"].append(line.strip())
        if line.strip().endswith("\""):
          in_docstring = False
      else:
        store[current_function_name]["definition"].append(line)
        if len(store[current_function_name]["definition"]) > 1 and line.startswith("   "):
          passed_potential_docstring = True
  return store


# Read in the function definitions for each file to find which ones match

jak2_function_defs = find_all_function_defs(jak2_file_contents)
jak3_function_defs = find_all_function_defs(jak3_file_contents)

# print(jak2_function_defs["vector-xz-cross!"])
# print()
# print(jak3_function_defs["vector-xz-cross!"])

# Compare functions to see which ones are eligible
matching_func_names = []
for func_name in jak2_function_defs:
  if func_name in jak3_function_defs and jak2_function_defs[func_name]["definition"] == jak3_function_defs[func_name]["definition"]:
    matching_func_names.append(func_name)

# print(matching_func_names)
print("Found {} matching functions".format(len(matching_func_names)))

# Go grab the var casts for each game
def get_var_casts_for_game(game_name):
  return json.load(open("./decompiler/config/{}/ntsc_v1/var_names.jsonc".format(game_name), "r"))

jak2_var_casts = get_var_casts_for_game("jak2")
jak3_var_casts = get_var_casts_for_game("jak3")

# For each eligible matching function, copy over the var casts
# The assumption is if it exists in jak 3 it's better (done more recently) so we use that
# else, use jak 2's if it exists
for func_name in matching_func_names:
  if func_name in jak3_var_casts:
    jak2_var_casts[func_name] = jak3_var_casts[func_name]
  elif func_name in jak2_var_casts:
    jak3_var_casts[func_name] = jak2_var_casts[func_name]

def save_var_casts_for_game(game_name, casts):
  with open("./decompiler/config/{}/ntsc_v1/var_names.jsonc".format(game_name), "w") as f:
    json.dump(casts, f, indent=2)

save_var_casts_for_game("jak2", jak2_var_casts)
save_var_casts_for_game("jak3", jak3_var_casts)

def get_all_types_for_game(game_name):
  return open("./decompiler/config/{}/all-types.gc".format(game_name), "r").readlines()

jak2_alltypes = get_all_types_for_game("jak2")
jak3_alltypes = get_all_types_for_game("jak3")

# Automatically copy docstrings for functions (methods are way to annoying to do with hack scripts now)
for func_name in matching_func_names:
  if func_name.startswith("("):
    continue
  # handle the case where the jak 3 version has a docstring, but jak 2 does not
  if len(jak3_function_defs[func_name]["docstring"]) != 0 and len(jak2_function_defs[func_name]["docstring"]) == 0:
    for line_no, line in enumerate(jak2_alltypes):
      line = jak2_alltypes[line_no]
      if line.startswith("(define-extern {}".format(func_name)):
        jak2_alltypes[line_no] = line.replace("(define-extern {} ".format(func_name), "(define-extern {}\n  {}\n  ".format(func_name, "\n  ".join(jak3_function_defs[func_name]["docstring"])))
        break
  # handle the case where jak 2 has a docstring but jak 3 does not
  elif len(jak2_function_defs[func_name]["docstring"]) != 0 and len(jak3_function_defs[func_name]["docstring"]) == 0:
    for line_no, line in enumerate(jak3_alltypes):
      line = jak3_alltypes[line_no]
      if line.startswith("(define-extern {}".format(func_name)):
        jak3_alltypes[line_no] = line.replace("(define-extern {} ".format(func_name), "(define-extern {}\n  {}\n  ".format(func_name, "\n  ".join(jak2_function_defs[func_name]["docstring"])))
        break

# Copy over deftype docstrings from jak3 to jak2 if applicable
def get_type_docstrings_from_alltypes(lines):
  store = {}
  awaiting_next_docstring = True
  current_type_name = None
  for line in lines:
    if line.startswith("(deftype"):
      current_type_name = line.split("deftype ")[1].split("(")[0].strip()
      awaiting_next_docstring = False
      store[current_type_name] = []
      continue
    if line.strip().startswith("(") and not line.strip().endswith("\""):
      awaiting_next_docstring = True
      continue
    if not awaiting_next_docstring:
      store[current_type_name].append(line.strip())
  return store

jak2_type_docs = get_type_docstrings_from_alltypes(jak2_alltypes)
jak3_type_docs = get_type_docstrings_from_alltypes(jak3_alltypes)

# If a docstring exists in jak3 but not in jak2, copy it back
new_jak2_alltypes = []
for line_no, line in enumerate(jak2_alltypes):
  line = jak2_alltypes[line_no]
  new_jak2_alltypes.append(line)
  if line.startswith("(deftype "):
    current_type_name = line.split("deftype ")[1].split("(")[0].strip()
    if current_type_name in jak3_type_docs and len(jak2_type_docs[current_type_name]) == 0:
      for docstring_line in jak3_type_docs[current_type_name]:
        new_jak2_alltypes.append("  " + docstring_line + "\n")
jak2_alltypes = new_jak2_alltypes

# Save all-types
def get_all_types_for_game(game_name, lines):
  with open("./decompiler/config/{}/all-types.gc".format(game_name), "w") as f:
    f.writelines(lines)

get_all_types_for_game("jak2", jak2_alltypes)
get_all_types_for_game("jak3", jak3_alltypes)
