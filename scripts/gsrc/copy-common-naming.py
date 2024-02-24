# The goal of this script is fairly straight forward
# Given a object file name, decompile it for both jak 2 and jak 3 WITHOUT variable casts
# Get all function definitions and compare the function bodies.
# - if the function bodies are the same, copy the variables from whichever game has them defined to the other
#   - also, if it's a function and a docstring exists on one side but not the other, copy the docstring to the other side's all-types file

import argparse
import glob
import json
import os

from utils import decompile_file, is_file_in_game


parser = argparse.ArgumentParser("copy-common-naming")
parser.add_argument("--file", help="The name of the file", type=str)
parser.add_argument("--decompiler", help="The path to the decompiler", type=str)
parser.add_argument("--update-names-from-refs", help="The decomp config version", action='store_true')
args = parser.parse_args()

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

def get_var_casts_for_game(game_name):
  return json.load(open("./decompiler/config/{}/ntsc_v1/var_names.jsonc".format(game_name), "r"))

def save_var_casts_for_game(game_name, casts):
  with open("./decompiler/config/{}/ntsc_v1/var_names.jsonc".format(game_name), "w") as f:
    json.dump(casts, f, indent=2)

def get_all_types_for_game(game_name):
  return open("./decompiler/config/{}/all-types.gc".format(game_name), "r").readlines()

jak2_alltypes = get_all_types_for_game("jak2")
jak3_alltypes = get_all_types_for_game("jak3")

file_stats = ""

def update_file_var_name_casts(file_name, modify_alltypes):
  global file_stats
  # Check if the file exists in both games
  if not is_file_in_game("jak3", file_name) or not is_file_in_game("jak2", file_name):
    print("File not found in both games")
    return

  # Decompile the file for both games
  decompile_file(args.decompiler, "jak3/jak3_config.jsonc", "ntsc_v1", "[\"{}\"]".format(file_name), True)
  decompile_file(args.decompiler, "jak2/jak2_config.jsonc", "ntsc_v1", "[\"{}\"]".format(file_name), True)

  # Go grab the contents of each file
  jak2_file_contents = open("./decompiler_out/jak2/{}_ir2.asm".format(file_name), "r").readlines()
  jak3_file_contents = open("./decompiler_out/jak3/{}_ir2.asm".format(file_name), "r").readlines()

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
  file_stats = file_stats + "Found {} matching functions in {}\n".format(len(matching_func_names), file_name)

  # Go grab the var casts for each game
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

  save_var_casts_for_game("jak2", jak2_var_casts)
  save_var_casts_for_game("jak3", jak3_var_casts)

  # Automatically copy docstrings for functions (methods are way to annoying to do with hack scripts now)
  if modify_alltypes:
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

if args.update_names_from_refs:
  reference_test_files = glob.glob("./test/decompiler/reference/jak3/**/*_REF.gc", recursive=True)
  for file_no, reference_test_file in enumerate(reference_test_files):
    file_name = os.path.basename(reference_test_file).split("_REF.gc")[0]
    print("({}/{}) Checking Var Name Casts for {}...".format(file_no+1, len(reference_test_files), file_name))
    update_file_var_name_casts(file_name, False)
else:
  update_file_var_name_casts(args.file, True)

print(file_stats)

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
