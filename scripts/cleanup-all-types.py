# This does a (currently) 3 pass cleanup on all-types
# 1. Cleanup any symbol definitions that are redundant
# 2. Reorder symbol definitions based on file build order
# 3. Check for any necessary forward declarations

import os

# First pass!
print("First Pass - Cleaning up File")
new_file = []
with open("./decompiler/config/all-types.gc") as f:
  symbols_found = []
  lines = f.readlines()
  for line in lines:
    if line.startswith("(deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern"):
      symbol = line.split(" ")[1]
      if symbol in symbols_found and "unknown type" in line:
        continue
      else:
        symbols_found.append(symbol)
    new_file.append(line)

os.remove("./decompiler/config/all-types.gc")
with open("./decompiler/config/all-types.gc", "w") as f:
  f.writelines(new_file)

# Second Pass!
# I try to preserve comments as best I can:
# - comments that are file names are discarded, they are now redundant!
# - comments prior to a symbol definition are considered part of that symbol definition, comments after are NOT
# Build up a mapping of symbol-name -> symbol-definitions and then we can easily drop them into place
# Symbols are defined by reading line-by-line until we see the next:
# - deftype
# - define-extern
# - defenum
# declare-types can be discarded, they are handled in the third pass
# If something is defined more than once, take the longer definition

from jak1_file_list import file_list
import json

def is_filename_comment(line):
  if not line.startswith(";"):
    return False
  if "define" in line or "deftype" in line:
    return False
  for item in file_list:
    file_name = item[0]
    if file_name in line:
      return True
  return False

with open('./scripts/jak1-symbol-mapping.json') as f:
  data = json.load(f)
  all_symbols = []
  for object_file, symbols in data.items():
    for symbol in symbols:
      all_symbols.append(symbol)

symbol_definitions = {}
# Anything that is custom / not part of the game, I'll place at the top of the file because I have no idea where it should go
unknown_symbol_definitions = []

print("Second Pass - Re-Organizing File")
with open("./decompiler/config/all-types.gc") as f:
  lines = f.readlines()
  current_symbol = ""
  current_symbol_definition = []
  comment_buffer = []
  commented_type = False
  for i, line in enumerate(lines):
    # Ignore the following lines:
    # - declare-types
    # - empty lines
    # - file name comments
    # - comments i generate
    if commented_type is False and ("declare-type" in line or (line == "\n" and i != len(lines) - 1) or is_filename_comment(line) or line.startswith(";; File -") or line.startswith(";; Source Path -") or line.startswith(";; Containing DGOs -") or line.startswith(";; Version -") or line.startswith(";; Types") or line.startswith(";; Functions") or line.startswith(";; Unknowns") or line.startswith(";; NO FILE")):
      continue

    # Handle the first line of the file properly
    if len(current_symbol_definition) == 0: # The only time this variable should be empty, is at the beginning, after that it should always be in use
      current_symbol_definition.append(line)
      if line.startswith("(deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern") or line.startswith("(defenum"):
        current_symbol = line.split(" ")[1].rstrip("\n")
      continue

    # To support comments being associated with the following symbol def, we have to keep track of them
    # then either associate them with the new symbol OR realize they are part of the current one!
    if not commented_type and line.startswith(";") and not (line.startswith("; (deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern") or line.startswith("(defenum")):
      current_symbol_definition.append(line)
      comment_buffer.append(line)
      continue

    # Check if we've reached a new symbol or reached the end of the file
    if i == len(lines) - 1 or line.startswith("(deftype") or line.startswith("; (deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern") or line.startswith("(defenum"):
      # Remove any comments from the previous symbol def
      if not commented_type and len(comment_buffer) > 0:
        current_symbol_definition = current_symbol_definition[:-len(comment_buffer)]
      # Check if the symbol we found is valid or invalid
      if current_symbol in all_symbols:
        if current_symbol in symbol_definitions:
          # print("Symbol re-defintion found for '{}', choosing the bigger one!".format(current_symbol))
          if len(current_symbol_definition) > len(symbol_definitions[current_symbol]):
            symbol_definitions[current_symbol] = current_symbol_definition.copy()
        else:
          symbol_definitions[current_symbol] = current_symbol_definition.copy()
      else:
        print("Found a symbol '{}' that is not part of Jak 1!".format(current_symbol))
        unknown_symbol_definitions.append(current_symbol_definition.copy())
      if i != len(lines) - 1:
        if line.startswith("; (deftype"):
          current_symbol = line.split(" ")[2].rstrip("\n")
          commented_type = True
        else:
          current_symbol = line.split(" ")[1].rstrip("\n")
          commented_type = False
        current_symbol_definition.clear()
        current_symbol_definition += comment_buffer
        comment_buffer.clear()
        current_symbol_definition.append(line)
    else:
      current_symbol_definition.append(line)
      if len(comment_buffer) > 0:
        comment_buffer.clear()

# Now armed with our complete list of symbols, print them out in a nice organized manner.
# Precedence:
# - unknown symbols (after kernel definitions - `gstate` is the last)
# - file build order (include info about src file path / DGOs it's contained in)
# - types
# - functions
# - unknown types/symbols/functions

with open('./scripts/jak1-symbol-mapping.json') as f:
  symbol_mapping = json.load(f)

def first_relevant_line(definition):
  if len(definition) == 1:
    return definition[0]
  for line in definition:
    if line.startswith("; (deftype") or not line.startswith(";"):
      return line

new_file = []
for item in file_list:
  file_name = item[0]
  extension = "gc"
  if item[2] == 4:
    extension = "gd"
  src_path = "{}/{}.{}".format(item[4], file_name, extension)
  new_file.append("\n;; File - {}\n;; Source Path - {}\n;; Containing DGOs - {}\n;; Version - {}\n\n".format(file_name, item[4], src_path, item[2]))
  types = []
  functions = []
  unknowns = []
  if file_name in symbol_mapping:
    for symbol in symbol_mapping[file_name]:
      if symbol not in symbol_definitions:
        print("Could not find definition for '{}'".format(symbol))
      else:
        symbol_definition = symbol_definitions[symbol]
        if ";;(define-extern" in first_relevant_line(symbol_definition):
          unknowns.append(symbol_definition)
        elif "(function" in first_relevant_line(symbol_definition):
          functions.append(symbol_definition)
        elif "deftype" in first_relevant_line(symbol_definition) or "define-extern" in first_relevant_line(symbol_definition):
          types.append(symbol_definition)
        else:
          print("Could not find associated symbol def for '{}'".format(symbol))
  # TODO - this can be cleaned up and still isn't perfect
  last_symbol_multi_line = False
  if len(types) > 0:
    new_file.append(";; Types\n\n")
    for t in types:
      if last_symbol_multi_line and len(t) == 1:
        new_file.append("\n")
        last_symbol_multi_line = False
      elif not last_symbol_multi_line and len(t) > 1:
        new_file.append("\n")
      new_file.append("".join(t))
      if len(t) > 1:
        new_file.append("\n")
        last_symbol_multi_line = True
  if len(functions) > 0:
    if len(types) > 0:
      new_file.append("\n")
    new_file.append(";; Functions\n\n")
    for f in functions:
      if last_symbol_multi_line and len(t) == 1:
        new_file.append("\n")
        last_symbol_multi_line = False
      elif not last_symbol_multi_line and len(t) > 1:
        new_file.append("\n")
      new_file.append("".join(f))
      if len(f) > 1:
        new_file.append("\n")
        last_symbol_multi_line = True
  if len(unknowns) > 0:
    if len(types) > 0 or len(functions) > 0:
      new_file.append("\n")
    new_file.append(";; Unknowns\n\n")
    for u in unknowns:
      if last_symbol_multi_line and len(t) == 1:
        new_file.append("\n")
        last_symbol_multi_line = False
      elif not last_symbol_multi_line and len(t) > 1:
        new_file.append("\n")
      new_file.append("".join(u))
      if len(u) > 1:
        new_file.append("\n")
        last_symbol_multi_line = True

  if file_name == "gcommon":
    new_file.append("\n;; NO FILE\n;; Unknowns / Built-Ins / Non-Original Types\n\n")
    for definition in unknown_symbol_definitions:
      new_file.append("".join(definition) + "\n")
    

os.remove("./decompiler/config/all-types-test.gc")
with open("./decompiler/config/all-types-test.gc", "w") as f:
  f.writelines(new_file)


# Third pass!  Add any necessary forward declarations
# - First, let's identify the line numbers where types are defined, and used
# - Then, repeat the process, adding forward declarations when appropriate
# TODO - there might be some false positives here if symbols are used in comments/partial names of other symbols

type_usages = {}

def get_root_parent_type(t):
  if t["parent_type"] in ["basic", "structure", "symbol", "object", "integer", "pair", "number", "binteger", "function", "array", "type", "string", "uint8", "int8", "uint16", "int16", "uint32", "int32", "uint64", "int64", "uint128", "int128", "float", "kheap"]:
    return t["parent_type"]
  return get_root_parent_type(type_usages[t["parent_type"]])

new_file = []
print("Third Pass - Adding Forward Type Declarations")
with open("./decompiler/config/all-types-test.gc") as f:
  lines = f.readlines()
  # Get the types
  for i, line in enumerate(lines):
    if line.startswith("(deftype"):
      symbol = line.split(" ")[1]
      parent_type = line.split(" ")[2].rstrip("\n").replace("(", "").replace(")", "")
      type_usages[symbol] = {
        "type_name": symbol,
        "parent_type": parent_type,
        "declared_on_line": i,
        "first_symbol_usage": "",
        "used_on_lines": []
      }
  # Identify Usages - heavy loop
  for symbol, usage_info in type_usages.items():
    symbol_index = list(type_usages.keys()).index(symbol)
    if symbol_index % 50 == 0:
      print("[{}/{}]: Finding Type Usages".format(symbol_index, len(type_usages)))
    current_symbol = ""
    for i, line in enumerate(lines):
      if line.startswith("(deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern"):
        current_symbol = line.split(" ")[1]
      if i != usage_info["declared_on_line"] and usage_info["type_name"] in line.strip().split(" ") or "({})".format(usage_info["type_name"]) in line.strip().split(" "):
        if len(usage_info["used_on_lines"]) == 0:
          usage_info["first_symbol_usage"] = current_symbol
        usage_info["used_on_lines"].append(i)

  # Identify Necessary Forward Declarations
  forward_declarations = {}
  for symbol, usage_info in type_usages.items():
    declaration_line = usage_info["declared_on_line"]
    if len(usage_info["used_on_lines"]) == 0:
      continue
    earliest_usage = usage_info["used_on_lines"][0]
    if declaration_line > earliest_usage:
      if usage_info["first_symbol_usage"] not in forward_declarations:
        forward_declarations[usage_info["first_symbol_usage"]] = ["(declare-type {} {})\n".format(symbol, get_root_parent_type(usage_info))]
      else:
        forward_declarations[usage_info["first_symbol_usage"]].append("(declare-type {} {})\n".format(symbol, get_root_parent_type(usage_info)))

  # FINALLY - add the forward declarations
  for i, line in enumerate(lines):
    if line.startswith("(deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern"):
      current_symbol = line.split(" ")[1]
      if current_symbol in forward_declarations:
        new_file.append("".join(forward_declarations[current_symbol]))
    new_file.append(line)

os.remove("./decompiler/config/all-types-test.gc")
with open("./decompiler/config/all-types-test.gc", "w") as f:
  f.writelines(new_file)

