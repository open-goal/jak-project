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

script_comments = [
  ";; ----------------------",
  ";; File -",
  ";; Source Path -",
  ";; Containing DGOs -",
  ";; Version -",
  ";; - Types",
  ";; - Functions",
  ";; - Symbols",
  ";; - Unknowns",
  ";; NO FILE",
  ";; Unknowns / Built-Ins / Non-Original Types",
  ";; - Nothing Defined in This File!",
  ";; Unknowns with No Definition"
]

def is_filename_comment(line):
  if not line.startswith(";"):
    return False
  if "define" in line or "deftype" in line:
    return False
  cleaned_line = line.replace(";", "").strip()
  for item in file_list:
    file_name = item[0]
    if file_name == cleaned_line:
      return True
  return False

with open('./scripts/jak1-symbol-mapping.json') as f:
  data = json.load(f)
  all_symbols = []
  for object_file, symbols in data.items():
    for symbol in symbols:
      all_symbols.append(symbol)

def no_runtime_type(definition):
  for line in definition:
    if "no-runtime-type" in line:
      return True
  return False

def strip_trailing_new_lines(definition):
  new_definition = []
  found_content = False
  for line in reversed(definition):
    if found_content:
      new_definition.insert(0, line)
    else:
      cleaned_line = line.strip()
      if len(cleaned_line) != 0:
        found_content = True
        new_definition.insert(0, line)
  # Check if the last line has a new-line or not, if it doesn't add one
  if not new_definition[len(new_definition)-1].endswith("\n"):
    new_definition[len(new_definition)-1].append("\n")
  return new_definition

symbol_definitions = {}
# Anything that is custom / not part of the game, I'll place at the top of the file because I have no idea where it should go
unknown_symbol_definitions = []

def is_script_comment(line):
  for comment in script_comments:
    if line.startswith(comment):
      return True
  return False

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
    if is_filename_comment(line) or is_script_comment(line) or (commented_type is False and ("declare-type" in line or (line == "\n" and i != len(lines) - 1))):
      continue

    # Handle the first line of the file properly
    if len(current_symbol_definition) == 0: # The only time this variable should be empty, is at the beginning, after that it should always be in use
      if line.startswith("(deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern") or line.startswith("(defenum"):
        current_symbol_definition.append(line)
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
            if no_runtime_type(current_symbol_definition):
              current_symbol_definition.insert(0, "(define-extern {} type) ; deftype provided by C Kernel\n".format(current_symbol))
            symbol_definitions[current_symbol] = strip_trailing_new_lines(current_symbol_definition.copy())
        else:
          if no_runtime_type(current_symbol_definition):
              current_symbol_definition.insert(0, "(define-extern {} type) ; deftype provided by C Kernel\n".format(current_symbol))
          symbol_definitions[current_symbol] = strip_trailing_new_lines(current_symbol_definition.copy())
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
      elif line.startswith("(deftype") or line.startswith("; (deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern") or line.startswith("(defenum"):
        if line.startswith("; (deftype"):
          current_symbol = line.split(" ")[2].rstrip("\n")
        else:
          current_symbol = line.split(" ")[1].rstrip("\n")
        current_symbol_definition.clear()
        current_symbol_definition += comment_buffer
        comment_buffer.clear()
        current_symbol_definition.append(line)
        symbol_definitions[current_symbol] = strip_trailing_new_lines(current_symbol_definition.copy())
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

def was_previous_definition_multi_line(definition):
  line_count = 0
  for line in definition:
    if "; (deftype" in line:
      return True
    if not line.strip().startswith(";"):
      line_count = line_count + 1
  return line_count > 1

def print_definition_block(file_lines, header, prev_block_exists, def_list):
  if len(def_list) == 0:
    return False
  if prev_block_exists:
    file_lines.append("\n")
  file_lines.append(";; - {}\n\n".format(header))
  prev_definition = None
  for definition in def_list:
    if prev_definition is not None and was_previous_definition_multi_line(prev_definition):
      file_lines.append("\n")
    file_lines.append("".join(definition))
    prev_definition = definition
  return True

def print_definition_blocks(file_lines, types, functions, symbols, unknowns):
  if not types and not functions and not symbols and not unknowns:
    file_lines.append(";; - Nothing Defined in This File!\n")
  else:
    prev_block_exists = False
    prev_block_exists |= print_definition_block(file_lines, "Types", prev_block_exists, types)
    prev_block_exists |= print_definition_block(file_lines, "Functions", prev_block_exists, functions)
    prev_block_exists |= print_definition_block(file_lines, "Symbols", prev_block_exists, symbols)
    prev_block_exists |= print_definition_block(file_lines, "Unknowns", prev_block_exists, unknowns)
  file_lines.append("\n")

new_file = []
for item in file_list:
  file_name = item[0]
  extension = "gc"
  if item[2] == 4:
    extension = "gd"
  src_path = "{}/{}.{}".format(item[4], file_name, extension)
  new_file.append("\n;; ----------------------\n;; File - {}\n;; Source Path - {}\n;; Containing DGOs - {}\n;; Version - {}\n\n".format(file_name, src_path, item[3], item[2]))
  types = []
  functions = []
  symbols = []
  unknowns = []
  if file_name in symbol_mapping:
    for symbol in symbol_mapping[file_name]:
      if symbol not in symbol_definitions:
        print("Could not find definition for '{}'".format(symbol))
      else:
        symbol_definition = symbol_definitions[symbol]
        if ";;(define-extern" in first_relevant_line(symbol_definition):
          unknowns.append(symbol_definition)
        elif "(function" in first_relevant_line(symbol_definition) or "function)" in first_relevant_line(symbol_definition):
          functions.append(symbol_definition)
        elif "deftype" in first_relevant_line(symbol_definition):
          types.append(symbol_definition)
        elif "define-extern" in first_relevant_line(symbol_definition):
          symbols.append(symbol_definition)
        else:
          print("Could not find associated symbol def for '{}'".format(symbol))

  print_definition_blocks(new_file, types, functions, symbols, unknowns)

  cleaned_unknown_symbol_defs = []
  if file_name == "gcommon":
    new_file.append("\n;; ----------------------\n;; NO FILE\n;; Unknowns / Built-Ins / Non-Original Types\n\n")
    for definition in unknown_symbol_definitions:
      if not definition[0].startswith(";;(define-extern"):
        new_file.append("".join(definition) + "\n")
      else:
        cleaned_unknown_symbol_defs.append(definition)

if len(cleaned_unknown_symbol_defs) > 0:
  new_file.append("\n;; ----------------------\n;; NO FILE\n;; Unknowns with No Definition\n\n")
  for definition in cleaned_unknown_symbol_defs:
    if definition[0].startswith(";;(define-extern"):
      new_file.append("".join(definition).rstrip() + "\n")

os.remove("./decompiler/config/all-types.gc")
with open("./decompiler/config/all-types.gc", "w") as f:
  f.writelines(new_file)


# Third pass!  Add any necessary forward declarations
# - First, let's identify the line numbers where types are defined, and used
# - Then, repeat the process, adding forward declarations when appropriate

type_usages = {}

def get_root_parent_type(t):
  if t["parent_type"] in ["basic", "structure", "symbol", "object", "integer", "pair", "number", "binteger", "function", "array", "type", "string", "uint8", "int8", "uint16", "int16", "uint32", "int32", "uint64", "int64", "uint128", "int128", "float", "kheap"]:
    parent_type = t["parent_type"]
    if parent_type not in ["basic", "structure"]:
      return "type" # NOTE - this does not work currently!!! but is VERY RARE
    return t["parent_type"]
  return get_root_parent_type(type_usages[t["parent_type"]])

def get_safe_parent_type(current_type, all_types, earliest_usage_line):
  parent_type_name = current_type["parent_type"]
  if parent_type_name in ["basic", "structure", "type"]:
    return parent_type_name
  parent_type = all_types[parent_type_name]
  if parent_type["declared_on_line"] < earliest_usage_line:
    return parent_type["type_name"]
  return get_root_parent_type(current_type)

def symbol_usage(line, sym):
  if line.strip().startswith(";"):
    return False
  tokens = line.strip().split(" ")
  for token in tokens[1:]:
    sanitized_token = token.replace("(", "").replace(")", "").strip()
    if sanitized_token == sym:
      return True
  return False

new_file = []
print("Third Pass - Adding Forward Type Declarations")
with open("./decompiler/config/all-types.gc") as f:
  lines = f.readlines()
  # Get the types
  for i, line in enumerate(lines):
    clean_line = line.replace(";", "").strip()
    if clean_line.startswith("(deftype"):
      symbol = clean_line.split(" ")[1]
      parent_type = clean_line.split(" ")[2].rstrip("\n").replace("(", "").replace(")", "")
      if parent_type == "UNKNOWN":
        continue
      type_usages[symbol] = {
        "type_name": symbol,
        "parent_type": parent_type,
        "declared_on_line": i,
        "first_symbol_usage": "",
        "used_on_lines": [],
        "commented_out_type": line.startswith(";")
      }
  # Identify Usages - heavy loop
  for symbol, usage_info in type_usages.items():
    symbol_index = list(type_usages.keys()).index(symbol)
    if symbol_index % 100 == 0:
      print("[{}/{}]: Finding Type Usages".format(symbol_index, len(type_usages)))
    current_symbol = ""
    for i, line in enumerate(lines):
      if i > usage_info["declared_on_line"]:
        break # For speed reasons, we don't care about usages after the declaration
      if "; deftype provided by C Kernel" in line:
        continue
      if line.startswith("(deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern") or line.startswith("(defenum"):
        current_symbol = line.split(" ")[1]
      if i != usage_info["declared_on_line"] and symbol_usage(line, usage_info["type_name"]):
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
    if declaration_line > earliest_usage or usage_info["commented_out_type"]:
      if usage_info["first_symbol_usage"] not in forward_declarations:
        forward_declarations[usage_info["first_symbol_usage"]] = ["(declare-type {} {})\n".format(symbol, get_safe_parent_type(usage_info, type_usages, earliest_usage))]
      else:
        forward_declarations[usage_info["first_symbol_usage"]].append("(declare-type {} {})\n".format(symbol, get_safe_parent_type(usage_info, type_usages, earliest_usage)))

  # FINALLY - add the forward declarations\
  skip_next = False
  for i, line in enumerate(lines):
    if skip_next:
      skip_next = False
      new_file.append(line)
      continue
    if "; deftype provided by C Kernel" in line:
      skip_next = True
    if line.startswith("(deftype") or line.startswith("(define-extern") or line.startswith(";;(define-extern"):
      current_symbol = line.split(" ")[1]
      if current_symbol in forward_declarations:
        new_file.append("".join(forward_declarations[current_symbol]))
    new_file.append(line)

os.remove("./decompiler/config/all-types.gc")
with open("./decompiler/config/all-types.gc", "w") as f:
  f.writelines(new_file)

