from prompt_toolkit import PromptSession
from prompt_toolkit.history import FileHistory
from jsmin import jsmin
import shlex
import json
import collections

stack_cast_file_path = "./decompiler/config/jak1_ntsc_black_label/stack_structures.jsonc"
type_cast_file_path = "./decompiler/config/jak1_ntsc_black_label/type_casts.jsonc"
lambda_cast_file_path = "./decompiler/config/jak1_ntsc_black_label/anonymous_function_types.jsonc"

global_file_name = ""
global_func_name = ""

def ordered_dict_insert(ordered_dict, index, key, value):
    if key in ordered_dict:
        raise KeyError("Key already exists")
    if index < 0 or index > len(ordered_dict):
        raise IndexError("Index out of range")

    keys = list(ordered_dict.keys())[index:]
    ordered_dict[key] = value
    for k in keys:
        ordered_dict.move_to_end(k)

# TODO - optional size
def stack_cast(function_name, type_name, offset):
  with open(stack_cast_file_path, "r+") as config_file:
    minified = jsmin(config_file.read())
    json_data = json.JSONDecoder(object_pairs_hook=collections.OrderedDict).decode(minified)
    # Check if the function name has already been added
    if function_name not in json_data:
      ordered_dict_insert(json_data, len(json_data)-1, function_name, [])
    # Check if there already exists a cast with the same offset, if so replace it
    casts = json_data[function_name]
    for cast in casts:
      if cast[0] == offset:
        print("Found cast with the same offset, replacing!")
        cast[1] = type_name
        config_file.seek(0)
        json.dump(json_data, config_file, indent=2)
        config_file.truncate()
        return
    json_data[function_name].append([offset, type_name])
    config_file.seek(0)
    json.dump(json_data, config_file, indent=2)
    config_file.truncate()
    print("Stack Cast Applied!")

def type_cast(function_name, register, type_cast, cast_range):
  with open(type_cast_file_path, "r+") as config_file:
    minified = jsmin(config_file.read())
    json_data = json.JSONDecoder(object_pairs_hook=collections.OrderedDict).decode(minified)
    # Check if the function name has already been added
    if function_name not in json_data:
      ordered_dict_insert(json_data, len(json_data)-1, function_name, [])

    range_start = -1
    range_end = -1
    if "-" in cast_range:
      range_start = int(cast_range.split("-")[0])
      range_end = int(cast_range.split("-")[1])
    else:
      range_start = int(cast_range)

    # Check if there already exists a cast with the same offset, if so replace it
    casts = json_data[function_name]
    new_casts = []
    for cast in casts:
      cast_range_start = -1
      cast_range_end = -1
      if isinstance(cast[0], list):
        cast_range_start = cast[0][0]
        cast_range_end = cast[0][1]
      else:
        cast_range_start = cast[0]
      cast_register = cast[1]

      if range_end == -1 and cast_range_end == -1 and range_start == cast_range_start and register == cast_register:
        print("Found cast with the same range, replacing!")
        continue
      elif range_end == -1:
        if range_start >= cast_range_start and range_start <= cast_range_end and register == cast_register:
          print("Found cast with the same range, replacing!")
          continue
      elif cast_range_end == -1:
        if cast_range_start >= range_start and cast_range_start <= range_end and register == cast_register:
          print("Found cast with the same range, replacing!")
          continue
      new_casts.append(cast)

    json_data[function_name] = new_casts
    if range_end == -1:
      json_data[function_name].append([range_start, register, type_cast])
    else:
      json_data[function_name].append([[range_start, range_end], register, type_cast])
    config_file.seek(0)
    json.dump(json_data, config_file, indent=2)
    config_file.truncate()
    print("Type Cast Applied!")

def lambda_cast(file_name, func_sig, func_id):
  with open(lambda_cast_file_path, "r+") as config_file:
    minified = jsmin(config_file.read())
    json_data = json.JSONDecoder(object_pairs_hook=collections.OrderedDict).decode(minified)
    # Check if the function name has already been added
    if file_name not in json_data:
      ordered_dict_insert(json_data, len(json_data)-1, file_name, [])
    # Check if there already exists a cast with the same offset, if so replace it
    functions = json_data[file_name]
    for func in functions:
      if func[0] == func_id:
        print("Found lambda with the same id, replacing!")
        func[1] = func_sig
        config_file.seek(0)
        json.dump(json_data, config_file, indent=2)
        config_file.truncate()
        return
    json_data[file_name].append([func_id, func_sig])
    config_file.seek(0)
    json.dump(json_data, config_file, indent=2)
    config_file.truncate()
    print("Lambda Cast Applied!")

def main():
    global global_file_name
    global global_func_name

    from pathlib import Path
    home = str(Path.home())
    session = PromptSession(history=FileHistory('{}/.castReplHistory'.format(home)))

    # LOOP
    while True:
        # READ
        try:
            prompt_string = "castREPL> "
            if global_file_name and global_func_name:
              prompt_string = "castREPL-{}-{}> ".format(global_file_name, global_func_name)
            elif global_file_name:
              prompt_string = "castREPL-{}> ".format(global_file_name)
            elif global_func_name:
              prompt_string = "castREPL-{}> ".format(global_func_name)
            text = session.prompt(prompt_string)
        except KeyboardInterrupt:
            continue
        except EOFError:
            break

        # TODO - help command
        # TODO - command to list current casts
        # TODO - command to delete casts
        # TODO - command to lookup function signature in all-types
        # TODO - command to tie into the C++ program I'll right to narrow down an unknown type
        # TODO - command to define function signature in all-types

        # EVAL / CAST / PRINT
        tokens = shlex.split(text)
        if len(tokens) < 1:
          continue
        # PROCESS COMMANDS
        command = tokens[0]
        # Allows you to persist a file name -- cutting down on command args (useful if working on a big file)
        if command == "enter-file" and len(tokens) == 2:
          global_file_name = tokens[1]
        elif command == "exit-file":
          global_file_name = ""
        # Allows you to persist a function name -- cutting down on command args (useful if working on a big function)
        elif command == "enter-func" and len(tokens) == 2:
          global_func_name = tokens[1]
        elif command == "exit-func":
          global_func_name = ""
        elif command == "stack" and len(tokens) == 4:
          # Stack casts are in the following format stack <func> <type> <offset>
          stack_cast(tokens[1], tokens[2], int(tokens[3]))
        elif command == "lambda" and len(tokens) == 4:
          # Stack casts are in the following format lambda <file_name> <id> <func_sig>
          lambda_cast(tokens[1], tokens[2], int(tokens[3]))
        elif command == "type" and len(tokens) == 5:
          # Stack casts are in the following format type <func> <register> <type_cast> <range = X | X-Y>
          type_cast(tokens[1], tokens[2], tokens[3], tokens[4])
        else:
          print("Invalid Cast Command!")
    # Exit
    print('GoodBye!')

if __name__ == '__main__':
    main()
