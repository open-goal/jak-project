## Given a list of files(comma delimited), decompile it, then place it under the specified placeholder (if it exists)
## if the placeholder doesn't exist, error out
## the placeholder is `;; DECOMP BEGINS`

from jak1_file_list import file_list
import os
import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--files")
args = parser.parse_args()

files = args.files.split(",")

throw_error = False

# files in this list have manual modifications in in code block
# sometimes these modifications don't prevent a compiler error
# and are easy to commit
#
# if you know of / add such a modification, you should append to this
# list, it will remind you such a file was touched by making a root file
# that will be picked up by git to shame you
files_with_modifications = [
  "target-util",
  "ambient"
]

for file in files:
  if file in files_with_modifications:
    file_name = "{}.manual_restore_reminder".format(file)
    with open(file_name, 'w') as fp:
      pass

  disasm_path = "./decompiler_out/jak1/{}_disasm.gc".format(file)
  if not os.path.exists(disasm_path):
    print("{} doesn't exist!".format(disasm_path))
    throw_error = True
    continue

  src_path = ""
  for f in file_list:
    if f[2] != 3:
      continue
    if f[0] == file:
      src_path = f[4]
      break

  if not os.path.exists("./goal_src/{}".format(src_path)):
    print("{} couldn't find in /goal_src!".format(file))
    throw_error = True
    continue

  file_path = "./goal_src/{}/{}.gc".format(src_path, file)
  new_lines = []
  with open(file_path) as f:
    lines = f.readlines()
    found_placeholder = False
    for line in lines:
      if ";; decomp begins" in line.lower():
        found_placeholder = True
        new_lines.append(line.upper())
        break
      new_lines.append(line)
    if found_placeholder == False:
      print("No placeholder found in {}, skipping".format(file_path))
      throw_error = True
      continue

  # finally...update the file
  lines_to_ignore = [
    ";;-*-Lisp-*-",
    "(in-package goal)",
    ";; definition",
    ";; INFO:",
    ";; failed to figure",
    ";; Used lq/sq"
  ]
  def skippable_line(line):
    for prefix in lines_to_ignore:
      if line.startswith(prefix):
        return True
    return False
  with open(disasm_path) as f:
    lines = f.readlines()
    in_inspect_method = False
    for i, line in enumerate(lines):
      # strip inspect methods
      if line.startswith("(defmethod inspect") or (line.startswith("(defmethod") and (i + 1 < len(lines) and "inspect" in lines[i+1])):
        in_inspect_method = True
        continue
      if in_inspect_method and line == "\n":
        in_inspect_method = False
      elif in_inspect_method:
        continue
      # strip comments we dont care about
      if skippable_line(line):
        continue
      # otherwise, add it to the file
      new_lines.append(line)

  # write the damn thing
  os.remove(file_path)
  with open(file_path, "w") as f:
    f.writelines(new_lines)

  print("Copied - {}!".format(file))

if throw_error:
  exit(1)
