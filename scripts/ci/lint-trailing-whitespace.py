import glob
import re

import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--fix", action="store_true")
parser.set_defaults(fix=False)
args = parser.parse_args()

folders_to_check = ["goal_src"]

flagged_instances = []

for folder in folders_to_check:
  files_to_check = glob.glob("./{}/**/*.gc".format(folder), recursive=True)
  files_to_check += glob.glob("./{}/**/*.gs".format(folder), recursive=True)
  files_to_check += glob.glob("./{}/**/*.gd".format(folder), recursive=True)
  for filename in files_to_check:
    # Get the file contents
    with open(filename, "r", encoding="utf-8") as f:
      lines = f.readlines()
      lines_with_trailing_whitespace = [line.rstrip('\n').endswith(' ') for line in lines]
      if any(lines_with_trailing_whitespace):
        flagged_instances.append(filename)
        if args.fix:
          cleaned_lines = [line.rstrip() + '\n' if line.rstrip() != line else line for line in lines]
          with open(filename, 'w') as file:
            file.writelines(cleaned_lines)

if len(flagged_instances) == 0:
  exit(0)

if args.fix:
  print("Fixed whitespace in {} files:".format(len(flagged_instances)))
else:
  print("Found empty whitespace in the following files, please trim them:")
  for file in flagged_instances:
    print(file)
  exit(1)
