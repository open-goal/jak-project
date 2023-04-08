import glob
import re

folders_to_check = ["common", "decompiler", "game", "goalc", "test", "tools", "lsp"]

flagged_instances = []

for folder in folders_to_check:
  files_to_check = glob.glob("./{}/**/*.cpp".format(folder), recursive=True)
  files_to_check += glob.glob("./{}/**/*.h".format(folder), recursive=True)
  for filename in files_to_check:
    # Get the file contents
    with open(filename, "r", encoding="utf-8") as f:
      lines = f.readlines()
      for i, line in enumerate(lines):
        results = re.findall(r"\bassert\(", line)
        if len(results) > 0:
          flagged_instances.append(filename + ":" + str(i + 1))

if len(flagged_instances) == 0:
  exit(0)

print("Found asserts in the following files:")
for file in flagged_instances:
  print(file)
exit(1)
