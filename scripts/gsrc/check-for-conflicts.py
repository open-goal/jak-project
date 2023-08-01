# Merge tools use specific algorithms or assumptions to detect conflicts
# and not all of them will obviously flag them, even if they use the standard format
#
# So this is to ensure no conflict markers get ignored in goal_src atleast
import os

files_with_unresolved_conflicts = []

for dirpath, subdirs, files in os.walk("./goal_src"):
  for filename in files:
    # Get the file contents
    with open(os.path.join(dirpath, filename), "r") as f:
      lines = f.readlines()
      for line in lines:
        if "<<<<<<<" in line:
          files_with_unresolved_conflicts.append(os.path.join(dirpath, filename))
          break

if len(files_with_unresolved_conflicts) == 0:
  exit(0)

print("There are unresolved conflicts in ./goal_src/")
for file in files_with_unresolved_conflicts:
  print(file)
exit(1)
