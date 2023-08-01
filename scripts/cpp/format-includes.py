# Visual Studio is dumb and doesn't let you customize the automatic include formats
# so i'll do it myself.

import glob
import re

folders_to_check = ["common", "decompiler", "game", "goalc", "test", "tools", "lsp"]

for folder in folders_to_check:
  files_to_check = glob.glob("./{}/**/*.cpp".format(folder), recursive=True)
  files_to_check += glob.glob("./{}/**/*.h".format(folder), recursive=True)
  for filename in files_to_check:
    # Get the file contents
    with open(filename, "r", encoding="utf-8") as f:
      lines = f.readlines()
      new_lines = []
      need_to_write = False
      for i, line in enumerate(lines):
        include_match = re.search(r"#include <(.*)>", line)
        if include_match:
          include = include_match.groups()[0]
          if include.startswith("sys/") or include.startswith("netinet/") or include.startswith("arpa/"):
            new_lines.append(line)
          elif "/" in include:
            new_lines.append(line.replace("<", "\"").replace(">", "\""))
            need_to_write = True
          else:
            new_lines.append(line)
        else:
          new_lines.append(line)
      if need_to_write:
        print("Fixing includes in {}".format(filename))
        with open(filename, "w", encoding="utf-8") as f:
          f.writelines(new_lines)
