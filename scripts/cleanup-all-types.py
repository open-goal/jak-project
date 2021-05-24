# This does a (currently) 3 pass cleanup on all-types
# 1. Cleanup any symbol definitions that are redundant
# 2. Reorder symbol definitions based on file build order
# 3. Check for any necessary / missing forward declarations

import os

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
