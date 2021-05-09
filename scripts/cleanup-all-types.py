# all-types.gc can easily drift
# initially it's populated with many types that are unknown (these are commented out)
# but when we manually replace them, it's easy to forget to delete the duplicate commented out variant.
#
# additionally, ideally we'd be organizing them as well but this can come later (dont want to manually move all the type defs!)

# (deftype) or (define-extern)
# if we find a deftype or define-extern that has a define-extern with ;; unknown type on the same line, delete it!

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
