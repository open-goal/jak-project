import os

new_file = []
with open("./decompiler/config/all-types.gc") as f:
  symbols_found = []
  lines = f.readlines()

  for line_num, line in enumerate(lines):
    if "flag-assert" in line:
      heap_base_amount = ""
      flag_assert = line.split("#x")[1].strip()
      heap_base_amount = flag_assert[len(flag_assert)-7:len(flag_assert)-4]
      if len(flag_assert) > 1 and heap_base_amount != "000":
        # First let's check to see if the heap-base already exists around this line
        found_heapbase = False
        for i in range(line_num - 4, line_num + 3):
          temp_line = lines[i]
          if "heap-base" in temp_line:
            found_heapbase = True
            break
        if found_heapbase == False:
          if line.startswith(";"):
            new_file.append(";   :heap-base           #x{}\n".format(heap_base_amount.lstrip("0")))
          else:
            new_file.append("  :heap-base #x{}\n".format(heap_base_amount.lstrip("0")))
    new_file.append(line)

os.remove("./decompiler/config/all-types.gc")
with open("./decompiler/config/all-types.gc", "w") as f:
  f.writelines(new_file)
