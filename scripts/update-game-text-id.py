## Given a list of files(comma delimited), decompile it, then place it under the specified placeholder (if it exists)
## if the placeholder doesn't exist, error out
## the placeholder is `;; DECOMP BEGINS`

import os

print("Copying game-text-id enum")
begin_str = ";; GAME-TEXT-ID ENUM BEGINS"
end_str = ";; GAME-TEXT-ID ENUM ENDS"
enum_lines = []
with open('./decompiler/config/all-types.gc') as f:
  lines = f.readlines()
  found_enum = False
  for line in lines:
    if found_enum and end_str in line:
      break
    if found_enum:
      enum_lines.append(line)
    if begin_str in line:
      found_enum = True
new_texth_lines = []
with open('goal_src/engine/ui/text-h.gc') as f:
  lines = f.readlines()
  found_enum = False
  for line in lines:
    if begin_str in line:
      found_enum = True
      new_texth_lines.append(begin_str + "\n")
      new_texth_lines += enum_lines
      new_texth_lines.append(end_str + "\n")
      continue
    if end_str in line:
      found_enum = False
      continue
    if found_enum:
      continue
    new_texth_lines.append(line)
os.remove('goal_src/engine/ui/text-h.gc')
with open('goal_src/engine/ui/text-h.gc', "w") as f:
  f.writelines(new_texth_lines)
print("game-text-id enum updated!")

