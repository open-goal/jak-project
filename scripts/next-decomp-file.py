import sys
from jak1_file_list import file_list

skip_count = 0
if len(sys.argv) >= 2:
  skip_count = int(sys.argv[1])

print(skip_count)

new_file_contents = []

for file in file_list:
  with open("goal_src/" + file[4] + "/" + file[0] + ".gc") as f:
    lines = f.readlines()
    # print("goal_src/" + file[4] + "/" + file[0] + ".gc" + "  "  + str(len(lines)))
    if skip_count <= 0 and len(lines) <= 7:
      print("Next file to decompile is - " + file[0])
      print("Target is - " + "goal_src/" + file[4] + "/" + file[0] + ".gc")
      print("Uses the following CGO / DGO - " + str(file[3]))
      # TODO, update the CGO/DGO
      with open("decompiler\config\jak1_ntsc_black_label.jsonc", "r") as config_file:
        cfg_lines = config_file.readlines()
        for line in cfg_lines:
          if "allowed_objects" in line:
            line = "  \"allowed_objects\": [\"" + file[0] + "\"],\n"
          new_file_contents.append(line)
      if len(new_file_contents) > 0:
        with open("decompiler\config\jak1_ntsc_black_label.jsonc", "w") as f:
          f.writelines(new_file_contents)
          print("Allowed objects list updated")
      break
    elif len(lines) <= 7:
      skip_count = skip_count - 1


