import os
import glob

def get_goal_files(root_dir, ext = "*.gc"):
    """Get all GOAL source files under root_dir."""
    return [goal_file for file in os.walk(root_dir) for goal_file in glob.glob(os.path.join(file[0], ext))]

all_files = get_goal_files("./decompiler_out/jak2/import")
result = ""
for file in all_files:
	with open(file) as f:
		for line in f:
			if line.startswith("(def"):
				result += line
print(result)