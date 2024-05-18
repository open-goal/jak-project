# import glob
# import json
# import os
# files = glob.glob("./goal_src/jak1/**/*.gc", recursive=True)
# json_data = []
# for file in files:
#     json_data.append({'path': os.path.abspath(file), 'status': 'not-formatted'})

# with open("./scripts/gsrc/format-jak1.json", "w") as f:
#     f.write(json.dumps(json_data, indent=2))

import json
import subprocess
import sys

apply_status = None
if len(sys.argv) > 1:
    apply_status = sys.argv[1]

with open("./scripts/gsrc/format-jak1.json", "r") as f:
    formatting_progress = json.load(f)

# find the next file
index = 0
for index, file in enumerate(formatting_progress):
    if file['status'] == 'not-formatted':
        next_file = file['path']
        break

# format it
print(f"Formatting {next_file}")
subprocess.run(["./out/build/Debug/bin/formatter", "--write", "--file", next_file]) 

# save status
if apply_status is not None and (apply_status == "skip" or apply_status == "next"):
    if apply_status == "next":
        formatting_progress[index]['status'] = 'formatted'
        print(f"Marking {next_file} as formatted")
    else:
        formatting_progress[index]['status'] = 'skipped'
        print(f"Marking {next_file} as skipped")
    with open("./scripts/gsrc/format-jak1.json", "w") as f:
        f.write(json.dumps(formatting_progress, indent=2))

subprocess.run(["git", "diff", "--shortstat", "origin/master"]) 