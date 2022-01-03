import glob

src_files = glob.glob("./goal_src/**/*.g[cs]", recursive=True)
data_files = glob.glob("./goal_src/**/*.gd", recursive=True)

# Find how many of each have been started

src_files_started = 0
src_files_finished = 0
data_files_started = 0

for f in src_files:
  with open(f, "r") as temp_file:
    lines = temp_file.readlines()
    line_count = len(lines)
    if line_count > 7:
      # Check to see if there are any TODOs
      if any("TODO" in string for string in lines):
        src_files_finished = src_files_finished + 1
      else:
        src_files_started = src_files_started + 1

for f in data_files:
  with open(f, "r") as temp_file:
    line_count = len(temp_file.readlines())
    if line_count > 7:
      data_files_started = data_files_started + 1

import json
with open('./docs/gh-pages-proj/src/config/progress.json', 'r+', encoding='utf-8') as f:
  data = {
    'jak1': {
      'fileProgress': {
        'src_files_total': len(src_files),
        'src_files_finished': src_files_finished,
        'src_files_started': src_files_started,
        'data_files_total': len(data_files),
        'data_files_started': data_files_started
      }
    }
  }
  f.seek(0)
  json.dump(data, f, ensure_ascii=False, indent=2)
  f.truncate()
