import os
import json

def get_file_comment(path):
  full_path = "./goal_src/{}".format(path)
  if os.path.exists(full_path):
    with open(full_path, 'r') as f:
      lines = f.readlines()
      parsing_comment = False
      comment = []
      for idx, line in enumerate(lines):
        if parsing_comment is False and idx >= 100:
          return None
        if "#|@file" in line.lower():
          # We found it, parse the lines
          parsing_comment = True
          continue
        if parsing_comment and "|#" in line:
          parsing_comment = False
          break
        if parsing_comment:
          comment.append(line)
      if len(comment) > 0:
        return "\n".join(comment)
  return None

def append_file_docs(game_name):
  with open('./{}-file-docs.json'.format(game_name), 'r') as f:
    file_docs = json.load(f)

  for file_path, file_info in file_docs.items():
    # Go and find the file in goal_src
    # Check to see if it has a #|@file line (within the first 100 lines)
    # If it does, parse out the description from that block comment and update the json
    comment = get_file_comment(file_path)
    if comment is not None:
      file_info["description"] = comment

  with open('./{}-file-docs.json'.format(game_name), 'w') as f:
    json.dump(file_docs, f)


append_file_docs("jak1")
append_file_docs("jak2")
append_file_docs("jak3")
