import json
import os
import glob

def process_game_text(game_name):
  # Load context file for game
  with open('./localization/{}/gametext-context.json'.format(game_name), 'r', encoding= 'utf-8') as f:
    context = json.load(f)

  # Update all files
  files = glob.glob("./localization/{}/text/text_*.json".format(game_name))
  for file in files:
    print("Updating - '{}'".format(file))
    with open(file, 'r', encoding= 'utf-8') as f:
      current_file = json.load(f)
    for text_id, data in current_file.items():
      if "crowdinContext" not in data and text_id in context:
        data["crowdinContext"] = context[text_id]
    with open(file, 'w', encoding= 'utf-8') as json_file:
      json.dump(current_file, json_file, indent=2)

process_game_text("jak1")
