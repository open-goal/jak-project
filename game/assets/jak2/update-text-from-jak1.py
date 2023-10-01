# Jak 2 shares many of the same custom menu strings as jak 1 obviously
# this is to copy them over so they are already translated.

# TODO - make it enumerate all languages

import json


with open("temp-text-id-mapping.json", "r", encoding="utf-8") as f:
  mapping = json.load(f)

with open("./text/game_custom_text_en-US.json", "r", encoding="utf-8") as f:
  jak2_file = json.load(f)

with open("../jak1/text/game_custom_text_en-US.json", "r", encoding="utf-8") as f:
  jak1_file = json.load(f)

for jak2_id, jak1_id in mapping.items():
  jak2_file[jak2_id] = jak1_file[jak1_id].title()

with open("./text/game_custom_text_en-US.json", "w", encoding="utf-8") as f:
  json.dump(jak2_file, f, indent=2)
