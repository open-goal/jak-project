# Simple script that cleans up and de-duplicates the subtitle files
# Most of the duplication happens in the metadata files, as most cutscenes share the same timing
# for some languages though (ie. en-US and en-GB) the actual translation files are heavily duplicated.

# Also this is a nice place to cleanup anything that C++ did not

# This file can also die once any active (but not commited) translation efforts actually committed

import os
import json
import shutil

def clean_lines(lines):
  new_lines = []
  for line in lines:
    new_lines.append(line.replace('\\"', '"'))
  return new_lines

# For the purposes of this script, it's assumed the base files are en-US
english_meta = None
english_lines = None

with open("./subtitle_meta_en-US.json", "r", encoding="utf-8") as f:
  english_meta = json.load(f)

with open("./subtitle_lines_en-US.json", "r", encoding="utf-8") as f:
  english_lines = json.load(f)
  for name, info in english_lines["cutscenes"].items():
    english_lines["cutscenes"][name] = clean_lines(info)
  for name, info in english_lines["hints"].items():
    english_lines["hints"][name] = clean_lines(info)
with open("./subtitle_lines_en-US.json", "w", encoding="utf-8") as line_file:
  json.dump(english_lines, line_file, indent=2, ensure_ascii=False)

# I'm lazy, uncomment this to make the other language base files
locales = ["jp-JP", "hu-HU", "da-DK", "fi-FI", "nl-NL", "no-NO", "pt-PT", "sv-SE", "ca-ES", "is-IS"]
for locale in locales:
  # duplicate the english files with the locale
  shutil.copy("./subtitle_meta_en-US.json", "./subtitle_meta_" + locale + ".json")

# Now, let's iterate through all the other files and remove any near-top level duplication.
# this is a very similar strategy to the cast file cleanup effort.

for f in os.listdir("./"):
  if not f.endswith(".json") or f.endswith("en-US.json"):
    continue
  # Check if it's a meta file, or a line file
  if "meta" in f:
    new_meta = {
      "cutscenes": {},
      "hints": {}
    }
    with open(f, "r", encoding="utf-8") as meta_file:
      print(f)
      meta = json.load(meta_file)
      # Iterate through every thing, if its the same as the base file, it can be removed from this file
      # otherwise, leave it!
      for name, info in meta["cutscenes"].items():
        if name not in english_meta["cutscenes"]:
          print(f"{name} not in english_meta['cutscenes']")
          new_meta["cutscenes"][name] = info
          continue
        # easy equality check since order matters and the files are machine generated, this should be good enough
        if json.dumps(info) != json.dumps(english_meta["cutscenes"][name]):
          new_meta["cutscenes"][name] = info
      for name, info in meta["hints"].items():
        if name not in english_meta["hints"]:
          print(f"{name} not in english_meta['hints']")
          new_meta["hints"][name] = info
          continue
        # easy equality check since order matters and the files are machine generated, this should be good enough
        if json.dumps(info) != json.dumps(english_meta["hints"][name]):
          new_meta["hints"][name] = info
    # write out the new file
    with open(f, "w", encoding="utf-8") as meta_file:
      json.dump(new_meta, meta_file, indent=2, ensure_ascii=False)
  if "lines" in f:
    new_lines = {
      "cutscenes": {},
      "hints": {},
      "speakers": {}
    }
    # now lines files
    with open(f, "r", encoding="utf-8") as line_file:
      print(f)
      lines = json.load(line_file)
      new_lines["speakers"] = lines["speakers"]
      # Iterate through every thing, if its the same as the base file, it can be removed from this file
      # otherwise, leave it!
      for name, info in lines["cutscenes"].items():
        if name not in english_lines["cutscenes"]:
          print(f"{name} not in english_lines['cutscenes']")
          new_lines["cutscenes"][name] = clean_lines(info)
          continue
        # easy equality check since order matters and the files are machine generated, this should be good enough
        if json.dumps(info) != json.dumps(english_lines["cutscenes"][name]):
          new_lines["cutscenes"][name] = clean_lines(info)
      for name, info in lines["hints"].items():
        if name not in english_lines["hints"]:
          print(f"{name} not in english_lines['hints']")
          new_lines["hints"][name] = clean_lines(info)
          continue
        # easy equality check since order matters and the files are machine generated, this should be good enough
        if json.dumps(info) != json.dumps(english_lines["hints"][name]):
          new_lines["hints"][name] = clean_lines(info)
    # write out the new file
    with open(f, "w", encoding="utf-8") as line_file:
      json.dump(new_lines, line_file, indent=2, ensure_ascii=False)

# Lines get copied after because we actually don't want duplication to be removed (it needs to be translated!)
for locale in locales:
  shutil.copy("./subtitle_lines_en-US.json", "./subtitle_lines_" + locale + ".json")

# Special case for portuguese brazilian
# it was done based off the spanish timings, but there is no portuguese audio
# so manually find the cutscenes that don't match so they can be adjusted...manually...
with open("./subtitle_lines_pt-BR.json", "r", encoding="utf-8") as f:
  port_lines = json.load(f)

for cutscene_name, cutscene_lines in port_lines["cutscenes"].items():
  if len(cutscene_lines) != len(english_lines["cutscenes"][cutscene_name]):
    print(cutscene_name)
for hint_name, hint_lines in port_lines["hints"].items():
  if len(hint_lines) != len(english_lines["hints"][hint_name]):
    print(hint_name)
