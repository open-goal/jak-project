# Reduces work and tries to maintain consistency by placing objects in the same folder
# as they were in the previous game.
jak1_files = None
jak2_files = None

import json

with open('../../goal_src/jak1/build/all_objs.json', 'r') as f:
  jak1_files = json.load(f)
with open('../../goal_src/jak2/build/all_objs.json', 'r') as f:
  jak2_files = json.load(f)

num_replicated = 0
num_left = 0

engine_files = {
  "profile": "util",
  "texture-anim": "anim",
  "capture": "util",
  "text-id": "ui",
  "camera-defs": "camera",
  "trail": "gfx",
  "minimap": "ui",
  "bigmap": "ui",
  "blit-displays": "gfx",
  "region": "level",
  "traffic": "ai",
  "gui": "ui",
  "ambient": "ambient",
  "speech": "sound",
  "foreground": "gfx",
  "lightning": "gfx",
  "penetrate": "gfx",
  "script": "util",
  "scene": "scene",
  "process-focusable": "game/focus",
  "focus": "game/focus",
  "collide-hash": "collide",
  "chain-physics": "physics",
  "projectile": "game",
  "find-nearest": "util",
  "simple-sprite": "gfx/sprite",
  "nav-mesh": "nav",
  "nav-control": "nav",
  "spatial-hash": "data",
  "actor-hash": "data",
  "joint-mod": "anim",
  "wind-work": "gfx",
  "sprite-glow": "gfx/sprite",
  "history": "data",
  "emerc-vu1": "gfx/emerc",
  "emerc": "gfx/emerc",
  "debug-foreground": "debug",
  "warp": "game",
  "texture-anim-funcs": "anim",
  "texture-anim-tables": "anim",
  "font-data": "data",
  "etie-vu1": "gfx/etie",
  "etie-near-vu1": "gfx/etie-near-vu1",
  "game-task": "game/task",
  "mood-tables2": "ambient",
  "mood-funcs": "ambient",
  "mood-funcs2": "ambient",
  "sky-data": "data",
  "load-state": "load",
  "water-flow": "gfx/water",
  "fma-sphere": "sound", # TODO - ?
  "carry": "game",
  "pilot": "game",
  "board": "game",
  "darkjak": "game",
  "darkjak": "game",
  "gun-part": "sparticle",
  "debug-part": "debug",
  "task-arrow": "game/task",
  "target-anim": "target",
  "target-swim": "target",
  "target-carry": "target",
  "target-darkjak": "target",
  "target-gun": "target",
  "gun-util": "util",
  "gun-blue-shot": "sparticle",
  "gun-yellow-shot": "sparticle",
  "gun-red-shot": "sparticle",
  "gun-dark-shot": "sparticle",
  "gun-states": "game",
  "board-util": "util",
  "target-board": "target",
  "board-part": "sparticle",
  "board-states": "game",
  "mech": "game",
  "process-taskable-h": "game/task",
  "gun-h": "game",
  "collide-debug": "collide",
  "bigmap-data": "data",
  "editable": "debug/nav", # TODO - hmmm, related to the nav?
  "editable-player": "debug/nav", # TODO - hmmm, related to the nav?
  "mysql-nav-graph": "debug/nav",
  "nav-graph-editor": "debug/nav",
  "sampler": "sound"
}

# i can be smarter than this...i swear....refactor eventually!
def level_name(file_meta):
  dgos = file_meta[3]
  # Handle files unique to one level
  if dgos == ["HIDEOUT"] or dgos == ["LHIPOUT"] or dgos == ["LTHRNOUT"]:
    return "levels/haven/hideout"
  elif dgos == ["GAME", "COMMON"]:
    return "levels/common"
  elif dgos == ["ORACLE"]:
    return "levels/haven/oracle"
  elif dgos == ["ONINTENT"]:
    return "levels/haven/onin_tent"
  elif dgos == ["VI1"]:
    return "levels/jak1/village1"
  elif dgos == ["INTROCST"]:
    return "levels/intro"
  elif dgos == ["OUTROCST"] or dgos == ["LOUTCSTB"]:
    return "levels/outro"
  elif dgos == ["ART", "GAME"]:
    return "levels/common"
  elif dgos == ["MTX"] or dgos == ["MTN"]:
    return "levels/mountain_temple"
  elif dgos == ["FOR"]:
    return "levels/haven_forest"
  elif dgos == ["FOB"]:
    return "levels/haven_forest/lifeseed"
  elif dgos == ["HIPHOG"] or dgos == ["LHIPOUT"] or dgos == ["LWHACK"]:
    return "levels/haven/hiphog"
  elif dgos == ["GGA"]:
    return "levels/haven/guncourse"
  elif dgos == ["DMI"]:
    return "levels/drill_platform/eggs" # TODO - not sure
  elif dgos == ["DRI"]:
    return "levels/drill_platform" # TODO - not sure
  elif dgos == ["PAC"]:
    return "levels/palace/access_cable"
  elif dgos == ["PALBOSS"]:
    return "levels/palace/baron"
  elif dgos == ["THR"]:
    return "levels/palace/throne_room"
  elif dgos == ["PAS"]:
    return "levels/palace/shaft"
  elif dgos == ["PAR"]:
    return "levels/palace/roof"
  elif dgos == ["PAE"]:
    return "levels/palace/lobby" # TODO - i think wrong
  elif dgos == ["STR"]:
    return "levels/strip_mine"
  elif dgos == ["D3A"]:
    return "levels/dig/lurker_village"
  elif dgos == ["DG1"]:
    return "levels/dig/drill_equipment"
  elif dgos == ["DRILLMTN"]:
    return "levels/drill_platform/destroy" # TODO - ?
  elif dgos == ["DRB"]:
    return "levels/drill_platform/destroy" # TODO - ?
  elif dgos == ["FEA"] or dgos == ["FEB"]:
    return "levels/fortress/escape" # TODO - ?
  elif dgos == ["FRA"] or dgos == ["FRB"]:
    return "levels/fortress/rescue"
  elif dgos == ["PRI"]:
    return "levels/fortress/prison_room"
  elif dgos == ["CAS"]:
    return "levels/landing_pad"
  elif dgos == ["CAP"] or dgos == ["CASEXT"]:
    return "levels/weapons_factory"
  elif dgos == ["CAB"]:
    return "levels/weapons_factory/krew"
  elif dgos == ["FDB"] or dgos == ["FORDUMPC"] or dgos == ["FDA"]:
    return "levels/fortress/ammo_dump"
  elif dgos == ["TOA"]:
    return "levels/mars_tomb"
  elif dgos == ["TOB"]:
    return "levels/mars_tomb/left_tomb"
  elif dgos == ["TOC"]:
    return "levels/mars_tomb/right_tomb"
  elif dgos == ["TOD"]:
    return "levels/mars_tomb/entrance"
  elif dgos == ["TOE"]:
    return "levels/mars_tomb/left_tomb/chase"
  elif dgos == ["VIN"]:
    return "levels/power_station"
  elif dgos == ["ATO"]:
    return "levels/pumping_station"
  elif dgos == ["ATE"]:
    return "levels/pumping_station/escort"
  elif dgos == ["TBO"]:
    return "levels/mars_tomb/baron"
  elif dgos == ["CTYKORA"]:
    return "levels/haven/slums/kor"
  elif dgos == ["LKIDDOGE"]:
    return "levels/haven/kid_escort"
  elif dgos == ["CTYASHA"]:
    return "levels/haven/east_bazaar/ashelin"
  elif dgos == ["CMA"]:
    return "levels/haven/east_bazaar" # TODO - which bazaar is A and B?
  elif dgos == ["CMB"]:
    return "levels/haven/west_bazaar" # TODO - which bazaar is A and B?
  elif dgos == ["KIOSK"]:
    return "levels/haven/west_bazaar/brutter_kiosk"
  elif dgos == ["LPORTRUN"]:
    return "levels/haven/port/mines"
  elif dgos == ["LSACK"]:
    return "level/haven/misc/backpack"
  elif dgos == ["GARAGE"]:
    return "levels/haven/stadium/garage"
  elif dgos == ["STADBLMP"]:
    return "levels/haven/stadium/defend"
  elif dgos == ["SKA"]:
    return "levels/haven/stadium/jetboard"
  elif dgos == ["STC"]:
    return "levels/haven/stadium/c" # which race is this?
  elif dgos == ["STD"]:
    return "levels/haven/stadium/d" # which race is this?
  elif dgos == ["STB"]:
    return "levels/haven/stadium/b" # which race is this?
  elif dgos == ["STA"]:
    return "levels/haven/stadium/a" # which race is this?
  elif dgos == ["MCN"]:
    return "levels/no_mans_canyon"
  elif dgos == ["COA"] or dgos == ["COB"]:
    return "levels/construction_site"
  elif dgos == ["RUI"]:
    return "levels/dead_town"
  elif dgos == ["SAG"]:
    return "levels/dead_town/hut"
  elif dgos == ["SEW"]:
    return "levels/sewer"
  elif dgos == ["SEB"]:
    return "levels/sewer" # TODO - what is sewer b?
  elif dgos == ["NES"] or dgos == ["NESTT", "NES"]:
    return "levels/metal_head_nest/part-a"
  elif dgos == ["NEB"]:
    return "levels/metal_head_nest/part-b"
  elif dgos == ["SWE"]:
    return "levels/sewer_escort"
  elif dgos == ["HALFPIPE"]:
    return "levels/test/halfpipe"
  elif dgos == ["CWI"]:
    return "levels/haven/common"
  elif dgos == ["CTA"]:
    return "levels/haven/slums/a" # TODO - where?
  elif dgos == ["CTB"]:
    return "levels/haven/slums/b" # TODO - where?
  elif dgos == ["CTC"]:
    return "levels/haven/slums/water"
  elif dgos == ["CPA"]:
    return "levels/haven/palace"
  elif dgos == ["CIA"]:
    return "levels/haven/industrial/a" # TODO - ?
  elif dgos == ["CIB"]:
    return "levels/haven/industrial/b" # TODO - ?
  elif dgos == ["CPO"]:
    return "levels/haven/port"
  elif dgos == ["LPRTRACE"]:
    return "levels/haven/port/race"
  elif dgos == ["CFB"] or dgos == ["CFA"]:
    return "levels/haven/farm"
  elif dgos == ["LTESS"]:
    return "characters/tess"
  elif dgos == ["LGUARD"]:
    return "characters/guards"
  elif dgos == ["LDJAKBRN"]:
    return "characters/jak/intro_clothes"
  elif dgos == ["LYSKDCD"]:
    return "characters/young_samos_kid_dog"
  elif dgos == ["UND"] or dgos == ["UNB"]:
    return "levels/underport"
  elif dgos == ["CGB"]:
    return "levels/haven/genb" # find better names for these
  elif dgos == ["CGA"]:
    return "levels/haven/gena" # find better names for these, generic?

remaining_dgos = {}

for jak2_file in jak2_files:
  if jak2_file[3] == ["NO-XGO"]:
    num_replicated = num_replicated + 1
    continue
  # port over manually specified engine files
  if jak2_file[0] in engine_files or jak2_file[0].removesuffix("-h") in engine_files:
    num_replicated = num_replicated + 1
    if jak2_file[0] in engine_files:
      jak2_file[4] = str.format("engine/{}", engine_files[jak2_file[0]])
    else:
      jak2_file[4] = str.format("engine/{}", engine_files[jak2_file[0].removesuffix("-h")])
    continue
  # attempt to find the object with the same name in jak1
  jak1_path = None
  for jak1_file in jak1_files:
    if jak1_file[0] == jak2_file[0]:
      jak1_path = jak1_file[4]
      break
  if jak1_path is not None:
    jak2_file[4] = jak1_path
    num_replicated = num_replicated + 1
  elif level_name(jak2_file):
    jak2_file[4] = level_name(jak2_file)
    num_replicated = num_replicated + 1
  else:
    num_left = num_left + 1
    if jak2_file[3] == ["GAME", "COMMON"]:
      print(jak2_file[0])
    if ",".join(jak2_file[3]) in remaining_dgos:
      remaining_dgos[",".join(jak2_file[3])] = remaining_dgos[",".join(jak2_file[3])] + 1
    else:
      remaining_dgos[",".join(jak2_file[3])] = 1


# TODO - print nicer
with open('../../goal_src/jak2/build/all_objs.json', 'w') as json_file:
  json.dump(jak2_files, json_file)

print("Mapped: {} and Left: {}".format(num_replicated, num_left))

limit = 0
for dgo_set in dict(sorted(remaining_dgos.items(), reverse=True, key=lambda item: item[1])):
  print("{}: {}".format(dgo_set, remaining_dgos[dgo_set]))
  if limit > 20:
    break
  limit = limit + 1
print(len(remaining_dgos))
