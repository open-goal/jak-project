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
  if dgos == ["HIDEOUT"] or dgos == ["LHIPOUT"] or dgos == ["LTHRNOUT"] or dgos == ["LTRNTESS"] or dgos == ["LTRNKRKD"]:
    return "levels/haven/hideout"
  elif dgos == ["ORACLE"]:
    return "levels/haven/oracle"
  elif dgos == ["DEMO"] or dgos == ["DEMO", "TITLE"]:
    return "levels/demo"
  elif dgos == ["ONINTENT"] or dgos == ["LTENTOB"] or dgos == ["LTENTOUT"]:
    return "levels/haven/onin_tent"
  elif dgos == ["VI1"]:
    return "levels/jak1/village1"
  elif dgos == ["INTROCST"] or dgos == ["LINTCSTB"]:
    return "levels/intro"
  elif dgos == ["OUTROCST"] or dgos == ["LOUTCSTB"] or dgos == ["LGARCSTA"]:
    return "levels/outro"
  elif dgos == ["ART", "GAME"] or dgos == ["ART"] or dgos == ["GAME"]:
    return "levels/common"
  elif dgos == ["MTX"] or dgos == ["MTN"] or dgos == ["MTX", "MCN"]:
    return "levels/mountain_temple"
  elif dgos == ["FOR"] or dgos == ["LWIDEB", "FOR"]:
    return "levels/haven_forest"
  elif dgos == ["FOB"] or dgos == ["LPROTECT"]:
    return "levels/haven_forest/lifeseed"
  elif dgos == ["HIPHOG"] or dgos == ["LHIPOUT"] or dgos == ["LWHACK"]:
    return "levels/haven/hiphog"
  elif dgos == ["GGA"]:
    return "levels/haven/guncourse"
  elif dgos == ["DMI"]:
    return "levels/drill_platform"
  elif dgos == ["DRI"] or dgos == ["DRI", "DRILLMTN"]:
    return "levels/drill_platform"
  elif dgos == ["PAC"]:
    return "levels/palace/access_cable"
  elif dgos == ["PAE", "PAC"]:
    return "levels/palace"
  elif dgos == ["PALBOSS"]:
    return "levels/palace/baron"
  elif dgos == ["THR"] or dgos == ["LASHTHRN"]:
    return "levels/palace/throne_room"
  elif dgos == ["PAS"]:
    return "levels/palace/shaft"
  elif dgos == ["PAR"]:
    return "levels/palace/roof"
  elif dgos == ["PALOUT"]:
    return "levels/palace/outside"
  elif dgos == ["PAE"]:
    return "levels/palace/explore"
  elif dgos == ["STR"]:
    return "levels/strip_mine"
  elif dgos == ["DG1", "D3A"]:
    return "levels/dig"
  elif dgos == ["D3A"] or dgos == ["D3B"]:
    return "levels/dig/lurker_village"
  elif dgos == ["DG1"]:
    return "levels/dig/drill_equipment"
  elif dgos == ["DRILLMTN"]:
    return "levels/drill_platform/bg_mountain"
  elif dgos == ["DRB"]:
    return "levels/drill_platform/tower"
  elif dgos == ["FEA"] or dgos == ["FEB"]:
    return "levels/fortress/escape"
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
  elif dgos == ["FDB"] or dgos == ["FORDUMPC"] or dgos == ["FDA"] or dgos == ["FORDUMPD"] or dgos == ["FORDUMPC", "FDA"]:
    return "levels/fortress/ammo_dump"
  elif dgos == ["FDA", "FRB"] or dgos == ["FEB", "FDB"]:
    return "levels/fortress"
  elif dgos == ["TOA"] or dgos == ["TOC", "TOA"] or dgos == ["TOC", "TOE", "TOB"]:
    return "levels/mars_tomb"
  elif dgos == ["TOB"]:
    return "levels/mars_tomb/left"
  elif dgos == ["TOC"]:
    return "levels/mars_tomb/right"
  elif dgos == ["TOD"]:
    return "levels/mars_tomb/entrance"
  elif dgos == ["TOE"]:
    return "levels/mars_tomb/left/chase"
  elif dgos == ["VIN"]:
    return "levels/power_station"
  elif dgos == ["LPOWER"]:
    return "levels/power_station/power_switches"
  elif dgos == ["ATO"] or dgos == ["ATE"]:
    return "levels/pumping_station"
  elif dgos == ["ATE", "CTYASHA"]:
    return "levels/pumping_station/ashelin"
  elif dgos == ["TBO"] or dgos == ["TOMBEXT"]:
    return "levels/mars_tomb/baron"
  elif dgos == ["CTYKORA"]:
    return "levels/haven/slums/kor"
  elif dgos == ["LKIDDOGE"]:
    return "levels/haven/kid_escort"
  elif dgos == ["CTYASHA"]:
    return "levels/haven/bazaar/east/ashelin"
  elif dgos == ["CMA", "CMB"]:
    return "levels/haven/bazaar"
  elif dgos == ["CMB"]:
    return "levels/haven/bazaar/east"
  elif dgos == ["CMA"]:
    return "levels/haven/bazaar/west"
  elif dgos == ["KIOSK"] or dgos == ["LMEETBRT"]:
    return "levels/haven/bazaar/west/brutter_kiosk"
  elif dgos == ["LPORTRUN"]:
    return "levels/haven/port/mines"
  elif dgos == ["TITLE"]:
    return "levels/title"
  elif dgos == ["LSACK"]:
    return "level/haven/misc/collection_task"
  elif dgos == ["LPACKAGE"]:
    return "level/haven/misc/delivery"
  elif dgos == ["GARAGE"]:
    return "levels/haven/stadium/garage"
  elif dgos == ["LRACELIT"]:
    return "levels/haven/stadium/races"
  elif dgos == ["STADBLMP"]:
    return "levels/haven/stadium/defend"
  elif dgos == ["SKA"]:
    return "levels/haven/stadium/jetboard"
  elif dgos == ["STC"] or dgos == ["LWIDESTA"] or dgos == ["STD"] or dgos == ["STB"] or dgos == ["STA"] or dgos == ["STD", "STC", "STB"] or dgos == ["STD", "STC"]:
    return "levels/haven/stadium"
  elif dgos == ["MCN"]:
    return "levels/no_mans_canyon"
  elif dgos == ["COA"] or dgos == ["COB"]:
    return "levels/construction_site"
  elif dgos == ["RUI"]:
    return "levels/dead_town"
  elif dgos == ["SAG"]:
    return "levels/dead_town/hut"
  elif dgos == ["SEW"] or dgos == ["SEB", "SWB"] or dgos == ["SEB"] or dgos == ["SWE", "SEW"] :
    return "levels/sewer"
  elif dgos == ["NES"] or dgos == ["NESTT", "NES"] or dgos == ["NESTT"]:
    return "levels/metal_head_nest"
  elif dgos == ["NEB"]:
    return "levels/metal_head_nest/boss_room"
  elif dgos == ["SWE"] or dgos == ["SWB"] or dgos == ["SEB", "SWB", "UNB"]: # interesting this is in underport too... who is 'ruf' and 'hal'?
    return "levels/sewer_escort"
  elif dgos == ["HALFPIPE"]:
    return "levels/test/halfpipe"
  elif dgos == ["CWI"] or dgos == ["PAC", "CWI"] or dgos == ["LWIDEA"] or dgos == ["LWIDEB"] or dgos == ["LWIDEB", "LWIDEA"] or dgos == ["LWIDEC"] or dgos == ["LWIDEC", "LWIDEA"] or dgos == ["LWIDEB", "LWIDEC", "LWIDEA"]:
    return "levels/haven/common"
  elif dgos == ["LBBUSH"]:
    return "levels/haven/side_missions"
  elif dgos == ["CTA"] or dgos == ["CTB"] or dgos == ["CTC"]:
    return "levels/haven/slums"
  elif dgos == ["CPA"]:
    return "levels/haven/palace"
  elif dgos == ["CIA"] or dgos == ["CIB"]:
    return "levels/haven/industrial"
  elif dgos == ["CPO"] or dgos == ["PORTWALL"]:
    return "levels/haven/port"
  elif dgos == ["LPRTRACE"]:
    return "levels/haven/port/race/side_mission"
  elif dgos == ["LERLCHAL"]:
    return "levels/haven/port/race/errol"
  elif dgos == ["LERLCHAL", "LPRTRACE"]:
    return "levels/haven/port/race"
  elif dgos == ["CFB"] or dgos == ["CFA"] or dgos == ["CFB", "CFA"]:
    return "levels/haven/farm"
  elif dgos == ["LBOMBBOT"]:
    return "levels/haven/bombots"
  elif dgos == ["LTESS"] or dgos == ["LERLTESS", "LTESS"]:
    return "characters/tess"
  elif dgos == ["ATE", "UNB"]:
    return "characters/sig"
  elif dgos == ["LGUARD"] or dgos == ["LWIDEB", "DG1", "FRA", "FOB", "LWIDEC", "CAS", "PAE", "FEA", "FDB", "LWIDEA"] or dgos == ["LCGUARD"]:
    return "characters/guards"
  elif dgos == ["FRA", "FOB", "DRILLMTN"]:
    return "characters/hover_guards"
  elif dgos == ["FOR", "DMI", "FRA", "STR", "NEB", "D3A", "UNB"]:
    return "levels/common/enemy/hover"
  elif dgos == ["FOR", "DG1", "FRA", "CAS", "PAE", "FEA", "FDB"]:
    return "levels/common/enemy/guards"
  elif dgos == ["FOR", "ATE"]:
    return "levels/common/enemy/crimson_spyder"
  elif dgos == ["STD", "STC", "LERLCHAL", "STB", "LPRTRACE"] or dgos == ["LRACEDF"] or dgos == ["LRACECF"] or dgos == ["LRACECB"] or dgos == ["LRACEBB"] or dgos == ["LRACEDB"] or dgos == ["LRACEBF"]:
    return "levels/common/races"
  elif dgos == ["LTHRNOUT", "NEB"] or dgos == ["PAS", "TOD"] or dgos == ["FRB", "TOA"] or dgos == ["FRB", "FDB"] or dgos == ["LWIDEC", "CAS"] or dgos == ["TBO", "FRB"] or dgos == ["NESTT", "NES", "LPROTECT"] or dgos == ["SEB", "SWB", "PAE", "PAC"] or dgos == ["ATO", "NESTT", "NES", "LPROTECT"] or dgos == ["ATE", "CTYKORA"] or dgos == ["PAC", "LCITYLOW", "CASCITY"] or dgos == ["FRA", "FRB"] or dgos == ["DRI", "DRB"] or dgos == ["D3B", "RUI"] or dgos == ["PAC", "HALFPIPE"] or dgos == ["CASEXT", "LWIDEC", "LWIDEA"] or dgos == ["LOUTCSTB", "LHIPOUT"] or dgos == ["CTC", "MTN", "CAS", "COA", "CFA", "CPA"] or dgos == ["LOUTCSTB", "VI1"] or dgos == ["CIB", "CAP"] or dgos == ["LKEIRIFT", "LINTCSTB"] or dgos == ["FDA", "FEA", "TOB"] or dgos == ["CTB", "SEB", "SWB", "CFA", "PAS", "CPA"]:
    return "levels/common/entities"
  elif dgos == ["LKEIRIFT"]:
    return "characters/keira_riftrider"
  elif dgos == ["LERLTESS"]:
    return "characters/errol_tess"
  elif dgos == ["LERROL"]:
    return "characters/errol"
  elif dgos == ["LWIDESTA", "ATE", "VIN"]:
    return "levels/common/particls"
  elif dgos == ["LSHUTTLE"]:
    return "characters/underground_fighters"
  elif dgos == ["ATO", "SEB", "SWB", "RUI"]:
    return "characters/fodder"
  elif dgos == ["LHELLDOG"]:
    return "characters/helldog" # TODO - what? lol
  elif dgos == ["LJAKDAX"]:
    return "characters/jak_daxter"
  elif dgos == ["ATE", "SEB", "SWB", "RUI"] or dgos == ["ATE", "SEW", "RUI"]:
    return "characters/amphibian"
  elif dgos == ["LWIDEB", "STR", "DRILLMTN", "STADBLMP", "HALFPIPE", "SEW", "UNB", "RUI", "CTYASHA"]:
    return "characters/metalhead_grunt"
  elif dgos == ["LWIDEB", "STR", "NEB", "D3A", "STADBLMP", "RUI"]:
    return "characters/metalhead_scout"
  elif dgos == ["NEB", "DRILLMTN"]:
    return "characters/metalhead_wasp"
  elif dgos == ["DMI", "MTN"] or dgos == ["DRI", "MTX"]:
    return "characters/metalhead_shielder"
  elif dgos == ["DG1", "D3A", "UNB", "RUI"] or dgos == ["D3A", "RUI"]:
    return "characters/metalhead_slinger"
  elif dgos == ["DG1", "MTN", "ATE", "D3A"] or dgos == ["ATE", "MTX", "D3A"]:
    return "characters/metalhead_monk" # TODO - ?
  elif dgos == ["LSAMERGD"]:
    return "characters/samos_errol_guard"
  elif dgos == ["DG1", "D3A", "TOA"]:
    return "characters/baby_spider"
  elif dgos == ["LERBRNGD"]:
    return "characters/errol_baron_guard"
  elif dgos == ["LSMYSBRT"]:
    return "characters/samos_youngsamos_brutter"
  elif dgos == ["LBRNERMK"]:
    return "characters/baron_errol_metalkor"
  elif dgos == ["LDJAKBRN"]:
    return "characters/jak/intro_clothes"
  elif dgos == ["NEB", "CTYKORA"]:
    return "characters/kid"
  elif dgos == ["LYSKDCD"]:
    return "characters/youngsamos_kid_dog"
  elif dgos == ["LYSAMSAM"]:
    return "characters/youngsamos_and_samos"
  elif dgos == ["LTRNYSAM"]:
    return "characters/torn_youngsamos"
  elif dgos == ["LJKDXASH"]:
    return "characters/jak_daxter_ashelin"
  elif dgos == ["LWIDESTA", "LERLTESS", "LBRNERMK", "INTROCST", "LERROL", "LSAMERGD"]:
    return "characters/high-res/errol"
  elif dgos == ["NEB", "COB"]:
    return "characters/high-res/metalkor"
  elif dgos == ["ATE", "LHIPOUT", "LTESS", "UND", "LGUARD"]:
    return "characters/high-res/sig"
  elif dgos == ["LTHRNOUT", "LTRNYSAM", "LTRNTESS", "SAG", "LTRNKRKD"]:
    return "characters/high-res/torn"
  elif dgos == ["ORACLE", "NEB", "VI1"]:
    return "characters/high-res/darkjak"
  elif dgos == ["LERLTESS", "LGARCSTA", "CAB", "LTESS", "LGUARD"]:
    return "characters/high-res/krew"
  elif dgos == ["LERLTESS", "LGARCSTA", "LPRSNCST", "LTRNTESS", "OUTROCST", "LWHACK", "LTESS"]:
    return "characters/high-res/tess"
  elif dgos == ["KIOSK", "LOUTCSTB", "STADBLMP", "LSMYSBRT"]:
    return "characters/high-res/brutter"
  elif dgos == ["OUTROCST", "ONINTENT"]:
    return "characters/high-res/onin"
  elif dgos == ["LERBRNGD", "LSAMERGD"]:
    return "characters/low-res/guard"
  elif dgos == ["LGARCSTA", "LPRSNCST", "OUTROCST", "LERBRNGD", "STADBLMP", "LSAMERGD", "LINTCSTB", "LSMYSBRT", "LYSAMSAM"]:
    return "characters/high-res/samos"
  elif dgos == ["LTRNYSAM", "LOUTCSTB", "LPRSNCST", "FOB", "STADBLMP", "LYSKDCD", "TOD", "LTENTOUT", "LSMYSBRT", "LYSAMSAM"]:
    return "characters/high-res/youngsamos"
  elif dgos == ["LOUTCSTB", "NEB", "LTRNKRKD", "LYSKDCD", "TOD", "VIN", "CTYKORA"]:
    return "characters/high-res/kid"
  elif dgos == ["LHIPOUT", "LYSKDCD", "TOD", "VIN"]:
    return "characters/high-res/crocadog"
  elif dgos == ["LHIPOUT", "ONINTENT"]:
    return "characters/high-res/pecker"
  elif dgos == ["LTENTOB", "LTRNKRKD", "TOD", "VIN", "CTYKORA"]:
    return "characters/high-res/kor"
  elif dgos == ["OUTROCST", "LKEIRIFT", "LINTCSTB", "GARAGE"]:
    return "characters/high-res/keira"
  elif dgos == ["LWIDESTA", "LBRNERMK", "LERBRNGD", "COA", "TOMBEXT", "PALBOSS", "LDJAKBRN"]:
    return "characters/high-res/baron"
  elif dgos == ["LWIDESTA", "LERBRNGD", "COA", "LCGUARD", "FORDUMPD", "LSAMERGD", "LASHGRD", "LGUARD", "CTYKORA", "CTYASHA"]:
    return "characters/high-res/guard"
  elif dgos == ["CASEXT", "HIDEOUT", "KIOSK", "ORACLE", "INTROCST", "ATE", "FOB", "LJAKDAX", "FORDUMPC", "LTRNTESS", "THR", "PRI", "SAG", "NEB", "DEMO", "MTX", "LJKDXASH", "COA", "TOMBEXT", "PALBOSS", "TITLE", "ONINTENT", "STA", "TOA", "UND", "LYSKDCD", "D3B", "SWE", "GGA", "TOD", "MCN", "SEW", "VIN", "CAP", "CTYKORA", "CTYASHA", "HIPHOG", "VI1"]:
    return "characters/high-res/daxter"
  elif dgos == ["CASEXT", "HIDEOUT", "KIOSK", "ATE", "LPRSNCST", "FOB", "LJAKDAX", "FORDUMPC", "LTRNTESS", "THR", "SAG", "MTX", "LJKDXASH", "COA", "PALBOSS", "ONINTENT", "STA", "TOA", "UND", "LYSKDCD", "D3B", "SWE", "GGA", "TOD", "MCN", "SEW", "VIN", "CAP", "CTYKORA", "CTYASHA", "HIPHOG"]:
    return "characters/high-res/jak"
  elif dgos == ["CASEXT", "LTHRNOUT", "LGARCSTA", "ATE", "LASHTHRN", "LJKDXASH", "LASHGRD", "CTYASHA"]:
    return "characters/high-res/ashelin"
  elif dgos == ["UND"] or dgos == ["UNB"]:
    return "levels/underport"
  elif dgos == ["CGB"] or dgos == ["CGA"] or dgos == ["CGC"]:
    return "levels/haven/generic"
  elif dgos == ["LCITYLOW"] or dgos == ["CASCITY"]:
    return "levels/haven/low-quality"
  elif dgos == ["GAME", "COMMON"]:
    return "levels/common"
  elif dgos == ["LASHGRD"] or dgos == ["SEB", "SWB", "LKIDDOGE", "UNB", "CTYKORA"] or dgos == ["LPRSNCST"] or dgos == ["ATE", "SEB", "SWB", "LKIDDOGE", "UNB", "CTYKORA", "CTYASHA"]:
    return "levels/undefined"
  elif dgos == ["DRILLMTN", "UNB", "RUI"] or dgos == ["UNB", "RUI"] or dgos == ["DMI", "RUI"]:
    return "levels/common/mech_suit"

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

with open('../../goal_src/jak2/build/all_objs.json', 'w') as json_file:
  # Calculate these to make the file as compact as possible
  longest_name = 0
  longest_name_in_dgo = 0
  for jak2_file in jak2_files:
    if len(jak2_file[0]) > longest_name:
      longest_name = len(jak2_file[0])
    if len(jak2_file[1]) > longest_name_in_dgo:
      longest_name_in_dgo = len(jak2_file[1])
  # Actually write things out
  json_file.write("[\n")
  i = 0
  for jak2_file in jak2_files:
    name = '{: <{}}'.format("\"{}\",".format(jak2_file[0]), longest_name + 2)
    name_in_dgo = '{: <{}}'.format("\"{}\",".format(jak2_file[1]), longest_name_in_dgo + 2)
    dgo_set = "["
    for dgo in jak2_file[3]:
      dgo_set += "\"{}\", ".format(dgo)
    dgo_set = dgo_set.removesuffix(", ")
    dgo_set += "]"
    if i == (len(jak2_files) - 1):
      json_file.write("[{}{}{},{}, \"{}\"]\n".format(name, name_in_dgo, jak2_file[2], dgo_set, jak2_file[4]))
    else:
      json_file.write("[{}{}{},{}, \"{}\"],\n".format(name, name_in_dgo, jak2_file[2], dgo_set, jak2_file[4]))
    i = i + 1
  json_file.write("]\n")

print("Mapped: {} and Left: {}".format(num_replicated, num_left))

limit = 0
for dgo_set in dict(sorted(remaining_dgos.items(), reverse=True, key=lambda item: item[1])):
  dgo_set_nice = ""
  for dgo in dgo_set.split(","):
    dgo_set_nice += "\"{}\", ".format(dgo)
  dgo_set_nice = dgo_set_nice.removesuffix(", ")
  print("or dgos == [{}]: {}".format(dgo_set_nice, remaining_dgos[dgo_set]))
  if limit > 100:
    break
  limit = limit + 1
print(len(remaining_dgos))
