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
  "texture-anim": "gfx/texture",
  "capture": "util",
  "text-id": "ui",
  "camera-defs": "camera",
  "minimap": "ui",
  "bigmap": "ui",
  "blit-displays": "gfx",
  "region": "level",
  "traffic": "ai",
  "gui": "ui",
  "ambient": "ambient",
  "speech": "sound",
  "lightning": "gfx",
  "penetrate": "game",
  "script": "util",
  "scene": "scene",
  "process-focusable": "process-drawable",
  "focus": "process-drawable",
  "collide-hash": "spatial-hash",
  "chain-physics": "physics",
  "projectile": "game",
  "find-nearest": "collide",
  "simple-sprite": "gfx/sprite",
  "nav-mesh": "nav",
  "nav-control": "nav",
  "spatial-hash": "spatial-hash",
  "actor-hash": "spatial-hash",
  "joint-mod": "anim",
  "wind-work": "gfx",
  "sprite-glow": "gfx/sprite",
  "history": "debug",
  "emerc-vu1": "gfx/merc",
  "emerc": "gfx/merc",
  "warp": "gfx",
  "texture-anim-funcs": "gfx/texture",
  "texture-anim-tables": "gfx/texture",
  "font-data": "data",
  "etie-vu1": "gfx/tie",
  "etie-near-vu1": "gfx/tie",
  "game-task": "game/task",
  "mood-tables": "gfx/mood",
  "mood-tables2": "gfx/mood",
  "mood-funcs": "gfx/mood",
  "mood-funcs2": "gfx/mood",
  "mood": "gfx/mood",
  "sky-data": "gfx/sky",
  "load-state": "load",
  "fma-sphere": "anim",
  "carry": "game",
  "pilot": "game",
  "board": "target/board",
  "darkjak": "target",
  "collide-reaction-target": "target",
  "gun-part": "target/gun",
  "debug-part": "debug",
  "task-arrow": "game/task",
  "target-anim": "target",
  "target-swim": "target",
  "target-carry": "target",
  "target-darkjak": "target",
  "target-gun": "target",
  "gun-util": "target/gun",
  "gun-blue-shot": "target/gun",
  "gun-yellow-shot": "target/gun",
  "gun-red-shot": "target/gun",
  "gun-dark-shot": "target/gun",
  "gun-states": "target/gun",
  "board-util": "target/board",
  "target-board": "target/board",
  "board-part": "target/board",
  "board-states": "target/board",
  "mech": "game",
  "simple-nav-sphere": "process-drawable",
  "process-taskable": "process-drawable",
  "gun": "target/gun",
  "collide-debug": "collide",
  "bigmap-data": "ui",
  "editable": "debug",
  "editable-player": "debug",
  "mysql-nav-graph": "debug/nav",
  "nav-graph-editor": "debug/nav",
  "sampler": "debug",
  "weather-part": "gfx/mood",
  "time-of-day": "gfx/mood",
  "path": "geometry",
  "progress": "ui/progress",
  "sparticle": "gfx/sprite/particles",
  "sparticle-launcher": "gfx/sprite/particles",
  "video": "gfx/hw",
  "target-tube": "target",
  "texture-upload": "gfx/texture",
  "texture-finish": "gfx/texture",
  "vu1-user-h": "gfx",
  "math-camera": "gfx",
  "decomp": "load",
  "texture": "gfx/texture",
  "shadow-cpu": "gfx/foreground",
  "shadow-vu1": "gfx/foreground",
  "bones": "gfx/foreground",
  "eye": "gfx/foreground",
  "foreground": "gfx/foreground",
  "debug-foreground": "gfx/foreground",
  "ripple": "gfx/foreground",
  "res": "entity",
  "pat": "collide",
  "wind": "gfx/background",
  "wind-work": "gfx/background",
  "prototype": "gfx/background",
  "process-drawable": "process-drawable",
  "simple-focus": "process-drawable",
  "bsp": "level",
  "idle-control": "game",
  "joint-exploder": "anim",
  "background": "gfx/background",
  "subdivide": "gfx/background",
  "water": "common_objs",
  "enemy": "ai",
  "water-flow": "common_objs",
  "generic-obs": "common_objs",
  "voicebox": "common_objs",
  "projectile": "common_objs",
  "water-anim": "common_objs",
  "blocking-plane": "common_objs",
  "dark-eco-pool": "common_objs",
  "collectables-part": "common_objs",
  "crates": "common_objs",
  "collectables": "common_objs",
  "powerups": "common_objs",
  "los-control": "collide",
  "carry": "target/mech_suit",
  "mech": "target/mech_suit",
  "nav-enemy": "nav",
  "base-plat": "common_objs",
  "plat": "common_objs",
  "basebutton": "common_objs",
  "bouncer": "common_objs",
  "conveyor": "common_objs",
  "elevator": "common_objs",
  "rigid-body-plat": "common_objs",
  "rigid-body": "physics",
  "rigid-body-queue": "physics",
}

path_overrides = {
  "hopper-ag": "levels/common/enemy",
  "oracle": "levels/city/oracle",
  "hopper": "levels/common/enemy",
  "bouncer": "levels/common/enemy",
  "yakow": "levels/city/farm",
  "yakow-ag": "levels/city/farm",
  "trail": "levels/city/common",
  "village1-vis": "levels/jak1/village1"
}

# i can be smarter than this...i swear....refactor eventually!
def level_name(file_meta):
  dgos = file_meta[3]
  # Handle files unique to one level
  if dgos == ["HIDEOUT"] or dgos == ["LHIPOUT"] or dgos == ["LTHRNOUT"] or dgos == ["LTRNTESS"] or dgos == ["LTRNKRKD"]:
    return "levels/hideout"
  elif dgos == ["ORACLE"]:
    return "levels/city/oracle"
  elif dgos == ["DEMO"] or dgos == ["DEMO", "TITLE"]:
    return "levels/demo"
  elif dgos == ["ONINTENT"] or dgos == ["LTENTOB"] or dgos == ["LTENTOUT"]:
    return "levels/city/onin_tent"
  elif dgos == ["VI1"]:
    return "levels/jak1/village1"
  elif dgos == ["INTROCST"] or dgos == ["LINTCSTB"]:
    return "levels/intro"
  elif dgos == ["OUTROCST"] or dgos == ["LOUTCSTB"] or dgos == ["LGARCSTA"]:
    return "levels/outro"
  elif dgos == ["ART", "GAME"] or dgos == ["ART"] or dgos == ["GAME"]:
    return "levels/common"
  elif dgos == ["MTX"] or dgos == ["MTN"] or dgos == ["MTX", "MCN"]:
    return "levels/temple"
  elif dgos == ["FOR"] or dgos == ["LWIDEB", "FOR"]:
    return "levels/forest"
  elif dgos == ["FOB"] or dgos == ["LPROTECT"]:
    return "levels/forest/lifeseed"
  elif dgos == ["HIPHOG"] or dgos == ["LHIPOUT"] or dgos == ["LWHACK"]:
    return "levels/hiphog"
  elif dgos == ["GGA"]:
    return "levels/gungame"
  elif dgos == ["DMI"]:
    return "levels/drill_platform"
  elif dgos == ["DRI"] or dgos == ["DRI", "DRILLMTN"]:
    return "levels/drill_platform"
  elif dgos == ["PAC"]:
    return "levels/palace/cable"
  elif dgos == ["PAE", "PAC"]:
    return "levels/palace"
  elif dgos == ["PALBOSS"]:
    return "levels/palace/boss"
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
    return "levels/strip"
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
    return "levels/fortress/exit"
  elif dgos == ["FRA"] or dgos == ["FRB"]:
    return "levels/fortress/rescue"
  elif dgos == ["PRI"] or dgos == ["LPRSNCST"]:
    return "levels/fortress/prison"
  elif dgos == ["CAS"]:
    return "levels/landing_pad"
  elif dgos == ["CAP"] or dgos == ["CASEXT"]:
    return "levels/castle/pad"
  elif dgos == ["CAB"]:
    return "levels/castle/boss"
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
    return "levels/atoll"
  elif dgos == ["ATE", "CTYASHA"]:
    return "levels/atoll/ashelin"
  elif dgos == ["TBO"] or dgos == ["TOMBEXT"]:
    return "levels/mars_tomb/baron"
  elif dgos == ["CTYKORA"]:
    return "levels/city/slums/kor"
  elif dgos == ["LKIDDOGE"]:
    return "levels/city/kid_escort"
  elif dgos == ["CTYASHA"]:
    return "levels/city/market/east/ashelin"
  elif dgos == ["CMA", "CMB"]:
    return "levels/city/market"
  elif dgos == ["CMB"]:
    return "levels/city/market/east"
  elif dgos == ["CMA"]:
    return "levels/city/market/west"
  elif dgos == ["KIOSK"] or dgos == ["LMEETBRT"]:
    return "levels/city/market/west/brutter_kiosk"
  elif dgos == ["LPORTRUN"]:
    return "levels/city/port/mines"
  elif dgos == ["TITLE"]:
    return "levels/title"
  elif dgos == ["LSACK"]:
    return "level/haven/misc/collection_task"
  elif dgos == ["LPACKAGE"]:
    return "level/haven/misc/delivery"
  elif dgos == ["GARAGE"]:
    return "levels/city/stadium/garage"
  elif dgos == ["LRACELIT"]:
    return "levels/stadium/races"
  elif dgos == ["STADBLMP"]:
    return "levels/city/stadium/defend"
  elif dgos == ["SKA"]:
    return "levels/stadium/jetboard"
  elif dgos == ["LWIDESTA"] or dgos == ["STA"] or dgos == ["STD", "STC", "STB"] or dgos == ["STD", "STC"]:
    return "levels/stadium"
  elif dgos == ["STB"] or dgos == ["LRACEBF"]:
    return "levels/stadium/races/class3"
  elif dgos == ["STC"] or dgos == ["LRACECF"]:
    return "levels/stadium/races/class2"
  elif dgos == ["STD"] or dgos == ["LRACEDF"]:
    return "levels/stadium/races/class1"
  elif dgos == ["MCN"]:
    return "levels/temple/canyon"
  elif dgos == ["COA"] or dgos == ["COB"]:
    return "levels/consite"
  elif dgos == ["RUI"]:
    return "levels/ruins"
  elif dgos == ["SAG"]:
    return "levels/ruins/hut"
  elif dgos == ["SEW"] or dgos == ["SEB", "SWB"] or dgos == ["SEB"] or dgos == ["SWE", "SEW"] :
    return "levels/sewer"
  elif dgos == ["NES"] or dgos == ["NESTT", "NES"] or dgos == ["NESTT"]:
    return "levels/nest"
  elif dgos == ["NEB"]:
    return "levels/nest/boss"
  elif dgos == ["SWE"] or dgos == ["SWB"] or dgos == ["SEB", "SWB", "UNB"]: # interesting this is in underport too... who is 'ruf' and 'hal'?
    return "levels/sewer/escort"
  elif dgos == ["HALFPIPE"]:
    return "levels/test/halfpipe"
  elif dgos == ["CWI"] or dgos == ["PAC", "CWI"] or dgos == ["LWIDEA"] or dgos == ["LWIDEB"] or dgos == ["LWIDEB", "LWIDEA"] or dgos == ["LWIDEC"] or dgos == ["LWIDEC", "LWIDEA"] or dgos == ["LWIDEB", "LWIDEC", "LWIDEA"]:
    return "levels/city/common"
  elif dgos == ["LBBUSH"]:
    return "levels/city/side_missions"
  elif dgos == ["CTA"] or dgos == ["CTB"] or dgos == ["CTC"]:
    return "levels/city/slums"
  elif dgos == ["CPA"]:
    return "levels/city/palace"
  elif dgos == ["CIA"] or dgos == ["CIB"]:
    return "levels/city/industrial"
  elif dgos == ["CPO"] or dgos == ["PORTWALL"]:
    return "levels/city/port"
  elif dgos == ["LPRTRACE"]:
    return "levels/city/port/race/side_mission"
  elif dgos == ["LERLCHAL"]:
    return "levels/city/port/race/errol"
  elif dgos == ["LERLCHAL", "LPRTRACE"]:
    return "levels/city/port/race"
  elif dgos == ["CFB"] or dgos == ["CFA"] or dgos == ["CFB", "CFA"]:
    return "levels/city/farm"
  elif dgos == ["LBOMBBOT"]:
    return "levels/common/enemy/bombots"
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
    return "levels/common/enemy"
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
    return "levels/common/enemy/fodder"
  elif dgos == ["LHELLDOG"]:
    return "levels/common/enemy/hellcat"
  elif dgos == ["LJAKDAX"]:
    return "characters/jak_daxter"
  elif dgos == ["ATE", "SEB", "SWB", "RUI"] or dgos == ["ATE", "SEW", "RUI"]:
    return "levels/common/enemy/amphibian"
  elif dgos == ["LWIDEB", "STR", "DRILLMTN", "STADBLMP", "HALFPIPE", "SEW", "UNB", "RUI", "CTYASHA"]:
    return "levels/common/enemy/metalhead_grunt"
  elif dgos == ["LWIDEB", "STR", "NEB", "D3A", "STADBLMP", "RUI"]:
    return "levels/common/enemy/metalhead_scout"
  elif dgos == ["NEB", "DRILLMTN"]:
    return "levels/common/enemy/metalhead_wasp"
  elif dgos == ["DMI", "MTN"] or dgos == ["DRI", "MTX"]:
    return "levels/common/enemy/metalhead_bearer"
  elif dgos == ["DG1", "D3A", "UNB", "RUI"] or dgos == ["D3A", "RUI"]:
    return "levels/common/enemy/metalhead_slinger"
  elif dgos == ["DG1", "MTN", "ATE", "D3A"] or dgos == ["ATE", "MTX", "D3A"]:
    return "levels/common/enemy/metalhead_brown"
  elif dgos == ["LSAMERGD"]:
    return "characters/samos_errol_guard"
  elif dgos == ["DG1", "D3A", "TOA"]:
    return "levels/common/enemy/baby_spider"
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
    return "levels/city/generic"
  elif dgos == ["LCITYLOW"] or dgos == ["CASCITY"]:
    return "levels/city/low-quality"
  elif dgos == ["GAME", "COMMON"]:
    return "levels/common"
  elif dgos == ["ATE", "SEB", "SWB", "LKIDDOGE", "UNB", "CTYKORA", "CTYASHA"]:
    return "levels/common/ai"
  elif dgos == ["LASHGRD"]:
    return "characters/ashelin_guard"
  elif dgos == ["SEB", "SWB", "LKIDDOGE", "UNB", "CTYKORA"]: # TODO - wtf is 'hal'
    return "levels/undefined"
  elif dgos == ["DRILLMTN", "UNB", "RUI"] or dgos == ["UNB", "RUI"] or dgos == ["DMI", "RUI"]:
    return "levels/target/mech_suit"

remaining_dgos = {}

folders = {}

for jak2_file in jak2_files:
  if jak2_file[3] == ["NO-XGO"]:
    num_replicated = num_replicated + 1
    continue
  # manual overrides
  if jak2_file[0] in path_overrides or jak2_file[0].removesuffix("-h") in path_overrides:
    num_replicated = num_replicated + 1
    if jak2_file[0] in path_overrides:
      jak2_file[4] = path_overrides[jak2_file[0]]
    else:
      jak2_file[4] = path_overrides[jak2_file[0].removesuffix("-h")]
    if jak2_file[4] not in folders:
      folders[jak2_file[4]] = 1
    else:
      folders[jak2_file[4]] = folders[jak2_file[4]] + 1
    continue
  # port over manually specified engine files
  if jak2_file[0] in engine_files or jak2_file[0].removesuffix("-h") in engine_files:
    num_replicated = num_replicated + 1
    if jak2_file[0] in engine_files:
      jak2_file[4] = str.format("engine/{}", engine_files[jak2_file[0]])
    else:
      jak2_file[4] = str.format("engine/{}", engine_files[jak2_file[0].removesuffix("-h")])
    if jak2_file[4] not in folders:
      folders[jak2_file[4]] = 1
    else:
      folders[jak2_file[4]] = folders[jak2_file[4]] + 1
    continue
  # attempt to find the object with the same name in jak1
  jak1_path = None
  if not jak2_file[0].startswith("tpage"):
    for jak1_file in jak1_files:
      if jak1_file[0] == jak2_file[0]:
        jak1_path = jak1_file[4]
        if (jak1_path.startswith("levels")):
          print("{} - {}".format(jak1_path, jak2_file[0]))
        break
  if jak1_path is not None:
    jak2_file[4] = jak1_path
    num_replicated = num_replicated + 1
  elif level_name(jak2_file):
    jak2_file[4] = level_name(jak2_file)
    num_replicated = num_replicated + 1
  else:
    num_left = num_left + 1
    if ",".join(jak2_file[3]) in remaining_dgos:
      remaining_dgos[",".join(jak2_file[3])] = remaining_dgos[",".join(jak2_file[3])] + 1
    else:
      remaining_dgos[",".join(jak2_file[3])] = 1
  if jak2_file[4] not in folders:
    folders[jak2_file[4]] = 1
  else:
    folders[jak2_file[4]] = folders[jak2_file[4]] + 1

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
      json_file.write("[{}{}{}, {}, \"{}\"]\n".format(name, name_in_dgo, jak2_file[2], dgo_set, jak2_file[4]))
    else:
      json_file.write("[{}{}{}, {}, \"{}\"],\n".format(name, name_in_dgo, jak2_file[2], dgo_set, jak2_file[4]))
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

print("Folder | Number of Files")
for folder in dict(sorted(folders.items(), reverse=True, key=lambda item: item[1])):
  print("{}: {}".format(folder, folders[folder]))
  if limit > 100:
    break
  limit = limit + 1
