# Reduces work and tries to maintain consistency by placing objects in the same folder
# as they were in the previous game.
jak2_files = None
jak3_files = None

import json

with open('../../../goal_src/jak2/build/all_objs.json', 'r') as f:
  jak2_files = json.load(f)
with open('../../../goal_src/jak3/build/all_objs.json', 'r') as f:
  jak3_files = json.load(f)

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
  "lightning": "gfx/generic/lightning",
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
  "emerc-vu1": "gfx/foreground/merc",
  "emerc": "gfx/foreground/merc",
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
  "target-pilot": "target",
  "pilot-states": "target",
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
  "water": "common-obs",
  "enemy": "ai",
  "water-flow": "common-obs",
  "generic-obs": "common-obs",
  "voicebox": "common-obs",
  "projectile": "common-obs",
  "water-anim": "common-obs",
  "blocking-plane": "common-obs",
  "dark-eco-pool": "common-obs",
  "collectables-part": "common-obs",
  "crates": "common-obs",
  "collectables": "common-obs",
  "powerups": "common-obs",
  "los-control": "collide",
  "carry": "target/mech",
  "mech": "target/mech",
  "nav-enemy": "nav",
  "base-plat": "common-obs",
  "plat": "common-obs",
  "basebutton": "common-obs",
  "bouncer": "common-obs",
  "conveyor": "common-obs",
  "elevator": "common-obs",
  "rigid-body-plat": "common-obs",
  "rigid-body": "physics",
  "rigid-body-queue": "physics",

  "lightjak": "target",
  "lightjak-wings": "target",
  "target-lightjak": "target",
  "target-ladder": "target",
  "target-launch": "target",
  "target-invisible": "target",
  "target-flut": "target/flut",
  "indax-h": "target/indax",
  "target-indax": "target/indax",
  "target-indax-hang": "target/indax",
  "ragdoll": "physics",
  "cloth": "physics",
  "nav-engine": "nav",
  "light-trails": "gfx/sprite/particles",
  "lightning-new": "gfx/generic/lightning",

  "hfrag": "gfx/background/hfrag",
  "hfrag-h": "gfx/background/hfrag",
  "hfrag-vu1": "gfx/background/hfrag",
  "hfrag-vu1-h": "gfx/background/hfrag",
  "hfrag-common": "gfx/background/hfrag",
  "hfrag-work": "gfx/background/hfrag",
  "hfrag-texture-anim": "gfx/background/hfrag",

  "bug-report": "debug",
  "visvol-edit": "debug",
  "collision-editor": "debug",
  "nav-mesh-editor-h": "debug",
  "nav-mesh-editor": "debug",

  "matrix-compose": "common-obs",
  "cloth-art-h": "common-obs",
  "prim-h": "common-obs",
  "water-info-h": "common-obs",
  "prim": "common-obs",
  "curves": "common-obs",
  "prim-beam-h": "common-obs",
  "particle-curves": "common-obs",
  "water-part": "common-obs",
  "gem-pool": "common-obs",
  "vent": "common-obs",
  "secrets-menu": "common-obs",
  "speech-manager": "common-obs",
  "dir-tpages": "common-obs",
  "airlock": "common-obs",
  "proc-focusable-spawner": "common-obs",
  "enemy-states": "common-obs",
  "scene-actor": "common-obs",
  "warp-gate": "common-obs",
  "guard-projectile": "common-obs",
  "metalhead-projectile": "common-obs",
  "enemy-part": "common-obs",
  "ragdoll-test": "common-obs",
  "debris": "common-obs",
  "shield-sphere": "common-obs"
}

path_overrides = {
  "trail": "levels/city/common",
  "tomb-baby-spider": "levels/temple",
  "tomb-baby-spider-ag": "levels/temple",
  "mhcity-obs": "levels/mhcity",
  "mhcity-obs2": "levels/mhcity",
  "mhcity-part": "levels/mhcity",
  "race-control": "levels/common/race",
  "rhino-ag": "levels/desert/wvehicle",
  "mantis": "levels/common/enemy",
  "mantis-ag": "levels/common/enemy",
  "roboguard": "levels/common/enemy",
  "roboguard-ag": "levels/common/enemy",
  "ladder": "levels/common-obs",
  "ladder-ag": "levels/common-obs",
  "desert-dust-storm": "levels/desert",
  "des-bush": "levels/desert",
  "des-bush-part": "levels/desert",
  "des-burning-bush": "levels/desert",
  "jump-pad": "levels/sewer",
  "robo-hover": "levels/common/enemy/hover",
  "kg-grunt": "levels/common/enemy",
  "spydroid-orig": "levels/common/enemy",
  "cty-borrow-manager-h": "levels/city/common",
  "cty-faction-h": "levels/city/common",
  "tentacle": "levels/desert",
  "tentacle-ag": "levels/desert",
  "external-player-control": "levels/common",
  "forestb-vis": "levels/forest",
  "transport-ag": "levels/forest",
  "cty-homing-missile-ag": "levels/city/hijack",
  "predator": "levels/city/destroy-grid",
  "predator-ag": "levels/city/destroy-grid",
  "neo-wasp": "levels/common/enemy/darkprec",
  "neo-wasp-part": "levels/common/enemy/darkprec",
  "neo-wasp-ag": "levels/common/enemy/darkprec",
  "neo-wasp-b-ag": "levels/common/enemy/darkprec",
  "pecker-ingame": "levels/comb/pecker",
  "pecker-ingame-ag": "levels/comb/pecker",
  "battle-amulet-ag": "levels/wascity",
  "prebot-eco-creature": "levels/common/enemy",
  "bombbot-bomb-ag": "levels/city/bombbot",
  "jakc-highres-ag": "levels/borrow/jak",
  "king-highres-ag": "levels/borrow/damas",
  "daxter-highres-ag": "levels/borrow/daxter",
  "vin-effect-ag": "levels/borrow/jak-daxter-vin",
  "spotlight-ag": "levels/comb",
  "sew-fan-ag": "levels/sewer",
  "sew-jump-pad-ag": "levels/sewer",
  "sew-floor-switch-ag": "levels/sewer",
  "monk-ag": "levels/wascity/leaper",
  "errol-lowres-ag": "levels/factory",
  "eco-crystal-light-ag": "levels/common",
  "lfac-hanger-door-ag": "levels/factory",
  "errol-effect-ag": "levels/desert/hover",
  "jak-pilot-wcar+0-ag": "engine/target",
  "monster-frog": "levels/mine",
  "monster-frog-ag": "levels/mine",
  "stadiumb-vis": "levels/stadium",
  "conveyor": "levels/factory",
  "flut": "engine/target/flut",
  "flut-part": "engine/target/flut",
  "flut-racer": "engine/target/flut",
  "jak-flut+0-ag": "engine/target/flut",
  "jak-tube+0-ag": "engine/target",
  "jak-pole+0-ag": "engine/target",
  "jak-turret+0-ag": "engine/target",
  "warp-gate-ag": "levels/temple",
  "ctymark-obs": "levels/wascity",
  "brutter-highres-ag": "levels/museum",
  "crocadog-highres-ag": "levels/museum",
  "baron-highres-ag": "levels/museum",
  "errol-highres-ag": "levels/museum",
  "kid-highres-ag": "levels/museum",
  "kor-highres-ag": "levels/museum",
  "youngsamos-highres-ag": "levels/museum",
  "pecker-highres-ag": "levels/museum",
  "tess-highres-ag": "levels/museum",
  "torn-highres-ag": "levels/museum",
  "ashelin-highres-ag": "levels/museum",
  "jak-highres-ag": "levels/museum",
  "keira-highres-ag": "levels/museum",
  "onin-highres-ag": "levels/museum",
  "sig-highres-ag": "levels/museum",
  "kid-ag": "levels/stadium",
  "crimson-guard-ag": "levels/city",
  "samos-highres-ag": "levels/museum",
  "crimson-guard-highres-ag": "levels/intro",
  "lerrol": "levels/borrow/errol",
  "darkjak-highres-ag": "levels/mhcity",
  "wascity-burning-bush-ag": "levels/wascity",
  "fort-entry-gate-ag": "levels/gungame",
  "palmpilot-b-ag": "levels/city",
  "0credits-tx": "levels/title",
  "1credits-tx": "levels/title",
  "2credits-tx": "levels/title",
  "3credits-tx": "levels/title",
  "4credits-tx": "levels/title",
  "5credits-tx": "levels/title",
  "6credits-tx": "levels/title",
  "7credits-tx": "levels/title",
  "8credits-tx": "levels/title",
  "9credits-tx": "levels/title",
  "10credits-tx": "levels/title",
  "credits-h": "levels/title",
  "rub-rhino-door-ag": "levels/stadium",
  "rub-tower-ag": "levels/stadium",
  "ottsel-dummy-ag": "levels/museum",
  "babak-ag": "levels/museum",
  "precur-warp-effect-ag": "levels/precursor",
  "pre-artifact-a-ag": "levels/desert/artifact-race",
  "gauntlets-ag": "levels/desert/artifact-race",
  "jak-ladder+0-ag": "engine/target",
  "ctycrate-ag": "levels/city/common",
  "neo-satellite-break-ag": "levels/wascity",
  "jinx-highres-ag": "levels/city/hiphog",
  "blue-gun-mod-three-ag": "levels/city/destroy-grid",
  "jakc-scarf-ag": "levels/desert",
  "krimson-wall-ag": "levels/city/industrial",
  "precursor-ship-ag": "levels/outro",
  "kg-grunt-ag": "levels/common/enemy",
  "krimson-wall-break-ag": "levels/city/port/attack",
  "des-burning-bush-ag": "levels/desert/bbush",
  "precur-door-a-ag": "levels/precursor",
  "precur-swingpole-pop-ag": "levels/precursor",
  "jakc-wings-ag": "levels/temple",
  "dark-maker-idol-ag": "levels/temple",
  "urn-a-ag": "levels/temple",
  "urn-b-ag": "levels/temple",
  "urn-c-ag": "levels/temple",
  "flamer-lava-ag": "levels/volcano",
  "light-eco-vent-ag": "levels/common",
  "grunt-ag": "levels/common/enemy",
  "gekko-ag": "levels/mine",
  "manta-ag": "levels/mine",
  "kg-debris-ag": "levels/city",
  "eco-crystal-dark-ag": "levels/desert/rescue",
  "wlander-male-ag": "levels/desert/rescue",
  "robo-hover-ag": "levels/sewer",
  "cty-door-ag": "levels/city/slums",
  "egg-spider-ag": "levels/nest",
  "tpl-elevator-ag": "levels/temple",
  "spydroid-orig-ag": "levels/factory",
  "kg-flying-turret-ag": "levels/city/blow-tower",
  "jak-pilot-gun+0-ag": "engine/target",
  "jak-indax+0-ag": "engine/target/indax",
  "kg-hopper-ag": "levels/sewer",
  "mhcity-de-tower-egg-ag": "levels/mhcity",
  "kleever-highres-ag": "levels/borrow/kleiver",
  "veger-highres-ag": "levels/borrow/veger",
  "cav-eco-lg-ag": "levels/mine",
  "errol-ag": "levels/borrow/errol",
  "kg-pickup-ag": "levels/city/hijack",
  "snake-wheel-fma-ag": "levels/borrow/snake_wheels",
  "comb-rail-rider-ag": "levels/comb",
  "rhino-wheel-fma-ag": "levels/stadium",
  "seem-highres-ag": "levels/borrow/seem",
  "city-flitter-ag": "levels/common/enemy",
  "terraformer-head-ag": "levels/desert/boss",
  "jak-pilot-hcar+0-ag": "engine/target",
  "rapid-gunner": "levels/stadium",
  "spydroid": "levels/common/enemy",
  "credits": "levels/title",
  "ragdoll-edit": "engine/physics",
  "manipulator": "engine/debug"
}

# i can be smarter than this...i swear....refactor eventually!
def level_name(file_meta):
  dgos = file_meta[3]
  # Handle files unique to one level
  if dgos == ["DESA"] or dgos == ["DESB"] or dgos == ["DESC"] or dgos == ["DESD"] or dgos == ["DESE"] or dgos == ["DESF"] or dgos == ["DESG"] or dgos == ["DESH"] or dgos == ["DST"] or dgos == ["DESD", "WIN"] or dgos == ["LWASBBV"] or dgos == ["HGA", "WIN", "DST"] or dgos == ["LDESGCST"] or dgos == ["LWLANDM"] or dgos == ["WARPCAST"]:
    return "levels/desert"
  elif dgos == ["DESRACE2", "DESJUMP", "DESOASIS", "DESRESCG", "DESRACE1", "DESRALLY", "DESTRACK", "DESINTER", "DESCHASE"] or dgos == ["DESRACE2", "DESOASIS", "DESRESCG", "DESRACE1", "DESRALLY", "DESTRACK", "DESINTER", "DESCHASE"] or dgos == ["DESINTER"]:
      return "levels/desert"
  elif dgos == ["LBBRING1"] or dgos == ["LBBRING2"] or dgos == ["LBBRING3"] or dgos == ["LBBRING4"] or dgos == ["LBBRING5"] or dgos == ["LBBRING6"] or dgos == ["LBBSPID"] or dgos == ["LBBSPIRT"] or dgos == ["LBBSPRT2"] or dgos == ["LBBSPRT3"]:
    return "levels/desert/bbush"
  elif dgos == ["DESRALLY"] or dgos == ["DESTRACK"] or dgos == ["DESRALLY", "DESTRACK"]:
      return "levels/desert/race"
  elif dgos == ["DESLIZ"]:
    return "levels/desert/lizard"
  elif dgos == ["DESBOSS1"] or dgos == ["DESBOSS2"] or dgos == ["DESW"] or dgos == ["DESERROL"] or dgos == ["DESBCST"] or dgos == ["DESW", "DESBOSS1", "DESBOSS2"] or dgos == ["DESBOSS1", "PRECD"] or dgos == ["DESW", "DESBOSS1"]:
    return "levels/desert/boss"
  elif dgos == ["DESHOVER"] or dgos == ["DESBATTL"] or dgos == ["DESBATTL", "DESHOVER"] or dgos == ["DESHUNT"]:
    return "levels/desert/hover"
  elif dgos == ["DESRACE1"] or dgos == ["DESRACE2"] or dgos == ["DESRACE2", "DESRACE1"]:
    return "levels/desert/artifact-race"
  elif dgos == ["DESRESC"] or dgos == ["DESRESCG"] or dgos == ["DESRESCC"] or dgos == ["DESRESC", "RUBA"]:
    return "levels/desert/rescue"
  elif dgos == ["DESJUMP"] or dgos == ["DESCHASE"] or dgos == ["DESJUMP", "DESCHASE"] or dgos == ["DESOASIS", "WASSTADC", "DESCHASE"]:
    return "levels/desert/chase"
  elif dgos == ["DESOASIS"] or dgos == ["OASISCST"] or dgos == ["OASISCST", "WSD"] or dgos == ["OASISCST", "TEMA", "RBCT", "COMBA"]:
    return "levels/desert/oasis"
  elif dgos == ["LPATK", "LFACCAR", "WASALL"]:
    return "levels/desert/wvehicle"
  elif dgos == ["NSA"] or dgos == ["NSB"] or dgos == ["LNSTOA"] or dgos == ["LNSTOBB"] or dgos == ["LNSTCST"] or dgos == ["NSA", "LFORP", "LBBSPID"] or dgos == ["LNSTOBC"]:
    return "levels/nest"
  elif dgos == ["LBBTCHA1"] or dgos == ["LBBTCHA2"] or dgos == ["LBBTCHA3"] or dgos == ["LBBTCHA3", "LBBTCHA2", "LBBTCHA1"] or dgos == ["LBBSDRP1"] or dgos == ["LBBSDRP2"] or dgos == ["LBBSDRP3"]:
    return "levels/wascity/bbush"
  elif dgos == ["WASALL"] or dgos == ["WWD"] or dgos == ["WCA"] or dgos == ["WCASEEM"] or dgos == ["WCB"] or dgos == ["DESRESC", "WWD", "CWI"] or dgos == ["WCB", "WCA", "WASCHASE"] or dgos == ["WSD", "WCA"] or dgos == ["WSD", "DESB"] or dgos == ["WCB", "WCA"] or dgos == ["WASSEEM"] or dgos == ["WASCAST"]:
    return "levels/wascity"
  elif dgos == ["WASPALA"]:
    return "levels/wascity/palace"
  elif dgos == ["WIN"]:
    return "levels/wascity/intro"
  elif dgos == ["WASSTADA"] or dgos == ["WASSTADB"] or dgos == ["WASSTADC"] or dgos == ["WASSTADC", "DESRACE1"] or dgos == ["LWASSIG"] or dgos == ["ARENACST"] or dgos == ["LWSTDPCK"] or dgos == ["WASSTADA", "WASSTADC", "WASSTADB"]:
    return "levels/wascity/wasstadium"
  elif dgos == ["WASLEAPR"] or dgos == ['WWD', 'DESLIZ', 'VOCA']:
    return "levels/wascity/leaper"
  elif dgos == ["WASCHASE"]:
    return "levels/wascity/chase"
  elif dgos == ["WSD"]:
    return "levels/wascity/doors"
  elif dgos == ["WASPGAME"] or dgos == ["WASDEFEN"]:
    return "levels/wascity/defend"
  elif dgos == ["TEMA"] or dgos == ["TEMB"] or dgos == ["TEMC"] or dgos == ["TEMD"] or dgos == ["TEMX"] or dgos == ["TEMA", "TEMB"] or dgos == ["TEMPLEE"]:
    return "levels/temple"
  elif dgos == ["TEMA", "DESW", "TEMD", "HALFPIPE", "LPTRL", "TEMB"]:
    return "levels/temple"
  elif dgos == ["HGA"] or dgos == ["HGB"] or dgos == ["HGA", "VOCX"]:
    return "levels/glider"
  elif dgos == ["VOCA"] or dgos == ["VOCX"] or dgos == ["VOCX", "TEMX"]:
    return "levels/volcano"
  elif dgos == ["FACTORYA"] or dgos == ["FACB"] or dgos == ["FACC"] or dgos == ["FACD"] or dgos == ["LFACCITY"] or dgos == ["LFACB"] or dgos == ["LFACTORY"] or dgos == ["LFACRM1"] or dgos == ["LFACRM2"] or dgos == ["LFACB", "LFACCITY"] or dgos == ["FACC", "FACTORYA"] or dgos == ["LFACO"]:
    return "levels/factory"
  elif dgos == ["HGA", "CWI", "LFACTORY"] or dgos == ["LFACCAR"]:
    return "levels/factory/car"
  elif dgos == ["PRECA"] or dgos == ["PRECB"] or dgos == ["PRECC"] or dgos == ["PRECD"] or dgos == ["PRECB", "PRECA"] or dgos == ["PRECA", "PRECD"] or dgos == ["LPRECC"] or dgos == ["PRECD", "FRSTA"] or dgos == ["LPRENME"] or dgos == ["LPRENME", "PRECD"] or dgos == ["LPRENME", "TOWB", "LFORM", "FACD", "LPATK", "TOWERA"]:
    return "levels/precursor"
  elif dgos == ["COMBA"] or dgos == ["COMBB"] or dgos == ["COMBC"] or dgos == ["COMBD"] or dgos == ["COMBE"] or dgos == ["COMBN"] or dgos == ["COMBX"]:
    return "levels/comb"
  elif dgos == ["RAILA"] or dgos == ["RAILB"] or dgos == ["RAILC"] or dgos == ["RAILD"] or dgos == ["RAILE"] or dgos == ["RAILF"] or dgos == ["RAILX"] or dgos == ["RAILA", "COMBA"] or dgos == ["RAILB2"] or dgos == ["RAILCST"]:
    return "levels/comb"
  elif dgos == ["SEA"] or dgos == ["SEB"] or dgos == ["SEC"] or dgos == ["SED"] or dgos == ["SEE"] or dgos == ["SEF"] or dgos == ["SEG"] or dgos == ["SEH"] or dgos == ["SEI"] or dgos == ["SEJ"] or dgos == ["SEK"] or dgos == ["SEL"] or dgos == ["SEM"] or dgos == ["SEN"] or dgos == ["SEO"] or dgos == ["SEO", "SEL"] or dgos == ["SEK", "SEF"] or dgos == ["SEH", "SEG"]:
    return "levels/sewer"
  elif dgos == ["MIA"] or dgos == ["MIB"] or dgos == ["MIC"] or dgos == ["MINED"] or dgos == ["MINEE"] or dgos == ["MIA", "COMBN"] or dgos == ["MIA", "MIB", "MINED"]:
    return "levels/mine"
  elif dgos == ["LMECH"]:
    return "engine/target/mech"
  elif dgos == ["CITYCAST"] or dgos == ["LCITYSML"]:
    return "levels/city"
  elif dgos == ["LCTYHIJK"]:
    return "levels/city/hijack"
  elif dgos == ["LCTYPROT"] or dgos == ["LCTYPALT"] or dgos == ["CTYPESA"] or dgos == ["CTYPESB"] or dgos == ["CTYPESC"] or dgos == ["CTYPEPA"] or dgos == ["CTYPEPB"]or dgos == ["CTYPEPC"]:
    return "levels/city/protect"
  elif dgos == ["LCTYBLOW"] or dgos == ["LBLOWCST"] or dgos == ["LBLOWTMH"] or dgos == ["LBLOWTKG"]:
    return "levels/city/blow-tower"
  elif dgos == ["LJINX"] or dgos == ["GRIDCST"]:
    return "levels/city/destroy-grid"
  elif dgos == ["FREEHQ"] or dgos == ["FREECAST"] or dgos == ["LFREEOUT"]:
    return "levels/city/freehq"
  elif dgos == ["LCTYSNPR"]:
    return "levels/city/sniper"
  elif dgos == ["VIN"] or dgos == ["POWERGD"] or dgos == ["LVINCST"] or dgos == ["LVINCST", "POWERGD"]:
    return "levels/city/vinroom"
  elif dgos == ["LCTYPATK"] or dgos == ["LPATKCS"]:
    return "levels/city/port/attack"
  elif dgos == ["LCTYASS"]:
    return "levels/city/assault"
  elif dgos == ["CWI"] or dgos == ["PAC", "CWI"] or dgos == ["LWIDEA"] or dgos == ["LWIDEB"] or dgos == ["LWIDEB", "LWIDEA"] or dgos == ["LWIDEC"] or dgos == ["LWIDEC", "LWIDEA"] or dgos == ["LWIDEB", "LWIDEC", "LWIDEA"]:
    return "levels/city/common"
  elif dgos == ["CTYCARA"] or dgos == ["CTYCARB"] or dgos == ["CTYCARC"] or dgos == ["CTYCARKG"]:
    return "levels/city/vehicle"
  elif dgos == ["CTA"] or dgos == ["CTB"] or dgos == ["CTC"] or dgos == ["SLUMBSET"]:
    return "levels/city/slums"
  elif dgos == ["CPA"]:
    return "levels/city/palace"
  elif dgos == ["CIA"] or dgos == ["CIB"]:
    return "levels/city/industrial"
  elif dgos == ["CPO"] or dgos == ["PORTWALL"]:
    return "levels/city/port"
  elif dgos == ["CFB"] or dgos == ["CFA"] or dgos == ["CFB", "CFA"]:
    return "levels/city/farm"
  elif dgos == ["CGB"] or dgos == ["CGA"] or dgos == ["CGC"]:
    return "levels/city/generic"
  elif dgos == ["LTOWCITY"]:
    return "levels/city/palace"
  elif dgos == ["HHG"] or dgos == ["LTNJXHIP"] or dgos == ["LTNFXHIP"]:
    return "levels/hiphog"
  elif dgos == ["ONINTENT"] or dgos == ["LTENTOB"] or dgos == ["LTENTOUT"]:
    return "levels/city/onintent"
  elif dgos == ["LBOMBBOT"]:
    return "levels/city/bombbot"
  elif dgos == ["MHCA"] or dgos == ["MHCB"] or dgos == ["MHCB", "MHCA"] or dgos == ["MHCB", "LCTYDEST", "MHCA"] or dgos == ["LCTYDEST"] or dgos == ["LMHCA"] or dgos == ["LMHCB"] or dgos == ["MHCTYCST"]:
    return "levels/mhcity"
  elif dgos == ["TOWERA"] or dgos == ["TOWB"] or dgos == ["TOWERC"] or dgos == ["LTOWA"] or dgos == ["LTOWB"] or dgos == ["TOWERCST"] or dgos == ["LTOWA", "TOWB"] or dgos == ["LTOWA", "TOWB", "TOWERCST"]:
    return "levels/tower"
  elif dgos == ["FRSTA"] or dgos == ["FRSTB"] or dgos == ["FRSTX"] or dgos == ["LFORM"] or dgos == ["LFORP"] or dgos == ["LFORRING"]:
    return "levels/forest"
  elif dgos == ["STA"] or dgos == ["STAA"] or dgos == ["STB"] or dgos == ["RUBA"] or dgos == ["RUBA2"] or dgos == ["RUBB"] or dgos == ["RUBC"] or dgos == ["RBCT"] or dgos == ["LPATK"] or dgos == ["LPTRL"] or dgos == ["RUBB", "RUBA2"]:
    return "levels/stadium"
  elif dgos == ["GGA"] or dgos == ["GUNGAME1"] or dgos == ["GUNGAME2"] or dgos == ["LGUNRNC"] or dgos == ["LGUNNORM"]:
    return "levels/gungame"
  elif dgos == ["TITLE"] or dgos == ["INTTITLE"]:
    return "levels/title"
  elif dgos == ["IPF"] or dgos == ["INTROCST"] or dgos == ["INTPALRF"]:
    return "levels/intro"
  elif dgos == ["OUTROCST"] or dgos == ["LOUTRO"] or dgos == ["LOUTRO2"] or dgos == ["LOUTRO3"] or dgos == ["OUTCAST3"] or dgos == ["LOUTRO3", "LONINSIM"] or dgos == ["TEMP"]:
    return "levels/outro"
  elif dgos == ["MUSEUM"] or dgos == ["MUSEUM2"] or dgos == ["MUSEUM2", "LOUTRO2"] or dgos == ["MUSEUM2", "LOUTRO", "RAILCST"] or dgos == ["MUSEUM3"] or dgos == ["MUSEUM3B"] or dgos == ["MUSEUM4"] or dgos == ["MUSEUM4B"]:
    return "levels/museum"
  elif dgos == ["HALFPIPE"]:
    return "levels/test/halfpipe"
  elif dgos == ["ART", "GAME"] or dgos == ["ART"] or dgos == ["GAME"]:
    return "levels/common"
  elif dgos == ["HGA", "LPATK", "RAILA", "LFACCAR", "CWI", "WASALL", "LFACTORY", "COMBA"]:
    return "levels/common/hvehicle"
  elif dgos == ["TEMA", "LFORM", "FACD", "LBIPED", "LPATK", "TOWERA", "PRECA"] or dgos == ["TEMA", "CTYPESA", "PRECB", "LFORM", "FACD", "LBIPED", "LPATK", "TOWERA", "PRECA"]:
    return "levels/common/enemy/darkprec"
  elif dgos == ["LBIPED"]:
    return "levels/common/enemy/darkprec"
  elif dgos == ["LONINSIM"]:
    return "levels/borrow/onin-simple"
  elif dgos == ["LKEIRA"]:
    return "levels/borrow/keira"
  elif dgos == ["LSAMOS"]:
    return "levels/borrow/samos"
  elif dgos == ["LTORN"]:
    return "levels/borrow/torn"
  elif dgos == ["LTORNSAM"]:
    return "levels/borrow/torn-samos"
  elif dgos == ["LSEEMWCA"]:
    return "levels/borrow/seem"
  elif dgos == ["LKLEEVER"]:
    return "levels/borrow/kleiver"
  elif dgos == ["LTORNJNX"]:
    return "levels/borrow/torn-jinx"
  elif dgos == ["LJAK"] or dgos == ["LJAKC"] or dgos == ["LJKFEET"]:
    return "levels/borrow/jak"
  elif dgos == ["LJAKNDAX"]:
    return "levels/borrow/jak-daxter"
  elif dgos == ["LJAKCKLV"] or dgos == ["LJAKKLEV"]:
    return "levels/borrow/jak-kleiver"
  elif dgos == ["LJKDMPK"]:
    return "levels/borrow/jak-damas-pecker"
  elif dgos == ["LJKCDMKL"]:
    return "levels/borrow/jak-damas-kleiver"
  elif dgos == ["LJKDXVIN"]:
    return "levels/borrow/jak-daxter-vin"
  elif dgos == ["LJNDKLEV"]:
    return "levels/borrow/jak-daxter-kleiver"
  elif dgos == ["LDAX"]:
    return "levels/borrow/daxter"
  elif dgos == ["LASHELIN"]:
    return "levels/borrow/ashelin"
  elif dgos == ["LDAMKLEV"]:
    return "levels/borrow/damas-kleiver"
  elif dgos == ["LDAMPECK"] or dgos == ["LDMPCKGN"]:
    return "levels/borrow/damas-pecker"
  elif dgos == ["LDAMPKSM"]:
    return "levels/borrow/damas-pecker-samos"
  elif dgos == ["LDAMSIG"]:
    return "levels/borrow/damas-sig"
  elif dgos == ["LSIG"]:
    return "levels/borrow/sig"
  elif dgos == ["LSIGKLV"]:
    return "levels/borrow/sig-kleiver"
  elif dgos == ["LJAKSIG"] or dgos == ["LSIGJAKC"]:
    return "levels/borrow/jak-sig"
  elif dgos == ["LJKDMPK"]:
    return "levels/borrow/jak-damas-pecker"
  elif dgos == ["LTRNJNX"]:
    return "levels/borrow/torn-jinx"
  elif dgos == ["LTRTWHLS"]:
    return "levels/borrow/turtle-wheels"
  elif dgos == ["LSNKWHLS"]:
    return "levels/borrow/snake-wheels"
  elif dgos == ["GAME", "COMMON"]:
    return "levels/common"

remaining_dgos = {}

folders = {}

for jak3_file in jak3_files:
  if jak3_file[3] == ["NO-XGO"]:
    num_replicated = num_replicated + 1
    continue
  # manual overrides
  if jak3_file[0] in path_overrides or jak3_file[0].removesuffix("-h") in path_overrides:
    num_replicated = num_replicated + 1
    if jak3_file[0] in path_overrides:
      jak3_file[4] = path_overrides[jak3_file[0]]
    else:
      jak3_file[4] = path_overrides[jak3_file[0].removesuffix("-h")]
    if jak3_file[4] not in folders:
      folders[jak3_file[4]] = 1
    else:
      folders[jak3_file[4]] = folders[jak3_file[4]] + 1
    continue
  # port over manually specified engine files
  if jak3_file[0] in engine_files or jak3_file[0].removesuffix("-h") in engine_files:
    num_replicated = num_replicated + 1
    if jak3_file[0] in engine_files:
      jak3_file[4] = str.format("engine/{}", engine_files[jak3_file[0]])
    else:
      jak3_file[4] = str.format("engine/{}", engine_files[jak3_file[0].removesuffix("-h")])
    if jak3_file[4] not in folders:
      folders[jak3_file[4]] = 1
    else:
      folders[jak3_file[4]] = folders[jak3_file[4]] + 1
    continue
  # attempt to find the object with the same name in jak 2
  jak2_path = None
  if not jak3_file[0].startswith("tpage"):
    for jak2_file in jak2_files:
      if jak2_file[0] == jak3_file[0]:
        jak2_path = jak2_file[4]
        if (jak2_path.startswith("levels")):
          print("{} - {}".format(jak2_path, jak3_file[0]))
        break
  if jak2_path is not None:
    jak3_file[4] = jak2_path
    num_replicated = num_replicated + 1
  elif level_name(jak3_file):
    jak3_file[4] = level_name(jak3_file)
    num_replicated = num_replicated + 1
  else:
    num_left = num_left + 1
    if ",".join(jak3_file[3]) in remaining_dgos:
      remaining_dgos[",".join(jak3_file[3])] = remaining_dgos[",".join(jak3_file[3])] + 1
    else:
      remaining_dgos[",".join(jak3_file[3])] = 1
  if jak3_file[4] not in folders:
    folders[jak3_file[4]] = 1
  else:
    folders[jak3_file[4]] = folders[jak3_file[4]] + 1

with open('../../../goal_src/jak3/build/all_objs_TMP.json', 'w') as json_file:
  # Calculate these to make the file as compact as possible
  longest_name = 0
  longest_name_in_dgo = 0
  for jak3_file in jak3_files:
    if len(jak3_file[0]) > longest_name:
      longest_name = len(jak3_file[0])
    if len(jak3_file[1]) > longest_name_in_dgo:
      longest_name_in_dgo = len(jak3_file[1])
  # Actually write things out
  json_file.write("[\n")
  i = 0
  for jak3_file in jak3_files:
    name = '{: <{}}'.format("\"{}\",".format(jak3_file[0]), longest_name + 2)
    name_in_dgo = '{: <{}}'.format("\"{}\",".format(jak3_file[1]), longest_name_in_dgo + 2)
    dgo_set = "["
    for dgo in jak3_file[3]:
      dgo_set += "\"{}\", ".format(dgo)
    dgo_set = dgo_set.removesuffix(", ")
    dgo_set += "]"
    if i == (len(jak3_files) - 1):
      json_file.write("[{}{}{}, {}, \"{}\"]\n".format(name, name_in_dgo, jak3_file[2], dgo_set, jak3_file[4]))
    else:
      json_file.write("[{}{}{}, {}, \"{}\"],\n".format(name, name_in_dgo, jak3_file[2], dgo_set, jak3_file[4]))
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
