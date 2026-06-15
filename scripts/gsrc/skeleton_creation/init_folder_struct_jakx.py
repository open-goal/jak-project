# Reduces work and tries to maintain consistency by placing objects in the same folder
# as they were in the previous game.
jak3_files = None
jakx_files = None

import json

with open("./goal_src/jak3/build/all_objs.json", "r") as f:
    jak3_files = json.load(f)
with open("./goal_src/jakx/build/all_objs.json", "r") as f:
    jakx_files = json.load(f)

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
    "shield-sphere": "common-obs",
    "math-fx": "math",
    "sound-info": "sound",
    "view-h": "gfx",
    "car-info-h": "vehicle",
    "car-tables": "vehicle",
    "viewport-h": "gfx",
    "sparks-h": "gfx",
    "view": "gfx",
    "viewport": "gfx",
    "car-textures": "vehicle",
    "advanced-options": "util",
    "scert-1-h": "net",
    "scert-2-h": "net",
    "scert-3-h": "net",
    "scert-4-h": "net",
    "scert-5-h": "net",
    "scert-6-h": "net",
    "scert-7-h": "net",
    "scert-8-h": "net",
    "scert-9-h": "net",
    "scert-10-h": "net",
    "scert-11-h": "net",
    "scert-funcs": "net",
    "mem-buffer-h": "util",
    "net-mgr-h": "net",
    "obj-list": "util",
    "process-nettable-h": "process-drawable",
    "entity-more-perm": "entity",
    "fmv-player-h": "scene",
    "hflip": "gfx",
    "sparticle-subsampler": "gfx/sprite/particles",
    "lobby-dma": "dma",
    "movie-path": "scene",
    "sparks": "gfx",
    "mem-buffer": "util",
    "dynamic-mem": "util",
    "headset-h": "ps2",
    "stream-media-h": "util",
    "net-process-mgr-h": "net",
    "statistics": "util",
    "net-mgr-medius-cache-h": "net",
    "net-mgr-medius-players-h": "net",
    "net-mgr-medius-clans-h": "net",
    "net-mgr-medius-games-h": "net",
    "net-predict-h": "net",
    "rigid-body-surface-h": "physics",
    "vehicle-h": "vehicle",
    "race-ai-tuning-h": "vehicle",
    "race-line-h": "vehicle",
    "race-h": "vehicle",
    "race-mesh-h": "vehicle",
    "race-control": "vehicle",
    "wvehicle-weapons-h": "vehicle",
    "wvehicle-h": "vehicle",
    "net-player-h": "net",
    "net-world-h": "net",
    "net-logging-h": "net",
    "net-powerup-h": "net",
    "net-game-mgr-h": "net",
    "net-race-h": "net",
    "net-game-modes-h": "net",
    "net-game-modes2-h": "net",
    "net-simple-destruct-h": "net",
    "net-hud-h": "net",
    "net-util-h": "net",
    "net-proxy-h": "net",
    "net-projectile-h": "net",
    "net-time-trial-h": "net",
    "net-eco-h": "net",
    "menu2-h": "ui",
    "menu2-lists": "ui",
    "keyboard": "ps2",
    "lobby-menu-manager-h": "net",
    "hostnames": "net",
    "net-mgr-async": "net",
    "net-mgr-chat": "net",
    "net-mgr-sysmsg": "net",
    "net-mgr": "net",
    "net-mgr-dme": "net",
    "net-aux-voice": "net",
    "net-mgr-medius": "net",
    "net-mgr-muis": "net",
    "net-mgr-medius-cache": "net",
    "net-mgr-medius-players": "net",
    "net-mgr-medius-buddies": "net",
    "net-mgr-medius-clans": "net",
    "net-mgr-medius-ladders": "net",
    "net-mgr-medius-rooms": "net",
    "net-mgr-medius-games": "net",
    "lobby-ghost": "net",
    "net-mgr-mgcl": "net",
    "net-mgr-playback": "net",
    "net-colarb": "net",
    "net-init": "net",
    "net-start": "net",
    "net-process-mgr": "net",
    "net-http": "net",
    "capture": "util",
    "fmv-player": "scene",
    "dynamic-patch": "util",
    "process-nettable": "process-drawable",
    "net-player": "net",
    "net-util": "net",
    "net-predict": "net",
    "net-logging": "net",
    "net-world": "net",
    "stream-media": "util",
    "net-projectile": "net",
    "udp-layer": "net",
    "headset": "ps2",
    "rigid-body-debug": "physics",
    "rigid-body-surface": "physics",
    "rigid-body-object": "physics",
    "spartacus": "util",
    "attackable-hash": "target",
    "helmet": "common-obs",
    "driver": "common-obs/driver",
    "driver-jak": "common-obs/driver",
    "driver-ashelin": "common-obs/driver",
    "driver-razer": "common-obs/driver",
    "driver-klever": "common-obs/driver",
    "driver-kiera": "common-obs/driver",
    "driver-thug-a": "common-obs/driver",
    "driver-thug-b": "common-obs/driver",
    "driver-thug-c": "common-obs/driver",
    "driver-taryn": "common-obs/driver",
    "driver-torn": "common-obs/driver",
    "driver-sig": "common-obs/driver",
    "driver-ur-86": "common-obs/driver",
    "driver-kaeden": "common-obs/driver",
    "driver-rayn": "common-obs/driver",
    "driver-ratchet": "common-obs/driver",
    "driver-jaka": "common-obs/driver",
    "driver-jakb": "common-obs/driver",
    "driver-jakc": "common-obs/driver",
    "driver-daxter": "common-obs/driver",
    "driver-gtblitz": "common-obs/driver",
    "driver-pecker": "common-obs/driver",
    "driver-ximon": "common-obs/driver",
    "driver-osmo": "common-obs/driver",
    "speech-jak": "sound/speech",
    "speech-daxter": "sound/speech",
    "speech-pecker": "sound/speech",
    "speech-gtblitz": "sound/speech",
    "speech-mizo": "sound/speech",
    "vehicle-part": "vehicle",
    "vehicle-debris": "vehicle",
    "vehicle-effects": "vehicle",
    "vehicle": "vehicle",
    "wcar-skel-template": "vehicle",
    "vehicle-util": "vehicle",
    "vehicle-physics": "vehicle",
    "vehicle-states": "vehicle",
    "vehicle-manager": "vehicle",
    "vehicle-hud": "vehicle",
    "vehicle-net": "vehicle",
    "construction-obs-h": "common-obs",
    "daxter": "common-obs",
    "wvehicle-weapons-debug": "vehicle",
    "spartacus-editor": "util",
}

path_overrides = {
    "jungle-shared": "levels/haven/jungle",
    "garage-obs": "levels/garage",
    "garage-part": "levels/garage",
}


def level_name(file_meta):
    dgos = file_meta[3]
    # Handle files unique to one level
    if (
        dgos == ["DESA"]
        or dgos == ["DSR", "DSRX"]
        or dgos == ["DESCLCT"]
        or dgos == ["DESARENS"]
        or dgos == ["DESRAPT"]
        or dgos == ["DSR"]
        or dgos == ["DESART"]
        or dgos == ["DSRX"]
        or dgos == ["DESHUNT2"]
        or dgos == ["DESACTF"]
    ):
        return "levels/spargus/desert"
    elif (
        dgos == ["SPARGUSS"]
        or dgos == ["SPARTEMS"]
        or dgos == ["SPARTEMW"]
        or dgos == ["SPB"]
        or dgos == ["SPARGUSW"]
        or dgos == ["SPB", "SPX", "STY"]
        or dgos == ["SPY"]
    ):
        return "levels/spargus/city"
    elif (
        dgos == ["SPATOURS"]
        or dgos == ["SPATOURW"]
        or dgos == ["SPATTBOX"]
        or dgos == ["SPTMTT"]
        or dgos == ["SPARTT"]
        or dgos == ["SPTRTT"]
    ):
        return "levels/spargus/tour"
    elif (
        dgos == ["WOMBL"]
        or dgos == ["CHEEL"]
        or dgos == ["COUGL"]
        or dgos == ["THCVL", "WOMBL"]
        or dgos == ["GILAL"]
        or dgos == ["UR8LEV"]
        or dgos == ["TORVL3", "WOLFL"]
        or dgos == ["BOBCL"]
        or dgos == ["ASHLEV"]
        or dgos == ["BOBCL", "UR8VL2"]
        or dgos == ["DAXLEV"]
        or dgos == ["DAXTL"]
        or dgos == ["BOBCL", "KIEVL", "PECVL", "UR8VL2"]
        or dgos == ["XIMLEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["WOLFL"]
        or dgos == ["BOBCL", "PECVL"]
        or dgos == ["BEARL"]
        or dgos == ["UR8VL3", "WOLFL"]
        or dgos == ["FALCL"]
        or dgos == ["ASHVL3", "THCVL", "WOMBL"]
        or dgos == ["FALCL", "JAKVL"]
        or dgos == ["MONGL"]
        or dgos == ["TURTL"]
        or dgos == ["FALCL", "THCVL2"]
        or dgos == ["RAYVL", "UR8VL3", "WOLFL"]
        or dgos == ["PANTL"]
        or dgos == ["TOADL"]
        or dgos == ["POSSL"]
        or dgos == ["FOXL"]
        or dgos == ["KLEVL", "PANTL"]
        or dgos == ["LEOPL"]
        or dgos == ["SIGLEV"]
        or dgos == ["TIGEL"]
        or dgos == ["RAYVL2", "TIGEL"]
        or dgos == ["COUGL", "SIGVL2"]
        or dgos == ["LEOPL", "SIGVL"]
        or dgos == ["BOBCL", "KIEVL"]
        or dgos == ["COUGL", "THBVL"]
        or dgos == ["ASHVL3", "WOMBL"]
        or dgos == ["PANTL", "THAVL2"]
        or dgos == ["ASHVL", "RAYVL2", "TIGEL", "TORVL"]
        or dgos == ["CHEEL", "RAZVL"]
        or dgos == ["POSSL", "TORVL2"]
        or dgos == ["CHEEL", "THCVL3"]
        or dgos
        == [
            "ASHVL",
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "THBVL2",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["POSSL", "THAVL"]
        or dgos == ["GILAL", "THAVL3"]
        or dgos == ["ASHVL"]
        or dgos == ["BEARL", "KLEVL2", "SIGVL3", "UR8VL"]
        or dgos == ["GILAL", "THBVL3"]
        or dgos == ["RAYVL", "WOLFL"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "KIEVL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "RAYVL2",
            "SNAKL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["ASHVL2", "FALCL", "GTBVL", "JAKVL", "THCVL2"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "THAVL",
            "TIGEL",
            "TORVL2",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["BEARL", "UR8VL"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "PECVL",
            "POSSL",
            "SNAKL",
            "THCVL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["ASHVL2"]
        or dgos == ["ASHVL3"]
        or dgos == ["ASHVL2", "FALCL"]
        or dgos
        == [
            "ASHVL3",
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "TIGEL",
            "TORVL3",
            "WOLFL",
            "WOMBL",
        ]
        or dgos
        == [
            "ASHVL2",
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "THCVL3",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["BEARL", "SIGVL3"]
        or dgos == ["TIGEL", "TORVL"]
        or dgos == ["BEARL", "KLEVL2"]
        or dgos == ["FALCL", "GTBVL"]
        or dgos == ["LEOPL", "THBVL2"]
        or dgos == ["FALCL", "GTBVL", "JAKVL"]
        or dgos == ["ASHVL", "TIGEL"]
        or dgos == ["SNAKL"]
        or dgos == ["KAELEV"]
        or dgos == ["THALEV"]
        or dgos == ["KIELEV"]
        or dgos == ["TARLEV"]
        or dgos == ["KLELEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "THAVL2",
            "THAVL3",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["RAZLEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "KLEVL2",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["OSMLEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SIGVL3",
            "SNAKL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["RAYLEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "GTBVL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "RAZVL",
            "SNAKL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["TORLEV"]
        or dgos == ["PECLEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SIGVL",
            "SNAKL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["RATLEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "KLEVL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "RAYVL",
            "SNAKL",
            "TIGEL",
            "UR8VL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["JKCLEV"]
        or dgos == ["THBLEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "JAKVL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "TIGEL",
            "TORVL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["JKALEV"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "THBVL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["THCLEV"]
        or dgos == ["THBVL2"]
        or dgos == ["THBVL3"]
        or dgos == ["KIEVL"]
        or dgos == ["THBVL"]
        or dgos == ["THCVL"]
        or dgos == ["JAKVL"]
        or dgos == ["LEOPL", "SIGVL", "THBVL2"]
        or dgos == ["THAVL3"]
        or dgos == ["POSSL", "THAVL", "TORVL2"]
        or dgos == ["THCVL2"]
        or dgos == ["RAYVL", "TORVL3", "UR8VL3", "WOLFL"]
        or dgos == ["JAKLEV"]
        or dgos == ["RAYVL"]
        or dgos == ["THAVL2"]
        or dgos == ["RAYVL2"]
        or dgos == ["THCVL3"]
        or dgos == ["RAZVL"]
        or dgos == ["KLEVL", "PANTL", "THAVL2"]
        or dgos == ["TORVL"]
        or dgos == ["SIGVL"]
        or dgos == ["KLEVL"]
        or dgos == ["SIGVL2"]
        or dgos == ["FALCL", "GTBVL", "THCVL2"]
        or dgos == ["THAVL"]
        or dgos == ["TORVL3"]
        or dgos == ["TORVL2"]
        or dgos == ["SIGVL3"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SIGVL2",
            "SNAKL",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["UR8VL"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "TIGEL",
            "UR8VL2",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["KLEVL2"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "THCVL2",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["UR8VL2"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "TIGEL",
            "UR8VL3",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["GILAL", "THAVL3", "THBVL3"]
        or dgos == ["COUGL", "SIGVL2", "THBVL"]
        or dgos
        == [
            "BEARL",
            "BOBCL",
            "CHEEL",
            "COUGL",
            "DAXTL",
            "FALCL",
            "GILAL",
            "LEOPL",
            "MONGL",
            "PANTL",
            "POSSL",
            "SNAKL",
            "THBVL3",
            "TIGEL",
            "WOLFL",
            "WOMBL",
        ]
        or dgos == ["UR8VL3"]
        or dgos == ["BOBCL", "PECVL", "UR8VL2"]
        or dgos == ["PECVL"]
        or dgos == ["GTBVL"]
        or dgos == ["CHEEL", "RAZVL", "THCVL3"]
        or dgos == ["JKBLEV"]
        or dgos == ["GTBLEV"]
        or dgos == ["CHEEL", "RAZVL", "THCVL3"]
    ):
        return "engine/sound/speech"
    elif (
        dgos == ["COL", "COLX"]
        or dgos == ["COLICTF"]
        or dgos == ["COL"]
        or dgos == ["COLIREV"]
        or dgos == ["COLISEUS"]
        or dgos == ["COLICLCT"]
        or dgos == ["COLX"]
        or dgos == ["COL", "COLISEUS"]
        or dgos == ["COLART"]
        or dgos == ["COLICLCT", "COLIREV", "DESCLCT", "DESREV", "KCRSCLCT"]
    ):
        return "levels/kras/coliseum"
    elif (
        dgos == ["RTH", "RUSTYH"]
        or dgos == ["RUSTYH"]
        or dgos == ["RTH"]
        or dgos == ["BRDROOM", "BRDROOMF"]
        or dgos == ["BRDROOMF"]
    ):
        return "levels/kras/rusty-hook"  # aka bloody hook
    elif (
        dgos == ["KRASS"]
        or dgos == ["KRA"]
        or dgos == ["KRASTRN"]
        or dgos == ["KRASFOOT"]
        or dgos == ["KRASTT"]
        or dgos == ["KRX"]
        or dgos == ["KRB", "KRX"]
        or dgos == ["KRB"]
        or dgos == ["KRASS", "KRASW"]
        or dgos == ["KRY"]
        or dgos == ["KRC"]
        or dgos == ["DRDKTBOX"]
        or dgos == ["KRASW"]
        or dgos == ["DRDKFOOT"]
        or dgos == ["DKKX", "KRA", "KRX"]
    ):
        return "levels/kras"
    elif (
        dgos == ["KCRX"]
        or dgos == ["KCRSPLOW"]
        or dgos == ["KCROSCTF"]
        or dgos == ["KCRSCLCT"]
        or dgos == ["KCR"]
        or dgos == ["KCROSSS"]
        or dgos == ["KCR", "KCRX"]
        or dgos == ["KCROSART"]
    ):
        return "levels/kras/cross"
    elif (
        dgos == ["KRATOURS"]
        or dgos == ["KRASTBOX"]
        or dgos == ["KRATFOOT"]
        or dgos == ["KRATOURW"]
        or dgos == ["KRATTBOX"]
        or dgos == ["KRTRTT"]
    ):
        return "levels/kras/tour"
    elif (
        dgos == ["DROMDOCS"]
        or dgos == ["DROMES"]
        or dgos == ["DROMEX"]
        or dgos == ["DROMDOCW"]
        or dgos == ["DRC"]
        or dgos == ["DRD"]
        or dgos == ["DRDX"]
        or dgos == ["DRDY"]
        or dgos == ["DRX"]
        or dgos == ["DROMFOOT"]
        or dgos == ["DRB"]
        or dgos == ["DRY"]
        or dgos == ["DROMETT"]
        or dgos == ["DROMTBOX"]
        or dgos == ["DRA"]
        or dgos == ["DRDKTT"]
        or dgos == ["DROMEW"]
        or dgos == ["DRB", "DRDY", "DROMEX", "DRX"]
        or dgos == ["DRDX", "DROMDOCW", "DROMEW", "DROMEX", "DRX", "KRATOURW"]
    ):
        return "levels/kras/dethdrome"
    elif (
        dgos == ["STX"]
        or dgos == ["STY"]
        or dgos == ["SPA"]
        or dgos == ["SPARFOOT"]
        or dgos == ["SPATFOOT"]
        or dgos == ["SPX"]
        or dgos == ["SBWLCTF"]
        or dgos == ["SPC"]
        or dgos == ["SPTMTBOX"]
        or dgos == ["SPD"]
        or dgos == ["SPRGSTBX"]
        or dgos == ["SPE"]
        or dgos == ["DESREV"]
        or dgos == ["SPARTEMW", "SPATOURW", "STX", "TEMPLEW", "TPX"]
        or dgos == ["SPTMFOOT"]
        or dgos == ["SPARGUSW", "SPARTEMW", "SPATOURW", "SPX", "STX"]
        or dgos == ["CANSPARW", "CSX", "SPARGUSW", "SPARTEMW", "SPX", "STX"]
    ):
        return "levels/spargus"
    elif (
        dgos == ["TEMPLES"]
        or dgos == ["TPY"]
        or dgos == ["TPE"]
        or dgos == ["TPC"]
        or dgos == ["TEMPLETT"]
        or dgos == ["TPX"]
        or dgos == ["TEMPLEW"]
        or dgos == ["TPB"]
        or dgos == ["TPD"]
        or dgos == ["TEMPFOOT"]
        or dgos == ["TPA"]
        or dgos == ["TEMPTBOX"]
    ):
        return "levels/spargus/temple"
    elif (
        dgos == ["CYA"]
        or dgos == ["CANYONTT"]
        or dgos == ["CANSPARW", "CSX"]
        or dgos == ["CANYONS"]
        or dgos == ["CNSPTT"]
        or dgos == ["CANYONW", "CYX", "SPATOURW"]
        or dgos == ["CYY"]
        or dgos == ["CYX"]
        or dgos == ["CANSPARW"]
        or dgos == ["CANYONW"]
        or dgos == ["CANSPARW", "CANYONW", "CSX", "CYX", "SPATOURW"]
        or dgos == ["CANSPARS"]
        or dgos == ["CANFOOT"]
        or dgos == ["CYD"]
        or dgos == ["CANTBOX"]
        or dgos == ["CYB"]
        or dgos == ["CNSPTBOX"]
        or dgos == ["CSX"]
        or dgos == ["CSY"]
        or dgos == ["CYC"]
        or dgos == ["CYE"]
        or dgos == ["CNSPFOOT"]
    ):
        return "levels/spargus/canyon"
    elif (
        dgos == ["DSI"]
        or dgos == ["DESISLES"]
        or dgos == ["DSI", "DSX"]
        or dgos == ["DISLEART"]
        or dgos == ["DSX"]
        or dgos == ["DISLECTF"]
    ):
        return "levels/spargus/isle"
    elif (
        dgos == ["HAVENS"]
        or dgos == ["HVD"]
        or dgos == ["HVY"]
        or dgos == ["HVX"]
        or dgos == ["HAVNFOOT"]
        or dgos == ["HAVENS", "HAVSEWS", "HAVTOURS"]
        or dgos == ["HVA"]
        or dgos == ["HAVENW", "HVX"]
        or dgos == ["HAVNTBOX"]
        or dgos == ["HAVENW", "HAVSEWW", "HAVTOURW", "HSX", "HVX"]
        or dgos == ["HVB"]
        or dgos == ["HAVENW", "HAVJUNGW", "HAVTOURW", "HJX", "HVX", "JGX", "JUNGLEW"]
        or dgos == ["HVC"]
        or dgos == ["HAVSEWW", "HAVTOURW", "HSX", "SEWERW", "SWX"]
        or dgos == ["HAVENS", "HAVJUNGS", "HAVTOURS"]
        or dgos == ["HAVTOURW", "JGX", "JUNGLEW"]
        or dgos == ["HAVENW", "HAVJUNGW", "HAVSEWW", "HAVTOURW", "HJX", "HSX", "HVX"]
        or dgos == ["HVE"]
        or dgos == ["HAVJUNGW", "HJX"]
        or dgos == ["HAVENW"]
        or dgos == ["HAVJUNGW", "HAVTOURW", "HJX"]
    ):
        return "levels/haven"
    elif (
        dgos == ["DOCKKRAS"]
        or dgos == ["DKKRFOOT"]
        or dgos == ["DKKRTBOX"]
        or dgos == ["DKX"]
        or dgos == ["DOCKSS"]
        or dgos == ["DOCKKRAW"]
        or dgos == ["DKA"]
        or dgos == ["DOCKSW"]
        or dgos == ["DKB"]
        or dgos == ["DOCKSTT"]
        or dgos == ["DKC"]
        or dgos == ["DKB", "DKKY", "DKX", "DRDX"]
        or dgos == ["DKE"]
        or dgos == ["DKA", "DKE", "DKKY", "DRA", "DRDY", "DROMEX", "DRX"]
        or dgos == ["DKKY"]
        or dgos == ["DKKRTT"]
        or dgos == ["DOCKFOOT"]
        or dgos == ["DKKX", "DOCKKRAW", "KRA", "KRASW", "KRATOURW", "KRC", "KRX", "KRY"]
        or dgos == ["DKKX"]
        or dgos
        == [
            "DKKX",
            "DKX",
            "DOCKKRAW",
            "DOCKSW",
            "DRDX",
            "DROMEW",
            "DROMEX",
            "DRX",
            "KRATOURW",
        ]
        or dgos == ["DOCKTBOX"]
        or dgos == ["DKD"]
        or dgos == ["DKKX", "DOCKKRAW", "KRASW", "KRATOURW", "KRX"]
        or dgos == ["DKKX", "DKX", "DOCKKRAW", "DOCKSW", "DRDX", "DROMDOCW", "KRATOURW"]
    ):
        return "levels/kras/docks"
    elif (
        dgos == ["JUNGLES"]
        or dgos == ["HJX", "JGA", "JGC", "JGD", "JGE", "JGG", "JGX", "JGY"]
        or dgos == ["HAVJUNGS"]
        or dgos == ["HJX"]
        or dgos == ["JUNGFOOT"]
        or dgos == ["HVJGTBOX"]
        or dgos == ["HJY"]
        or dgos == ["HAVJUNGW"]
        or dgos == ["JGA"]
        or dgos == ["JUNGLEW"]
        or dgos == ["JGE"]
        or dgos == ["JUNGTBOX"]
        or dgos == ["JGF"]
        or dgos == ["HAVJUNGW", "HAVTOURW", "HJX", "JGX", "JUNGLEW"]
        or dgos == ["JGB"]
        or dgos == ["JGC"]
        or dgos == ["JGX"]
        or dgos == ["JGD"]
        or dgos == ["JGG"]
        or dgos == ["HJX", "JGA", "JGE", "JGX", "JGY"]
        or dgos == ["JGY"]
        or dgos == ["HJNGFOOT"]
        or dgos == ["JUNGLETT"]
        or dgos == ["JUNGTRN"]
        or dgos == ["JGX", "JUNGLEW"]
        or dgos == ["HVJGTT"]
    ):
        return "levels/haven/jungle"
    elif (
        dgos == ["HAVTOURS"]
        or dgos == ["HAVTFOOT"]
        or dgos == ["HAVTOURW"]
        or dgos == ["HAVJUNGS", "HAVTOURS", "JUNGLES"]
        or dgos == ["HAVTTBOX"]
        or dgos == ["HVTRTT"]
        or dgos == ["HAVTT"]
    ):
        return "levels/haven/tour"
    elif (
        dgos == ["HAVSEWS"]
        or dgos == ["SWA"]
        or dgos == ["HAVSEWW", "HSX"]
        or dgos == ["SEWERS"]
        or dgos == ["SEWERTT"]
        or dgos == ["SEWERW"]
        or dgos == ["HVSWTT"]
        or dgos == ["HAVSEWW"]
        or dgos == ["HSX"]
        or dgos == ["HVSWTBOX"]
        or dgos == ["SWD"]
        or dgos == ["HVSWFOOT"]
        or dgos == ["SEWTBOX"]
        or dgos == ["SWB"]
        or dgos == ["HSY"]
        or dgos == ["SWC"]
        or dgos == ["SEWFOOT"]
        or dgos == ["SWF"]
        or dgos == ["SWE"]
        or dgos == ["S3A"]
        or dgos == ["SWX"]
        or dgos == ["S2A"]
    ):
        return "levels/haven/sewers"
    elif dgos == ["ICETOURS"] or dgos == ["ICETOURW"] or dgos == ["ICBGTT"]:
        return "levels/icelands/tour"
    elif (
        dgos == ["PEAKS"]
        or dgos == ["PKD"]
        or dgos == ["PEAKTT"]
        or dgos == ["PKX"]
        or dgos == ["PKD", "PKX"]
        or dgos == ["PEAKW"]
        or dgos == ["PEAKFOOT"]
        or dgos == ["PKA"]
        or dgos == ["PEAKTBOX"]
        or dgos == ["PKB"]
        or dgos == ["PKC"]
        or dgos == ["PKE"]
    ):
        return "levels/icelands/peak"
    elif (
        dgos == ["ICEBERGS"]
        or dgos == ["ICEPASSS"]
        or dgos == ["ICES"]
        or dgos == ["SNO"]
        or dgos == ["SNOWTBOX"]
        or dgos == ["SNOWTRN2"]
        or dgos == ["SNOX"]
        or dgos == ["SNOWFOOT"]
        or dgos == ["SNOWS"]
        or dgos == ["SNW", "SNWX"]
        or dgos == ["SNW"]
        or dgos == ["ICETTBOX"]
        or dgos == ["IPY"]
        or dgos == ["ICB"]
        or dgos == ["ICETRN"]
        or dgos == ["ICY"]
        or dgos == ["SNWX"]
        or dgos == ["ICETFOOT"]
        or dgos == ["ICETBOX"]
        or dgos == ["ICX"]
        or dgos == ["ICA"]
        or dgos == ["ICC"]
        or dgos == ["ICEPFOOT"]
        or dgos == ["ICEPTBOX"]
        or dgos == ["IBX"]
        or dgos == ["ICEFOOT"]
        or dgos == ["ICD"]
        or dgos == ["ICEBTBOX"]
        or dgos == ["SNOBOWLS"]
        or dgos == ["IPX"]
        or dgos == ["ICTRTT"]
        or dgos == ["SNOBART"]
        or dgos == ["SNOWTT"]
        or dgos == ["ICPSTT"]
        or dgos == ["SNO", "SNOX"]
        or dgos == ["ICEBFOOT"]
        or dgos == ["SNO", "SNW"]
        or dgos == ["ICEW"]
        or dgos == ["ICES", "ICETOURS", "PEAKS"]
        or dgos == ["ICEPASSW"]
        or dgos == ["ICETT"]
        or dgos
        == [
            "IBX",
            "ICEBERGW",
            "ICEPASSW",
            "ICETOURW",
            "ICEW",
            "ICX",
            "IPX",
            "PEAKW",
            "PKX",
        ]
        or dgos == ["IBY"]
        or dgos
        == ["IBX", "IPX", "PKA", "PKB", "PKC", "PKX", "SNO", "SNOX", "SNW", "SNWX"]
        or dgos == ["ICETOURW", "ICEW", "ICX"]
        or dgos == ["ICEBERGW"]
        or dgos == ["ICA", "ICB", "ICX", "IPY"]
        or dgos == ["ICETOURW", "ICEW", "ICX", "PEAKW", "PKX"]
    ):
        return "levels/icelands"
    elif (
        dgos == ["GAME", "COMMON"]
        or dgos == ["COMMON", "GAME"]
        or dgos
        == [
            "DKKX",
            "DOCKKRAW",
            "HAVENW",
            "HAVJUNGW",
            "HAVSEWW",
            "HAVTOURW",
            "HJX",
            "HSX",
            "HVX",
            "KCR",
            "KCRX",
            "KRASW",
            "KRX",
        ]
        or dgos == ["DETHRACE"]
        or dgos == ["CLF", "CLFX", "SNO", "SNOX"]
        or dgos
        == [
            "DKKX",
            "DKX",
            "DOCKKRAW",
            "DOCKSW",
            "DRDX",
            "DROMDOCW",
            "HAVENW",
            "HAVSEWW",
            "HSX",
            "HVX",
        ]
        or dgos == ["CLIFFSS", "DESARENS", "EIGHT", "KRASS"]
        or dgos == ["ART", "GAME"]
        or dgos == ["CLIFTRN", "DRONE", "ICETRN", "JUNGTRN", "KRASTRN", "SNOWTRN2"]
        or dgos == ["ATOPLOW", "CLIFHUNT", "DESHUNT", "DESHUNT2", "DESRAPT", "KCRSPLOW"]
        or dgos == ["DESHUNT"]
        or dgos == ["RACEWEAP"]
        or dgos
        == [
            "ATOLLCTF",
            "CANFOOT",
            "CLIFCTF",
            "CNSPFOOT",
            "COLICTF",
            "DESACTF",
            "DISLECTF",
            "DKKRFOOT",
            "DOCKFOOT",
            "DRDKFOOT",
            "DROMFOOT",
            "HAVNFOOT",
            "HAVTFOOT",
            "HJNGFOOT",
            "HVSWFOOT",
            "ICEBFOOT",
            "ICEFOOT",
            "ICEPFOOT",
            "ICETFOOT",
            "JUNGFOOT",
            "KCROSCTF",
            "KRASFOOT",
            "KRATFOOT",
            "PEAKFOOT",
            "SBWLCTF",
            "SEWFOOT",
            "SNOWFOOT",
            "SPARFOOT",
            "SPATFOOT",
            "SPTMFOOT",
            "TEMPFOOT",
        ]
        or dgos == ["HAVENS", "KRASS"]
        or dgos == ["CLIFTRN", "ICETRN", "JUNGTRN", "KRASTRN", "SNOWTRN2"]
        or dgos
        == [
            "CLF",
            "CLFX",
            "IBX",
            "ICEBERGW",
            "ICEPASSW",
            "ICETOURW",
            "IPX",
            "PEAKW",
            "PKX",
            "SNO",
            "SNOX",
            "SNW",
            "SNWX",
        ]
        or dgos == ["CLIFHUNT", "DESHUNT", "DESHUNT2", "DESRAPT"]
        or dgos == ["HAVTOURS", "ICETOURS", "ICETRN", "KRATOURS", "SPATOURS"]
        or dgos == ["CLF", "CLFX", "SNO", "SNOX", "SNW", "SNWX"]
        or dgos
        == [
            "CANFOOT",
            "CNSPFOOT",
            "DKKRFOOT",
            "DOCKFOOT",
            "DRDKFOOT",
            "DROMFOOT",
            "HAVNFOOT",
            "HAVTFOOT",
            "HJNGFOOT",
            "HVSWFOOT",
            "ICEBFOOT",
            "ICEFOOT",
            "ICEPFOOT",
            "ICETFOOT",
            "JUNGFOOT",
            "KRASFOOT",
            "KRATFOOT",
            "PEAKFOOT",
            "RACEWEAP",
            "SEWFOOT",
            "SNOWFOOT",
            "SPARFOOT",
            "SPATFOOT",
            "SPTMFOOT",
            "TEMPFOOT",
        ]
        or dgos == ["DRONE"]
        or dgos == ["ATOPLOW", "KCRSPLOW"]
        or dgos
        == [
            "CANFOOT",
            "CNSPFOOT",
            "DKKRFOOT",
            "DOCKFOOT",
            "DRDKFOOT",
            "DROMFOOT",
            "HAVNFOOT",
            "HAVTFOOT",
            "HJNGFOOT",
            "HVSWFOOT",
            "ICEBFOOT",
            "ICEFOOT",
            "ICEPFOOT",
            "ICETFOOT",
            "JUNGFOOT",
            "KRASFOOT",
            "KRATFOOT",
            "PEAKFOOT",
            "SEWFOOT",
            "SNOWFOOT",
            "SPARFOOT",
            "SPATFOOT",
            "SPTMFOOT",
            "TEMPFOOT",
        ]
        or dgos
        == [
            "ATOLLART",
            "CLIFFART",
            "COLART",
            "DESART",
            "DISLEART",
            "KCROSART",
            "SNOBART",
        ]
        or dgos
        == [
            "ATOLLCTF",
            "CLIFCTF",
            "COLICTF",
            "DESACTF",
            "DISLECTF",
            "KCROSCTF",
            "SBWLCTF",
        ]
        or dgos
        == [
            "CANTBOX",
            "CNSPTBOX",
            "DKKRTBOX",
            "DOCKTBOX",
            "DRDKTBOX",
            "DROMTBOX",
            "HAVNTBOX",
            "HAVTTBOX",
            "HVJGTBOX",
            "HVSWTBOX",
            "ICEBTBOX",
            "ICEPTBOX",
            "ICETBOX",
            "ICETTBOX",
            "JUNGTBOX",
            "KRASTBOX",
            "KRATTBOX",
            "PEAKTBOX",
            "SEWTBOX",
            "SNOWTBOX",
            "SPATTBOX",
            "SPRGSTBX",
            "SPTMTBOX",
            "TEMPTBOX",
        ]
        or dgos == ["CARS"]
        or dgos
        == [
            "CANYONTT",
            "CNSPTT",
            "DKKRTT",
            "DOCKSTT",
            "DRDKTT",
            "DROMETT",
            "HAVTT",
            "HVJGTT",
            "HVSWTT",
            "HVTRTT",
            "ICBGTT",
            "ICETT",
            "ICPSTT",
            "ICTRTT",
            "JUNGLETT",
            "KRASTT",
            "KRTRTT",
            "PEAKTT",
            "SEWERTT",
            "SNOWTT",
            "SPARTT",
            "SPTMTT",
            "SPTRTT",
            "TEMPLETT",
        ]
        or dgos == ["EIGHT"]
        or dgos == ["EIGHTB"]
        or dgos == ["FMVLEV"]
        or dgos == ["DRONE", "KRASTRN", "SNOWTRN2"]
    ):
        return "levels/common"
    elif (
        dgos == ["JAKCRED"]
        or dgos == ["PECCRED"]
        or dgos == ["KEICRED"]
        or dgos == ["RAYCRED"]
        or dgos == ["TORCRED"]
        or dgos == ["ASHCRED"]
        or dgos == ["THCCRED"]
        or dgos == ["RAZCRED"]
        or dgos == ["THBCRED"]
        or dgos == ["DAXCRED"]
        or dgos == ["SIGCRED"]
        or dgos == ["KLECRED"]
        or dgos == ["UR8CRED"]
        or dgos == ["THACRED"]
        or dgos == ["GTBCRED"]
        or dgos == ["CREDITS"]
    ):
        return "levels/credits"
    elif dgos == ["GARAGE"] or dgos == ["GARAGEB"]:
        return "levels/garage"
    elif (
        dgos == ["MENU2"]
        or dgos == ["GARAGEB", "MENU2"]
        or dgos == ["GARAGE", "GARAGEB", "MENU2"]
    ):
        return "engine/ui/menu"
    elif dgos == ["MENUMAP"] or dgos == ["GARAGE", "MENU2"]:
        return "engine/ui/menu/map"
    elif (
        dgos == ["ATL", "ATX"]
        or dgos == ["ATL"]
        or dgos == ["ATX"]
        or dgos == ["ATOPLOW"]
        or dgos == ["ATOLLCTF"]
        or dgos == ["ATOLLART"]
        or dgos == ["ATOLLS"]
    ):
        return "levels/haven/atoll"
    elif (
        dgos == ["CLF"]
        or dgos == ["CLIFCTF"]
        or dgos == ["CLIFFSS"]
        or dgos == ["CLIFFART"]
        or dgos == ["CLIFHUNT"]
        or dgos == ["CLIFTRN"]
        or dgos == ["CLFX"]
        or dgos == ["CLF", "CLFX"]
    ):
        return "levels/haven/cliffs"


remaining_dgos = {}

folders = {}

for jakx_file in jakx_files:
    if jakx_file[3] == ["NO-XGO"]:
        num_replicated = num_replicated + 1
        continue
    # manual overrides
    if (
        jakx_file[0] in path_overrides
        or jakx_file[0].removesuffix("-h") in path_overrides
    ):
        num_replicated = num_replicated + 1
        if jakx_file[0] in path_overrides:
            jakx_file[4] = path_overrides[jakx_file[0]]
        else:
            jakx_file[4] = path_overrides[jakx_file[0].removesuffix("-h")]
        if jakx_file[4] not in folders:
            folders[jakx_file[4]] = 1
        else:
            folders[jakx_file[4]] = folders[jakx_file[4]] + 1
        continue
    # port over manually specified engine files
    if jakx_file[0] in engine_files or jakx_file[0].removesuffix("-h") in engine_files:
        num_replicated = num_replicated + 1
        if jakx_file[0] in engine_files:
            jakx_file[4] = str.format("engine/{}", engine_files[jakx_file[0]])
        else:
            jakx_file[4] = str.format(
                "engine/{}", engine_files[jakx_file[0].removesuffix("-h")]
            )
        if jakx_file[4] not in folders:
            folders[jakx_file[4]] = 1
        else:
            folders[jakx_file[4]] = folders[jakx_file[4]] + 1
        continue
    # attempt to find the object with the same name in jak 3
    jak3_path = None
    if not jakx_file[0].startswith("tpage"):
        for jak3_file in jak3_files:
            if jak3_file[0] == jakx_file[0]:
                jak3_path = jak3_file[4]
                # if (jak3_path.startswith("levels")):
                #   print("{} - {}".format(jak3_path, jakx_file[0]))
                break
    if jak3_path is not None:
        jakx_file[4] = jak3_path
        num_replicated = num_replicated + 1
    elif level_name(jakx_file):
        jakx_file[4] = level_name(jakx_file)
        num_replicated = num_replicated + 1
    else:
        num_left = num_left + 1
        if ",".join(jakx_file[3]) in remaining_dgos:
            remaining_dgos[",".join(jakx_file[3])] = (
                remaining_dgos[",".join(jakx_file[3])] + 1
            )
        else:
            remaining_dgos[",".join(jakx_file[3])] = 1
    if jakx_file[4] not in folders:
        folders[jakx_file[4]] = 1
    else:
        folders[jakx_file[4]] = folders[jakx_file[4]] + 1

with open("./goal_src/jakx/build/all_objs_TMP.json", "w") as json_file:
    # Calculate these to make the file as compact as possible
    longest_name = 0
    longest_name_in_dgo = 0
    for jakx_file in jakx_files:
        if len(jakx_file[0]) > longest_name:
            longest_name = len(jakx_file[0])
        if len(jakx_file[1]) > longest_name_in_dgo:
            longest_name_in_dgo = len(jakx_file[1])
    # Actually write things out
    json_file.write("[\n")
    i = 0
    for jakx_file in jakx_files:
        name = "{: <{}}".format('"{}",'.format(jakx_file[0]), longest_name + 2)
        name_in_dgo = "{: <{}}".format(
            '"{}",'.format(jakx_file[1]), longest_name_in_dgo + 2
        )
        dgo_set = "["
        for dgo in jakx_file[3]:
            dgo_set += '"{}", '.format(dgo)
        dgo_set = dgo_set.removesuffix(", ")
        dgo_set += "]"
        if i == (len(jakx_files) - 1):
            json_file.write(
                '[{}{}{}, {}, "{}"]\n'.format(
                    name, name_in_dgo, jakx_file[2], dgo_set, jakx_file[4]
                )
            )
        else:
            json_file.write(
                '[{}{}{}, {}, "{}"],\n'.format(
                    name, name_in_dgo, jakx_file[2], dgo_set, jakx_file[4]
                )
            )
        i = i + 1
    json_file.write("]\n")

print("Mapped: {} and Left: {}".format(num_replicated, num_left))

limit = 0
for dgo_set in dict(
    sorted(remaining_dgos.items(), reverse=True, key=lambda item: item[1])
):
    dgo_set_nice = ""
    for dgo in dgo_set.split(","):
        dgo_set_nice += '"{}", '.format(dgo)
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
