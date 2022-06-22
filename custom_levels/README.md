# Custom Levels
Disclaimer: custom levels are still in development and are missing most features.


The first three steps are already done for "test zone", so this can be used as a starting point.

# 1: File Setup
To create a custom level, copy the layout of `custom_levels/test-zone`. See `test-zone.jsonc` for information on how to name things. The `.gd` file also contains the level name.

# 2: Modify the engine
Modify `goal_src/engine/level/level-info.gc` to add level info for each custom level. There is level info for `test-zone` at the bottom that can be used as an example.

# 3: Modify the build system
Modify `goal_src/game.gp` and add a custom level target:
```lisp
(build-custom-level "test-zone")
;; the DGO file
(custom-level-cgo "TESTZONE.DGO" "test-zone/testzone.gd")
```

# 4: Export the GLTF file from blender.
For now, all meshes are displayed and treated as ground collision. This causes buggy collision because walls shouldn't use "floor" mode.

Blender will create a `.glb` file, which must have the name specified in the `.jsonc` file and should be located in `custom_level/your_level`

# 5: Rebuild the game
Any time the `.glb` file is changed, you must rebuild the game. Launch the compiler (`goalc`) and run `(mi)` to rebuild everything. It's recommended to leave the compiler open - it will remember files that haven't changed and skip rebuilding them.

# 6: Go to the custom level
Start the game in debug mode `gk`.

In the compiler window, run `(lt)` to connect to the game. You must run this again every time you restart the game.  If this doesn't work, there could be a firewall issue and you must allow goalc/gk to use the network. They don't make any outside connections.

In the compiler window, run a command like `(bg-custom 'test-zone-vis)` to load and start at a custom level.


