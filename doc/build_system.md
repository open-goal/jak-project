# Build System
Game Build System. The build system should allow us to
- automatically extract large asset files from the game so we don't have to keep them in this repository
- support modifications (adding new files, modifying files, etc)
- have checks that the resulting data matches the original if expected, assuming no modifications are applied

Intermediate files should go somewhere in `data` and final files should go in `out`. 

## 1. C++ Build
All C++ code should be built through `cmake`, including the compiler, decompiler, runtime, and any tools. It's expected to be built in the `build` folder.

## 2. Unpack Assets
This step extracts data from asset files from the game. An asset file is any file that's not a CGO/DGO. All files on the DVD will be in `iso_data/`, and all unpacked data should go in `data/`

## 3. Unpack DGOs
This step extracts data from CGO/DGO files and unpacks it. All unpacked data should go in `data/`

## 4. Pack Assets
This step creates final asset files from data in `data/`.  The final files should do in `out`.

## 5. Compile GOAL Code / Data
This step creates all the data for the CGOs/DGOs.  The compiler will have a big list of files which it will compile in order.  Each `.gc` file contains GOAL code, which is compiled to a GOAL code object (`.o` file) stored in `data/obj`.  Each `.gd` file is a "GOAL data description file" which contains a short script to launch the appropriate tool to produce the data object. The objects will also be stored in `data/obj`, but will have a `.go` extension.

## 6. Create CGO/DGOs
This step takes the output from step 5, (stored in `data/obj`) and creates all the DGO/CGO files, which go in `out`.  The list of what goes in the DGOs is in `goal_src/build/dgos.txt`.

## 7. Asset Check
There might be some asset files which we expect to be unchanged between the version in `iso_data` and in `out`. At this point we should check if these are actually the same.

## 8. CGO/DGO check
The data objects contained in CGO/DGO files should be the same between the original version and the ported version. We should go through each CGO/DGO file, look at each object, and if it's a data object, make sure it matches. 


# What can be worked on now?
## C++ Build
Getting more things working on Windows.

### Compiler
The compiler right now only supports GOOS, the macro language. It may be useful to play around with this. There may be missing features or bugs in the language.


## Unpack Assets
I imagine that each type of asset file will have its own tool to unpack things.  We'll need a master "unpacker" tool which runs the correct tool on each file.  

This unpacker could take a config file, maybe JSON?, that says what to do for each file:
```
[
{"filename":"MUS/SNOW.MUS", "unpack_tool":"music_unpacker", "destination":"data/music/snow/"... arguments to music_unpacker},
...
]
```
and then would run the correct tool on each file.  For starters, we could make a simple unpacking tool named `copy` that just directly copies the asset file into the `data` folder.

We could also look into particular files and start writing unpacking tools. 

## Unpack DGOs
Again, there will be a main tool which runs through each DGO/CGO file, finds data objects, then runs an appropriate unpacking tools based on the type of data object (level, texture, art-group,...).  I imagine there would a config file (json maybe?) that looks something like this:
```
{"dgo":"MAI.DGO", "objects":[{"tpage1234", "texture-unpacker", ...}, {"spider", "art-group-unpacker", ...}]}
```

My plan for duplicate data objects:
- When there are duplicates, only one will actually be unpacked
- Ones that aren't unpacked will be checked if they are the same as the unpacked one to make sure we don't make a mistake
- The list of which objects should be unpacked vs. checked, and what name the unpacking should get will be determined by the decompiler.
- If there is an object not in the config file, or a missing object, then there is an error

The actual unpacking tools probably can't be developed until I package up the GOAL disassembler into a usable library, and we probably won't be able to understand the data format until later.  We could write a very simple `copy` unpacker which just copies the object data directly into `data` and doesn't unpack it at all.

## Pack Assets
Again, there will be a main tool which runs the appropriate packer for each file.  We could write this main tool, and a `copy` packer which just copies the exact file in `data/` to `out/`.

## Asset Check
Just a tool with a list of asset files to check, does a comparison and reports an error if there is a difference.  We might expect some files to change slightly in the PC version, and these will be excluded from the check.

## CGO/DGO Check
Iterates through objects in the original / rebuilt CGO/DGOs. Code objects are not checked, but data objects are checked.  Should also check that the order of objects is the same, and all are present.