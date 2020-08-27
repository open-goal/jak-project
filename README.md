Project Structure
----------------------
Requirements:
- `cmake` for build system
- `clang-format` for formatting code (there is already a `.clang-format` provided)
- `gtest` for testing. (Run `git submodule update --init --recursive` to check out the repository)
- `nasm` for assembling x86. There isn't much x86 assembly so if there's a better way to do this for windows, we can change.
- Third party libraries (`nlohmann/json`, `minilzo`, and `linenoise`) are provided in the `third-party` folder

Setup (for Ubuntu):
```
sudo apt install gcc make cmake build-essential g++ nasm clang-format
```

Layout:
- `goalc` is the GOAL compiler
  - `gs` contains GOOS code for parts of GOOS implemented in GOOS
  - `gc` contains GOAL code for parts of GOAL implemented in GOAL (must generate no machine code, just defining macros)
- `decompiler` is the decompiler
- `decompiler_out` is the decompiler output
- `data` will contain big assets and the output of the GOAL compiler (not checked in to git)
- `out` will contain the finished game (not checked into git)
- `resources` will contain data which is checked into git
- `game` will contain the game source code
- `common` will contain all data/type shared between different applications.
- `doc` will contain documentation (markdown format?)
- `iso_data` is where the files from the DVD go
- `third-party` will contain code we didn't write. Google Test is a git submodule in this folder.
- `tests` will contain all tests
- `asset_tool` will contain the asset packer/unpacker

Design:
(if anybody has better ideas, feel free to suggest improvements! This is just a rough plan for now)
- All C++ code should build from the top-level `cmake`.
- All C++ applications (GOAL compiler, asset extractor, asset packer, runtime, test) should have a script in the top level which launches them.
- All file paths should be relative to the `jak` folder.
- The planned workflow for building a game:
  - `git submodule update --init --recursive` : check out gtest
  - `mkdir build; cd build` : create build folder for C++
  - `cmake ..; make -j` : build C++ code
  - `cd ..`
  - `./test.sh` : run gtests
  - `./asset_extractor.sh ./iso_data` : extract assets from game
  - `./build_engine.sh` : run GOAL compiler to build all game code
  - `./build_game.sh` : run the asset packer to build the game
  - `./run_game.sh` : run the game
- Workflow for development:
  - `./gc.sh` : run the compiler in interactive mode
  - `./gs.sh` : run a goos interpreter in interactive mode
  - `./decomp.sh : run the decompiler
  
Current state:
- GOAL compiler just implements the GOOS Scheme Macro Language. Running `./gc.sh` just loads the GOOS library (`goalc/gs/goos-lib.gs`) and then goes into an interactive mode. Use `(exit)` to exit.
- `./test.sh` runs tests for some game C++ code, for GOOS, for the reader, for the listener connection, and for some early emitter stuff.
- The runtime boots in `fakeiso` mode which will load some dummy files.  Then the C Kernel (`game/kernel`) will load the `KERNEL.CGO` and `GAME.CGO` files, which are from the "proof of concept" GOAL compiler.  If you run `./gk.sh`, you should see it load stuff, then print:
```
calling play!
~~ HACK ~~ : fake play has been called
InitListenerConnect
InitCheckListener
kernel: machine started

```
where the `~~ HACK ~~` message is from code in `KERNEL.CGO`.

Code Guidelines:
- Avoid warnings
- Use asserts over throwing exceptions in game code (throwing exceptions from C code called by GOAL code is sketchy)

TODOS:
- Build on Windows!
  - Networking
  - File paths
  - Timer
  - CMake?
  - Assembly
  - Windows calling convention for assembly stuff
  - pthreads (can probably replace with `std::thread`, I don't remember why I used `pthread`s)
  - performance stats for `SystemThread` (probably just get rid of these performance stats completely)
  - `mmap`ing executable memory
  - line input library (appears windows compatible?)
- Clean up possible duplicate code in compiler/decompiler `util` folder, consider a common utility library
- Clean up header guard names (or just use `#pragma once`?)
- Investigate a better config format
  - The current JSON library seems to have issues with comments, which I really like 
- Clean up use of namespaces  
- Clean up the print message when `gk` starts.
- Finish commenting runtime stuff
- Runtime document
- GOOS document
- Listener protocol document
- GOAL Compiler IR
- GOAL Compiler Skeleton
- Gtest setup for checking decompiler results against hand-decompiled stuff
- Clean up decompiler print spam, finish up the CFG stuff
- Decompiler document


Project Description
-----------------------

This project is to port Jak 1 (NTSC, "black label" version) to PC. The strategy is to:
- recompile for x86 to get much better performance than emulation
- create human-reabable GOAL source code that can be modified
- create a GOAL compiler for x86-64 which supports live patching of code like the original
- attempt to match the original game as close as possible (for no reason other than it's fun)
- unpack assets in a format that can be modified

There are 6 components to this project
- GOAL decompiler. The result will be manually cleaned up for running on a PC.
- GOAL compiler for x86-64.
- Game source code, made from cleaning up the result of the GOAL decompiler.
- GOAL runtime. This will replace the parts of the game written in C++
- Asset extraction tool to extract the models/textures/large data from the game
- Asset packing tool.

The process to build the port will be
- Build data extraction tool, GOAL compiler, and GOAL runtime library (all written in C++)
- Run the GOAL compiler on the game source code to build the game engine
- Run asset extraction on the game disc to get level data, textures, geometry data, music...
- Run the asset packing tool to combine the unpacked assets with the compiled game engine to create the game!

Some statistics:
- Estimated ~500k lines of GOAL code
- 10410 functions
- 5451 functions with no control flow (no branching, loops, if/else, short-circuiting boolean operators, gotos, etc)

The rough timeline is to finish sometime in 2022.  If it looks like this is impossible, the project will be abandoned. But I have already spent about 4 months preparing to start this and seems doable. I also have some background in compilers, and familiarity with PS2 (worked on DobieStation PS2 emulator) / MIPS in general (wrote a PS1 emulator).  I think the trick will be making good automated tools - the approach taken for SM64 and other N64 decompilations is way too labor-intensive to work.


GOAL Decompiler
------------------
The decompiler is in progress, at 
https://github.com/water111/jak-disassembler

Here is the plan for writing the decompiler:
- [x] Decode the CGO/DGO format.
- [x] Decode the linking data format.
- [x] Identify all code and disassemble
- [x] Recover references
- [x] Split code into functions, and build a graph of basic blocks
- [ ] Create a control flow graph for each function (currently succeeds for 9857/10410 functions)
- [ ] Extract type/method information from debug data
- [ ] Convert instructions to an intermediate representation (IR) and eliminate GOAL/MIPS idioms
- [ ] Regsiter liveness analysis
- [ ] Type propagation
- [ ] Variable map and scoping
- [ ] S-expression construction (expression stack)


GOAL Runtime
--------------
The "runtime" will be a replacement for all of the C/C++ code of the original game.  There is C/C++ code that runs on the main processor (EE) and the separate I/O processor (IOP).
- The "C Kernel", which runs on the EE and contains
  - [ ] File I/O (for debugging, not used by retail game)
  - [x] Initialization to boostrap the GOAL Kernel and start the game engine
  - [x] Connection to compiler for debugging/live code patching
  - [x] Interface to OVERLORD (see next section) for DGO loading
  - [x] GOAL Linker
  - [ ] PS2-specific hardware initialization as required by Sony libraries
  - [x] GOAL "kheap" allocator
  - [ ] Memory card interfaces
  - [x] GOAL printf (called `format`) implementation
  - [x] GOAL hash/symbol table implementation
  - [x] Implementation of some built-in GOAL methods/functions
- The "OVERLORD" IOP driver, which ran on the PS2's separate I/O Processor for loading things off the DVD and doing sound things
  - [x] DGO loader
  - [x] File system for loading from DVD or "fakeiso" mode for loading from a folder
  - [x] "ISOThread" queue system for prioritizing reads
  - [ ] Sound stuff
  - [ ] Streaming animation stuff
  - [ ] Ramdisk stuff (implemented but totally untested)
- The "989_snd" sound driver (no progress has been made here, the rough plan is to do a high level emulation of the sound system)
- Sony libraries
  - [x] SIF (interfaces between EE/IOP for sending data, receiving data, and making remote procedure calls)
  - [x] IOP Kernel (single-processor non-preemptive multitasking)
  - [x] stubs for stuff that doesn't really matter

The "Sony libraries" are a simple wrapper around my `system` library, which implements the threading/communication stuff.

Likely there will be sound/graphics code in here at some point, but this part is not fully planned yet.

GOAL Compiler
---------------
The GOAL compiler will target x86-64. At first just Linux. There is a macro language called GOOS which is basically just Scheme but with a few bonus features.

The compiler will reuse a significant amount of code from my existing LISP compiler for x86-64. I have a very bad WIP combination which is capable of building a modified `gkernel.gc` for x86 as a proof of concept.  It can create and run functions in threads.

An important part of the compiler is the test suite.  Without tests the compiler will be full of bugs. So every feature should have a good set of tests.

The major components are

- [ ] GOAL-IR, a typed linear intermediate representation for GOAL code
  - [ ] "Environment"
  - [ ] "Ref"
  - [ ] Constant propagation of integers/floats
  - [ ] Constant propagation of memory locations (this is required to make sure bitfields updates know where to write their result)
  - [ ] Ref `in_gpr`

- [ ] The type system
  - [ ] Type inheritance
  - [ ] Integer/float/pointer types (value semantics)
  - [ ] Reference types
  - [ ] Bitfield types
  - [ ] 128-bit integer types (EE GPRs are 128-bit when used with MMI instructions, these will become `xmm`'s)
  - [ ] Floating point vector types
  - [ ] Structure types
  - [ ] Inline structures as fields
  - [ ] Array access / alignment rules
  - [ ] Pointer dereferencing
  - [ ] Methods (dynamic and static dispatch)
  - [ ] Function arguments/returns
  - [ ] Global symbols
  - [ ] Type checking
  - [ ] Lowest common ancestor
  - [ ] Built-in types in the GOAL runtime/C Kernel

- [x] The GOOS Macro Language
  - [x] S-expression parser (the "Reader")
  - [x] Reader text db (for error messages that point to a specific line)
  - [x] Scheme interpreter
 
- [ ] Front-end (convert s-expressions (a tree structure) to GOAL-IR (a linear representation))
 - [ ] Parsing helpers
 - [ ] Macro expansion
 - [ ] Control flow/block forms
 - [ ] Type definitions
 - [ ] Inline assembly forms
 - [ ] Function/method call
 - [ ] Math forms
 - [ ] Lexical scoping (immediate application of `lambda`)
 - [ ] Function inlining (slightly different scoping rules of immediate `lambda`)
 - [ ] Function/macro definition
 - [ ] Static Objects

- [ ] Regsiter allocation
 - [ ] Analysis
 - [ ] Allocation
 - [ ] Stack spilling
 - [ ] `xmm` and `gpr` promotion/demotions for EE 128-bit register usage


- [ ] Codegen / Emitter (convert GOAL-IR + register allocations to x86 object file format)
 - [ ] Emitter (convert GOAL-IR to instructions)
 - [ ] x86-64 instruction generation (actually generate the machine code)
 - [ ] Linking data
 - [ ] 64-bit GPR
 - [ ] 32-bit float
 - [ ] 128-bit GPR
 - [ ] 32-bit float x4 vector register
 - [ ] function prologue/epilogue
 - [ ] stack spilling
 - [ ] static object and static object links

- [ ] Listener/REPL
  - [ ] Network connection
  - [ ] Tracking loaded files
  - [ ] Sending code from REPL
  - [ ] GOOS REPL option
  - [ ] Expand single macro debugging feature
  - [ ] Interface for running gtests


Asset Extraction Tool
-----------------------
Not started yet.  The simplest version of this tool is just to use the decompiler logic to turn the level/art-group/texture/TXT files into GOAL source code, and copy all STR/sound/visibility files, as these don't need to be modified.

Eventually this should export to a more useful format.

File formats:
- [ ] Art group (a GOAL object format)
  - There may be more formats related to art groups.
- [ ] Texture page (a GOAL object format)
  - [ ] Texture page directory (a GOAL object format)
- [ ] Level (`vis-bt`) (a GOAL object format)
- [ ] `TEXT/*.TXT` (text, a GOAL object format)
- [ ] `MUS` (sequenced music)
- [ ] `SBK` (sound bank)
- [ ] `STR` (streaming animation)
- [ ] `VAG` (ADPCM audio)
- [ ] `VIS` (visibility data bitstream)
- [ ] Loading screen image
- [ ] save game icon (I do not care about this)


Asset Packing Tool
-----------------------
Packs together all assets/compiled code/runtime into a format that can be played.  The simplest version to go with the simplest extraction tool will just pass the level/art-group/texture/TXT files to the compiler, and copy STR/sound/visbility files into the fakeiso. Then pack in CGOs/DGOs.

It's important that the asset extraction/packing can be automated so we can avoid distributing the assets, which are large and probably not supposed to be distributed.



