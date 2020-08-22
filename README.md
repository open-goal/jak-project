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
The "runtime" will be a replacement for:
- The "C Kernel" which contains
  - [ ] File I/O (for debugging, not used by retail game)
  - [x] Initialization to boostrap the GOAL Kernel and start the game engine
  - [x] Connection to compiler for debugging/live code patching
  - [x] Interface to OVERLORD (see next section) for DGO loading
  - [x] GOAL Linker
  - [ ] PS2-specific hardware initialization as required by Sony libraries
  - [x] GOAL "kheap" allocator
  - [ ] Memory card interface
  - [x] GOAL printf (called `format`) implementation
  - [x] GOAL hash/symbol table implementation
  - [x] Implementation of some built-in GOAL methods/functions
- The "OVERLORD" IOP driver, which ran on the PS2's separate I/O Processor
  - [x] DGO loader
  - [x] File system for loading from DVD or "fakeiso" mode for loading from a folder
  - [x] "ISOThread" queue system for prioritizing reads
  - [ ] Sound stuff
  - [ ] Streaming animation stuff
  - [ ] Ramdisk stuff (implemented but totally untested)
- The "989_snd" sound driver (no progress has been made here)
- Sony libraries
  - [x] SIF (interface between EE/IOP for sending data, receiving data, and making remote procedure calls)
  - [x] IOP Kernel (single-processor non-preemptive multitasking)
  - [x] stubs for stuff that doesn't really matter

The "Sony libraries" are a simple wrapper around my `system` library.

Likely there will be sound/graphics code in here at some point, but this part is not fully planned yet.

GOAL Compiler
---------------
The GOAL compiler will target x86-64. At first just Linux. There is a macro language called GOOS which is basically just Scheme but with a few bonus features.

The compiler will reuse a significant amount of code from my existing LISP compiler for x86-64. I have a very bad WIP combination which is capable of building a modified `gkernel.gc` for x86 as a proof of concept. An important part of the compiler is the test suite.  Without tests the compiler will be full of bugs. So every feature should have a good set of tests.

The major components are

- [ ] GOAL-IR, a linear intermediate representation for GOAL code
  - [ ] "Environment"
  - [ ] "Ref"
  - [ ] Constant propagation of integers/floats
  - [ ] Constant propagation of memory locations
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
 
- [ ] Front-end (convert s-expressions to GOAL-IR)
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


- [ ] Codegen (convert GOAL-IR + register allocations to x86 object file format)
 - [ ] Linking data
 - [ ] 64-bit GPR
 - [ ] 32-bit float
 - [ ] 128-bit GPR
 - [ ] 32-bit float x4 vector register
 - [ ] function prologue/epilogue
 - [ ] stack spilling

- [ ] Listener/REPL
  - [ ] Network connection
  - [ ] Tracking loaded files
  - [ ] Sending code from REPL
  - [ ] GOOS REPL option
  - [ ] Expand single macro debugging feature
  - [ ] Interface for running gtests


Asset Extraction Tool
-----------------------
Not started yet.  The simplest version of this tool is just to use the decompiler logic to turn the level/art-group/texture/TXT files into GOAL source code, and copy all STR/sound/visibility files.

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
Packs together all assets into a format that can be played.  The simplest version to go with the simplest extraction tool will just pass the level/art-group/texture/TXT files to the compiler, and copy STR/sound/visbility files into the fakeiso. Then pack in CGOs/DGOs.

