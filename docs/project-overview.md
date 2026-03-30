# Project Overview

- [Project Overview](#project-overview)
  - [`goalc`](#goalc)
    - [Running the compiler](#running-the-compiler)
  - [`decompiler`](#decompiler)
    - [Running the decompiler](#running-the-decompiler)
  - [`goal_src/`](#goal_src)
  - [`game` runtime](#game-runtime)

There are four main components to the project.

1. `goalc` - the GOAL compiler for x86-64
2. `decompiler` - our decompiler
3. `goal_src/` - the folder containing all OpenGOAL / GOOS code
4. `game` - aka the runtime written in C++

Let's break down each component.

## `goalc`

Our implementation of GOAL is called OpenGOAL.

All of the compiler source code is in `goalc/`. The compiler is controlled through a prompt which can be used to enter commands to compile, connect to a running GOAL program for interaction, run the OpenGOAL debugger, or, if you are connected to a running GOAL program, can be used as a REPL to run code interactively. In addition to compiling code files, the compiler has features to pack and build data files.

### Running the compiler

**Environment Agnostic**

If you have installed `task` as recommended above, you can run the compiler with `task repl`

**Linux**

To run the compiler on Linux, there is a script `scripts/shell/gc.sh`.

**Windows**

On Windows, there is a `scripts/batch/gc.bat` scripts and a `scripts/batch/gc-no-lt.bat` script, the latter of which will not attempt to automatically attach to a running target.

## `decompiler`

The second component to the project is the decompiler.

The decompiler will output code and other data intended to be inspected by humans in the `decompiler_out` folder. Files in this folder will not be used by the compiler.

### Running the decompiler

You must have a copy of the PS2 game and place all files from the DVD inside a folder corresponding to the game within `iso_data` folder (`jak1` for Jak 1 Black Label, etc.), as seen in this picture:

![](./docs/img/iso_data-help.png)

The decompiler will extract assets to the `assets` folder. These assets will be used by the compiler when building the port, and you may want to turn asset extraction off after running it once.

**Environment Agnostic**

If you have installed `task` as recommended above, you can run the compiler with `task decomp`

**Linux**

To run, you can use `scripts/shell/decomp.sh` to run the decompiler

**Windows**

To run, you can use `scripts/shell/decomp-jak1.bat` to run the decompiler

## `goal_src/`

The game source code, written in OpenGOAL, is located in `goal_src`. All GOAL and GOOS code should be in this folder.

## `game` runtime

The final component is the "runtime", located in `game`. This is the part of the game that's written in C++.

In the port, that includes:
- The "C Kernel", which contains the GOAL linker and some low-level GOAL language features. GOAL has a completely custom dynamically linked object file format so in order to load the first GOAL code, you need a linker written in C++. Some low-level functions for memory allocation, communicating with the I/O Processor, symbol table, strings, and the type system are also implemented in C, as these are required for the linker. It also listens for incoming messages from the compiler and passes them to the running game. This also initializes the game, by initializing the PS2 hardware, allocating the GOAL heaps, loading the GOAL kernel off of the DVD, and executing the kernel dispatcher function. This is in the `game/kernel` folder. This should be as close as possible to the game, and all differences should be noted with a comment.
- Implementation of Sony's standard library. GOAL code can call C library functions, and Naughty Dog used some Sony library functions to access files, memory cards, controllers, and communicate with the separate I/O Processor. The library functions are in `game/sce`. Implementations of library features specific to the PC port are located in `game/system`.
- The I/O Processor driver, OVERLORD. The PS2 had a separate CPU called the I/O Processor (IOP) that was directly connected to the DVD drive hardware and the sound hardware. Naughty Dog created a custom driver for the IOP that handled streaming data off of the DVD. It is much more complicated than I first expected. It's located in `game/overlord`. Like the C kernel, we try to keep this as close as possible to the actual game.
- Sound code. Naughty Dog used a third party library for sound called `989SND`. Code for the library and an interface for it is located in `game/sound`.
- PC specific graphics code. We have a functional OpenGL renderer and context that can create a game window and display graphics on it. The specific renderers used by the game however are mostly implemented. Aside from post-processing effects, everything in the game is rendered. This is located in `game/graphics`. While many liberties will be taken to make this work, the end result should very closely match the actual game.
- Extra assets used by the port in some fashion, located in `game/assets`. These include extra text files, icons, etc.
