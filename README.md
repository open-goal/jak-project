# Jak Project

![Linux](https://github.com/water111/jak-project/workflows/Linux/badge.svg)
![Windows](https://github.com/water111/jak-project/workflows/Windows/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/water111/jak-project/badge.svg?branch=master)](https://coveralls.io/github/water111/jak-project?branch=master)

## Table of Contents

<!-- toc -->

- [Project Description](#project-description)
- [Table of Contents](#table-of-contents)
- [Getting Started - Linux (Ubuntu)](#getting-started---linux-ubuntu)
- [Getting Started - Windows](#getting-started---windows)
- [Project Layout](#project-layout)
<!-- tocstop -->
## Project Description

This project is to port Jak 1 (NTSC, "black label" version) to PC. Over 99% of this game is written in GOAL, a custom Lisp language developed by Naughty Dog. Our strategy is:
- decompile the original game code into human-readable GOAL code
- develop our own compiler for GOAL and recompile game code for x86-64
- create a tool to extract game assets into formats that can be easily viewed or modified
- create tools to repack game assets into a format that our port uses.

Our objectives are:
- make the port a "native application" on x86-64, with high performance. It shouldn't emulated, interpreted, or transpiled. 
- Our GOAL compiler's performance should be around the same as unoptimized C.
- try to match things from the original game and development as possible. For example, the original GOAL compiler supported live modification of code while the game is running, so we do the same, even though it's not required for just porting the game.
- support modifications. It should be possible to make edits to the code without everything else breaking.
- 

We support both Linux and Windows on x86-64.


## Getting Started - Linux (Ubuntu)

Install Packages and Init Repository

```bash
sudo apt install gcc make cmake build-essential g++ nasm clang-format
git submodule update --init --recursive
```

Compile

```bash
mkdir build && cd build && cmake .. && make -j
```

Run Tests

```bash
./test.sh
```

## Getting Started - Windows

Install Visual Studio 2019 and get the C++ and CMake tools via the Visual Studio Installer

On Windows, it's recommended to get Scoop to use as a package manager, making the follow steps _much_ easier. Follow the steps on the bottom of the homepage here https://scoop.sh/

Once Scoop is installed, run the following command:

```ps
scoop install llvm nasm
```

Initialize the repository's third-party dependencies:

```bash
git submodule update --init --recursive
```

Open the project as a CMake project, browse for the root level `CMakeLists.txt`:

![](./doc/imgs/open-cmake-vs.png)

In the toolbar, you should be able to select an individual component to compile, or combine within the root CMakeLists.txt. In the future we will pre-define configurations to make this easier.

![](./doc/imgs/cmake-build-vs.png)

You may also wish to view the files that pertain to each CMake target, rather than the project as it is normally:

![](./doc/imgs/cmake-target-view.png)


## Project Layout
There are four main components to the project. 

The first is `goalc`, which is a GOAL compiler for x86-64. Our implementation of GOAL is called OpenGOAL. All of the compiler source code is in `goalc`. To run the compiler on Linux, there is a script `gc.sh`.  The compiler is controlled through a prompt which can be used to enter commands to compile, connect to a running GOAL program for interaction, run the OpenGOAL debugger, or, if you are connected to a running GOAL program, can be used as a REPL to run code interactively. In addition to compiling code files, the compiler has features to pack and build data files.

The second component to the project is the decompiler. You must have a copy of the PS2 game and place all files from the DVD into the `iso_data` folder. Then run `decomp.sh` to run the decompiler. The decompile will extract assets to the `assets` folder. These assets will be used by the compiler when building the port. The decompiler will output code and other data intended to be inspected by humans in the `decompiler_out` folder. Stuff in this folder will not be used by the compiler.

The third component is the "runtime", located in `game`. This is the part of the game that's written in C++. In the port, that includes:
- The "C Kernel", which contains the GOAL linker and some low-level GOAL language features. GOAL has a completely custom dynamically linked object file format so in order to load the first GOAL code, you need a linker written in C++. Some low-level functions for memory allocation, communicating with the I/O Processor, symbol table, strings, and the type system are also implemented in C, as these are required for the linker.  It also listens for incoming messages from the compiler and passes them to the running game. This also initializes the game, by initializing the PS2 hardware, allocating the GOAL heaps, loading the GOAL kernel off of the DVD, and executing the kernel dispatcher function. This is in the `game/kernel` folder.

- Implementation of Sony's standard library. GOAL code can call C library functions, and Naughty Dog used some Sony library functions to access files, memory cards, controllers, and communicate with the separate I/O Processor. The library functions are in `sce`, and the implementation of more complicated things are in `system.`

- The I/O Processor driver, Overlord. The PS2 had a separate CPU called the I/O Processor (IOP) that was directly connected to the DVD drive hardware and the sound hardware. Naughty Dog created a custom driver for the IOP that handled 

- Sound Code. Naughty Dog used a third party library for sound.


## Tests