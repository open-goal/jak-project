# An Overview

This is the main documentation for the OpenGOAL language. It's designed to be read in order to learn OpenGOAL. **It does not explain the OpenGOAL kernel or state system.**

The syntax descriptions uses these rules:
- Something `[in-brackets]` is optional and can be left out.
- Something like `[:type type-name]` means there is an optional named argument.
  - It can be used like `:type type-name`, replacing `type-name` with what you want, or left out entirely.
- When there are multiple choices, they are separated by `|`. Example: `#t|#f` is either `#t` or `#f`.
- `...` means more of the thing before can be included. Example `(f arg...)` can have multiple arguments.

## Language Basics

OpenGOAL is a compiled language. Source code is stored in `.gc` files. Each `.gc` file is compiled into a `.o` file.  These `.o` files are then loaded by the game. When they are loaded, it has the effect of running every "top level" expression in the file. Usually these are function, type, and method declarations, but you can also use this for initialization code.  For example, it is common to first define types, functions, and methods, then set up global instances.

There are effectively three different "languages":
1. **OpenGOAL** - the normal compiled language.
2. **OpenGOAL compiler commands** - simple commands to run the compiler, listener, and debugger.  These run in the compiler only.
3. **GOOS** macro language.  This is used in OpenGOAL macros and runs at compile-time. These macros generate OpenGOAL compiler commands or OpenGOAL source which is then processed. These run in the compiler only.

The OpenGOAL language uses a LISP syntax, but on the inside is closer to C or C++. There is no protection against use-after-free or other common pointer bugs.

Unlike a C/C++ compiler, the OpenGOAL compiler has a state. It remembers functions/methods/types/macros/constants/enums/etc defined in previous files.

## Type System Introduction

OpenGOAL has a type system. Every expression and object in OpenGOAL has a type. With the exception of three special types (`none`, `_varags_`, and `_types_`), every type has a parent type, and the root of all types is `object`.  Types themselves are objects in the runtime that contain some basic information and their method table.

One annoying detail of OpenGOAL is that the type system has some slightly different types for variables and places in memory, and automatic conversions between them.

Another annoying detail is that there are a totally separate set of rules for 128-bit integer types (or children of these). Nothing in this section applies to these.

Some types are boxed vs. unboxed. If you have an object of boxed type, it is possible to figure out its type at runtime. If you have an object of unboxed type, you can't.  If you have an unboxed type, you can't tell if it's a boxed or unboxed object.

Some types are value or reference. A value type means it has value semantics, it is passed by value everywhere. A reference type is like a C/C++ pointer or reference, where there is memory allocated for the data somewhere, and the you just pass around a reference to this memory.

For more information on the type system, see [the following](type_system.md)