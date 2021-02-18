# KERNEL

## `gcommon`: **Done**
- `vec4s` print/inpsect unimplemented (believed not working in GOAL either)
- `array`'s `print`/`inspect` will not work on `uint128` or `int128`. These are believed to be unused, and also don't work in GOAL.
- `quad-copy!` is an optimized assembly memory copy that was rewritten. It should have identical behavior, but may be slow. In the future, we could improve the performance if it's used a lot.
- `valid?` uses some inline assembly to check if a pointer is inside the symbol table, rewritten.
- Lots of important memory constants should be defined here.

## `gstring-h`: **Done**
- This file generates no code.

## `gkernel-h`: **Done**
- The types `cpu-thread` and `catch-frame` have a slightly different layout in OpenGOAL to back up x86-64 registers

## `gkernel`:
- Many changes for x86-64/OpenGOAL
- A few bugs in the game related to `process-tree` vs `(pointer process-tree)`.
- `change-brother` is totally wrong, unused, and left out.

## `pskernel`: **Done**
- Unimplemented in OpenGOAL. Seems to be debugging hooks into the PS2's kernel. Error strings indicate that there should have been a second related file included that actually contained the debugging handlers, but this file is not present.

## `gstring`: **Done**
- `string->int` doesn't handle negative numbers correctly. This appears to be a bug in the original version.

## `dgo-h`: **Done**
- Just type definitions. These don't seem to match the version of DGO files found in the game, so maybe this is outdated? Also GOAL never sees DGOs, they are always processed on the IOP.

## `gstate`:
- Doing a `go` from a non-main thread of the process that is changing state is implemented a tiny bit differently. I don't think it should matter.
