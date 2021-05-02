# Compiler REPL

When you start the OpenGOAL compiler, you'll see a prompt like this:

```lisp
OpenGOAL Compiler 0.2
g  >
```

The `g` indicates that you can input OpenGOAL compiler commands.  For a listing of common commands run:

```lisp
(repl-help)
```

## Connecting To Target Example

In order to execute OpenGOAL code, you must connect to the listener.

```lisp
;; we cannot execute OpenGOAL code unless we connect the listener
g  > (+ 1 2 3)
REPL Error: Compilation generated code, but wasn't supposed to

;; connect to the target
g  > (lt)
[Listener] Socket connected established! (took 0 tries). Waiting for version...
Got version 0.2 OK!
[Debugger] Context: valid = true, s7 = 0x147d24, base = 0x2000000000, tid = 1297692

;; execute OpenGOAL code
gc > (+ 1 2 3)
6

;; quit the compiler and reset the target for next time.
gc > (e)
[Listener] Closed connection to target
```

Once we are connected, we see that there is a `gc` prompt. This indicates that the listener has an open socket connection. Now the REPL will accept both compiler commands and OpenGOAL source code.  All `(format #t ...` debugging prints (like `printf`) will show up in this REPL. Each time you run something in the REPL, the result is printed as a decimal number. If the result doesn't make sense to print as a decimal, or there is no result, you will get some random number.

In the future there will be a fancier printer here.

## General Command Listing

### `(e)`

```lisp
(e)
```

Exit the compiler once the current REPL command is finished. Takes no arguments. If we are connected to a target through the listener, attempts to reset the target.

### `(:exit)`

Exit Compiler

```lisp
(:exit)
```

Same as `(e)`, just requires more typing. `(e)` is actually a macro for `(:exit)`. Takes no arguments.

### `(lt)`

Listen to Target

```lisp
(lt ["ip address"] [port-number])
```

Connect the listener to a running target. The IP address defaults to `"127.0.0.1"` and the port to `8112` (`DECI2_PORT` in `listener_common.h`). These defaults are usually what you want, so you can just run `(lt)` to connect.

Example:

```lisp
g  > (lt)
[Listener] Socket connected established! (took 0 tries). Waiting for version...
Got version 0.2 OK!
[Debugger] Context: valid = true, s7 = 0x147d24, base = 0x2000000000, tid = 1296302
gc >
```

### `r`

Reset the target.

```lisp
(r ["ip address"] [port-number])
```

Regardless of the current state, attempt to reset the target and reconnect. After this, the target will have nothing loaded.  Like with `(lt)`, the default IP and port are probably what you want.

Note: `r` is actually a macro.

### `shutdown-target`

If the target is connected, make it exit.

```lisp
(shutdown-target)
```

The target will print

```
GOAL Runtime Shutdown (code 2)
```

before it exits.

### `:status`

Ping the target.

```lisp
(:status)
```

Send a ping-like message to the target. Requires the target to be connected. If successful, prints nothing.  Will time-out and display and error message if the GOAL kernel or code dispatched by the kernel is stuck in an infinite loop.  Unlikely to be used often.

## Compiler Forms - Compiler Commands

These forms are used to control the GOAL compiler, and are usually entered at the GOAL REPL, or as part of a macro that's executed at the GOAL REPL. These shouldn't be used in GOAL source code.

### `reload`

Reload the GOAL compiler
```lisp
(reload)
```

Disconnect from the target and reset all compiler state.  This is equivalent to exiting the compiler and opening it again.

### `get-info`

Get information about something.

```lisp
(get-info <something>)
```

Use `get-info` to see what something is and where it is defined.

For example:

```lisp
;; get info about a global variable:
g  > (get-info *kernel-context*)
[Global Variable] Type: kernel-context Defined: text from goal_src/kernel/gkernel.gc, line: 88
(define *kernel-context* (new 'static 'kernel-context

;; get info about a function. This particular function is forward declared, so there's an entry for that too.
;; global functions are also global variables, so there's a global variable entry as well.
g  > (get-info fact)
[Forward-Declared] Name: fact Defined: text from goal_src/kernel/gcommon.gc, line: 1098
(define-extern fact (function int int))

[Function] Name: fact Defined: text from kernel/gcommon.gc, line: 1099
(defun fact ((x int))

[Global Variable] Type: (function int int) Defined: text from goal_src/kernel/gcommon.gc, line: 1099
(defun fact ((x int))

;; get info about a type
g  > (get-info kernel-context)
[Type] Name: kernel-context Defined: text from goal_src/kernel/gkernel-h.gc, line: 114
(deftype kernel-context (basic)

;; get info about a method
g  > (get-info reset!)
[Method] Type: collide-sticky-rider-group Method Name: reset! Defined: text from goal_src/engine/collide/collide-shape-h.gc, line: 48
(defmethod reset! collide-sticky-rider-group ((obj collide-sticky-rider-group))
[Method] Type: collide-overlap-result Method Name: reset! Defined: text from goal_src/engine/collide/collide-shape-h.gc, line: 94
(defmethod reset! collide-overlap-result ((obj collide-overlap-result))
[Method] Type: load-state Method Name: reset! Defined: text from goal_src/engine/level/load-boundary.gc, line: 9
(defmethod reset! load-state ((obj load-state))

;; get info about a constant
g  > (get-info TWO_PI)
[Constant] Name: TWO_PI Value: (the-as float #x40c90fda) Defined: text from goal_src/engine/math/trigonometry.gc, line: 34
(defconstant TWO_PI (the-as float #x40c90fda))

;; get info about a built-in form
g  > (get-info asm-file)
[Built-in Form] asm-file
```

### `autocomplete`

Preview the results of the REPL autocomplete:

```lisp
(autocomplete <sym>)
```

For example:

```lisp
g  > (autocomplete *)
 *
 *16k-dead-pool*
 *4k-dead-pool*
...
Autocomplete: 326/1474 symbols matched, took 1.29 ms
```

### `seval`

Execute GOOS code.

```lisp
(seval form...)
```

Evaluates the forms in the GOOS macro language. The result is not returned in any way, so it's only useful for getting side effects.  It's not really used other than to bootstrap some GOAL macros for creating macros.

### `asm-file`

Compile a file.

```lisp
(asm-file "file-name" [:color] [:write] [:load] [:no-code])
```

This runs the compiler on a given file. The file path is relative to the `jak-project` folder. These are the options:
- `:color`: run register allocation and code generation. Can be omitted if you don't want actually generate code. Usually you want this option.
- `:write`: write the object file to the `out/obj` folder. You must also have `:color` on. You must do this to include this file in a DGO.
- `:load`: send the object file to the target with the listener. Requires `:color` but not `:write`. There may be issues with `:load`ing very large object files (believed fixed).
- `:disassemble`: prints a disassembly of the code by function.  Currently data is not disassebmled. This code is not linked so references to symbols will have placeholder values like `0xDEADBEEF`.  The IR is printed next to each instruction so you can see what symbol is supposed to be linked. Requires `:color`.
- `:no-code`: checks that the result of processing the file generates no code or data. This will be true if your file contains only macros / constant definition. The `goal-lib.gc` file that is loaded by the compiler automatically when it starts must generate no code. You can use `(asm-file "goal_src/goal-lib.gc" :no-code)` to reload this file and double check that it doesn't generate code.

To reduce typing, there are some useful macros:
- `(m "filename")` is "make" and does a `:color` and `:write`.
- `(ml "filename")` is "make and load" and does a `:color` and `:write` and `:load`. This effectively replaces the previous version of file in the currently running game with the one you just compiled, and is a super useful tool for quick debugging/iterating.
- `(md "filename")` is "make debug" and does a `:color`, `:write`, and `:disassemble`. It is quite useful for working on the compiler and seeing what code is output.
- `(build-game)` does `m` on all game files and rebuilds DGOs
- `(blg)` (build and load game) does `build-game` then sends commands to load KERNEL and GAME CGOs. The load is done through DGO loading, not `:load`ing individual object files.

### `asm-data-file`

Build a data file.

```lisp
(asm-data-file tool-name "file-name")
```

The `tool-name` refers to which data building tool should be used. For example, this should be `game-text` when building the game text data files.

There's a macro `(build-data)` which rebuilds everything.

### `gs`

Enter a GOOS REPL.

```lisp
(gs)
```

Example:

```scheme
g> (gs)
goos> (+ 1 2 3)
6
goos> (exit)
()
```

mainly useful for debugging/trying things out in GOOS. The GOOS REPL shares its environment with the GOOS interpreter used by the compiler, so you can inspect/modify things for debugging with this. Likely not used much outside of initial debugging.

### `set-config!`

```lisp
(set-config! config-name config-value)
```

Used to set compiler configuration. This is mainly for debugging the compiler and enabling print statements. There is a `(db)` macro which sets all the configuration options for the compiler to print as much debugging info as possible. Not used often.

### `in-package`

```lisp
(in-package stuff...)
```

The compiler ignores this. GOAL files evidently start with this for some reason related to emacs.

### `build-dgos`

```lisp
(build-dgos "path to dgos description file")
```

Builds all the DGO files described in the DGO description file. See `goal_src/builds/dgos.txt` for an example. This just packs existing things into DGOs - you must have already built all the dependencies.

In the future, this may become part of `asm-data-file`.

### `add-macro-to-autocomplete`

```lisp
(add-macro-to-autocomplete macro-name)
```

Makes the given name show up as a macro in the GOAL REPL. Generating macros may be done programmatically using GOOS and this form can be used to make these show up in the autocomplete. This also makes the macro known to `get-info` which will report that the macro was defined at the location where the macro which expanded to `add-macro-to-autocomplete` is located in GOAL code.  This is used internally by `defmacro`.
