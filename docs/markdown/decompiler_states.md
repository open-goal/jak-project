# States in the Decompiler

## How can I tell if a file has states?
Search in the `ir2.asm` file for `.type state`.  If you see something like this:
```
    .type state
L28:
    .symbol plat-button-move-downward
    .symbol #f
    .symbol #f
    .symbol #f
```
that's a state, and you can expect the file to have `defstate`s.

## Virtual vs. Non-Virtual States
A non-virtual state is stored in a global variable with the same name as the state.  This is just like a global function.  You don't have to do anything special with this - the decompiler will insert a `defstate` with the appropriate name and make sure that the name of the symbol and the name stored in the `state` itself match.

A virtual state is stored in the method table of a type, similar to a method.  When doing a `go`, the virtual state will be looked up from the method table of the current process, allowing a child type of process to override the state of its parent.  You can tell if a state is virtual by looking for a call to `inherit-state` or `method-set!`.

## Decompiling state handers
Each state has up to 6 handler functions: `enter`, `post`, `exit`, `trans`, `code`, and `event`. In order for the decompiler to recognize these, you _must_ have the top-level function decompile. Do not attempt to decompile these functions until the top-level function passes.

The top-level analysis will find state handlers and name them appropriately.  For non-virtual states, they will be named like `(<handler-name> <state-name>)`. Like `(code teetertotter-launch)`. For virtual states, the name will be `(<handler-name> <state-name> <type-name>)`.  Use these names in type casts, stack structures, etc. These names will not work unless the top-level has been decompiled.

The type of the state handlers will be set up automatically by the type system, but requires that you get the type of the state itself correct.

Note: inside of `find_defstates.cpp` there is an option to enable rename prints that will print out the old name of the function before the rename.

## State Types
Each state object must have its type set.  The type of a state is:
```
(state <arg0> <arg1> ... <parent-type>)
```

The args are the arguments given to the enter/code function. Both enter and code get the same arguments, and some may be unused, but this is ok.

The parent type is the type that the state belongs to.  It must be `process` or a child of `process`.  All state handlers are automatically behaviors of this type.

Here are two examples:
```
(define-extern silostep-rise (state symbol silostep))
```
will make all the `silostep-rise` handlers a behavior of `silostep` and will make `enter` and `code` take a single `symbol` argument.

## Go
The `go` macro changes state. Internally it uses `enter-state`.  Do not insert casts for `go`, it should work automatically.

TODO: there may be issues with the decompiler casting arguments to `go` - this is a bit tricky and I couldn't find a test case yet.

## Special Cases for Virtual States
There are a few special cases for virtual states.

1. The state must be declared in the `deftype`, within the list of methods.
2. The name of the method must be correct.
3. Any `go` should be in a behavior of the appropriate process.

The next three sections explain these in more detail

### Declaring a virtual state
Do not use `define-extern` to declare a virtual state.  Instead, use the method list in the `deftype`.

As an example:
```
    lui v1, L30               ;; [ 25] (set! gp-0 L30) [] -> [gp: state ]
    ori gp, v1, L30
    lw t9, inherit-state(s7)
    or a0, gp, r0
    lw v1, plat-button(s7)
    lwu a1, 104(v1)
    jalr ra, t9
    sll v0, ra, 0

    lw t9, method-set!(s7)
    lw a0, sunken-elevator(s7)
    addiu a1, r0, 22
    or a2, gp, r0
    jalr ra, t9
```

This means
- The state object is `L30`.
- The state is for type `sunken-elevator`.
- The parent type of `sunken-elevator` should be `plat-button`
- The method ID is 22.

The correct declaration should go under `:methods`. Like a normal method, it only needs to be defined in the parent type that first defines it. So we only have to put it in `plat-button` and can leave it out of `sunken-elevator`.

```
(plat-button-pressed () _type_ :state 22)
```

The first thing is the state name (described more in the next section).  The next thing is a list of argument types given to the `enter` and `code` functions.

The ID should match the ID given to `method-set!` and it will be checked just like the normal method IDs.

If you get this wrong, you will get an error message like this:
```
virtual defstate attempted on something that isn't a state: (the-as state (method-of-type plat-button plat-button-move-upward))
Did you forget to put :state in the method declaration?
```
which means that the `plat-button-move-upward` entry in `:methods` in `(deftype plat-button` is missing the `:state`

### Name of a virtual state
The decompiler will check that the name in the method is correct.  If you get it wrong, there will be an error that tells you the right name.

For example:
```
Disagreement between state name and type system name. The state is named plat-button-move-upward, but the slot is named dummy-24, defined in type plat-button
```

This means you should rename `dummy-24` in `plat-button` to `plat-button-move-upward`.


### Go to a virtual state
You must be in a behavior in order for `go-virtual` to decompile successfully.

## The return value of `event` problem.
There is one place that seems to rely on the return value of `event`. As a result, the default is to assume that `event` returns an `object`. However, there are sometimes `event` functions that clearly don't return a value and will refuse to decompile.  The recommendation is:
- Try to make all `event` functions return a value.
- If it is absolutely not possible, make the function type return `none`, then the defstate should automatically insert a cast.


## Unsupported
Calls to `find-parent-method` that actually return a `state` will have type `function`. You must manually cast it. Make sure you get the argument types correct. This same problem exists for finding methods, but it has been very rare. If needed we can add special support in the decompiler/compiler to make this work.

If there is a function with multiple virtual `go`s which assume a different type at compile-time (accessing different parts of the type tree), then it is not possible to insert the right kind of cast yet.

