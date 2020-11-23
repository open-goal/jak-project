# OpenGOAL Document
This is the main documentation for the OpenGOAL language. It explains the syntax of OpenGOAL programs and also how OpenGOAL can be decompiled from original GOAL.  It's broken up into three sections:

1. Compiler forms, which are things built-in to the compiler and have names.  Like `+` or `deftype`.
2. Important Syntax Macros, which are really important features of the languages that are implemented as macros, like `if` and `defun`.
3. Compiler features, which are things that are built-in to the compiler but don't have explicit names. Like calling functions or the rules for how names are scoped.
4. Built-in types

Each feature will have a description, explanation of the syntax, a guess at how often it's used, an example or two in OpenGOAL, and an example or two in MIPS GOAL. There will also be details on the order of evaluation that is useful for the decompiler but can mostly be ignored for normal programming in OpenGAL.

The syntax description uses these rules:
- Something `[in-brackets]` is optional and can be left out.
- Something like `[:type type-name]` means there is an optional named argument. It can be used like `:type type-name`, replacing `type-name` with what you want, or left out entirely.
- When there are multiple choices, they are separated by `|`. Example: `#t|#f` is either `#t` or `#f`.
- A `...` means more of the thing before can be included. Example `(f arg...)` can have multiple arguments.

When talking about ordering things, GOAL code fragments can be `compile`d and `flush`ed as two separate things. For the most part, everything is done during compilation, like calling functions. But consider `(set! (-> my-object value) x)`. We don't actually want the value of `(-> my-object value)`, we want to set its value. The `compile` stage gets us description of how to read/write a thing (if possible), and the `flush` stage gets us an actual value in a register.  This can basically be ignored outside of the decompiler. 


# Compiler Forms - Block Related

## `begin`
Execute forms in order.
```lisp
(begin form...)
```
A `begin` form is just a list of other forms which are executed in order. A `begin` form evaluates to the value of the last form.

Example:
```lisp
(begin
  (print "hello ")
  (print "world!")
  7
  )
```
will print `hello world!` and the value of the entire form is `7`.

In `begin` and similar "do everything in the list" forms, each form is `compile`d then `flush`ed. 

The `begin` form is used a lot in macros, but not that much in code. It's generally used when you want to execute multiple things, but fit it into a single form.


## `block`
A `block` form is pretty similar to a `begin`, except the `block` has a name. You can then "return" from the block early with `return-from`.
```lisp
(block block-name form...)
```

Example:
```lisp
(block my-block
  (print "hello ")
  (return-from my-block 7)
  (print "world")
  "test"
  )
```
will print `hello ` only and the value of the entire `block` form is `7`.  The type of the `block` is the most specific type that describes all of the possible return values from any `return-from` or from reaching the end (even if its technically not possible to reach the end). In the case above, the possible return types are `int` and `string`, so the return type of the whole block is `object`, the lowest common ancestor type of `int` and `string`.

Block is used rarely, and possibly almost never?

## `return-from`
Exit a `block` or function early. 
```lisp
(return-from block-name value)
```

Looks up the block and exits from it with the value. You can exit out nested blocks. If you are enclosed in multiple blocks with the same name, exits from the inner-most one with a matching name. Everything in a function is wrapped in a block named `#f`, so you can use `(return-from #f x)` to return early from a function with `x`.  When using this form, it may change the return type of the function or block. The return type will be the lowest common ancestor type of all written return paths. If there is an unreachable return path, it will still be considered.

Example
```lisp
(if (is-a-match? x)
  (return-from #f x)
  )
```
if `x` is a match, returns `x` from the function (not shown) immediately.

The `return-from` form is very rarely used to return from a block, but sometimes used to return from a function.

## `label`
Create a named label for `goto` or `goto-when`.
```lisp
(label label-name)
```
The label spaces are per-function and not nested. You can't jump from function to function. You can't jump in or out of functions which end up getting inlined. You can't jump in or out of an anonymous lambda function. You can jump in and out of `let`s.

See `goto` for an example.

Labels are used extremely rarely. Usually only in inline assembly and part of macros for `while` loops and similar.

## `goto`
Jump to a label.
```lisp
(goto label-name)
```
The label must be in the current label space. You can jump forward or backward.

Example:
```lisp
(if skip-code?
  (goto end)
  )

;; code here runs only if skip-code is false.

(label end)
```

The `goto` form used very rarely outside of macros and inline assembly. Try to avoid using `goto`.

## `top-level`
This form is reserved by the compiler. Internally all forms in a file are grouped under a `top-level` form, so you may see this in error messages. Do not name anything `top-level`.

# Compiler Forms - Compiler Control
These forms are used to control the GOAL compiler, and are usually entered at the GOAL REPL, or as part of a macro that's executed at the GOAL REPL. These shouldn't really be used in GOAL source code.

## `:exit`
This causes the compiler to exit after the current REPL command is over. 
```lisp
(:exit)
```
If the listener is connected, it sends a reset command to reboot the target so it is ready for the next compiler connection.

There's a useful macro `(e)` to exit with less typing.  Obviously this shouldn't be used in game code.

## `seval`
Execute GOOS code.
```lisp
(seval form...)
```
Evaluates the forms in the GOOS macro language. The result is not returned in any way, so it's only useful for getting side effects.  It's not really used other than to bootstrap some GOAL macros for creating macros.

## `asm-file`
Compile a file.
```lisp
(asm-file "file-name" [:color] [:write] [:load] [:no-code])
```
This runs the compiler on a given file. The file path is relative to the `jak-project` folder. These are the options:
- `:color`: run register allocation and code generation. Can be omitted if you don't want actually generate code. Usually you want this option.
- `:write`: write the object file to the `data` folder. You must also have `:color` on. You must do this to include this file in a DGO.
- `:load`: send the object file to the target with the listener. Requires `:color` but not `:write`. There may be issues with `:load`ing very large object files.

To reduce typing, there are some useful macros:
- `(m "filename")` is "make" and does a `:color` and `:write`.
- `(ml "filename")` is "make and load" and does a `:color` and `:write` and `:load`. This effectively replaces the previous version of file in the currently running game with the one you just compiled, and is a super useful tool for quick debugging/iterating.
- `(build-game)` does `m` on all game files and rebuilds DGOs
- `(blg)` (build and load game) does `build-game` then sends commands to load KERNEL and GAME CGOs. The load is done through DGO loading, not `:load`ing individual object files.

## `lt`
Listen to target.
```lisp
(lt ["ip address"] [port-number])
```
This causes the compiler to connect to the target/runtime. Usually it's just run as `(lt)`, as the default IP is `127.0.0.1` and the default port is the right one.  If it works, you should see something like this:
```
[Listener] Socket connected established! (took 0 tries). Waiting for version...
Got version 2.6 OK!
[OUTPUT] reset #x147d24
```
The `OUTPUT` message is a pending message from the runtime saying that it has reset and the location of the symbol table.

Note: `lt` is actually a macro. Use these target control macros over the direct compiler forms (currently undocumented) whenever possible, as running the compiler forms in the wrong order can leave the target/compiler in a strange state.

## `r`
Reset the target.
```lisp
(r)
```
Regardless of the current state, attempt to reset the target and reconnect. 

Note: `r` is actually a macro. Use it over the (currently undocumented) compiler forms.

## `shutdown-target`
If the target is connected, shut it down.
```lisp
(shutdown-target)
```
The target will print
```
GOAL Runtime Shutdown (code 2)
```
when it shuts down.

## `:status`
Ping the target.
```lisp
(:status)
```
Send a ping-like message to the target. Requires the target to be connected. If successful, prints nothing.  Will time-out if the GOAL kernel or code dispatched by the kernel is stuck in an infinite loop.  Unlikely to be used often.

## `gs`
Enter a GOOS REPL.
```lisp
(gs)
```
Example:
```
g> (gs)
goos> (+ 1 2 3)
6
goos> (exit)
()
```
mainly useful for debugging/trying things out in GOOS. The GOOS REPL shares its environment with the GOOS interpreter used by the compiler, so you can inspect/modify things for debugging with this. Likely not used much outside of initial debugging.

## `set-config!`
```lisp
(set-config! config-name config-value)
```
Used to set compiler configuration. This is mainly for debugging the compiler and enabling print statements. There is a `(db)` macro which sets all the configuration options for the compiler to print as much debugging info as possible. Not used often.

## `in-package`
```lisp
(in-package stuff...)
```
The compiler ignores this. GOAL files evidently start with this for some reason related to emacs.

## `build-dgos`
```lisp
(build-dgos "path to dgos description file")
```
Builds all the DGO files described in the DGO description file. See `goal_src/builds/dgos.txt` for an example. This just packs existing things into DGOs - you must have already built all the dependencies.

# Compiler Forms - Control Flow

## GOAL "Two Element" Conditions
These are `!=`, `eq?`, `neq?`, `=`, `>`, `<`, `>=`, `<=`.  The default is to compare the two objects as unsigned 64-bit integers, unless a special case is hit. The special case is determined by the type of the __first__ argument:
- Floating point: if second argument is a number, convert to floating point. Use floating point comparisons
- Binteger: convert first argument to integer. If second argument is a number, convert to integer. Use signed integer comparisons
- Integer: If second argument is a number, convert to integer. Use signed integer comparison
- Unsigned Integer: If second argument is a number, convert to integer. Use unsigned integer comparison

Note that comparing structures just checks if they refer to the same memory, not if the contents are the same.

Currently there is absolutely no type checking done on comparisons, which makes it quite easy to get things wrong.

When a two-element condition is used as a condition for an `if`, `cond`, `while`, `goto-when`, or any other branching condition, there is an optimization that avoids computing a GOAL boolean, then checking that GOAL boolean as a branch condition. Instead the comparison operation directly sets the x86-64 flags for a jump. Original GOAL has a similar optimization.

There are a lot of unknown details
- What order are things evaluated / converted?
- How does the order work when there's a `not`?
- Type Checking is probably needed.
- Currently `=` and `eq?` are exactly the same. Likely `=` should only work on numbers and `eq?` should work on references?

## `not`
```lisp
(not value)
```
If value is `#f`, return `#t`. Otherwise return `#f`.

Using `not` on a "two element condition" will be optimized to modifying that condition.

## `when-goto`
```lisp
(when-goto value-or-condition destination)
```
Examples:
```lisp
(when-goto (> x y) some-label)
(when-goto (is-player-dead?) some-label)
```
Jump to `destination` if the condition is truthy (not `#f`).  This ends up generating much better code than `(if condition (goto x))`. 

Like normal `goto`, this isn't used much outside of macros.

## `cond`
A normal Lisp/Scheme `cond`.
```lisp
(cond (test clause...)...)
```
Evaluates `test`s until one is truthy. Then evaluates all `clause`s in that case and returns the value of the last one.  Optionally there can be a case with `else` as the test which runs if no other cases match. This must be the last case.

If there is no `else`, and no cases match, return `#f`. (is it always `#f`?)

The return type is the lowest common ancestor type of all cases.  Note that for a `cond` without `else`, the possible return value of `#f` is __not__ considered when determining the return type. So:
```lisp
(print-type
  (cond (test1 "hi") (test2 "bye"))
  )
```
will print `[TYPE] string`, even though `cond` may return `#f`.

This behavior seems correct if the return value is a `basic`, but less clear if the return value is supposed to be a number.

Note that GOAL will load `#f` into a register for an else case even if the value of the `cond` is never used. Also, even if the value of the `cond` is never used, all cases will store their result into a common `cond` return-register. This is incredibly helpful for the decompiler!

## `if`
A normal Lisp/Scheme `if`.
```lisp
(if test true-case [false-case])
```
The value of the entire statement is the value of the taken case. The false case can be left out. If the condition evaluates to false and the false case is left out, `#f` is returned.  The return type follows the same logic as `cond`, as `if` is a macro using `cond`.

## `when`
A normal Lisp/Scheme `when`.
```lisp
(when test forms...)
```
If `test` is truthy, evaluate all forms and return the value of the last one. If `test` isn't true, return `#f`.

## `unless`
A normal Lisp/Scheme `unless`.
```lisp
(unless test forms...)
```
If `test` is false, evaluate all forms and return the value of the last one. If `test` isn't false, return `#f`.

# Compiler Forms - Definition



## `set!`
Set a value!
```lisp
(set! place value)
```

Sets `place` to `value`. The `place` can be once of:
- A lexical variable defined by `let`, or an argument of a function
- A global symbol
- A `car` or `cdr` of a pair
- A field of an object
- A dereferenced pointer
- An element in an array
- A bitfield within any of the above (not yet implemented)

## `define`
Define a global symbol
```lisp
(define symbol-name value)
```
Kind of like `set!`, but works exclusively on global symbols. Also sets the type of the global symbol. If the global symbol already has a type, and the `define` tries to change it, there will be an error or warning. (to be determined)

## `define-extern`
Inform the compiler about the type of a global symbol
```lisp
(define-extern symbol-name type)
```
Useful to forward declare functions or to let the compiler know about things in the C Kernel. See `goal-externs.gc` for where all the C Kernel stuff is declared.

# Compiler Forms - Functions

## `defun`
Define a new named global function!
```lisp
(defun name arg-list ["doc-string"] body...)
arg-list = (arg...)
arg = (arg-name arg-type)|arg-name
```
Example:
```
(defun 1/ ((x float))
  "Compute 1.0 / x"
  (/ 1.0 x)
  )
```
Define a new function with the given name. Note that until the `defun` itself executes, the function __cannot be used!__. So don't call functions before the `defun`.  This is unlike C, where you can forward declare a function and use it before the actual definition.

There is an optional docstring. Currently the docstring is just thrown away but in the future we could save them and and generate documentation or something.

## `defmethod`
Define a method!
```lisp
(defmethod method-name type-name arg-list ["doc-string"] body...)
```
In many ways is similar to `defun`. See section on methods for more details.

You can only `defmethod` if one of these two is true:
- You are overriding a parent method
- You have declared this method in the type's `deftype`

Like `defun`, the method only exists after the `defmethod` executes. Before that the method may be undefined or the parent's method.

## `inline`
Make this function call inlined.
```
(inline function-name)
```
Example: inline call `my-function`
```
((inline my-function) my-arg)
```
Attempts to make the function call inline. If it is not possible, it will throw an error. You can't save the value of `(inline my-function)`, only use it immediately in a function call. You must provide a name of a global function as the function, not a function pointer.

Methods cannot be inlined. (Maybe new methods can be in original GOAL?)

## `lambda`
Create a GOAL Lambda.
```lisp
(lambda [:name name] [:inline-only #f|#t] [:segment main|debug] arg-list body-form..)
arg-list = (arg...)
arg = (arg-name arg-type)|arg-name
```

The `:name` option is only used for debugging the compiler - it cannot be accessed or referenced outside of internal compiler debugging features. It should be ignored unless you are debugging the compiler.

The `:inline-only` flag defaults to `#f`, but can be set to `#t` to force the compiler to not generate x86-64 code for the function. In this case, the lambda cannot be used as a function pointer/GOAL `function` object.  This is used in the `let` macro and not really useful for general programming.  If the flag is `#f` (default value), it will generate and store an x86-64 function, regardless of if the function is ever called.

The `:segment` option defaults to `main`, unless the `lambda` is defined in a function in the `debug` segment, in which case it defaults to `debug`. It can be overridden by this flag.  This picks the segment of the object file where the function will be stored.

The arguments default to type of `object` if no type is provided.  In the case where a lambda is used immediately (a `let`) the compiler will propagate more specific types. This is why you don't have to give types when using `let`.

A GOAL lambda has three purposes:
1. Generate a function as a "real" x86-64 function.
2. Save the code so the function can later be inlined
3. Immediately use the lambda in the head of a list as part of a `let` or `let*` macro

__Example of 1:__
```
(sort my-list (lambda ((x int) (y int) (< x y))))
```
We create a real x86-64 function to pass to the `sort` function.  In this case you should specify types - the default of `object` is not suitable.

The other two cases are handled by `let` and `defun` macros, and shouldn't show up in other places, so they are left out.

## `declare`
Set options for a function or method
```lisp
(declare [(inline)] [(allow-inline)] [(disallow-inline)] [(asm-func)])
```
If used, this should be the first thing inside of a `defun`/`defmethod`. Don't use it anywhere else.
Example:
```lisp
(defun my-function ()
  (declare (inline))
  ...
  )
```

- `inline` means "inline whenever possible". See function inlining section for why inlining may be impossible in some cases.
- `allow-inline` or `disallow-inline`. You can control if inlining is allowed, though it is not clear why I thought this would be useful. Currently the default is to allow always.
- `asm-func` currently does nothing. Eventually should disable generating prologues/epilogues. Use if you want an entirely asm function. Used very rarely and probably only in the GOAL kernel.

This form will probably get more options in the future.

# Compiler Forms - Macro Forms

## `#cond`
This is like an `#ifdef`, but the language for the conditions is GOOS and it's like a `cond`.
```lisp
(#cond (test clause...)...)
```
The `#cond` is executed at compile time. The cases which don't match aren't compiled. There are `#when` and `#unless` macros that work like you would expect.

Example:
```lisp
(#when PRINT_DEBUG_INFO
  (format #t "debug info~%")
  )
```

## `quote` / `'`
The reader will expand `'thing` to `(quote thing)`

```lisp
(quote thing)
```
Currently `quote` supports:
- `'a-symbol` - will return the symbol `a-symbol`
- `'()` - will return the empty list.

In the future, it should support quoted lists of static data.

## `defglobalconstant`
Define a GOOS and GOAL constant.
```lisp
(defconstant name value)
```

The constant is available in both GOOS and GOAL. You can use it in `#cond` expressions (GOOS) and in GOAL code. The constant inserts the `value` as an s-expression in the code. This means it's a "code" constant, and doesn't have a type.

There are a few restrictions:
- Currently constants do not work in `deftype`. (this should be fixed eventually)
- Constants may not work in places where the compiler is expecting a symbol which isn't actually compiled. So `(+ MY_CONSTANT 2)` is fine, but `(defun MY_CONSTANT () ...)` isn't. Don't use constants for type names, function names, or symbol names. It is fine to have a constant with a value of a symbol, but don't `(define MY_CONSTANT)` or `(set! MY_CONSTANT)` or `(new MY_CONSTANT)` or things like that.
- Don't use constants with `method` form.
- You can modify constants created with `defglobalconstant` in GOOS (effect won't be seen in GOAL) with `(set!)`
- There is no warning if one `defglobalconstant` changes the value of an existing `defglobalconstant`

In general constants should have `UPPERCASE` names otherwise things get very confusing when there are name conflicts.

The recommendation is to use constants for things like numbers or expressions like `(* NUM_THINGS SIZE_OF_EACH_THING)`.

## `mlet`
Scoped constants in GOAL. Syntax is like a `let`. This feature has all the restrictions of `defglobalconstant`.
Avoid using `mlet` because it's confusing and not useful.
```lisp
(mlet ((constant-name constant-value)...)
  body...
  )
```

Example:
```lisp
(mlet ((NUM-THINGS 12)
       (THING-NAME "test"))
  ; in here, NUM-THING and THING-NAME are constants
  )
```

# Compiler Forms - Math Forms

Math forms will look at the type of the first argument to determine the "mode".  So if you have `(+ 1 1.2)`, it will convert the `1.2` to an integer, do the add, and return an integer.

## `+`
Addition. Can take 1 or more arguments. `(+ 1)` will give you `1`, like you'd expect.
```lisp
(+ form...)
```
Works on integers and floats. Integer add is 64-bit and wrapping.

## `-`
Subtraction or negative. Can take 1 or more arguments. 
```lisp
(- form...)
```
`(- 1)` gives you `-1`, but `(- 1 3)` gives you `-2`, which may be slightly confusing at first. Works on integers and floats. Integer subtract is 64-bit and wrapping.

## `*`
Multiplication. Can take 1 or more arguments.
```lisp
(* form...)
```
`(* 7)` will become `7`, as you'd expect. Works on integers and floats. The exact behavior for integers needs to be explored more because the EE is weird. GOAL generates different code for multiplication of signed vs. unsigned integers.

## `/`
Division. Takes exactly two arguments
```lisp
(/ dividend divisor)
```
Works on floats and integers. The exact behavior for integers needs to be explored more because the EE is weird. GOAL generates different code for division of signed vs. unsigned integers.

## `mod`
Modulo operation. Takes exactly two arguments.
```lisp
(/ dividend divisor)
```
Works on integers only. The exact behavior needs to be explored because the EE is weird. It is unknown if the same code is generate for signed/unsigned mod.

## `slhv`, `sarv`, `shrv`
```lisp
(shlv value shift-amount)
```
The exact behavior of GOAL shifts isn't fully understood, so these are temporary wrappers around x86-64 variable shift instructions.
- `shlv` shift left variable
- `sarv` shift arithmetic right variable
- `shrv` shift logical right variable
64-bit operation.

## `logand`
Bitwise And
```lisp
(logand a b)
```
64-bit operation.

## `logior`
Bitwise Inclusive Or (normal Or)
```lisp
(logior a b)
```
64-bit operation.

## `logxor`
Bitwise Exclusive Or (Xor
```lisp
(logxor a b)
```
64-bit operation.

## `lognot`
Bitwise Not
```lisp
(lognot a)
```
64-bit operation.

# Compiler Forms - Type Forms

## `defmethod`

## `deftype`


## `method`
Get a method of a type or an object.
__Warning - I will probably change this in the future.__
```
(method type method-name)
(method object method-name)
```

The first form of this takes a type name and method name and returns a GOAL `function` for this method. For example:
```
(method string inspect)
```
will return the `inspect` method of `string`.

The second form of this takes an object and gets the method from it. If the object has runtime type information, will consult the method table to get a possibly more specific method than what is available at compile time. This uses the same lookup logic as method calling - see the section on method calls for more information.

## `car` and `cdr`
Get element from pair
```lisp
(car some-pair)
(cdr some-pair)
```

The type of the result is always `object`, as pairs can hold any `object`. The type-check for `car` and `cdr` is relaxed - it allows it to be applied to any `pair` or `object`.  The reason for allowing `object` is so you can write `(car (car x))` instead of `(car (the pair (car x)))`. However, if the argument to `car` is not a `pair`, you will get garbage or a crash.

## `new`
```lisp
(new [allocation] [new-type-specification] [args]) 
```
See section on creating new GOAL objects. 

## `print-type`
Print the type of some GOAL expression at compile time.
```lisp
(print-type form)
```
This is mainly used to debug the compiler or figure out why some code is failing a type check. The thing inside is actually executed at runtime. Example:
```lisp
(print-type "apples")        ;; [TYPE] string
(print-type (+ 12 1.2))      ;; [TYPE] int
(print-type (the float 12))  ;; [TYPE] float
```

## `the`
Convert between types, doing the expected thing for number conversions.
```lisp
(the type thing)
```

If the `type` and the `thing` are both numbers, it will automatically convert between the different numeric types as needed. In all other cases, it does a dangerous `reinterpret_cast`.  cppreference.com explains this idea clearly with "Converts between types by reinterpreting the underlying bit pattern."

If the `thing` is a number:
- If `type` is `binteger`, convert the number to a `binteger`
- If `type` is `int` or `uint` (or some user-defined child type of these), convert the number to an integer.
- If `type` is `float`, convert the number to a `float`

In all other cases, directly use the 64-bit value in the register as the value of the desired `type`.


Example of number conversions:
```
(the binteger 12) ;; boxed integer for 12
(the float 1)     ;; floating point 1.0
(the int 1.234)   ;; signed integer 1
```

Examples of common casts:
```
(the uint 1)             ;; unsigned integer, doesn't do any modification if input is already an int.
(the (pointer uint8) x)  ;; like C (uint8_t*)x
(the string (car x))     ;; if you know (car x) is a string, do this.
```

Examples of dangerous things you can do but probably shouldn't:
```
(the string 1.234) ;; take the binary representation of 1.234 and treat it as a GOAL memory address to string data.
(the float 'bean)  ;; take the GOAL memory address of the "bean" symbol and treat it as the binary representation of a float.
```

There are some weird edge cases with `the` that are worth mentioning, if you try to set the value of something you've used `the` on. In the future the compiler should block you from doing something bad, but this is not yet implemented.

```
(set! (-> (the (pointer uint8) x)) 1)      ;; OK, not casting the actual value
(set! (the int (-> obj x)) 1)              ;; NOT OK. Int is a value type
;; Will actually work if x is already an int/uint, and with non-numeric values types like pointer.  Avoid just to be safe.
(set! (the string (-> obj x)) "test")      ;; OK, string is a basic, which is a reference type.
```

This becomes more clear if we look at the C equivalent:
```
*(uint8_t*)(a->x) = 1; // makes sense

(int)(a->x) = 1; // doesn't make sense

*(thing*)(a->x) = thing(); // makes sense
```


## `the-as`
Convert between types always using `reinterpret_cast`
```lisp
(the-as type thing)
```
cppreference.com explains this idea clearly with "Converts between types by reinterpreting the underlying bit pattern."

The recommendation is to prefer `the` in almost all cases, unless you want to set an exact bit pattern of a `float`, or want to examine the bits in a `float`.

Example:
```
(the-as int 1.234) ;; result happens to be 1067316150
```

None of the edge cases of `the` apply to `the-as`.


## Pointer Math
Not implemented well yet.

# Compiler Forms - Unsorted

## `let`
```lisp
(let ((var-name value)...) body...)
```
Define local variables. The variables are automatically assigned a type, like C++ `auto`.

Example:
```lisp
(let ((count-1 (+ a b))   ;; count_1 = a + b
      (count-2 (+ c d)))  ;; count_2 = c + d
  (if (count > 10)
    ;; print a message if count > 10.
    (format #t "count = ~D, ~D~%" count-1 count-2)
    )    
  )
```

Note, if you define multiple variables in a `let`, you cannot refer to variables defined previously in the same `let`.  In the example above, `count-2`'s value couldn't be defined in terms of `count-1` for example.  See `let*`.

## `let*`
```lisp
(let ((var-name value)...) body...)
```

Define local variables. If you define multiple in a single `let*`, you can refer to previous variables.

Example:
```lisp
(let* ((count-1 (+ a b))
       (count-2 (+ count-1 d)))  ;; okay because we used let*
  (if (count > 10)
    ;; print a message if count > 10.
    (format #t "count = ~D, ~D~%" count-1 count-2)
    )    
  )
```

## Things related to enums
Not yet implemented

## `defmacro`

## Loop forms

## `&`

## `->`

## Type

## Compile-Time Size stuff

## `object-new`


# Compiler Features

## Compiling a list
When the compiler encounters a list like `(a b c)` it attempts to parse in multiple ways in this order:
1. A compiler form
2. A GOOS macro
3. An enum (not yet implemented)
4. A function or method call

## Compiling an integer
Integers can be specified as
- decimal: `1` or `-1234` (range of `INT64_MIN` to `INT64_MAX`)
- hex: `#x123`, `#xbeef` (range of `0` to `UINT64_MAX`)
- binary: `#b101010` (range of `0` to `UINT64_MAX`)

All integers are converted to the signed "integer in variable" type called `int`, regardless of how they are specified.
Integer "constant"s are not stored in memory but instead are generated by code, so there's no way to modify them.

## Compiling a string
A string constant can be specified by just putting it in quotes. Like `"this is a string constant"`.
There is an escape code `\` for string:
- `\n` newline
- `\t` tab character
- `\\` the `\` character
- `\"` the `"` character
- `\cXX` where `XX` is a two character hex number: insert this character.
- Any other character following a `\` is an error.

OpenGOAL stores strings in the same segment of the function which uses the string. I believe GOAL does the same. 

In GOAL, string constants are pooled per object file (or perhaps per segment)- if the same string appears twice, it is only included once. OpenGOAL currently does not pool strings. If any code is found that modifies a string "constant", or if repeated strings take up too much memory, string pooling will be added.  

For now I will assume that string constants are never modified.


## Compiling a float
A floating point constant is distinguished from an integer by a decimal point. Leading/trailing zeros are optional. Examples of floats: `1.0, 1., .1, -.1, -0.2`.  Floats are stored in memory, so it may be possible to modify a float constant. For now I will assume that float constants are never modified. It is unknown if they are pooled like strings.

Trivia: Jak 2 realized that it's faster to store floats inline in the code.

## Compiling a symbol
A `symbol` appearing in code is compiled by trying each of these in the following order
1. Is it `none`? (see section on `none`)
2. Try `mlet` symbols
3. Try "lexical" variables (defined in `let`)
4. Try global constants
5. Try global variables (includes named functions and all types)

## The Special `none` type
Anything which doesn't return anything has a return type of `none`, indicating the return value can't be used.  This is similar to C's `void`.

## GOAL Structs vs. C Structs
There is one significant difference between C and GOAL when it comes to structs/classes - GOAL variables can only be references to structs.

As an example, consider a GOAL type `my-type` and a C type `my_type`.  In C/C++, a variable of type `my_type` represents an entire copy of a `my_type` object, and a `my_type*` is like a reference to an existing `my_type` object.  In GOAL, an object of `my-type` is a reference to an existing `my-type` object, like a C `my_type*`.  There is no equivalent to a C/C++ `my_type`.

As a result you cannot pass or return a structure by value.

Another way to explain this is that GOAL structures (including `pair`) always have reference semantics.  All other GOAL types have value semantics.

## Pointers
GOAL pointers work a lot like C/C++ pointers, but have some slight differences:
- A C `int32_t*` is a GOAL `(pointer int32)`
- A C `void*` is a GOAL `pointer`
- In C, if `x` is a `int32_t*`, `x + 1` is equivalent to `uintptr_t(x) + sizeof(int32_t)`.  In GOAL, all pointer math is done in units of bytes.
- In C, you can't do pointer math on a `void*`. In GOAL you can, and all math is done in units of bytes.

In both C and GOAL, there is a connection between arrays and pointers.  A GOAL array field will have a pointer-to-element type, and a pointer can be accessed as an array.

One confusing thing is that a `(pointer int32)` is a C `int32_t*`, but a `(pointer my-structure-type)` is a C `my_structure_type**`, because a GOAL `my-structure-type` is like a C `my_structure_type*`.

## Inline Arrays
One limitation of the system above is that an array of `my_structure_type` is actually an array of references to structures (C `object*[]`).  It would be more efficient if instead we had an array of structures, laid out together in memory (C `object[]`).  

GOAL has a "inline array" to represent this.  A GOAL `(inline-array thing)` is like a C `thing[]`. The inline-array can only be used on structure types, as these are the only reference types.


## Fields in Structs
For a field with a reference type (structure/basic)
- `(data thing)` is like C `Thing* data;`
- `(data thing :inline #t)` is like C `Thing data;`
- `(data thing 12)` is like C `Thing* data[12];`. The field has `(pointer thing)` type.
- `(data thing 12 :inline #t)` is like `Thing data[12];`. The field has `(inline-array thing)` type

For a field with a value type (integer, etc)
- `(data int32)` is like C `int32_t data;`
- `(data int32 12)` is like `int32_t data[12];`. The field has `(array int32)` type.

Using the `:inline #t` option on a value type is not allowed.

## Dynamic Structs
GOAL structure can be dynamically sized, which means their size isn't determined at compile time. Instead the user should implement `asize-of` to return the actual size. 

This works by having the structure end in an array of unknown size at compile time. In a dynamic structure definition, the last field of the struct should be an array with an unspecified size. To create this, add a `:dynamic #t` option to the field and do not specify an array size.  This can be an array of value types, an array of reference types, or an inline-array of reference types.

### Unknown
Is the `size` of a dynamic struct:
- size assuming the dynamic array has 0 elements (I think it's this)
- size assuming the dynamic array doesn't 

These can differ by padding for alignment.


## Methods

## Method `_type_` type

## Calling Methods

## Built-in Methods

## New - How To Create GOAL Objects
GOAL has several different ways to create objects, all using the `new` form.

### Heap Allocated Objects
A new object can be allocated on a heap with `(new 'global 'obj-type [new-method-arguments])`.
This simply calls the `new` method of the given type. You can also replace `'global` with `'debug` to allocate on the debug heap.
Currently these are the only two heaps supported, in the future you will be able to call the new method with other arguments
to allow you to do an "in place new" or allocate on a different heap.

This will only work on structures and basics. If you want a heap allocated float/integer/pointer, create an array of size 1.
This will work on dynamically sized items.

### Heap Allocated Arrays
You can construct a heap array with `(new 'global 'inline-array 'obj-type count)` or `(new 'global 'array 'obj-type count)`.
These objects are not initialized. Note that the `array` version creates a `(pointer obj-type)` plain array, 
__not__ a GOAL `array` type fancy array.  In the future this may change because it is confusing.

Because these objects are uninitialized, you cannot provide constructor arguments.
You cannot use this on dynamically sized member types. However, the array size can be determined at runtime.

### Static Objects
You can create a static object with `(new 'static 'obj-type [field-def]...)`. For now it must be a structure or basic.
Each field def looks like `:field-name field-value`. The `field-value` is evaluated at compile time. For now, fields
can only be integers, floats, or symbols.

Fields which aren't explicitly initialized are zeroed, except for the type field of basics, which is properly initialized to the correct type.

This does not work on dynamically sized structures.

### Stack Allocated Objects
Currently only arrays of integers, floats, or pointers can be stack allocated.
For example, use `(new 'array 'int32 1)` to get a `(pointer int32)`. Unlike heap allocated arrays, these stack arrays
must have a size that can be determined at compile time. 

## Defining a `new` Method

## Integer Type
GOAL has some weird behavior when it comes to integers. It may seem complicated to describe, but it really makes the implementation simpler - the integer types are designed around the available MIPS instructions.

Integers that are used as local variables (defined with `let`), function arguments, function return values, and intermediate values when combining these are called "register integers", as the values will be stored in CPU registers. 

Integers that are stored in memory as a field of a `structure`/`basic`, an element in an array, or accessed through a `pointer` are "memory integers", as the values will need to be loaded/stored from memory to access them.

The "register integer" types are `int` and `uint`. They are 64-bit and mostly work exactly like you'd expect. Multiplication, division, and mod, are a little weird and are documented separately.

The "memory integer" types are `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, and `uint64`.

Conversions between these types are completely automatic - as soon as you access a "memory integer", it will be converted to a "register integer", and trying to store a "register integer" will automatically convert it to the appropriate "memory integer". It (should be) impossible to accidentally get this wrong.

### Side Note
- It's not clear what types `(new 'static 'integer)` or `(new 'stack 'integer)` are, though I would assume both are memory.

- If there aren't enough hardware registers, "register integers" can be spilled to stack, but keep their "register integer" types. This process should be impossible to notice, so you don't have to worry about it.

## Array Spacing
In general, all GOAL objects are 16-byte aligned and the boxing system requires this.  All heap memory allocations are 16-byte aligned too, so this is usually not an issue.  

## Truth
Everything is true except for `#f`. This means `0` is true, and `'()` is true.
The value of `#f` can be used like `nullptr`, at least for any `basic` object.  It's unclear if `#f` can/should be used as a null for other types, including `structure`s or numbers or pointers.

Technical note: the hex number `0x147d24` is considered false in Jak 1 NTSC due to where the symbol table happened to be allocated.  However, checking numbers for true/false shouldn't be done, you should use `(zero? x)` instead.

## Empty Pair

# Built-in Types

## `structure`
A structure is the parent type of all types with fields.

## `basic`
A basic is a structure with runtime type information. The first field is named `type` and contains the `type` of the basic.

# Implemented in Runtime Language Functions

## `string->symbol`

## `symbol->string`

## `format`
GOAL's `printf`.
```lisp
(format destination format-string args...)
```

The following destinations are currently supported:
- `#t` - print to the listener. After the current game frame is over, this will be sent to the compiler and you can see it at the GOAL prompt.
- `0` - print to `stdout` in the runtime immediately. This won't be visible from within the compiler - you must look at the runtime to see it.  This is useful for debugging if the runtime crashes before flushing the normal print buffer.
- `#f` - print to a new string allocated on the global heap.
- a GOAL `string` object - append to an existing string
- a GOAL `file-stream` object - currently unsupported but eventually may allow printing into a file somewhere

The global variable `*print-column*` can be used to automatically print at a certain indentation.  The very first thing printed during a frame will not have the indentation applied.

The format string escape sequences all start with `~`, then have arguments (possibly none), then have a single character code.  The arguments look like:
- `~A`, the `A` code with no arguments
- `~12A`, the `A` code with an integer argument of `12`
- `~'zA`, the `A` code with a character argument of `z`
- ``` ~`hello`A```, the `A` code with a string argument of `"hello"`
- `~12,'bA`, the `A` code with two arguments, integer `12` and character `b`


The escape codes are

### `~%`
Replace with a newline character.

### `~~`
Replace with a `~` character

### `~G` or `~g`
Replace with a C-style string given in `args`. Note that this will not work on a GOAL string.

### `~A` or `~a`
Print a boxed object given in `args`. Uses the `print` method. Can take optional arguments for length and padding character. If the `print` method gives something shorter than the length argument, it will be padded on the left with the padding character (default is space).  If the `print` method gives something too long, it will be truncated on the right. The last character printed will be changed to `~` to indicate it was truncated here.

There is an option to left justify instead but I don't think there's a way to turn it on.

### `~S` or `~s`
Very similar to `~A`, but a GOAL string will be printed without quotes around it.

### `~C` or `~c`
Print a single character (GOAL `uint8` or `int8` type)

### `~P` or `~p`
Print an object, similar to `~A`, but does not have optional arguments for length or padding. Instead, there is an optional argument to give a type name, then use that types `print` method.  This is useful for printing non-boxed objects. If no type name is given, behaves like `~A`.

### `~I` or `~i`
Like `~P`, but uses the inspect method instead.

### `~Q` or `~q`
Likely was supposed to be for printing 128-bit integers stored in registers, but will always prints `0`.  The `format` function is written in C using varargs, which doesn't support 128-bit register values.

### `~B` or `~b`
Print integer as binary. Optional arguments for pad-length and pad-character (default is 0). Won't truncate.

### `~D` or `~d`
Print integer as decimal (signed). Optional arguments for pad-length and pad-character (default is space). Won't truncate.

### `~X` or `~x`
Print integer as hex. Optional arguments for pad-length and pad-character (default is 0). Won't truncate.

### `~F`
Print floating point. Will print 4 digits after the decimal and pad with space to a width of 12 and does not accept any options.

### `~f`
Print floating point.  Takes optional arguments for pad-length (default don't pad), pad-character (default space), and precision (default 4).

## `~R` or `~r`
Like `~f` but scales in-game rotation units to degrees.  The float `65536.0` is 360 degrees.

### `~M` or `~m`
Like `~f` but scales in-game distance units to meters.  The float `4096.0` is 1 meter.

### `~E` or `~e`
Like `~f` for argument, but takes an integer time-code as an input and scales time-code units to seconds.  There are 300 ticks / second, which is the smallest number which has an integer number of ticks per NTSC and PAL frame.  Something very weird happens when the input is negative?

### `~T` or `~t`
Insert a tab character.

### Pass Through Codes
Some codes will be passed through automatically, along with any arguments.  This will also pass through arguments of `+` or `-`.  I believe that these options could later be interpreted by the code for printing on-screen characters.

The pass through codes are `H,J,K,L,N,V,W,Y,Z,h,j,k,l,n,v,w,y,z`.

Any other codes will result in an error message being printed.



## Load Stuff