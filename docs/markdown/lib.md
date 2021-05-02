# Standard Library

## Runtime Functions

### `string->symbol`

### `symbol->string`

### `format`

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


The escape codes are:

#### `~%`

Replace with a newline character.

#### `~~`

Replace with a `~` character

#### `~G` or `~g`

Replace with a C-style string given in `args`. Note that this will not work on a GOAL string.

#### `~A` or `~a`

Print a boxed object given in `args`. Uses the `print` method. Can take optional arguments for length and padding character. If the `print` method gives something shorter than the length argument, it will be padded on the left with the padding character (default is space).  If the `print` method gives something too long, it will be truncated on the right. The last character printed will be changed to `~` to indicate it was truncated here.

There is an option to left justify instead but I don't think there's a way to turn it on.

#### `~S` or `~s`

Very similar to `~A`, but a GOAL string will be printed without quotes around it.

#### `~C` or `~c`

Print a single character (GOAL `uint8` or `int8` type)

#### `~P` or `~p`

Print an object, similar to `~A`, but does not have optional arguments for length or padding. Instead, there is an optional argument to give a type name, then use that types `print` method.  This is useful for printing non-boxed objects. If no type name is given, behaves like `~A`.

#### `~I` or `~i`

Like `~P`, but uses the inspect method instead.

#### `~Q` or `~q`

Likely was supposed to be for printing 128-bit integers stored in registers, but will always prints `0`.  The `format` function is written in C using varargs, which doesn't support 128-bit register values.

#### `~B` or `~b`

Print integer as binary. Optional arguments for pad-length and pad-character (default is 0). Won't truncate.

#### `~D` or `~d`

Print integer as decimal (signed). Optional arguments for pad-length and pad-character (default is space). Won't truncate.

#### `~X` or `~x`

Print integer as hex. Optional arguments for pad-length and pad-character (default is 0). Won't truncate.

#### `~F`

Print floating point. Will print 4 digits after the decimal and pad with space to a width of 12 and does not accept any options.

#### `~f`

Print floating point.  Takes optional arguments for pad-length (default don't pad), pad-character (default space), and precision (default 4).

#### `~R` or `~r`

Like `~f` but scales in-game rotation units to degrees.  The float `65536.0` is 360 degrees.

#### `~M` or `~m`

Like `~f` but scales in-game distance units to meters.  The float `4096.0` is 1 meter.

#### `~E` or `~e`

Like `~f` for argument, but takes an integer time-code as an input and scales time-code units to seconds.  There are 300 ticks / second, which is the smallest number which has an integer number of ticks per NTSC and PAL frame.  Something very weird happens when the input is negative?

#### `~T` or `~t`

Insert a tab character.

#### Pass Through Codes

Some codes will be passed through automatically, along with any arguments.  This will also pass through arguments of `+` or `-`.  I believe that these options could later be interpreted by the code for printing on-screen characters.

The pass through codes are `H,J,K,L,N,V,W,Y,Z,h,j,k,l,n,v,w,y,z`.

Any other codes will result in an error message being printed.

## Block Forms

### `begin`

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

The `begin` form is used a lot in macros, but not that much in code. It's generally used when you want to execute multiple things, but fit it into a single form.

### `block`

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

### `return-from`

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

### `return`

Exit a function early.

```lisp
(return value)
```

Has the same behavior as `(return-from #f value)`.

### `label`

Create a named label for `goto` or `goto-when`.

```lisp
(label label-name)
```

The label spaces are per-function and not nested. You can't jump from function to function. You can't jump in or out of functions which end up getting inlined. You can't jump in or out of an anonymous lambda function. You can jump in and out of `let`s.

See `goto` for an example.

Labels are used extremely rarely. Usually only in inline assembly and part of macros for `while` loops and similar.

### `goto`

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

### `top-level`

This form is reserved by the compiler. Internally all forms in a file are grouped under a `top-level` form, so you may see this in error messages. Do not name anything `top-level`.

## Control Flow Forms

### GOAL "Two Element" Conditions

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

### `not`

```lisp
(not value)
```

If value is `#f`, return `#t`. Otherwise return `#f`.

Using `not` on a "two element condition" will be optimized to modifying that condition.

### `when-goto`

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

### `cond`

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

### `if`

A normal Lisp/Scheme `if`.

```lisp
(if test true-case [false-case])
```

The value of the entire statement is the value of the taken case. The false case can be left out. If the condition evaluates to false and the false case is left out, `#f` is returned.  The return type follows the same logic as `cond`, as `if` is a macro using `cond`.

### `when`

A normal Lisp/Scheme `when`.

```lisp
(when test forms...)
```

If `test` is truthy, evaluate all forms and return the value of the last one. If `test` isn't true, return `#f`.

### `unless`

A normal Lisp/Scheme `unless`.

```lisp
(unless test forms...)
```

If `test` is false, evaluate all forms and return the value of the last one. If `test` isn't false, return `#f`.

## Definition Forms

### `set!`

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

### `define`

Define a global symbol

```lisp
(define symbol-name value)
```

Kind of like `set!`, but works exclusively on global symbols. Also sets the type of the global symbol. If the global symbol already has a type, and the `define` tries to change it, there will be an error or warning. (to be determined)

### `define-extern`

Inform the compiler about the type of a global symbol

```lisp
(define-extern symbol-name type)
```

Useful to forward declare functions or to let the compiler know about things in the C Kernel. See `goal-externs.gc` for where all the C Kernel stuff is declared.

### `let`

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

### `let*`

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

## Functions

### `defun`

Define a new named global function!

```lisp
(defun name arg-list ["doc-string"] body...)
arg-list = (arg...)
arg = (arg-name arg-type)|arg-name
```

Example:

```lisp
(defun 1/ ((x float))
  "Compute 1.0 / x"
  (/ 1.0 x)
  )
```

Define a new function with the given name. Note that until the `defun` itself executes, the function __cannot be used!__. So don't call functions before the `defun`.  This is unlike C, where you can forward declare a function and use it before the actual definition.

There is an optional docstring. Currently the docstring is just thrown away but in the future we could save them and and generate documentation or something.

### `defmethod`

Define a method!

```lisp
(defmethod method-name type-name arg-list ["doc-string"] body...)
```

In many ways is similar to `defun`. See section on methods for more details.

You can only `defmethod` if one of these two is true:
- You are overriding a parent method
- You have declared this method in the type's `deftype`

Like `defun`, the method only exists after the `defmethod` executes. Before that the method may be undefined or the parent's method.

### `inline`

Make this function call inlined.

```lisp
(inline function-name)
```

Example: inline call `my-function`

```lisp
((inline my-function) my-arg)
```

Attempts to make the function call inline. If it is not possible, it will throw an error. You can't save the value of `(inline my-function)`, only use it immediately in a function call. You must provide a name of a global function as the function, not a function pointer.

Methods cannot be inlined. (Maybe new methods can be in original GOAL?)

### `lambda`

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

### `declare`

Set options for a function or method

```lisp
(declare [(inline)] [(allow-inline)] [(disallow-inline)] [(asm-func return-typespec)] [(print-asm)])
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
- `print-asm` if codegen runs on this function (`:color #t`), disassemble the result and print it. This is intended for compiler debugging.
- `asm-func` will disable the prologue and epilogue from being generated. You need to include your own `ret` instruction or similar. The compiler will error if it needs to use the stack for a stack variable or a spilled register. The coloring system will not use callee saved registers and will error if you force it to use one.  As a result, complicated GOAL expression may fail inside an `asm-func` function. The intent is to use it for context switching routines inside in the kernel, where you may not be able to use the stack, or may not want to return with `ret`.  The return type of an `asm-func` must manually be specified as the compiler doesn't automatically put the result in the return register and cannot do type analysis to figure out the real return type.
- `allow-saved-regs` allows an `asm-func`'s coloring to use saved registers without an error. Stacks spills are still an error. The compiler will not automatically put things in a saved register, you must do this yourself. The move eliminator may still be used on your variables which use saved registers, so you should be careful if you really care about where saved variables are used.

This form will probably get more options in the future.

### `local-vars`

Declare variables local to a function, without an initial value. This will be used by the decompiler before `let` has been fully implemented.

```lisp
(local-vars (name type-spec)...)
```

The name can be any valid symbol. The scope of the variable is _always_ the function scope. Other scopes inside a function will always hide variables declared with `local-vars`.  The type can be any GOAL typespec. If you use `float`, you get a floating point register, otherwise you get a normal GPR.

It's recommended to avoid using this form.

## Macro Forms

### `#cond`

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

### `quote` / `'`

The reader will expand `'thing` to `(quote thing)`

```lisp
(quote thing)
```

Currently `quote` supports:
- `'a-symbol` - will return the symbol `a-symbol`
- `'()` - will return the empty list.

In the future, it should support quoted lists of static data.

### `defglobalconstant`

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

### `mlet`

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

## Math Forms

Math forms will look at the type of the first argument to determine the "mode".  So if you have `(+ 1 1.2)`, it will convert the `1.2` to an integer, do the add, and return an integer.

### `+`

Addition. Can take 1 or more arguments. `(+ 1)` will give you `1`, like you'd expect.

```lisp
(+ form...)
```

Works on integers and floats. Integer add is 64-bit and wrapping.

### `-`

Subtraction or negative. Can take 1 or more arguments.

```lisp
(- form...)
```

`(- 1)` gives you `-1`, but `(- 1 3)` gives you `-2`, which may be slightly confusing at first. Works on integers and floats. Integer subtract is 64-bit and wrapping.

### `*`

Multiplication. Can take 1 or more arguments.

```lisp
(* form...)
```

`(* 7)` will become `7`, as you'd expect. Works on integers and floats. The exact behavior for integers needs to be explored more because the EE is weird. GOAL generates different code for multiplication of signed vs. unsigned integers.

### `/`

Division. Takes exactly two arguments

```lisp
(/ dividend divisor)
```

Works on floats and integers. The exact behavior for integers needs to be explored more because the EE is weird. GOAL generates different code for division of signed vs. unsigned integers.

### `mod`

Modulo operation. Takes exactly two arguments.

```lisp
(/ dividend divisor)
```

Works on integers only. The exact behavior needs to be explored because the EE is weird. It is unknown if the same code is generate for signed/unsigned mod.

### `slhv`, `sarv`, `shrv`

```lisp
(shlv value shift-amount)
```

The exact behavior of GOAL shifts isn't fully understood, so these are temporary wrappers around x86-64 variable shift instructions.
- `shlv` shift left variable
- `sarv` shift arithmetic right variable
- `shrv` shift logical right variable
64-bit operation.

### `logand`

Bitwise And

```lisp
(logand a b)
```

64-bit operation.

### `logior`

Bitwise Inclusive Or (normal Or)

```lisp
(logior a b)
```

64-bit operation.

### `logxor`

Bitwise Exclusive Or (Xor

```lisp
(logxor a b)
```

64-bit operation.

### `lognot`

Bitwise Not

```lisp
(lognot a)
```

64-bit operation.

## Type Forms

### `defmethod`

### `deftype`

### `method-of-object`

Get a method of an object.

```lisp
(method-of-object object method-name)
```

This form takes an object and gets the method from it. If the object has runtime type information, will consult the method table at runtime to get a possibly more specific method than what is available at compile time. This uses the same lookup logic as method calling - see the section on method calls for more information.

### `method-of-type`

Get a method of a type or an object.

```lisp
(method-of-type type method-name)
```

The first form of this takes a type name and method name and returns a GOAL `function` for this method. For example:

```lisp
(method string inspect)
```

will return the `inspect` method of `string`.

### `car` and `cdr`

Get element from pair

```lisp
(car some-pair)
(cdr some-pair)
```

The type of the result is always `object`, as pairs can hold any `object`. The type-check for `car` and `cdr` is relaxed - it allows it to be applied to any `pair` or `object`.  The reason for allowing `object` is so you can write `(car (car x))` instead of `(car (the pair (car x)))`. However, if the argument to `car` is not a `pair`, you will get garbage or a crash.

### `new`

```lisp
(new [allocation] [new-type-specification] [args])
```

See section on creating new GOAL objects.

### `print-type`

Print the type of some GOAL expression at compile time.

```lisp
(print-type form)
```

This is mainly used to debug the compiler or figure out why some code is failing a type check. The thing inside is compiled fully and used as the result of `print-type`. Example:

```lisp
(print-type "apples")        ;; [TYPE] string
(print-type (+ 12 1.2))      ;; [TYPE] int
(print-type (the float 12))  ;; [TYPE] float
```

### `the`

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

```lisp
(the binteger 12) ;; boxed integer for 12
(the float 1)     ;; floating point 1.0
(the int 1.234)   ;; signed integer 1
```

Examples of common casts:

```lisp
(the uint 1)             ;; unsigned integer, doesn't do any modification if input is already an int.
(the (pointer uint8) x)  ;; like C (uint8_t*)x
(the string (car x))     ;; if you know (car x) is a string, do this.
```

Examples of dangerous things you can do but probably shouldn't:

```lisp
(the string 1.234) ;; take the binary representation of 1.234 and treat it as a GOAL memory address to string data.
(the float 'bean)  ;; take the GOAL memory address of the "bean" symbol and treat it as the binary representation of a float.
```

There are some weird edge cases with `the` that are worth mentioning, if you try to set the value of something you've used `the` on. In the future the compiler should block you from doing something bad, but this is not yet implemented.

```lisp
(set! (-> (the (pointer uint8) x)) 1)      ;; OK, not casting the actual value
(set! (the int (-> obj x)) 1)              ;; NOT OK. Int is a value type
;; Will actually work if x is already an int/uint, and with non-numeric values types like pointer.  Avoid just to be safe.
(set! (the string (-> obj x)) "test")      ;; OK, string is a basic, which is a reference type.
```

This becomes more clear if we look at the C equivalent:

```lisp
*(uint8_t*)(a->x) = 1; // makes sense

(int)(a->x) = 1; // doesn't make sense

*(thing*)(a->x) = thing(); // makes sense
```

### `the-as`

Convert between types always using `reinterpret_cast`

```lisp
(the-as type thing)
```

cppreference.com explains this idea clearly with "Converts between types by reinterpreting the underlying bit pattern."

The recommendation is to prefer `the` in almost all cases, unless you want to set an exact bit pattern of a `float`, or want to examine the bits in a `float`.

Example:

```lisp
(the-as int 1.234) ;; result happens to be 1067316150
```

None of the edge cases of `the` apply to `the-as`.

### `size-of`

Get the size of a type, in bytes.

```lisp
(size-of <type-name>)
```

Get the size of a type, by name. The type must be a plain name, like `pointer` or `dma-bucket`. Compound types are not supported. It works on value and structure types and returns the size in memory. For dynamic types, it returns the size as if the dynamic part has 0 size. For weird types like `none` it throws an error.

This value can be used in most places where the compiler is expecting a constant integer as well, such as the size of a stack array, which must be known at compile time.

Example:

```lisp
(size-of dma-bucket)
```

Returns the size of the `dma-bucket` type which is 16 bytes.

### Pointer Math

Not implemented well yet.

## Inline Assembly Forms

In general, assembly forms have a name that begins with a `.`. They all evaluate to `none` and copy the form of an x86-64 instruction. For example `(.sub dst src)`. A destination must be a settable register (ok if it's spilled). So you can't do something like `(.sub (-> obj field) x)`. Instead, do `(set! temp (-> obj field))`, `(.sub temp x)`, `(set! (-> obj field) temp)`.   The sources can be any expression, or a register. This allows you to mix high-level code with assembly easily, like `(.mov rax (-> obj field))` or `(.push (+ 1 (-> obj field)))`.

By default, assembly forms work with the coloring system. This means that assembly and high level expression can be mixed together without clobbering each other. It also means use of callee-saved registers will cause them to be backed up/restored in the function prologue and epilogue.  Use of weird registers like `r15`, `r14`, and `rsp` works as you would expect with the coloring system.

But you can also request to skip this with `:color #f` option, like `(.push my-reg-var :color #f)`. Be very careful with this. The `:color #f` option will only work with register variables from `rlet` which have a manually specified register. It will entirely bypass the coloring system and use this register. Use of this near high level GOAL variables is extremely dangerous and should be done very carefully or avoided, as the GOAL compiler will not know that you could be modifying its registers.  In a form with `:color #f`, you cannot use higher level code or variables - all variables must be defined in `rlet`s. This is because higher level expressions and variables cannot be used without the coloring system.

### `rlet`

```lisp
(rlet ((var-name [:reg reg-name] [:class reg-class] [:type typespec] [:reset-here #t|#f])...)
  body...
  )
```

Create register variables. You can optionally specify a register with the `:reg` option and a register name like `rax` or `xmm3`. The initial value of the register is not set. If you don't specify a register, a GPR will be chosen for you by the coloring system and it will behave like a `let`.  If you don't specify a register, you can specify a register class (`gpr`, a normal 64-bit integer register; `fpr`, a 32-bit single precision float; or  `vf`, and 128-bit floating point vector register) and the compiler will pick a GPR or XMM for you.

If you pick a callee-saved register and use it within the coloring system, the compiler will back it up for you in the prologue and restore it in the epilogue.
If you pick a special register like `rsp`, it won't be backed up.

Inside the `rlet`, all uses of `var-name` will always be in the given register.  If the variable goes dead (or is never live), the compiler may reuse the register as it wants.  The compiler may also spill the variable onto the stack.  Of course, if you are in an `asm-func`, the stack will never be used.  Be extremely careful about using "normal" registers without the coloring system and with higher-level code as the compiler may use your "normal" register as a temporary.  If you read the value of a register and use the coloring system, the variable will then be alive starting at the beginning of the function, and will make that register unavailable to the compiler and other `rlet`s that occur before. This is useful to preserve the value of a temporary register if needed, but can also be undesirable in other cases.  If you add the `:reset-here #t` flag, it will make the variable dead until the start of the `rlet`. It "resets" the value of the register in the coloring system at the start of the `rlet`.  The default value is false. It is recommended to keep the default value when accessing specific registers that are also normally used by the compiler.  For special registers like `rsp`, `r15`, `r14`, and `r13`, if you plan to use them with the coloring system, it is recommended to set the `reset-here` flag.

Here is an example of using an `rlet` to access registers:

```lisp
(defun get-goal-rsp-2 ()
  "Get the stack pointer as a GOAL pointer"
  (rlet ((rsp :reg rsp :type uint)
         (off :reg r15 :type uint))
        (the pointer (- rsp off))
        )
  )
```

### `.sub`

```lisp
(.sub dest src [:color #t|#f])
```

x86-64 subtraction (64-bit). If coloring is on (the default), the `dest` must be a settable register (`rlet` var, `let` var, function argument, ...). It can't be a place like a symbol, field, stack variable, etc.  If coloring is off, both `src` and `dest` must be registers defined and constrained in an enclosing `rlet`.

Example:

```lisp
(defun get-goal-rsp ()
  (declare (asm-func uint))
  (rlet ((rsp :reg rsp :type uint)
         (off :reg r15 :type uint)
         (ret :reg rax :type uint)
         )

        ;; mov rax, rsp
        (set! ret rsp)
        ;; sub rax, r15
        (.sub ret off)
        ;; ret
        (.ret)
        )
  )
```

### `.add`

```lisp
(.add dest src [:color #t|#f])
```

Addition (64-bit). Similar to subtraction.

### `.jr`

```lisp
(.jr addres-reg [:color #t|#f])
```

Jump-register. Jumps to the address given. The address is treated as a 64-bit pointer, not a GOAL pointer.

### `.load-sym`

```lisp
(.load-sym dest symbol-name [:sext #t|#f] [:color #t|#f])
```

Load the value of a symbol into a register.  By default, it will look at the type of the symbol to determine if it should be sign extended or not. You can override this with the `:sext` option if needed. The symbol must be known to the type system.

### `.push`

```lisp
(.push src [:color #t|#f])
```

The x86-64 push instruction. Does a 64-bit GPR.  The `src` can be any expression that can be put in a gpr if color is on. Otherwise it must be a register defined and constrained in an enclosing `rlet`.

### `.pop`

```lisp
(.pop dst [:color #t|#f])
```

The x86-64 pop instruction.  Does a 64-bit GPR. The `dst` can be any expression which evaluates to a settable register if color is on. Otherwise it must be a register defined and constrained in an enclosing `rlet`.

### `.ret`

```lisp
(.ret [:color #t|#f])
```

The x86-64 ret instruction. The color option does nothing. This is not recognized as a control flow instruction by the coloring system. It does not touch the return register `rax`.

### `.mov`

```lisp
(.mov dst src [:color #t|#f])
```

Move between two registers. The `dst` should be a register (either `rlet` or `let` variable), and the `src` can be a register or any expression.  The following moves are supported:
- `gpr` to `gpr`
- `fpr` to `fpr` (only moves lower 32-bits of the xmms, uses `movss`)
- `vf` to `vf` (moves all 128-bits of the xmms, uses `vmovaps`)
- `gpr` to `fpr` (only moves 32-bits, uses `movd`)
- `fpr` to `gpr` (only moves 32-bits, upper 32-bits are zero, uses `movd`)
This code generation is identical to using a `(set! dst src)` form.

### `.nop.vf`

```lisp
(.nop.vf)
```

Inserts a `FNOP` assembly instruction, which is fundamentally the same as a `NOP`. It is a 2-byte instruction.

### `.nop` or `(nop!)`

```lisp
(.nop)
;; or
(nop!)
```

Inserts a single-byte `nop`.

### `.wait.vf`

```lisp
(.wait.vf)
```

Inserts a `FWAIT` assembly instruction, x86 does not require as much synchronization as the PS2's VU registers did, but it has a purpose in rare cases. It is a 2-byte instruction.

### `.lvf`

```lisp
(.lvf dst-reg src-loc [:color #t|#f] [:offset <int>])
```

Load a vector float register from `src-loc`. The `dst-reg` must be a vector float register. The `src-loc` can be a gpr containing a GOAL pointer or expression which gives a GOAL pointer. There is no type checking on the `src-loc` so be careful. The load uses `vmovaps`, so the source must be 16-byte aligned.

If the source is in the form `base-reg + constant-offset`, like from a `(&-> my-object my-inline-vector-field)`, the constant offset will be folded into the load instruction like `vmovaps xmm1, [r15 + rax + 12]`.  An explicit offset can be provided via the `:offset` keyword, and will be used if applicable.

If the source is an immediate `(new 'static ...)` form that results in a statically allocated variable, it will use `RIP` relative addressing (32-bit immediate) form. This means that the code:

```lisp
(.lvf vf1 (new 'static 'vector :x 1.2 :y 2.3 :z 3.4 :w 5.6))
```

will be just a single instruction to do a `vmovaps xmm1, [rip + XXX]`.

### `.svf`

```lisp
(.svf dst-loc src-reg [:color #t|#f] [:offset <int>])
```

Store a vector float. Works similarly to the `lvf` form, but there is no optimized case for storing into a static because this isn't allowed in GOAL.

### Three operand vector float operations.

```lisp
(.<op-name>[.<broadcast-element>].vf dst src0 src1 [:color #t|#f] [:mask #b<0-15>])
```

All the three operand forms work similarly. You can do something like `(.add.vf vf1 vf2 vf3)`. All operations use the similarly named `v<op-name>ps` instruction, xmm128 VEX encoding. We support the following `op-name`s:
- `xor`
- `add`
- `sub`
- `mul`
- `min`
- `max`

An optional `:mask` value can be provided as a binary number between 0-15 (inclusive).  This determines _which_ of the resulting elements will be committed to the destination vector.  For example, `:mask #b1011` means that the `w`, `y` and `x` results will be committed.  Note that the components are defined left-to-right which may be a little counter-intuitive -- `w` is the left-most, `x` is the right-most.  This aligns with the PS2's VU implementation.

Additionally, all of these operations support defining a single `broadcast-element`.  This can be one of the 4 vector components `x|y|z|w`.  Take the following for an example: `(.add.x.xyzw vf10, vf20, vf30)`, translates into:

```cpp
vf10[x] = vf20[x] + vf30[x]
vf10[y] = vf20[y] + vf30[x]
vf10[z] = vf20[z] + vf30[x]
vf10[w] = vf20[w] + vf30[x]
```

### Three operand vector float operations with the accumulator

```lisp
(.<op-name>[.<broadcast-element>].vf dst src0 src1 acc [:color #t|#f] [:mask #b<0-15>])
```

There are a few functions that will perform multiple operations involving the accumulator. We support the following `op-name`s:
- `add.mul` - Calculate the product of `src0` and `src1` and add it to the value of `acc` => `acc + (src0 * src1)`
- `sub.mul` - Calculate the product of `src0` and `src1` and subtract it from the value of `acc` => `acc - (src0 * src1)`

An optional `:mask` value can be provided as a binary number between 0-15 (inclusive).  This determines _which_ of the resulting elements will be committed to the destination vector.  For example, `:mask #b1011` means that the `w`, `y` and `x` results will be committed.  Note that the components are defined left-to-right which may be a little counter-intuitive -- `w` is the left-most, `x` is the right-most.  This aligns with the PS2's VU implementation.

Additionally, all of these operations support defining a single `broadcast-element`.  This can be one of the 4 vector components `x|y|z|w`.

### `.abs.vf`

```lisp
(.abs.vf dst src [:color #t|#f] [:mask #b<0-15>])
```

Calculates the absolute value of the `src` vector, and stores in the `dst` vector.

### `.div.vf` and `.sqrt.vf`

```lisp
(.div.vf dst src1 src2 :ftf #b<0-3> :fsf #b<0-3> [:color #t|#f])
```

Calculates the quotient of _one_ of `src1`'s components specified by `fsf` _one_ of `src2`'s components specified by `ftf` and stores in every component of `dst`

```lisp
(.sqrt.vf dst src :ftf #b<0-3> [:color #t|#f])
```

Calculates the square-root of _one_ of `src`'s components specified by `ftf` and stores in every component of `dst`

These instructions are interesting as they behave differently than the other math operations.  In the original VU, results were stored in a seperate `Q` register, which was _NOT_ 128-bit.  Instead it was a 32-bit register, meaning you have to pick which component from `src` you want to use. `:fsf` and `:ftf` are used to accomplish this, as usual, this is through bit flags -- `00` will select `x` and `11` will select `w`.

As `dst` is just yet another vector / xmm register in x86, things are kept simple and the quotient is copied to _all_ packed single-float positions.  This allows:
- Selecting any of the resulting vector slots will be equal to the quotient.
- Since the low-floating-point (X) is defined, the xmm register should function as expected for normal math operations

### `.outer.product.vf`

```lisp
(.outer.product.vf dst src1 src2 [:color #t|#f])
```

Calculates the outer-product of `src1` and `src2` and stores the result in `dst`.  _ONLY_ the x,y,z components are considered, and `dst`'s `w` component will be untouched.  The following example illustrates what the outer-product is:

Given 2 vectors `V1 = <1,2,3,4>` and `V2 = <5,6,7,8>` and assume `VDEST = <0, 0, 0, 999>`
The outer product is computed like so (only x,y,z components are operated on):

`x = (V1y * V2z) - (V2y * V1z) => (2 * 7) - (6 * 3) => -4`

`y = (V1z * V2x) - (V2z * V1x) => (3 * 5) - (7 * 1) =>  8`

`z = (V1x * V2y) - (V2x * V1y) => (1 * 6) - (5 * 2) => -4`

`w = N/A, left alone                                => 999`

> `VDEST = <-4, 8, -4, 999>`

### `.blend.vf`

```lisp
(.blend.vf dst src0 src1 mask [:color #t|#f])
```

Wrapper around `vblendps` (VEX xmm128 version) instruction. The `mask` must evaluate to a constant integer at compile time. The integer must be in the range of 0-15.

### `.itof.vf` and `.ftoi.vf`

```lisp
(.itof.vf dst src [:mask mask-val] [:color #t|#f])
(.ftoi.vf dst src [:mask mask-val] [:color #t|#f])
```

Wrapper around `vcvtdq2ps` and `vcvtps2dq` to convert packed 32-bit signed integers to packed 32-bit floats and back.  The `mask` and `color` arguments behave like other assembly operations.

### `.pw.sra`, `.pw.srl`, and `pw.sll`

```lisp
(.pw.sra dst src shift-amount [:mask mask-val] [:color #t|#f])
(.pw.srl dst src shift-amount [:mask mask-val] [:color #t|#f])
(.pw.sll dst src shift-amount [:mask mask-val] [:color #t|#f])
```

Wrapper around `vpsrld`, `vpsrad`, and `vpslld`. Does shifts on each of the 4 32-bit integers in the register.

### `.pextlw`, `.pextuw`, `.pcpyud`, `.pcpyld`, `.pceqw`, `.ppach`

```lisp
(.pextlw dst src0 src1 [:color #t|#f])
(.pextuw dst src0 src1 [:color #t|#f])
(.pcpyud dst src0 src1 [:color #t|#f])
(.pcpyld dst src0 src1 [:color #t|#f])
(.pceqw dst src0 src1 [:color #t|#f])
(.ppach dest src0 src1)
```

Equivalents of the EE's MMI instructions with the same name. These can only be used on 128-bit variables.  Most map to single x86 instructions:
- `pextlw` is `VPUNPCKLDQ` (sources swapped)
- `pextuw` is `VPUNPCKHDQ` (sources swapped)
- `pcpyld` is `VPUNPCKLQDQ` (sources swapped)
- `pcpyud` is `VPUNPCKHQDQ` (sources _not_ swapped)
- `pceqw` is `VPCMPEQD`

Some map to multiple instructions. These must use the coloring system.
- `ppach` is a sequence of 7 instructions (`VPSHUFLW`, `VPSHUFHW`, `VPSRLDQ`, `VPUNPCKLQDQ`).

## TODO

### Things related to enums
Not yet implemented

### `defmacro`

### Loop forms

### `&`

### `->`

### Type

### Compile-Time Size stuff

### `object-new`