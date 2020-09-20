# OpenGOAL Document
This is the main documentation for the OpenGOAL language. It explains the syntax of OpenGOAL programs and also how OpenGOAL can be decompiled from original GOAL.  It's broken up into three sections:

1. Compiler forms, which are things built-in to the compiler and have names.  Like `+` or `deftype`.
2. Important Syntax Macros, which are really important features of the languages that are implemented as macros, like `if` and `defun`.
3. Compiler features, which are things that are built-in to the compiler but don't have explicit names. Like calling functions or the rules for how names are scoped.
4. Built-in types

Each "feature" of the compile will explain its syntax, a guess at how often it's used, an example or two in OpenGOAL, and an example or two in MIPS GOAL. There will also be details on the order of evaluation.

The syntax description uses these rules:
- Something `[in-brackets]` is optional and can be left out.
- Something like `[:type type-name]` means there is an optional named argument. To use it, you must put `:type type-name`, replacing `type-name` with what you want.
- When there are multiple choices, they are separated by `|`. Example: `#t|#f` is either `#t` or `#f`.
- A `...` means more of the thing before can be included. Example `(f arg...)` can have multiple arguments.

When talking about ordering things, GOAL code fragments can be `compile`d and `flush`ed as two separate things. For the most part, everything is done during compilation, like calling functions. But consider `(set! (-> my-object value) x)`. We don't actually want the value of `(-> my-object value)`, we want to set its value. The `compile` stage gets us a settable thing (if possible), and the `flush` stage gets us a value we can actually use.


# Compiler Forms

### `begin`
A `begin` form is just a list of other forms which are executed in order. A `begin` form evaluates to the value of the last form.
```
(begin form...)
```

Example:
```
(begin
	(print "hello ")
	(print "world!")
	7
	)
```
will print `hello world!` and the value of the entire form is `7`.

I believe in `begin` and similar "do everything in the list" forms, each form is `compile`d then `flush`ed. 

The `begin` form is used a lot in macros, but not that much in code. It's generally used when you want to execute multiple things, but fit it into a single form.


### `block`
A `block` form is pretty similar to a begin, except the `block` has a name. You can then "return" from the block early with `return-from`.
```
(block block-name form...)
```

Example:
```
(block my-block
	(print "hello ")
	(return-from my-block 7)
	(print "world")
	8
	)
```
will print `hello ` only and the value of the entire `block` form is `7`.  The type of the `block` is the most specific type that describes all of the possible return values from any `return-from` or from reaching the end (even if its technically not possible to reach the end).

Block is used rarely, and possibly almost never?

### `return-from`
Used to exit a `block` or function early. 
```
(return-from block-name value)
```

Looks up the block and exits from it with the value. You can exit out nested blocks. If you are enclosed in multiple blocks with the same name, exits from the inner-most one with a matching name. Everything in a function is wrapped in a block named `#f`, so you can use `(return-from #f x)` to return early from a function with `x`.  Unlike returning from a block, using `return-from` to exit a function currently does _not_ modify the return type of your function and does _not_ check the type of what you return. 

Example
```
(if (is-a-match? x)
  (return-from #f x)
  )
```
if `x` is a match, returns `x` from the function (not shown) immediately.

The `return-from` form is very rarely used to return from a block, but sometimes used to return from a function.

### `label`
Create a named label for `goto` or `goto-when`.
```
(label label-name)
```
The label spaces are per-function and not nested. You can't jump from function to function. You can't jump in or out of functions which end up getting inlined. You can't jump in or out of an anonymous lambda function. You can jump in and out of `let`s.

Labels are used extremely rarely. Usually only in inline assembly and part of macros for `while` loops and similar.

### `goto`
Jump to a label.
```
(goto label-name)
```
The label must be in the current label space.

Example:
```
(if skip-code?
	(goto end)
	)

;; code here runs only if skip-code is false.

(label end)
```

Like label, used very rarely outside of macros and inline assembly. Try to avoid using `goto`.

# Important Syntax Macros

# Compiler Features

### Compiling a list
When the compiler encounters a list like `(a b c)` it attempts to parse in multiple ways in this order:
1. A compiler form
2. A GOOS macro
3. An enum (not yet implemented)
4. A function or method call

### Compiling an integer
Integers can be specified as
- decimal: `1` or `-1234` (range of `INT64_MIN` to `INT64_MAX`)
- hex: `#x123`, `#xbeef` (range of `0` to `UINT64_MAX`)
- binary: `#b101010` (range of `0` to `UINT64_MAX`)
All integers are converted to the signed "integer in variable" type called `int`, regardless of how they are specified.
Integer "constant"s are not stored in memory but instead are generated by code, so there's no way to modify them.

### Compiling a string
A string constant can be specified by just putting it in quotes. Like `"this is a string constant"`.
There is an escape code `\` for string:
- `\n` newline
- `\t` tab character
- `\\` the `\` character
- `\"` the `"` character
- Any other character following a `\` is an error.

OpenGOAL stores strings in the same segment of the function which uses the string. I believe GOAL does the same. 

In GOAL, string constants are pooled per object file (or perhaps per segment)- if the same string appears twice, it is only included once. OpenGOAL currently does not pool strings. If any code is found that modifies a string "constant", or if repeated strings take up too much memory, string pooling will be added.  

For now I will assume that string constants are never modified.


### Compiling a float
A floating point constant is distinguished from an integer by a decimal point. Leading/trailing zeros are optional. Examples of floats: `1.0, 1., .1, -.1, -0.2`.  Floats are stored in memory, so it may be possible to modify a float constant. For now I will assume that float constants are never modified. It is unknown if they are pooled like strings.

Trivia: Jak 2 realized that it's faster to store floats inline in the code.

### Compiling a symbol

# Built-in Types

# GOAL MIPS Examples
Example in `vector` with flushing:
```
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; .function vector3s+!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
L8:
    daddiu sp, sp, -16
    sd fp, 8(sp)
    or fp, t9, r0
    daddiu v1, fp, L109 ;; string: "Add 2 vectors3."
    lwc1 f0, 0(a1)
    lwc1 f1, 0(a2)
    add.s f0, f0, f1
    swc1 f0, 0(a0)
    lwc1 f0, 4(a1)
    lwc1 f1, 4(a2)
    add.s f0, f0, f1
    swc1 f0, 4(a0)
    lwc1 f0, 8(a1)
    lwc1 f1, 8(a2)
    add.s f0, f0, f1
    swc1 f0, 8(a0)
    or v0, a0, r0
    ld fp, 8(sp)
    jr ra
    daddiu sp, sp, 16
```
The `daddiu v1, fp, L109` loads a `string` into the `v1` register which is never used, immediately after the prologue. This will only happen if the value is flushed. This is very likely a documentation comment that accidentally got included as a string constant. It's unused, so there was likely no consumer of the string that did the `flush` - it was done by the top level evaluation.
```
(defun vector3s+! (stuff)
  "Add 2 vectors3." ;; oops, a string constant instead of a comment.
  ... ; rest of the function
)
```