# Language Changes

## V0.1
- The GOAL language version has been set to 0.1
- Calling a function with unknown argument/return types is now an error instead of a warning
- Getting a method of an object or type with `method` returns the correct type for methods using the `_type_` feature
- The `object-new` macro will now type check arguments
- The size argument to `(method object new)` is now an `int` instead of `int32`
- Using `set!` incorrectly, like `(set! 1 2)` will now create an error instead of having no effect
- GOOS now has a `fmt` form which wraps `libfmt` for doing string formatting.
- GOOS now has an `error` form for throwing an error with a string to describe it
- GOAL `if` now throws errors on extra arguments instead of silently ignoring them
- The first 1 MB of GOAL memory now cannot be read/written/executed so dereferencing a GOAL null pointer will now segfault
- The runtime now accepts command line boot arguments
- The runtime now defaults to loading `KERNEL.CGO` and using its `kernel-dispatcher` function.
- The runtime now accepts a `-nokernel` parameter for running without `KERNEL.CGO`.
- The runtime will now refuse to load object files from another major GOAL version
- Using `&+` and `&+!` now produces a pointer with the same type as the original.
- There is a `&-` which returns a `uint` and works with basically any input types
- The `&` operator works on fields and elements in arrays
- The `&->` operator has been added
- The `new` operator can create arrays and inline arrays on heaps
- The value of `deftype` is now `none`
- Creating a method with more than 8 arguments is an error instead of a crash.
- The `defconstant` form for defining a constant in GOAL but not GOOS has been added
- Both `defconstant` and `defglobalconstant` throw an error if you define a constant with the same name as a symbol.
- The `uint64` type now uses 8 bytes instead of 81 in a type (this was a typo)
- `deftype` allows basics/structures with a field that is the same type as the basic/structure.
- Doing a `define-extern` with a type of `type` will forward declare the type.
- `deftype` now has a `:no-runtime-type` flag to disable the creation of a runtime type.
- There is a `declare-type` form for forward declaring types to allow circular dependencies.
- Types that are `structure` but not `basic` can request that they be tightly packed when possible with `:pack-me`.
- Using `method` on a forward declared type is an error. The old behavior was to get a method of `type`, which is confusing.
- Loading an `int64`/`uint64` gives a `int`/`uint`, like the other register integers.
- Defining a type with `deftype` will auto-generate an inspect method.
- The `new` operator can now create static structures and basics and set fields to integers or symbols.
- The `neq?` operator now works when used outside of a branch condition (previously it generated a syntax error)
- Methods which do not return a value no longer cause the compiler to abort
- The `&+` form now accepts more than two arguments.
- The `&+` form now works on `inline-array` and `structure`.
- In the case where the type system would use a result type of `lca(none, x)`, the result type is now `none` instead of compiler abort.
- The "none value" is now `(none)` instead of `none`

- Creating a field of 128-bit value type no longer causes a compiler crash
- 128-bit fields are inspected as `<cannot-print>`
- Static fields can now contain floating point values
- Fixed a bug where loading a float from an object and immediately using it math would cause a compiler crash

- Arrays of value types can be created on the stack with `new`.

## V0.2
- Breaking change: return type of a function using `return-from #f` to return a value from the entire function is now the lowest common ancestor of all possible return values.
- Fixed bug where `return-from` could reach outside of an inlined function.
- Fixed bug where `return-from` might not behave correctly when returning from inside a let inside an inlined function.
- Added `fmin` and `fmax` floating point min and max. These work on multiple arguments and use the `minss`/`maxss` instructions for the best performance.
- Added `imul64` instruction for doing a real 64-bit multiplication.  This must be used when porting code that looks at the `hi` register after an EE `mult`.
- Added `shl`, `shr`, and `sar` shifts which take a constant integer. These cannot be used with a variable shift amount.
- Added bitfield types to the type system
- Added the ability to cast integers to bitfield types
- Fixed a bug where casting between integer types with `the` that did not involve emitting code would permanently change the type of the variable.
- Added a `:disassemble` option to `asm-file` to disassemble functions for debugging purposes.

## V0.3
- Added typechecking when setting fields of a type.
- Added inline assembly `.ret`, `.sub`, `.push`, and `.pop`.
- Added `rlet` to declare register variables.
- Added `:color #f` option to inline assembly forms to exclude them from the coloring system.
- Added `asm-func` to declare for purely assembly functions.
- Enum values now work where constant integers are expected.
- The boolean values `#f` and `#t` now are gotten as symbol objects, not values of symbols.
- In a static field initialization, you can use `#f` and `#t` instead of `'#f` and `'#t`
- Added `no-typecheck` option to define.
- Reworked type checking for `set!`. You may now use `#f` for non-numeric types.
- Fixed a bug where arguments to a method were unmodifiable.
- Fixed a bug where multiple anonymous lambda functions in the same file would throw a compiler error related to function name uniqueness.
- Method declarations can now use compound types. Previously they could only use simple types due to a mistake in deftype parser.
- Added a declare option for `allow-saved-regs` to let `asm-func`s use saved registers in special cases
- Improved register allocation for the above case to avoid inserting extra moves to and from temp registers.
- Fixed a bug where early returns out of methods would not change the return type of the method.
- Fixed a bug where the return instruction was still emitted and the overridden return type of `asm-func` was ignored for methods
- Rearranged function stack frames so spilled register variable slots come after stack structures.
- Added `stack` allocated and constructed basic/structure types.
- Fixed a bug where functions with exactly 8 parameters created a compiler error.

## V0.4
- Breaking change: added new link kind to link table format. Code compiled with previous versions will still work, but code compiled in V0.4 that uses static pairs will cause a "unknown link table code 5" error.
- Added support for static pairs and lists. Symbols, integers, and strings are supported.
- Added much better support for setting fields of statics. Now you can set constants, symbols, strings, pairs, integers, floats, or other statics, including inlined structures/basics! Also uses the full type system for typechecking
- Fixed a bug where accessing an inline basic field did not apply the 4-byte basic offset.
- Made string/float constants go in the main segment when they are declared in the top-level segment, instead of the top-level segment. This is what GOAL seems to do (not 100% sure yet) and avoids issues where you set something to a string constant in the top-level. This avoids the possibility of memory bugs at the cost of more memory usage (likely very little additional memory).
- Added support for boxed arrays. They can be created with `new` and indexed with `->`. The compound type `(array <elt-type>)` is used to describe an array with a given content type.
- Added `reset-here` option for `rlet`.

## V0.5
- Breaking change: the register class `xmm` for a single float was renamed to `fpr` to distinguish it from other uses of `xmm` registers.
- Breaking change: the message format for reset and shutdown messages sent between the listener and runtime has changed.
- Improved code-generation quality where accessing a field or similar with an offset of zero from a base register.
- The listener now uses message IDs to more robustly handle the situation where a response messages comes, but is extremely late, or if some sent messages are skipped.
- Fixed bug where references to the debug segment using RIP-relative links were not set to zero by the linker when the debug segment isn't loaded.
- The `rlet` form now supports 128-bit vector float registers with the `vf` register class.
- Added support for "vector float" assembly operations, including `lvf`, `svf`, `xor`, `sub`, `add`, and `blend`.
- Added the ability to spill floating point variables to the stack if there aren't enough registers.
- Improved back up and restore of xmm registers
- Fixed an off-by-one in move eliminator (previous version was correct, but did not generate as good code). Complicated functions are 2 to 10% smaller.
- Improved getting a stack address.
- Improved getting the value of `#f`, `#t`, and `()`.
- Accessing a constant field of an array now constant propagates the memory offset like field access and avoids a runtime multiply.
- Fixed a bug where loading or storing a `vf` register from a memory location + constant offset would cause the compiler to throw an error.
- Accessing array elements uses more efficient indexing for power-of-two element sizes.
- Added a `local-vars` form for declaring a bunch of local variables for the decompiler.
- Split `method` into `method-of-type` and `method-of-object` to avoid ambiguity
- Fixed bug where `(-> obj type)` caused a compiler error when `obj` had compile time type of `array` (the fancy boxed array)
- Fixed use-after-free if the top-level form fails to compile and you continue trying to compile stuff.
- `and` and `or` are more efficient and the type of the result is more specific: `LCA(symbol, cases...)`
- `print-type` now fully compiles the argument and returns the result instead of `none`

## V0.6
- There is no longer a separate compiler form for variable vs. constant shifts. Instead the compiler will pick the constant shift automatically when possible. The shifts are called `sar`, `shl` and `shr`, like the x86 instructions.
- Static type reference link data now has the correct number of methods. This prevents errors like `dkernel: trying to redefine a type 'game-info' with 29 methods when it had 12, try restarting` when you did not actually redefine the number of methods.
- Added `get-info` to figure out what something is and where it's defined.
- Added `autocomplete` to get auto-completions for a prefix.