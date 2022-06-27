
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
- Added `add-macro-to-autocomplete` to add a macro to the auto-completer.

## V0.7
- There is now an option for `allow-misaligned` which allows the alignment of an struct type to be less than 16-bytes when inlined, without enabling array packing. This seems like a stupid option, but GOAL has this in some places, so we support it too.
- In method declarations in a `deftype`, you can no longer provide argument names.  There was ambiguity when parsing a compound typespec vs named argument. The names were not used for anything.
- 128-bit integer register variables (`i128`) are now supported. These work with assembly forms, `set!`s between registers, and `set!`s of memory locations with type `(pointer uint128)` or `(pointer int128)`.
- Fixed a bug where the compiler would abort if had to spill an `xmm` register containing an `i128` value.
- Added `.pextlw`, `.pextuw`, `.pcpyld`, and `.pcpyud` assembly forms
- Fixed a bug where `uint128` or children defined with `local-vars` would end up using a 64-bit GPR instead of a 128-bit XMM.
- Fixed a bug where 128-bit variable spills could be misaligned, causing a segfault at `vmovaps`.
- Added `.ppach` and `.pceqw`
- Fixed a bug where setting 128-bit / 64-bit variables from each other only did a 32-bit set
- Added support for creating a static bitfield where fields may not be known at compile time.
- Added support for `(size-of <typename>)`
- Fixed a bug where creating a stack array of 0 sized caused a compiler assertion. It is now a normal compiler error.
- Fixed a bug where the repl history was not loaded on compiler start
- Branch target addresses in the disassembly generated by `md`, `asm-file` with `:disassemble` and `(declare (print-asm))` are now correct
- Fixed a segfault when the `goal-library.gc` contains an `(exit)`
- `defenum` now creates real types. Boxed arrays of enums and bitfields correctly have runtime type of the parent integer.
- Added a `:copy-entries <typename>` to copy entries from a previous bitfield.
- Adding a duplicate entry to an enum now generates a compiler error.
- Added `.psubw` assembly form
- Changed `.ftoi` to `VCVTTPS2DQ` to make the rounding behavior match the PS2 (truncate).
- Forward declaring a type with `declare-type` also forward declares the global holding the runtime type object.
- The `new` method of basic now has a method return type of `_type_`. Previously it was `basic`, meaning you always needed to declare `new` with the right type.
- Using `(new` and `(the binteger` inside static pair definitions is now supported
- Invalid static pairs now have nice errors instead of exiting the compiler
- Added unsigned division (previously signed division was used for unsigned numbers)
- Use shifts (64-bit) for positive power of two multiply and divide. Otherwise use 32-bit. This matches GOAL.
- Allow setting a 64-bit or less memory location from a 128-bit variable (upper bits are discarded).
- It is now a compiler error to declare a bitfield type where a field crosses bit 64.
- Fixed a bug where a let/immediate lambda with an argument with type of child of int128/uint128 would end up in a 64 bit register.
- Support accessing and setting fields of a 128-bit bitfield type.
- Fixed a bug where the mask constant for clearing a bitfield was not computed correctly
- Support 128-bit bitfields inside of static structure
- Support 128-bit bitfield constants
- Support dynamic construction of 128-bit bitfield values

## V0.8 New Calling Convention for 128-bit
- 128-bit values may now be used in function arguments and return values.
- Fixed a bug where reader errors in `goal-lib.gc` or any error in `goos-lib.gs` would cause a crash
- Fixed a bug where `''a` or similar repeated reader macros would generate a reader error.
- Fixed a bug where reader macros without a following form would trigger an assert.
- It is now possible to take the address of a lexical variable. The variable will be spilled to the stack automatically.
- GOOS supports `string-ref`, `string-length`, `ash`, and characters can now be treated as a signed 8-bit number
- Fixed a bug where saved xmm registers might be clobbered when calling a C++ function that wasn't `format`.
- The `declare-type` form now supports any parent type. The type system will do a better job of trying to make things work out when only part of the type hierarchy is defined, and you can now chain type forward declarations. The compiler is stricter and will not accept forward declarations that are possibly incompatible. Instead, forward declare enough types and their parents for the compiler to be able to figure it out.  
- The `deftype` form is more strict and will throw an error if the type definition is in any way incompatible with existing forward declarations of types.
- Added a `type-ref` form to insert a reference to a type into a static structure and optionally forward declare the number of methods
- The `method-of-type` form will now accept an expression returning a type instead of just a type name.  In this case, it will only allow you to access method of `object`.
- Added a `defun-recursive` to make it easier to define recursive functions
- Forward declared basics can be used in more places
- You can now set a field which has a forward declared structure or basic type
- `cdr` now returns an object of type `pair`.
- `lambda`s can now be used inside of a static object definition.
- Methods can now be `:replace`d to override their type from their parent. Use this with extreme care.
- TypeSpecs now support "tags". This can specify a `:behavior` tag for a function.
- Lambdas and methods now support `:behavior` to specify the current process type.
- `defbehavior` has been added to define a global behavior.
- Auto-generated inspect methods of process now start by calling the parent type's inspect, like in GOAL.
- Fields with type `(inline-array thing)` can now be set in statics.
- `meters`, `degrees`, and `seconds` types have been added.
- Bitfields with `symbol` fields used in an immediate `(new 'static ...)` can now define the symbol in the `new` form.
- Bitfields with `float` fields used in an immediate `(new 'static ...)` in code can use a non-constant floating point value.
- Multiple variables assigned to the same register using `:reg` in `rlet` (or overlapping with `self` in a behavior) will now be merged to a single variable instead of causing a compiler error. Variables will have their own type, but they will all be an alias of the same exact register.
- Stack arrays of uint128 will now be 16-byte aligned instead of sometimes only 8.
- Inline arrays of structures are now allowed with `stack-no-clear`.
- Creating arrays on the stack now must be done with `stack-no-clear` as they are not memset to 0 or constructed in any way.
- The register allocator has been dramatically improved and generates ~5x fewer spill instructions and is able to eliminate more moves.
- Added a `(print-debug-compiler-stats)` form to print out statistics related to register allocation and move elimination
- Added `get-enum-vals` which returns a list of pairs. Each pair is the name (symbol) and value (int) for each value in the enum
- It is now possible to set a 64-bit memory location from a float, if you insert a cast. It will zero-extend the float, just like any other float -> 64-bit conversion.
- Added `:state` option to `:methods`.
- Accessing the `enter` field of `state` will now magically give you a function with the right type.
- It is possible to access fields of the parent of a forward declared type
- Fixed a bug where casting a value to seconds, then setting a field of type seconds would incorrectly fail type-check
- Fixed a bug where nested rlet's didn't properly share register constraints, leading to inefficient register allocation, and some rare cases a regalloc constraint error
- Lambdas may now be used in static pairs.
- Dynamically constructed bitfields created with `(new 'static ...` may now set fields with `structure` type.
- Allocations on `'loading-level` are now permitted.
- Converting a float larger than `INT32_MAX` now saturates to INT32_MAX, like on a real PS2.
- Treating a float as a 64-bit integer now sign extends, like on a real PS2
- It is now an error to have two arguments with the same name.
- It is now a warning to redefine a constant.
- Fix a bug where the size of static boxed arrays was only `length` and not `allocated-length`
- It is now possible to call a method on a forward declared type. The forward declared type must be a basic.
- Using `->` on a plain `pointer` or `inline-array` now generates an error instead of crashing the compiler
- It is now possible to use a macro to provide a static inline array element definition
- It is now possible to have symbol names that have a `#` in the middle of them
- `go-hook` now returns the return value of the `enter-state` function it calls
- Added a `(declare-file (debug))` to put things in the debug segment by default. This only changes _static_ objects. Dynamic allocation with forms like `cons` will continue to use the main segment like before.
- It is now an error to use a `none`-typed variable in a condition
- Debugger will now correctly track when object files are loaded over previous files
- Asm ops requiring 128-bit inputs will now try harder to convert their inputs when it is appropriate.
- 0's that are constant propagated to the input of a 128-bit instruction will use `vpxor` instruction to generate the value, instead of `xor` and a `mov`.
- Add a `stack-singleton-no-clear` stack construction type. It will create a "singleton" inside this function - all other `(new 'stack-singleton` forms with the same type will return the same stack object.
- Added support for using `(new 'static 'array ...)` for setting a static field of type `(pointer ...)`

## V0.9 Large change to macro expansion and constant propagation
The compiler is now much more aggressive in where and how it expands macros and handles expressions at compiler time.
- Several places where macros could be incorrectly executed more than once (possibly causing unwanted side effects) have been fixed.
- Fixed bug in size calculation of non-inline stack arrays. Previous behavior was a compiler assert.
- Correctly handle `mod` for unsigned numbers. Previous behavior was to treat all inputs as 32-bit signed integers.

## V1.0 Revised constant propagation, speed improvements
Improved error messages around macros