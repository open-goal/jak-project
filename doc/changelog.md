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