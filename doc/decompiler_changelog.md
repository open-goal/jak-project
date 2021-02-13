## Version 1
- Fixed bug where eliminated moves would appear in output in the first condition of a function as `(set! a a)`.
- Improved expression building immediately before the condition of `if`/`cond`
- Recognize  `(new 'global 'pair <a> <b>)` as `(cons <a> <b>)`
- Remove useless `(set! <a> #f)`
- Remove useless `(set! <a> <b>)` and eliminate useless temporaries created by these.
- Remove useless `set!`s sometimes appearing around functions `(set! <a> (some-function ...))` when the result is unused, but moved into a different register.
- Recognize `(break!)` (GOAL breakpoint)
- Support `(new 'process ...)`

## Version 2
- Expressions like `(set! (-> a b) (-> c d))` are much less likely to have fake temporaries.
- Many more useless `set!`s will be removed
- Stores into arrays are supported
- Fixed bug where unused/eliminated temporaries would sometimes be used as the result of a block

## Version 3
- Normal use of the process pointer will now show up as `pp`. Weird use will still be weird.
- `(method-of-object ...)` will now be recognized
- Accessing the address of a variable element of an inline array is supported in some cases. More examples are needed before all work. But for example: `(&-> v0-0 stack (-> v0-0 allocated-length))` where `stack` is an `uint8 :dynamic`.
- Cleaned up unneeded casts floating point constant `0.0`.
- Support for `fmin`/`fmax`
- Fixed a bug where integer `abs` appeared instead of `fabs`.
- Some support for float -> integer conversions, but it is not 100% yet.
- Eliminate inserted coloring moves for function arguments that use `mtc1`.
- Support for `>=` for signed numbers.