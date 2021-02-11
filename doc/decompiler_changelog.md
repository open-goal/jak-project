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
