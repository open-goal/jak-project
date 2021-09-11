## Using Mips2C

## Mips2C code linking and defining
The mips2c code must be "linked" before it can be accessed. There are two manual steps that must be done to make this happen.

First, at the bottom of the mips2 output there will be something like:
```cpp
// FWD DEC:
namespace draw_string { extern void link(); }
```
this must be copy-pasted into the top of `mips2c_table.cpp`

Second, add a new entry to the `gMips2CLinkCallbacks` table in that same file:
```cpp
{"font", {draw_string::link}}
```
the first thing in the list is the name of the source file that should contain the function (without `.gc`), and the second thing should have the same name as the `namespace` added before.

When the linker links the `font` object file, it will call the `link` function defined there.  This will add the function to the table of available mips2c functions.  Note: this does **not** define the function. You must do that manually - it is important that function is not defined too early.

Replace the `defun` with:
```
(define my-func (the (function <whatever>) (__pc-get-mips2c "draw-string")))
```

You can use the same idea for methods with `method-set!`.

## Running Mips2C code