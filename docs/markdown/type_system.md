# OpenGOAL's Type System

This document explains the GOAL type system.  The GOAL type system supports runtime typing, single inheritance, virtual methods, and dynamically sized structures.

There's a single type system library, located in `common/type_system`.  It will be used in both the decompiler and compiler. The plan is to have a single `all_types.gc` file which contains all type information (type definitions and types of globals). The decompiler will help generate this, but some small details may need to be filled in manually for some types.  Later versions of the decompiler can use this information to figure out what fields of types are being accessed.  We can also add a test to make sure that types defined in the decompiled game match `all_types.gc`.

The main features are:

- `TypeSystem` stores all type information and provides a convenient way to add new types or request information about existing types.
- `Type` information about a GOAL Type.  A "base GOAL type" is identified by a single unique string. Examples: `function`, `string`, `vector3h`.
- `TypeSpec` a way to specify either `Type` or a "compound type".  Compound types are used to create types which represent specific function types (function which takes two integer arguments and returns a string), or specific pointer/array types (pointer to an integer).  These can be represented as (possibly nested) lists, like `(pointer integer)` or `(function (integer integer) string)`.
- Type Checking for compiler
- Parsing of type definitions for compiler
- Lowest common ancestor implementation for compiler to figure out return types for branching forms.
- Logic to catch multiple incompatible type definitions for both compiler warnings and decompiler sanity checks

The type system will store:
- The types of all global variables (this includes functions)
- Information about all types:
  - Fields/specific details on how to load from memory, alignment, sign extension, size in arrays, etc...
  - Parent type
  - Methods not defined for the parent.

It's important that all of the type-related info is stored/calculated in a single location. The proof of concept compiler did not have the equivalent of `TypeSystem` and scattered field/array access logic all over the place.  This was extremely confusing to get right.

If type information is specified multiple times, and is also inconsistent, the TypeSystem can be configured to either throw an exception or print a warning.

This will be a big improvement over the "proof of concept" compiler which did not handle this situation well.  When debugging GOAL you will often put the same file through the compiler again and again, changing functions, but not types.  In this case, there should be no warnings. If the type does change, it should warn (as old code may exist that uses the old type layout), but shouldn't cause the compiler to abort, error, or do something very unexpected.

## Compile-time vs Run-time

The types in the runtime are only a subset of the compile time types. Here are the rules I've discovered so far
- Any compound types become just the first type. So `(pointer my-type)` becomes `pointer`.
- The `inline-array` class just becomes `pointer`.
- Some children of integers disappear, but others don't. The rules for this aren't known yet.

## Types of Types

Everything in GOAL has a type at compile time.  A subset of compile-time types are also available in the runtime as objects with the same name as the type.  For example, there is a `string` type, and at runtime there is a global object named `string` which is an object of type `type` containing information about the `string` type.

Some objects have runtime type information, and others don't.  Objects which have runtime type information can have their type identified at runtime, and are called "boxed objects".  Objects without runtime type information are called "unboxed objects".  An unboxed object cannot reliably be detected as a unboxed object - you can't write a function that takes an arbitrary object and tells you if its boxed or not.  However, boxed objects can always be recognized as boxed.

All types have a parent type, and all types descend from the parent type `object`, except for the special type `none` (and maybe `_type_`, but more on this later). The `none` type doesn't exist in the runtime and is used to represent an invalid value that the compiler should not use.  For example, the return type of a function which doesn't return anything is `none`, and attempting to use this value should cause an error.

Here are some important special types:
- `object` - the parent of all types
- `structure` - parent type of any type with fields
- `basic` - parent type of any `structure` with runtime type information.

### Value Types

Some GOAL types are "value types", meaning they are passed by value when used as arguments to functions, return values from functions, local variables, and when using `set!`.  These are always very small and fit directly into the CPU registers.  Some example value types:
- Floating point numbers
- Integers

### Reference Types

Other GOAL types are "reference types", meaning they act like a reference to data when used as arguments to functions, return values from functions, local variables, and when using `set!`.  The data can be allocated on a heap, on the stack, or as part of static data included when loading code (which is technically also on a heap).  All structure/basic types are reference types.

You can think of these like C/C++ pointers or references, which is how it is implemented.  The difference is that there's no special notation for this.  A GOAL `string` object is like a C/C++ `string*` or `string&`.  A GOAL "pointer to reference type" is like a C/C++ `my_type**`.

Note - this is quite a bit different from C/C++. In C++ you can have a structure with value semantics (normal), or reference semantics (C++ reference or pointer).  In GOAL, there is no value semantics for structures!  This is great because it means function arguments/variables always fit into registers.

### Dynamic Size Types

Any type which ends with a dynamic array as the last field is dynamic. For these, it's a good idea to implement the `asize-of` method.

### Compound Types

A compound type is a type like "a pointer to an int64" or "a function which takes int as an argument and returns a string". These exist only at compile time, and get simplified at runtime. For example, all pointers become `pointer` and all functions become `function`. (The one exception to this seems to be `inline-array-class` stuff, but this is not yet supported in OpenGOAL).

#### Pointer

Pointers work like you would expect. They can only point to memory types - you can't have a `(pointer int)`, instead you must have a `(pointer int32)` (for example).  Note that a `(pointer basic)` is like a C++ `basic**` as `basic` is already like a C++ pointer to struct. You can nest these, like `(pointer (pointer int64))`.  If you want a pointer with no type, (like C++ `void*`) just use a plain `pointer`. The `(pointer none)` type is invalid.

Like in C/C++, you can use array indexing with a pointer. One thing to note is that a `(pointer basic)` (or pointer to any reference type) is like a C++ "array of pointers to structs". To get the C++ "array of structs", you need an `inline-array`.

#### Arrays

For value types, arrays work as you expect.  They have type `(pointer your-type)`.  Arrays of references come in two versions:
- Array of references: `(pointer your-type)`, like a C array of pointers
- Array of inline objects: `(inline-array your-type)`, like a C array of structs

The default alignment of structs is 16 bytes, which is also the minimum alignment of `kmalloc`, and the minimum alignment used when using a reference type as an inline field.  However, it's possible to violate this rule in a `(inline-array your-type)` to be more efficient.  The `your-type` can set a flag indicating it should be packed in an inline array.

I believe the alignment then becomes the maximum of the minimum alignment of the `your-type` fields.  So if you have a type with two `uint32`s (alignment 4 bytes), an `(inline-array your-type)` can then have spacing of 8 bytes, instead of the usual minimum 16.  The behavior of a `(field-name your-type :inline #t)` is unchanged and will still align at the minimum of 16 bytes. I _believe_ that the first element of the array will still have an alignment of 16.

##### Inline Arrays

These are only valid for reference types. They refer to an array of the actual data (like C array of structs) rather than an array of reference (like C array of pointers to structs, or GOAL `(pointer structure)`).  At runtime, `inline-array` becomes pointer.

For an inline array of basics, elements are 16-byte aligned. For `structure`s that aren't `basic`, the alignment is usually the minimum alignment of all members of the structure, but there is an option to make it 16-byte aligned if needed.

For information about how to create these arrays, see `deftype` (fields in a type) and `new` (create just an array) sections.

#### Function

Function compound types look like this `(function arg0-type arg1-type... return-type)`. There can be no arguments. The `return-type` must always be specified, and should be `none` if there is no return value.  The argument types themselves can be compound types.  In order to call a function, you must have a compound function type - a `function` by itself cannot be called.

### Field Definitions

GOAL field definitions look like this:

`(name type-name [optional stuff])`

where optional stuff can include these, in any order:

- `:inline #t` (default is false), to mark field as inline. This can only be done for a reference type, and indicates that the data should be stored inline, in the type, rather than just storing a reference to data stored elsewhere.
- `:dynamic #t` (default is false), to mark field as dynamically-sized array (must be the last field in the type)
- a number, to give an array size.
- `:offset x` where x is a number, to manually specify where the field is located

There are many combinations of reference/value, dynamic/not-dynamic, inline/not-inline, array-size/no-array-size, and it can be confusing.  This list explains all that are valid.

- Value type, no modifiers: a single value is stored in the field. The field type is the value type.
- Value type, `:dynamic #t`: the field marks the beginning of an array (of unknown size). Field type is `(pointer your-type)`
- Value type, with array size: the field marks the beginning of an array (of known size). Field type is `(pointer your-type)`
- Value type, with `:inline #t`: invalid in all cases.
- Reference type, no modifiers: a single reference is stored in the type. Type of field is `your-type` (a C++ pointer).
- Reference type, `:inline #t`: a single object is stored inside the type. Type of field is `your-type` still (a C++ pointer). The access logic is different to make this work.
- Reference type, `:dynamic #t` or array size: the field marks the beginning of an **array of references**. Field type is `(pointer your-type)`.  Like C array of pointers.
- Reference type, `:inline #t` and (`:dynamic #t` or array size): the field marks the beginning of an **array of inline objects**. Field type is `(inline-array your-type)`. Like C array of structs.

Bonus ones, for where the array is stored _outside_ of the type:
- A dynamically typed GOAL array, stored outside your type (think `std::vector`): use `(name (array your-type))`
- A dynamically typed GOAL array, stored inside your type: Not allowed, `array` is dynamic!
- An array of value types, stored outside your type: use `(name (pointer your-type))`
- An array of references (C++ array of pointers), stored outside your type: use `(name (pointer your-ref-type))`
- An array of objects of reference type (C++ array of structs), stored outside your type: use `(name (inline-array your-ref-type))`

Of course, you can combine these, to get even more confusing types! But this seems uncommon.

#### Field Placement

The exact rules for placing fields in GOAL types is unknown, but the simple approach of "place the next field as close as possible to the end of the last field" seems to get it right almost all the time. However, we need to be extra certain that we lay out type fields correctly because many GOAL types have overlapping fields.

The theory I'm going with for now is:
- The order of fields in the `inspect` method is the order fields are listed in in the type definition
- In the rare cases this is wrong, this is due to somebody manually specifying an offset.

As a result, we should specify offsets like this:
- If we think a field was manually placed, use `:offset` to override. This is certain to be right
- If we think a field was automatically placed, use `:offset-assert` to inform the compiler where we expect it to be.  In this case it will still place the field automatically, but if the result is different from the `:offset-assert`, it will throw an error.
- Avoid defining any fields without `:offset` or `:offset-assert`

## Built-in Types

There are a number of built-in types. I use "abstract" type to refer to a type that is only a parent type.

### `none`
This is a special type that represents "no information". This is the return type of a function which returns nothing, and also the return type of an expression that doesn't return anything.  For example, the expression `(goto x)` does not produce a value, so its type is `none`.

### `object`
This is the parent type of all types. This is an abstract class. In a variable, this is always `object`, and can hold any `object`. In memory, this is either `object32` or `object64`. The `object32` can hold everything except for `binteger` and 64-bit integers. This type is neither boxed nor unboxed and is neither value nor reference.

### `structure` (child of `object`)
This is the parent type of all types with fields. This is an abstract class and a reference class.  A `structure` can hold any `structure`, both in memory and in a variable.  It is unboxed.

### `basic` (child of `structure`)
This is the "boxed" version of `structure`. The first field of a basic is `type`, which contains the `type` of the object. It is boxed and a reference. A `basic` can hold any `basic`, both in memory and in a variable.

### `symbol` (child of `basic`)
A symbol has a name and a value. The name is a string, and the value is an `object32`.  Note that the value is an `object32` so you cannot store a 64-bit integer in a symbol.  It is considered "bad" to store unboxed objects in symbols, though you can get away with it sometimes.

All `symbol`s are stored in the global symbol table, which is a hash table. As a result, you cannot have multiple symbols with the same name. A name is enough to uniquely determine the symbol.  To get a symbol, use the syntax `'symbol-name`. To get the value, use `symbol-name`.

Each global variable, type, and named global function has a symbol for it which has the variable, type, or function as its value. The linker is able to perform symbol table lookups at link time and patch the code so you don't have to do a hash table lookup every time you access a global variable, function, or type.

You can also use symbols as a efficient way to represent a enum. For example, a function may return `'error` or `'complete` as a status. The compiler is able to compare symbols for equality very efficiently (just a pointer comparison, as symbols are a reference type).

### `type` (child of `basic`)
A `type` stores information about an OpenGOAL type, including its size, parent, and name (stored as a `symbol`). It also stores the method table.  Some OpenGOAL types (children of integers, bitfield types, enums, compounds types) do not have runtime types, and instead become the parent/base type. But these types cannot have runtime type information or methods and are pretty rare.  It is a reference type, is boxed, and is dynamically sized (the method table's size is not fixed).

### `string` (child of `basic`)
A string. The string is null terminated and also stores the buffer size. This type is a reference type, is boxed, and is also dynamically sized.

### `function` (child of `basic`)
A function. Boxed and reference. It is a reference to a function, so it's like a C/C++ function pointer type.

### `kheap` (child of `structure`)
A simple bump-allocated heap. Doesn't store the heap memory, just metadata. Supports allocating from either the top or the bottom. This is used as the memory allocation strategy for the global, debug, and level heaps. Unboxed, reference, not dynamic.

### `array` (child of `basic`)
A "fancy" array. It is not yet implemented in OpenGOAL.

### `pair` (child of `object`)
A pair. It is boxed. You should not make child types of `pair`.  The two objects stored by the pair are `object32`s.

### `pointer` (child of `object`)
It is a 32-bit value type containing a pointer. Not boxed, value type. See section on compound types for more information.

### `number` (child of `object`)
Abstract type for all numbers. Value type. 64-bits.

### `float` (child of `number`)
4-byte, single precision floating point number.  Value type.

### `integer` (child of `number`)
Abstract class for integer numbers. Child classes are `sinteger` (signed integer), `uinteger` (unsigned integer), and `binteger` (boxed-integer, always signed).  These are all 64-bit types.

Children of `sinteger` and `uinteger` are `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`.  These are the size you expect, value types, and not boxed. These only exist as memory types. In a variable, there is only `int` and `uint`.  These are both 64-bit types. All integer operations (math, logical, shifts) are 64-bit.

The `binteger` is a boxed integer. It is a 61 bit signed integer (the other three bits are lost due to the number being boxed). There may actually be a `buinteger` (or `ubinteger`) but it doesn't exist in OpenGOAL at this point.

#### Weird Built-in types that aren't supported yet.
- `vu-function`
- `link-block`
- `connectable`
- `file-stream`
- `inline-array` (class)

## Unknown Areas

### Inline Array Class

There's a weird `inline-array-class` type that's not fully understood yet.  It uses `heap-base`.

### Heap Base

This is a field in `type`. What does it mean?  It's zero for most types (at least the early types).

### Second Size Field

There are two fields in `type` for storing the size. The first one stores the exact size, and by default the second stores the size rounded up to the nearest 16 bytes.  Why? Who uses it? Does it ever get changed?

## TODO

- [ ] Kernel types that are built-in
- [ ] Signed/unsigned for a few built-in type fields
- [ ] Tests for field placement logic (probably a full compiler test?)
- [ ] Bitfield types
- [ ] Type redefinition tests (these are a pain and probably useless, might just wait for full compiler tests?)
- [ ] Stuff for decompiler
  - [ ] What field is here?
  - [ ] Export all deftypes
