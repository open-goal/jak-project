Type System
--------------

There's a single type system library, located in `common/type_system`.  It will be used in both the decompiler and compiler. The plan is to have a single `all_types.gc` file which contains all type information (type definitions and types of globals). The decompiler will help generate this, but some small details may need to be filled in manually for some types.  Later versions of the decompiler can use this information to figure out what fields of types are being accessed.  We can also add a test to make sure that types defined in the decompiled game match `all_types.gc`. 
 
 The main features are:

- `TypeSystem` stores all type information and provides a convenient way to add new types or request information about existing types.
- `Type` a GOAL Type.  A `Type` is identified by a single unique string. Examples: `function`, `string`, `vector3h`.
- `TypeSpec` a way to specify either `Type` or a "compound type".  Compound types are used to create types which represent specific function types (function which takes two integer arguments and returns a string), or specific pointer/array types (pointer to an integer).  These can be represented as (possibly nested) lists, like `(pointer integer)` or `(function (integer integer) string)`.
- `FunctionSpec` (unimplemented) - contains a `TypeSpec` plus some additional information about a function (like if it is a global function, a method, names of the arguments, etc)
- Type Checking for compiler
- Parsing of type definitions for compiler
- Lowest common ancestor implementation for compiler to figure out return types for branching forms.
- Logic to catch multiple incompatible type defintions for both compiler warnings and decompiler sanity checks

The Type System
-------------------
The type system will store:
- The types of all global variables (this includes functions)
- Information about all types:
  - Fields/specific details on how to load from memory, alignment, sign extension, size in arrays, etc...
  - Parent type
  - Methods not defined for the parent.
  
It's important that all of the type-related info is stored/calculated in a single location. The proof of concept compiler did not have the equivalent of `TypeSystem` and scattered field/array access logic all over the place.  This was extremely confusing to get right.  

If type information is specified multiple times, and is also inconsistent, the TypeSystem can be configured to either throw an exception or print a warning.

This will be a big improvement over the "proof of concept" compiler which did not handle this situation well.  When debugging GOAL you will often put the same file through the compiler again and again, changing functions, but not types.  In this case, there should be no warnings. If the type does change, it should warn (as old code may exist that uses the old type layout), but shouldn't cause the compiler to abort, error, or do something very unexpected.


Method System
--------------
All type definitions should also define all the methods, in the order they appear in the vtable.  I suspect GOAL had this as well because the method ordering otherwise seems random, and in some cases impossible to get right unless (at least) the number of methods was specified in the type declaration.


Todo
---------
- [ ] Difference between "runtime" and "compile time" types?
    - [ ] `inline-array` vs `pointer`
- [ ] Arrays which aren't `array`s and aren't fields.
- [ ] `type_of_field` (returning the correct `pointer`, `inline-array` type for arrays/dynamics)
- [ ] `deref_type`
- [ ] Finish builtin types
- [ ] Tests for builtin types
- [ ] Ability to export type in `deftype` form.
- [ ] Tests for multiple definition checks
- [ ] Type Checking
- [ ] Function Specs
- [ ] Lowest Common Ancestor
- [ ] Document `:inline`, `:dynamic:` and field arrays.
- [ ] Document alignment rules
- [ ] Ability to read a `deftype` form.
  - [ ] In the decompiler
  - [ ] In the compiler, with the ability to do constant propagation and put things like `(+ 1 2)` or `MY_CONSTANT` as compile-time array size constants.
- [ ] Bitfield types