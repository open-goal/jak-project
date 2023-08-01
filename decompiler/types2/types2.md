# Type Pass: Version 2
Compared to the original type pass, the version 2 type pass should support:
- Automatic guessing of stack and label types
- Solution to ambiguous inline structure access uses casts problem

- Better handling of result in `cond` as value
- Still support for `typecase`-like patterns
- Fewer useless casts in `cond` as value cases


The main idea of the type pass is similar to the previous one: iterate through blocks in a topological sort order, propagating types and finding lowest-common-ancestors whenever multiple paths give us different types for the same register.

The new trick is the ability to handle "constraints" or hints about what certain types should be.  Types can tag themselves as "ambiguous", and the type pass will greedily resolve these ambiguities based on signatures of functions and constraints on instructions. To propagate backward through blocks, blocks will internally propagate back info to their entry types. On a second entire pass, blocks that end with ambiguous types will look ahead to their successors and resolve based on hints there.

## Automatic Guessing of Stack and Label Types

```asm
    lw t9, quaternion-vector-angle!
    daddiu a0, sp, 16
    daddiu a1, fp, L80
    jalr ra, t9 
```
From this code, the decompiler should be able to infer the types of `sp + 16` and `L80`.

Of course, there are situations where this type of guessing will be wrong, so the decompiler should produce some type of warning/notification that this guess has occurred, and the usual methods to specify these types should override any guesses.

This can be implemented by making the type of `sp + 16` an "ambiguous" type like `[unknown type (sp + 16)]`.  Then, when the ambiguity can be resolved, the decompiler can fill in this information.

## The Ambiguous Inline Structure Access Uses Casts Problem
There can be ambiguities when accessing inline members:
```
(&-> mat vector 1) ;; a vector
(&-> mat data 4)   ;; a (pointer float)
(&-> mat vector 1 data 0)
```
all are the same GOAL code.  The decompiler doesn't handle this very well:
```
;; arg1 is a matrix here
(vector-dot (the-as vector (&-> arg1 data 8)) (the-as vector (&-> arg1 data 8)))
```

Solving this problem completely and efficiently is extremely hard (you could use a full implementation of this to solve boolean satisfiability problems), so we'll support a somewhat limited version of this.

On the actual ambiguous deref, we'll create an "ambiguous deref" record, and propagate it along with the type. When we hit something that provides a constraint and takes an ambiguous input, we'll resolve the ambiguity, and cache this result for future type passes.  Note that it's possible to have whole trees of ambiguous types, but for now we'll ignore this case and not build trees. We'll greedily pick the highest scoring option when branching would be required. This should handle the majority of these problems, and this simplification will be somewhat necessary for the next step to work.

## Handle type of result in `cond`
Imagine this:
```
(foo! (if (bar) A B))
```
we'd like `A` and `B` to get hints about what type they should be from the signature of `foo!`. This will be really important in Jak 2, in the case where these values are floats.

Again, I think this should be done as a forward pass. The blocks look like this:
```
     [load foo!]
     [check bar]
     
  [A]            [B]
  
     [call foo]
```

On the forward pass, we'll get the type of `foo!` propagated to the `call foo` block.  The `call foo` block will know that it has multiple predecessors, so it'll add "ambiguous pred" tags to all input types. When `foo` is called, it'll propagate the constraint back to the top of the `call foo` block, which will be cached for the next iteration.

## What happens with typecase?
The challenge here is to handle two conflicting ambiguous resolutions.