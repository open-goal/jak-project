# Offline Reference Test
The offline reference test runs the decompiler on all files that have a corresponding `_REF.gc`, then compiles them.
The test passes if all files compile and all decompiler outputs match the `_REF.gc`.

The purpose of the offline reference test is:
- To make sure the output of the decompiler can be compiled
- To let us easily see "what source should change, if I changed this type?". This allows us to safely update types without worrying that we forgot to update some other file.

This test doesn't run as part of CI, so it relies on us running it manually. As a result, from time to time, it can be broken on master.

## Running the test
Just run `offline-test` in the build directory. It takes about a minute and will display diffs of any files that don't match and compiler errors on the first failing file.

## What to do if the diff test fails
First, manually read the diff and make sure that it's a good change.

If so, re-run the `offline-test` program with the `--dump-mode` flag. It will save copies of any differing output in a `failures` folder (make sure this is empty before running). To apply these to the `_REF.gc` files automatically, there's a python script that you can run like this:
```
cd jak-project/build
python3 ../scripts/update_decomp_reference.py ./failures ../test/decompiler/referenc
```

Next, make sure the actual `.gc` files in `goal_src/` are updated, if they need to be. For large changes, this part can be pretty annoying. There is a `update-goal-src.py` script that is helpful for huge changes.

## What to do if the compile test fails
Ideally we'd make all code compile successfully without any manual changes. But sometimes there's just one function that doesn't work in a big file, and you'd like to get the rest of it.  There's a `config.jsonc` file in the `test/offline` folder that lets you identify functions by name to skip compiling in the ref tests.
