## Old Compiler

This is the "old compiler", which was written as a proof of concept. It has a few flaws with how the type system is implemented, and a major flaw with how constant propagation works and it is not set up to allow for easy tests of small components (or easy tests with the gtest framework, or easy checking results of test programs).  There's also issues with managing the state of the compiler when reloading files and connecting to a runtime which has already loaded code.
  
  As a result, I'm writing a "new compiler" (in the `goalc` folder) with a much better design. I'll be able to reuse a ton of stuff from this old compiler, and I plan to remove the old compiler once the new one is ready.  

It's not clear if it's worth building this old compiler on windows or not. I think most things should work, except for `Timer` and `Listener`, as usual...

## How to Use It (Linux)
- Build everything as usual (`mkdir build; cd build; cmake ..; make -j; cd ..`)
- Start the runtime as usual with `./gk.sh`. After you see `kernel: machine started`, the runtime has loaded `KERNEL.CGO` and is waiting for code to be sent from the compiler
- Start the compiler with `./old_gc.sh`
- Once you see the `goal(n)>` prompt the compiler has loaded the GOAL syntax libraries (see `gc/goal-lib.gc`, `gc/goal-syntax.gc`, etc)
- Connect to the runtime by running `(lt)` in the compiler. ("Listen to Target")
- You should see that it gets the correct version (2.6), and a bunch of messages indicating which object files are loaded and where.
- Now any code you type in will be sent to the runtime, executed, and the result is printed.
- Example `(+ 1 2 3)` will print `6`.
- Note - all values will be printed as decimals by default. If you want to print things in a pretty way:
   - run `(define-extern fancy-listener-print boolean)` to inform the compiler there's a boolean value named `fancy-listener-print`
   - run `(set! fancy-listener-print #t)` to enable fancy printing (the kernel checks this value, see `gc/goal_kernel/gkernel.gc`)
   - The result will now be `1342764        #x147d2c              0.0000        #t`
   - The first value is decimal, hex, floating point, and using GOAL's `print` method.
- Run `(test)` to test all the files in `old_compiler/gc/tests`
- You can use `inspect` to run GOAL's `inspect` method on most objects.  For example:
  - `(inspect "test")`
  - `(inspect 'my-symbol)`
  - `(inspect (cons 'a 'b))` (don't try to put integers in the `cons`, it's looking for boxed objects...)
- Check out `old_compiler/goal_kernel/gcommon.gc` or `math.gc` for some sample GOAL code that works.