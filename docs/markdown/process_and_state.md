# Process and State

## What is a `process`?

A `process` object stores the state of some in-game object and tells the GOAL kernel how to update this object on each frame.

For example, there is a process for Jak, a process for each orb, and a process for each enemy. There is also a process for the time-of-day system and the pause menu.

In most cases, `process` is used as a parent type for a specific game object.  For example, `money` (orb) is a child of `process-drawable`, which is a child of `process`.  A `process-drawable` is a process that can be drawn as part of the `drawable` system.

## What does a process store?

Each `process` stores a small amount (112 bytes) of metadata, fields from child classes, some unknown stuff, and a process heap.  The process heap will automatically contain the "main thread" of the process, which contains space to back up the stack and registers when the thread suspends.  You may also allocate objects on the process heap yourself (not supported in OpenGOAL yet).

## How is a process run?

The `process` class is a child of `process-tree`, which is a left-child right-sibling binary tree.  On each frame, the kernel iterates through the `*active-pool*` and runs each process.  Each run consists of three steps:

- Run the `trans-hook` of the process in a temporary stack.
- Resume the main thread of the process.
- After the main thread suspends, run the `post-hook`.


## How do I create a process?

Setting up a process requires three steps:
- Getting an actual process object
- "Activating" a process so it will be run by the kernel
- Setting up the code for the process to run


There are a few "dead pools" which contain process objects that are not in use.  The `*4k-dead-pool*` contains processes that are 4kb each.  There is also a dynamic pool called the `*nk-dead-pool*` that allows you to create dynamically sized processes. You must do all allocations during initialization with these processes because they automatically "shrink" their heap as small as possible.  Also, `*nk-dead-pool*` processes will be relocated in memory as part of the process GC system, so you must make sure that all objects on the process heap support relocation, and you must use a `handle` to safely refer to the process, not just a normal `process` reference.

For example, to get a process:
```
gc> (define *test-proc* (get-process *nk-dead-pool* process 1024))
#<process process dead :state #f :stack -1904/1441188 :heap 0/1024 @ #x193454>
```

This shows that:
- The process name is `process` (just a temporary name, until we activate)
- The status is `dead`
- The process is not in a `state`.
- The stack is bogus because we don't have a main thread yet.
- We have used 0 out of 1024 bytes of our process heap.

Next, we need to activate it:
```
(activate *test-proc* *active-pool* 'hello *kernel-dram-stack*)
```

This means:
- We put it in the `*active-pool*`. We could specify another process in the `*active-pool*` if we wanted this to be a child process of an existing process.
- Our name is `'hello`.
- When we run code, it will run on the `*kernel-dram-stack*`.

Now, if we `(print *test-proc*)` we will see:
```
#<process hello ready :state #f :stack 0/256 :heap 384/1024 @ #x193454>
```
Indicating that we are "ready" to be initialized, and that we now have a correctly set up main thread/stack.

If we run `inspect`, it will print out all objects on the process heap, including our main thread:
```
        ----
        [001934c4] cpu-thread
        	name: code
        	process: #<process hello ready :state #f :stack 0/256 :heap 384/1024 @ #x193454>
        	previous: #f
        	suspend-hook: #<compiled function @ #x1679c4>
        	resume-hook: #<compiled function @ #x167b24>
        	pc: #x0
        	sp: #x170b30
        	stack-top: #x170b30
        	stack-size: 256
        	rreg[7] @ #x1934e8
        	freg[8] @ #x193520
        	stack[0] @ #x193540
        ----
```

If we want a reference to this process, we must create a handle.  For example:
```
gc> (process->handle *test-proc*)
#<handle :process #<process hello ready :state #f :stack 0/256 :heap 384/1024 @ #x192fe4> :pid 2>
```
this is now a safe reference to this process, even if it is relocated or deactivated.

## How do I make a process do something?
The `state` system is used to control a process.  Each process can be in a `state`, which specifies what functions should run.  To switch states in the current process, use `go`.

For example, we can create a simple test state like this:
```
(defstate test-state
    :enter (lambda () (format #t "enter!~%"))
    :exit (lambda () (format #t "exit!~%"))
    :trans (lambda () (format #t "trans!~%"))
    :post (lambda () (format #t "post!~%"))
    :code (lambda ()
            (dotimes (i 5)
              (format #t "Code ~D~%" i)
              (suspend)
              )
            (process-deactivate)
            )
    )
```
The `code` is the function to run in the main thread.  This code should `suspend` itself, and the kernel will resume it after the suspend on each frame. Once the process is done, it can call `process-deactivate`. This will cause it to exit the current state, immediately exit the `code`, and clean up the process, returning it to the dead pool.

To switch the process to this state, you can use the `run-now-in-process` to switch to the test process and run the given code.
```
(run-now-in-process *test-proc* (lambda () (go test-state)))
```

And you will see:
```
enter!
trans!
Code 0
post!

trans!
Code 1
post!

trans!
Code 2
post!

trans!
exit!
```

Note 1: After deactivation, the handle is no longer valid as the process is dead and it will print like this:
```
#<handle :process #f :pid 2>
```

Note 2: There is also a `run-next-time-in-process` that sets up the process to run your initialization stub function as the `code` on the next time the kernel iterates through the process tree.

## Some notes on "the current process"
When the kernel runs a process, it sets `(-> *kernel-context* current-process)` and the `pp` register to that process.  This process is called the "current kernel process".

This process may then "run code in another process".  This can be done with `run-now-in-process`, by deactivating another process, or using `go` on another process.  This changes `pp`, but not the kernel context.  The process in `pp` is called the "current pp process".

The value of the `pp` register determines the current process.


## Some notes on `process-deactivate`
To stop a process, you can do call the `deactivate` method of that process.  The `process-deactivate` macro just does this for the current process.

This does the following:
- Set state to `dead-state`.
- Calls `entity-deactivate-handler`, if you have an entity
- Calls `exit` of states
- Cleans up any pending `protect-frame` (calling them with pp set for the process)
- Disconnects it from the `connection` system
- Deactivates all children process
- Returns itself to the pool
- If you deactivated the process that the kernel-dispatcher started running, immediately bail out of the thread
- If you deactivated during a `run-now-in-process`, immediately bail out of the initialization and return to caller of `run-now-in-process`.

## Some notes on `go`.
The `go` macro is used to change the state of the current process.


If you use `go` when in `run-now-in-process`, it will immediately return to the caller of `run-now-in-process`, and the actual state change will happen on the next execution of the main thread of that process.

If you use `go-process` on another process, the `go-process` will return immediately and the state transition will happen on the next run of that process.

If you use `go` in the main thread, it will immediately transition states, run exits, enter, trans, and begin running the new state `code`.

If you use `go` in `trans` it will set up the next run of the main thread, then abandon the current `trans`.
If you use `go` in `post`, it will set up the next run of the main thread to transition, but not abandon the current `post`. 

