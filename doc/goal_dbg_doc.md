# OpenGOAL Debugger
Currently the debugger only works on Linux. All the platform specific stuff is in `xdbg.cpp`.

## `(dbs)`
Print the status of the debugger and listener.  The listener status is whether or not there is a socket connection open between the compiler and the target. The "debug context" is information that the runtime sends to the compiler so it can find the correct thread to debug. In order to debug, you need both.

## `(dbg)`
Attach the debugger. This will stop the target.

Example of connecting to the target for debugging:

```lisp
OpenGOAL Compiler 0.1

;; attach the listener over the network
g> (lt)
[Listener] Socket connected established! (took 0 tries). Waiting for version...
Got version 0.1 OK!

;; this message is sent from the target from kprint.cpp and contains the "debug context"
[OUTPUT] reset #x147d24 #x2000000000 1062568

;; the debugger gets the message and remembers it so it can connect in the future.
[Debugger] Context: valid = true, s7 = 0x147d24, base = 0x2000000000, tid = 1062568

;; attach the debugger and halt
gc> (dbg)
[Debugger] PTRACE_ATTACHED! Waiting for process to stop...
Debugger connected.

;; print the debugger status
gc> (dbs)
 Listener connected? true
 Debugger context? true
 Attached? true
 Halted? true
 Context: valid = true, s7 = 0x147d24, base = 0x2000000000, tid = 1062568
```

## `(:cont)`
Continue the target if it has been stopped.

## `(:break)`
Immediately stop the target if it is running. Will print some registers.

## `(:dump-all-mem <path>)`
Dump all GOAL memory to a file. Must be stopped.
```
(:dump-all-mem "mem.bin")
```
The path is relative to the Jak project folder.

The file will be the exact size of `EE_MAIN_MEM_SIZE`, but the first `EE_LOW_MEM_PROTECT` bytes are zero, as these cannot be written or read.