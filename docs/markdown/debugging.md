# OpenGOAL Debugger

The debugger works on Windows and Linux. All the platform specific code is in `xdbg.cpp`. When attached to a target, things like exceptions from invalid memory access or divides by zero break into the debugger for inspection on the code or values that caused the break. The technical implementation of the debugger across multiple platforms means there will be a few differences in how it handles or displays certain things. For example, the debugger on Linux will break if the GOAL (EE) thread runs into a breakpoint, but on Windows this can be caused by any thread on the target as the thread that `(:break)` breaks is unspecified.

## Commands

### `(dbs)`

Print the status of the debugger and listener.  The listener status is whether or not there is a socket connection open between the compiler and the target. The "debug context" is information that the runtime sends to the compiler so it can find the correct thread to debug. In order to debug, you need both.

### `(dbg)`

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

### `(:cont)`

Continue the target if it has been stopped.

### `(:stop)`

Detach from target.

### `(:break)`

Immediately stop the target if it is running. Will print some registers.

### `(:dump-all-mem <path>)`

Dump all GOAL memory to a file. Must be stopped.

```lisp
(:dump-all-mem "mem.bin")
```

The path is relative to the Jak project folder.

The file will be the exact size of `EE_MAIN_MEM_SIZE`, but the first `EE_LOW_MEM_PROTECT` bytes are zero, as these cannot be written or read.

## Address Spec

Anywhere an address can be used, you can also use an "address spec", which gives you easier ways to input addresses. For now, the address spec is pretty simple, but there will be more features in the future.

- `(sym-val <sym-name>)`. Get the address stored in the symbol with the given name. Currently there's no check to see if the symbol actually stores an address or not. This is like "evaluate `<sym-name>`, then treat the value as an address"
- `(sym <sym-name>)`. Get the address of the symbol object itself, including the basic offset.

Example to show the difference:

```lisp

;; the symbol is at 0x142d1c
gc> (inspect '*kernel-context*)
[  142d1c] symbol
	name: *kernel-context*
	hash: #x8f9a35ff
	value: #<kernel-context @ #x164a84>
1322268

;; the object is at 0x164a84
gc> (inspect *kernel-context*)
[00164a84] kernel-context
	prevent-from-run: 65
	require-for-run: 0
	allow-to-run: 0
	next-pid: 2
	fast-stack-top: 1879064576
	current-process: #f
	relocating-process: #f
	relocating-min: 0
	relocating-max: 0
	relocating-offset: 0
	low-memory-message: #t
1460868

;; break, so we can debug
gc> (:break)
Read symbol table (159872 bytes, 226 reads, 225 symbols, 1.96 ms)
rax: 0xfffffffffffffdfc rcx: 0x00007f745b508361 rdx: 0x00007f745b3ffca0 rbx: 0x0000000000147d24
rsp: 0x00007f745b3ffc40 rbp: 0x00007f745b3ffcc0 rsi: 0x0000000000000000 rdi: 0x0000000000000000
 r8: 0x0000000000000000  r9: 0x0000000000000008 r10: 0x00007f745b3ffca0 r11: 0x0000000000000293
r12: 0x0000000000147d24 r13: 0x00007ffdff32cfaf r14: 0x00007ffdff32cfb0 r15: 0x00007f745b3fffc0
rip: 0x00007f745b508361

;; reads the symbol's memory:
;; at 0x142d1c there is the value 0x164a84
gc> (dw (sym *kernel-context*) 1)
 0x00142d1c: 0x00164a84

;; treat the symbol's value as an address and read the memory there.
;; notice that the 0x41 in the first word is decimal 65, the first field of the kernel-context.
gc> (dw (sym-val *kernel-context*) 10)
 0x00164a84: 0x00000041 0x00000000 0x00000000 0x00000002
 0x00164a94: 0x70004000 0x00147d24 0x00147d24 0x00000000
 0x00164aa4: 0x00000000 0x00000000
```


### `(:pm)`

Print memory

```lisp
(:pm elt-size addr elt-count [:print-mode mode])
```

The element size is the size of each word to print. It can be 1, 2, 4, 8 currently.  The address is the GOAL Address to print at. The elt-count is the number of words to print.  The print mode is option and defaults to `hex`. There is also an `unsigned-decimal`, a `signed-decimal`, and `float`. The `float` mode only works when `elt-size` is 4.

There are some useful macros inspired by the original PS2 TOOL debugger (`dsedb`) for the different sizes. They are `db`, `dh`, `dw`, and `dd` for 1, 2, 4, and 8 byte hex prints which follows the naming convention of MIPS load/stores. There is also a `df` for printing floats. See the example below.

```lisp
OpenGOAL Compiler 0.1

;; first connect the listener
g> (lt)
[Listener] Socket connected established! (took 0 tries). Waiting for version...
Got version 0.1 OK!
[OUTPUT] reset #x147d24 #x2000000000 53371

[Debugger] Context: valid = true, s7 = 0x147d24, base = 0x2000000000, tid = 53371

;; define a new array of floats, and set a few values
gc> (define x (new 'global 'array 'float 12))
1452224

gc> (set! (-> x 0) 1.0)
1065353216

gc> (set! (-> x 2) 2.0)
1073741824

;; attach the debugger (halts the target)
gc> (dbg)
[Debugger] PTRACE_ATTACHED! Waiting for process to stop...
rax: 0xfffffffffffffdfc rcx: 0x00007f6b94964361 rdx: 0x00007f6b8fffeca0 rbx: 0x0000000000147d24
rsp: 0x00007f6b8fffec40 rbp: 0x00007f6b8fffecc0 rsi: 0x0000000000000000 rdi: 0x0000000000000000
 r8: 0x0000000000000000  r9: 0x000000000000000b r10: 0x00007f6b8fffeca0 r11: 0x0000000000000293
r12: 0x0000000000147d24 r13: 0x00007ffd16fb117f r14: 0x00007ffd16fb1180 r15: 0x00007f6b8fffefc0
rip: 0x00007f6b94964361
Debugger connected.

;; print memory as 10 bytes
gc> (db 1452224 10)
 0x001628c0: 0x00 0x00 0x80 0x3f 0x00 0x00 0x00 0x00 0x00 0x00

;; print memory as 10 words (32-bit words)
gc> (dw 1452224 10)
 0x001628c0: 0x3f800000 0x00000000 0x40000000 0x00000000
 0x001628d0: 0x00000000 0x00000000 0x00000000 0x00000000
 0x001628e0: 0x00000000 0x00000000

;; print memory as 10 floats
gc> (df 1452224 10)
 0x001628c0:   1.0000   0.0000   2.0000   0.0000
 0x001628d0:   0.0000   0.0000   0.0000   0.0000
 0x001628e0:   0.0000   0.0000

;; set some more values, must unbreak first
gc> (:cont)
gc> (set! (-> x 1) (the-as float -12))
-12

;; break and print as decimal
gc> (:break)
rax: 0xfffffffffffffdfc rcx: 0x00007f6b94964361 rdx: 0x00007f6b8fffeca0 rbx: 0x0000000000147d24
rsp: 0x00007f6b8fffec40 rbp: 0x00007f6b8fffecc0 rsi: 0x0000000000000000 rdi: 0x0000000000000000
 r8: 0x0000000000000000  r9: 0x0000000000000004 r10: 0x00007f6b8fffeca0 r11: 0x0000000000000293
r12: 0x0000000000147d24 r13: 0x00007ffd16fb117f r14: 0x00007ffd16fb1180 r15: 0x00007f6b8fffefc0
rip: 0x00007f6b94964361
gc> (:pm 4 1452224 10 :print-mode unsigned-dec)
 0x001628c0:   1065353216   4294967284   1073741824            0
 0x001628d0:            0            0            0            0
 0x001628e0:            0            0
gc> (:pm 4 1452224 10 :print-mode signed-dec)
 0x001628c0:   1065353216          -12   1073741824            0
 0x001628d0:            0            0            0            0
 0x001628e0:            0            0
```

### `(:disasm)`

Disassembly instructions in memory

```lisp
(:disasm addr len)
```

Example (after doing a `(lt)`, `(blg)`, `(dbg)`):

```nasm
gs> (:disasm (sym-val basic-type?) 59)
Object: gcommon

[0x28eb4c631c4] mov r9d, [r15+rdi*1-0x04]
[0x28eb4c631c9] mov r8d, [object]
[0x28eb4c631d1] cmp r9, rsi
[0x28eb4c631d4] jnz 0x0000028EB4C631E9
[0x28eb4c631da] lea rax, '#t
[0x28eb4c631df] jmp 0x0000028EB4C631FD
[0x28eb4c631e4] jmp 0x0000028EB4C631EC
[0x28eb4c631e9] mov rcx, '#f
[0x28eb4c631ec] mov r9d, [r15+r9*1+0x04]
[0x28eb4c631f1] cmp r9, r8
[0x28eb4c631f4] jnz 0x0000028EB4C631D1
[0x28eb4c631fa] mov rax, '#f
[0x28eb4c631fd] ret
```

For now, the disassembly is pretty basic, but it should eventually support GOAL symbols.

### `(:sym-name)`

Print the name of a symbol from its offset. The name is fetched from memory.

```lisp
(:sym-name offset)
```

Example (after doing a `(lt)`, `(blg)`, `(dbg)`):

```nasm
gs> (:sym-name 0)
symbol name for symbol 0h is #f
gs> (:sym-name 8)
symbol name for symbol 8h is #t
gs> (:sym-name 16)
symbol name for symbol 10h is function
gs> (:sym-name #x18)
symbol name for symbol 18h is basic
gs> (:sym-name #x20)
symbol name for symbol 20h is string
gs> (:sym-name #x30)
symbol name for symbol 30h is type
gs> (:sym-name #x80)
symbol name for symbol 80h is int64
gs> (:sym-name #x800)
symbol 800h is not loaded or is invalid
```

Keep in mind `-#xa8` is not valid syntax for a negative number in hexadecimal.

## Breakpoints

```lisp
OpenGOAL Compiler 0.1

;; first, connect to the target
g  > (lt)
[Listener] Socket connected established! (took 0 tries). Waiting for version...
Got version 0.1 OK!
[OUTPUT] reset #x147d24 #x2000000000 322300

[Debugger] Context: valid = true, s7 = 0x147d24, base = 0x2000000000, tid = 322300


;; run an infinite loop. This will time out because we don't see a response from the GOAL kernel that our function
;; has returned.
gc > (while #t (+ 1 2 3 4 5 6 7))
  Error - target has timed out. If it is stuck in a loop, it must be manually killed.
Runtime is not responding. Did it crash?


;; so we can attach the debugger!
gc > (dbg)
[Debugger] PTRACE_ATTACHED! Waiting for process to stop...
Target has stopped. Run (:di) to get more information.
Read symbol table (146816 bytes, 124 reads, 123 symbols, 2.02 ms)
rax: 0x000000000000000a rcx: 0x0000000000000005 rdx: 0x0000000000000000 rbx: 0x0000002000000000
rsp: 0x00007fddcde75c58 rbp: 0x00007fddcde75cc0 rsi: 0x0000000000000000 rdi: 0x0000000000000000
 r8: 0x0000000000147d24  r9: 0x0000002000000000 r10: 0x00007fddcde75ca0 r11: 0x0000000000000000
r12: 0x0000000000147d24 r13: 0x0000002007ffbf14 r14: 0x0000000000147d24 r15: 0x0000002000000000
rip: 0x0000002007ffbf3b
  [0x2007ffbf1b] add [rax], al
  [0x2007ffbf1d] add [rcx+0x02], bh
  [0x2007ffbf23] add rax, rcx
  [0x2007ffbf26] mov ecx, 0x03
  [0x2007ffbf2b] add rax, rcx
  [0x2007ffbf2e] mov ecx, 0x04
  [0x2007ffbf33] add rax, rcx
  [0x2007ffbf36] mov ecx, 0x05
- [0x2007ffbf3b] add rax, rcx
  [0x2007ffbf3e] mov ecx, 0x06
  [0x2007ffbf43] add rax, rcx
  [0x2007ffbf46] mov ecx, 0x07
  [0x2007ffbf4b] add rax, rcx
  [0x2007ffbf4e] mov eax, [r15+r14*1+0x08]
  [0x2007ffbf56] mov rcx, r14
  [0x2007ffbf59] add rcx, 0x00
  [0x2007ffbf60] cmp rax, rcx
  [0x2007ffbf63] jnz 0x0000002007FFBF19
  [0x2007ffbf69] mov eax, [r15+r14*1]
  [0x2007ffbf71] ret
  [0x2007ffbf72] add [rax], al
  [0x2007ffbf74] add [rax], al
  [0x2007ffbf76] add [rax], al
  [0x2007ffbf78] add [rax], al
  [0x2007ffbf7a] INVALID (0x00)

Debugger connected.

;; currently rcx = 5. let's set a breakpoint where it should be 7
gcs> (:bp #x2007ffbf4b)

;; and continue...
gcs> (:cont)

;; it hits the breakpoint. (this message should have more information...)
Target has stopped. Run (:di) to get more information.

;; get some info:
gcs> (:di)
Read symbol table (146816 bytes, 124 reads, 123 symbols, 1.46 ms)
rax: 0x0000000000000015 rcx: 0x0000000000000007 rdx: 0x0000000000000000 rbx: 0x0000002000000000
rsp: 0x00007fddcde75c58 rbp: 0x00007fddcde75cc0 rsi: 0x0000000000000000 rdi: 0x0000000000000000
 r8: 0x0000000000147d24  r9: 0x0000002000000000 r10: 0x00007fddcde75ca0 r11: 0x0000000000000000
r12: 0x0000000000147d24 r13: 0x0000002007ffbf14 r14: 0x0000000000147d24 r15: 0x0000002000000000
rip: 0x0000002007ffbf4c
  [0x2007ffbf2c] add eax, ecx
  [0x2007ffbf2e] mov ecx, 0x04
  [0x2007ffbf33] add rax, rcx
  [0x2007ffbf36] mov ecx, 0x05
  [0x2007ffbf3b] add rax, rcx
  [0x2007ffbf3e] mov ecx, 0x06
  [0x2007ffbf43] add rax, rcx
  [0x2007ffbf46] mov ecx, 0x07
  [0x2007ffbf4b] int3            ;; oops! should probably patch this in the disassembly!
- [0x2007ffbf4c] add eax, ecx
  [0x2007ffbf4e] mov eax, [r15+r14*1+0x08]
  [0x2007ffbf56] mov rcx, r14
  [0x2007ffbf59] add rcx, 0x00
  [0x2007ffbf60] cmp rax, rcx
  [0x2007ffbf63] jnz 0x0000002007FFBF19
  [0x2007ffbf69] mov eax, [r15+r14*1]
  [0x2007ffbf71] ret
  [0x2007ffbf72] add [rax], al
  [0x2007ffbf74] add [rax], al
  [0x2007ffbf76] add [rax], al
  [0x2007ffbf78] add [rax], al
  [0x2007ffbf7a] add [rax], al
  [0x2007ffbf7c] add [rax], al
  [0x2007ffbf7e] add [rax], al
  [0x2007ffbf80] in al, 0x08
  [0x2007ffbf82] INVALID (0x16)
  [0x2007ffbf82] add [rax], al
  [0x2007ffbf84] add [rcx], al
  [0x2007ffbf86] add [rbx], al
  [0x2007ffbf88] add [rax], al
  [0x2007ffbf8a] INVALID (0x00)

;; remove the breakpoint
gcs> (:ubp #x2007ffbf4b)

;; continue, it stays running
gcs> (:cont)
gcr>

;; break and check, the code is back to normal!
gcr> (:break)
Target has stopped. Run (:di) to get more information.
Read symbol table (146816 bytes, 124 reads, 123 symbols, 1.28 ms)
rax: 0x0000000000000015 rcx: 0x0000000000000007 rdx: 0x0000000000000000 rbx: 0x0000002000000000
rsp: 0x00007fddcde75c58 rbp: 0x00007fddcde75cc0 rsi: 0x0000000000000000 rdi: 0x0000000000000000
 r8: 0x0000000000147d24  r9: 0x0000002000000000 r10: 0x00007fddcde75ca0 r11: 0x0000000000000000
r12: 0x0000000000147d24 r13: 0x0000002007ffbf14 r14: 0x0000000000147d24 r15: 0x0000002000000000
rip: 0x0000002007ffbf4b
  [0x2007ffbf2b] add rax, rcx
  [0x2007ffbf2e] mov ecx, 0x04
  [0x2007ffbf33] add rax, rcx
  [0x2007ffbf36] mov ecx, 0x05
  [0x2007ffbf3b] add rax, rcx
  [0x2007ffbf3e] mov ecx, 0x06
  [0x2007ffbf43] add rax, rcx
  [0x2007ffbf46] mov ecx, 0x07
- [0x2007ffbf4b] add rax, rcx
  [0x2007ffbf4e] mov eax, [r15+r14*1+0x08]
  [0x2007ffbf56] mov rcx, r14
  [0x2007ffbf59] add rcx, 0x00
  [0x2007ffbf60] cmp rax, rcx
  [0x2007ffbf63] jnz 0x0000002007FFBF19
  [0x2007ffbf69] mov eax, [r15+r14*1]
  [0x2007ffbf71] ret
  [0x2007ffbf72] add [rax], al
  [0x2007ffbf74] add [rax], al
  [0x2007ffbf76] add [rax], al
  [0x2007ffbf78] add [rax], al
  [0x2007ffbf7a] add [rax], al
  [0x2007ffbf7c] add [rax], al
  [0x2007ffbf7e] add [rax], al
  [0x2007ffbf80] in al, 0x08
  [0x2007ffbf82] INVALID (0x16)
  [0x2007ffbf82] add [rax], al
  [0x2007ffbf84] add [rcx], al
  [0x2007ffbf86] add [rbx], al
  [0x2007ffbf88] add [rax], al

gcs>

;; we can still properly exit from the target, even in this state!
gcs> (e)
Tried to reset a halted target, detaching...
  Error - target has timed out. If it is stuck in a loop, it must be manually killed.
[Listener] Closed connection to target
```
