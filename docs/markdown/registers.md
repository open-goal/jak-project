# Registers

Although modern computers are much faster than the PS2, and we could probably get away with a really inefficient register allocation scheme, I think it's worth it to get this right.

## Register differences between MIPS and x86-64

The PS2's MIPS processor has these categories of register:
- General Purpose. They are 128-bit, but usually only lower 64 bits are used. 32 registers, each 128-bits.
- Floating point registers. 32 registers, each for a 32-bit float.
- Vector float registers. 32 registers, each for 4x 32-bit floats. Used only in inline assembly
- `vi` registers. 16 registers, each a 16-bit integer. Used very rarely in inline assembly

There are also some control/special registers too (`Q`, `R`...), but code using these will be manually ported.

In comparison, x86-64 has much fewer registers:
- 16 General Purpose. Each 64-bits
- 16 `xmm` registers. 128-bits, and can store either 128-bit integers or 4x 32-bit floats

Here is the mapping:
- MIPS GPR (lower 64 bits only) - x86-64 GPR
- MIPS GPR (128-bits, only special cases) - x64-64 `xmm`
- MIPS floating point - x64-64 `xmm` (lower 32-bits)
- MIPS vector float - x64-64 `xmm` (packed single)
- MIPS `vi` - manually handled??

Here is the MIPS GPR map
- `r0` or `zero` : always zero
- `r1` or `at`: assembler temporary, not saved, not used by compiler
- `r2` or `v0`: return value, not saved
- `r3` or `v1`: not saved
- `r4` or `a0`: not saved, argument 0
- `r5` or `a1`: not saved, argument 1
- `r6` or `a2`: not saved, argument 2
- `r7` or `a3`: not saved, argument 3
- `r8` or `t0`: not saved, argument 4
- `r9` or `t1`: not saved, argument 5
- `r10` or `t2`: not saved, argument 6
- `r11` or `t3`: not saved, argument 7
- `r12` or `t4`: not saved
- `r13` or `t5`: not saved
- `r14` or `t6`: not saved
- `r15` or `t7`: not saved
- `r16` or `s0`: saved
- `r17` or `s1`: saved
- `r18` or `s2`: saved
- `r19` or `s3`: saved
- `r20` or `s4`: saved
- `r21` or `s5`: saved
- `r22` or `s6`: saved, process pointer
- `r23` or `s7`: saved, symbol pointer
- `r24` or `t8`: not saved
- `r25` or `t9`: function call pointer
- `r26` or `k0`: kernel reserved (unused)
- `r27` or `k1`: kernel reserved (unused)
- `r28` or `gp`: saved
- `r29` or `sp`: stack pointer
- `r30` or `fp`: current function pointer
- `r31` or `ra`: return address pointer


And the x86-64 GPR map
- `rax`: return value
- `rcx`: argument 3
- `rdx`: argument 2
- `rbx`: saved
- `rsp`: stack pointer
- `rbp`: saved
- `rsi`: argument 1
- `rdi`: argument 0
- `r8`: argument 4
- `r9`: argument 5
- `r10`: argument 6, saved if not argument
- `r11`: argument 7, saved if not argument
- `r12`: saved
- `r13`: process pointer
- `r14`: symbol table
- `r15`: offset pointer

### Plan for Memory Access

The PS2 uses 32-bit pointers, and changing the pointer size is likely to introduce bugs, so we will keep using 32-bit pointers.  Also, GOAL has some hardcoded checks on the value for pointers, so we need to make sure the memory appears to the program at the correct address.

To do this, we have separate "GOAL Pointers" and "real pointers".  The "real pointers" are just normal x86-64 pointers, and the "GOAL Pointer" is an offset into a main memory array.  A "real pointer" to the main memory array is stored in `r15` (offset pointer) when GOAL code is executing, and the GOAL compiler will automatically add this to all memory accesses.

The overhead from doing this is not as bad as you might expect - x86 has nice addressing modes (Scale Index Base) which are quite fast, and don't require the use of temporary registers. If this does turn out to be much slower than I expect, we can introduce the concept of real pointers in GOAL code, and use them in places where we are limited in accessing memory.

The main RAM is mapped at `0x0` on the PS2, with the first 1 MB reserved for the kernel.  We should make sure that the first 1 MB of GOAL main memory will cause a segfault if read/written/executed, to catch null pointer bugs.

In the C Kernel code, the `r15` pointer doesn't exist. Instead, `g_ee_main_memory` is a global which points to the beginning of GOAL main memory.  The `Ptr<T>` template class takes care of converting GOAL and C++ pointers in a convenient way, and catches null pointer access.

The GOAL stack pointer should likely be a real pointer, for performance reasons.  This makes pushing/popping/calling/returning/accessing stack variables much faster (can use actual `push`, `pop`), with the only cost being getting a GOAL stack pointer requiring some extra work. The stack pointer's value is read/written extremely rarely (only in kernel code that will be rewritten anyway), so this seems like a good tradeoff.

The other registers are less clear.  The process pointer can probably be a real pointer.  But the symbol table could go a few ways:
1. Make it a real pointer.  Symbol value access is fast, but comparison against false requires two extra operations.
2. Make it a GOAL pointer. Symbol value access requires more complicated addressing modes to be one instruction, but comparison against false is fast.

Right now I'm leaning toward 2, but it shouldn't be a huge amount of work to change if I'm wrong.

### Plan for Function Call and Arguments

In GOAL for MIPS, function calls are weird.  Functions are always called by register using `t9`. There seems to be a different register allocator for function pointers, as nested function calls have really wacky register allocation.  In GOAL-x86-64, this restriction will be removed, and a function can be called from any register. (see next section for why we can do this)

Unfortunately, GOAL's 128-bit function arguments present a big challenge.  When calling a function, we can't know if the function we're calling is expecting an integer, float, or 128-bit integer. In fact, the caller may not even know if it has an integer, float, or 128-bit integer. The easy and foolproof way to get this right is to use 128-bit `xmm` registers for all arguments and return values, but this will cause a massive performance hit and increase code size, as we'll have to move values between register types constantly. The current plan is this:

- Floats go in GPRs for arguments/return values. GOAL does this too, and takes the hit of converting between registers as well. Probably the impact on a modern CPU is even worse, but we can live with it.
- We'll compromise for 128-bit function calls. When the compiler can figure out that the function being called expects or returns a 128-bit value, it will use the 128-bit calling convention.  In all other cases, it will use 64-bit. There aren't many places where 128-bit integer are used outside of inline assembly, so I suspect this will just work. If there are more complicated instances (call a function pointer and get either a 64 or 128-bit result), we will need to special case them.

### Plan for Static Data

The original GOAL implementation always called functions by using the `t9` register. So, on entry to a function, the `t9` register contains the address of the function. If the function needs to access static data, it will move this `fp`, then do `fp` relative addressing to load data. Example:

```nasm
function-start:
    daddiu sp, sp, -16  ;; allocate space on stack
    sd fp, 8(sp)        ;; back up old fp on stack
    or fp, t9, r0       ;; set fp to address of function
    lwc1 f0, L345(fp)   ;; load relative to function start
```

To copy this exactly on x86 would require reserving two registers equivalent to `t9` and `gp`.  A better approach for x86-64 is to use "RIP relative addressing". This can be used to load memory relative to the current instruction pointer.  This addressing mode can be used with "load effective address" (`lea`) to create pointers to static data as well.

### Plan for Memory

Access memory by GOAL pointer in `rx` with constant offset (optionally zero):

```nasm
mov rdest, [roff + rx + offset]
```
