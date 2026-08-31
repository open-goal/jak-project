// GOAL Runtime assembly functions. These exist only in the arm64 version of GOAL.
// - https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms#Pass-arguments-to-functions-correctly
// - https://en.wikipedia.org/wiki/Calling_convention#ARM_(A64)
// - https://student.cs.uwaterloo.ca/~cs452/docs/rpi4b/aapcs64.pdf
// - s16–s31 (d8–d15, q4–q7) must be preserved
// - s0–s15 (d0–d7, q0–q3) and d16–d31 (q8–q15) do not need to be preserved
// - https://devblogs.microsoft.com/oldnewthing/20220728-00/?p=106912
// - // - https://courses.cs.washington.edu/courses/cse469/19wi/arm64.pdf

.text

// Call C++ code on arm64 systems, from GOAL.
// Following the macOS documentation which mostly aligns with standard arm64
.global _arg_call_arm64
.align 4
_arg_call_arm64:
  stp	x29, x30, [sp, #-16]!
  mov	x29, sp

  // Putting an exclamation point after the close-bracket
  // means that the calculated effective address is written back to the base register. (pre-indexing)
  stp q15, q14, [sp, #-32]!
  stp q13, q12, [sp, #-32]!
  stp q11, q10, [sp, #-32]!
  stp q9, q8, [sp, #-32]!

  blr x8

  // restore the vector pairs in the same order they were saved
  ldp q9, q8, [sp], #32
  ldp q11, q10, [sp], #32
  ldp q13, q12, [sp], #32
  ldp q15, q14, [sp], #32

  ldp	x29, x30, [sp], #16
  ret


// Call C++ code on arm64 systems, from GOAL. 
// 
// Put arguments on the stack and put a pointer to this array in the first arg.
// this function pushes all 8 OpenGOAL registers into a stack array.
// it calls the function in x8 with a pointer to this array.
// it returns the return value of the called function.
.global _stack_call_arm64
.align 4
_stack_call_arm64:
  stp	x29, x30, [sp, #-16]!
  mov	x29, sp

  stp q15, q14, [sp, #-32]!
  stp q13, q12, [sp, #-32]!
  stp q11, q10, [sp, #-32]!
  stp q9, q8, [sp, #-32]!

  // create stack array of arguments
  // arg 7 (R11 in x86)
  // arg 6 (R10 in x86)
  // arg 5 (R8 in x86)
  // arg 4 (R8 in x86)
  // arg 3 (RCX in x86)
  // arg 2 (RDX in x86)
  // arg 1 (RSI in x86)
  // arg 0 (RDI in x86)
  // put x0 at the lowest address and x7 at the highest
  stp x6, x7, [sp, #-16]!
  stp x4, x5, [sp, #-16]!
  stp x2, x3, [sp, #-16]!
  stp x0, x1, [sp, #-16]!

  // set first argument
  // pass the array address as the first argument
  mov x0, sp
  // call function
  blr x8
  // keep x0 because it holds the return value
  ldr x1, [sp, #8]
  ldp x2, x3, [sp, #16]
  ldp x4, x5, [sp, #32]
  ldp x6, x7, [sp, #48]
  add sp, sp, #64

  // restore the vector pairs in the same order they were saved
  ldp q9, q8, [sp], #32
  ldp q11, q10, [sp], #32
  ldp q13, q12, [sp], #32
  ldp q15, q14, [sp], #32

  ldp	x29, x30, [sp], #16
  // return!
  ret

// Call c++ code through mips2c.
// GOAL will call a dynamically generated trampoline.
// x9 holds the C function and x10 holds the fake GOAL stack size.
.global _mips2c_call_arm64
.align 4
_mips2c_call_arm64:
  stp	x29, x30, [sp, #-16]!
  mov	x29, sp

  // first, save quadword registers
  // save all 128 bits because AAPCS64 only preserves the low half
  sub	sp, sp, #128
  stp	q8, q9, [sp]
  stp	q10, q11, [sp, #32]
  stp	q12, q13, [sp, #64]
  stp	q14, q15, [sp, #96]

  // oof
  // 1280-byte MIPS register context
  sub	sp, sp, #1280
  // arg 0 (RDI in x86) and
  str	x0, [sp, #64]	// a0
  // arg 1 (RSI in x86)
  str	x1, [sp, #80]	// a1
  // arg 2 (RDX in x86) and arg 3 (RCX in x86)
  str	x2, [sp, #96]	// a2
  // arg 2 (RDX in x86) and arg 3 (RCX in x86)
  str	x3, [sp, #112]	// a3
  // arg 4 (R8 in x86) and arg 5 (R8 in x86)
  str	x4, [sp, #128]	// t0
  // arg 4 (R8 in x86) and arg 5 (R8 in x86)
  str	x5, [sp, #144]	// t1
  // arg 6 (R10 in x86) and arg 7 (R11 in x86)
  str	x6, [sp, #160]	// t2
  // arg 6 (R10 in x86) and arg 7 (R11 in x86)
  str	x7, [sp, #176]	// t3
  // s6 (pp) (R13 in x86) and s7 (st) (R14 in x86)
  str	x20, [sp, #352]	// s6 (pp)
  // s6 (pp) (R13 in x86) and s7 (st) (R14 in x86)
  str	x21, [sp, #368]	// s7 (st)

  // store the context as a GOAL pointer in the MIPS sp slot
  mov	x11, sp
  sub	x11, x11, x22
  // mip2c code's MIPS stack
  str	x11, [sp, #464]	// r29 (sp)

  // move the stack pointer to arg 0
  mov	x0, sp		// pass the context as the sole argument

  // allocate space on the stack for GOAL fake stack
  // round the fake GOAL stack to 16 bytes
  add	x11, x10, #15
  and	x11, x11, #-16
  sub	sp, sp, x11
  // and remember this so we can find our way back
  str	x11, [sp, #-16]!	// save the stack size across the C call

  // call!
  blr	x9

  // unallocate
  ldr	x11, [sp], #16
  add	sp, sp, x11	// restore sp to the context base

  ldr	x0, [sp, #32]	// return the v0 slot written by mips2c

  // reset the stackpointer back
  add	sp, sp, #1280
  ldp	q8, q9, [sp]
  ldp	q10, q11, [sp, #32]
  ldp	q12, q13, [sp, #64]
  ldp	q14, q15, [sp, #96]
  add	sp, sp, #128
  ldp	x29, x30, [sp], #16
  ret

// The _call_goal_asm function is used to call a GOAL function from C.
// It calls on the parent stack, which is a bad idea if your stack is not already a GOAL stack.
// It supports up to 3 arguments and a return value.
// This should be called with the arguments:
// - first goal arg
// - second goal arg
// - third goal arg
// - address of function to call
// - address of the symbol table
// - GOAL memory space offset
.global _call_goal_asm_arm64
.align 4
_call_goal_asm_arm64:
  stp	x29, x30, [sp, #-16]!
  mov	x29, sp
  // saved registers we need to modify for GOAL should be preserved
  // ARM64 requires 16-byte stack pointer alignment
  stp x20, x21, [sp, #-16]!
  stp x22, x27, [sp, #-16]!
  sub sp, sp, #128
  stp q8, q9, [sp]
  stp q10, q11, [sp, #32]
  stp q12, q13, [sp, #64]
  stp q14, q15, [sp, #96]

  // x0 - first arg
  // x1 - second arg
  // x2 - third arg
  // x3 - function pointer
  // x4 - st (goes in x20 and x21)
  // x5 - off (goes in x22)
  // x6 holds the executable mapping base for x27

  // set GOAL process
  mov x20, x4
  // symbol table
  mov x21, x4
  // offset
  mov x22, x5
  mov x27, x6
  // call GOAL by function pointer
  blr x3

  // restore saved registers.
  ldp q8, q9, [sp]
  ldp q10, q11, [sp, #32]
  ldp q12, q13, [sp, #64]
  ldp q14, q15, [sp, #96]
  add sp, sp, #128
  ldp x22, x27, [sp], #16
  ldp x20, x21, [sp], #16
  ldp	x29, x30, [sp], #16
  ret

.global _call_goal8_asm_arm64
.align 4
_call_goal8_asm_arm64:
  stp	x29, x30, [sp, #-16]!
  mov	x29, sp
  // saved registers we need to modify for GOAL should be preserved
  // ARM64 requires 16-byte stack pointer alignment
  stp x20, x21, [sp, #-16]!
  stp x22, x27, [sp, #-16]!
  sub sp, sp, #128
  stp q8, q9, [sp]
  stp q10, q11, [sp, #32]
  stp q12, q13, [sp, #64]
  stp q14, q15, [sp, #96]

  // x0 - first arg (func)
  // x1 - second arg (arg array)
  // x2 - third arg  (0)
  // x3 - pp (goes in r13)
  // x4  - st (goes in r14)
  // x5  - off (goes in r15)
  // x6 holds the executable mapping base for x27

  // set GOAL function pointer
  mov x20, x3
  // st
  mov x21, x4
  // offset
  mov x22, x5
  // save the executable mapping before loading x6 from the argument array
  mov x27, x6
  // move function to temp
  mov x8, x0
  // extract arguments
  ldr x0, [x1]  // 0
  ldr x2, [x1, #+16] // 2
  ldr x3, [x1, #+24] // 3
  ldr x4, [x1, #+32]  // 4
  ldr x5, [x1, #+40]  // 5
  ldr x6, [x1, #+48] // 6
  ldr x7, [x1, #+56]  // 7
  ldr x1, [x1, #+8] // 1 (do this last)
  // call GOAL by function pointer
  blr x8

  // retore registers.
  ldp q8, q9, [sp]
  ldp q10, q11, [sp, #32]
  ldp q12, q13, [sp, #64]
  ldp q14, q15, [sp, #96]
  add sp, sp, #128
  ldp x22, x27, [sp], #16
  ldp x20, x21, [sp], #16
  ldp	x29, x30, [sp], #16
  ret

// Call goal, but switch stacks.
.global _call_goal_on_stack_asm_arm64
.align 4
_call_goal_on_stack_asm_arm64:
  stp	x29, x30, [sp, #-16]!
  mov	x29, sp
  // x0 - stack pointer
  // x1 - unused
  // x2 - unused
  // x3 - function pointer
  // x4  - st (goes in x21 and x20)
  // x5  - offset (goes in x22)
  // x6 holds the executable mapping base for x27

  // saved registers we need to modify for GOAL should be preserved
  // ARM64 requires 16-byte stack pointer alignment
  stp x20, x21, [sp, #-16]!
  stp x22, x27, [sp, #-16]!
  sub sp, sp, #128
  stp q8, q9, [sp]
  stp q10, q11, [sp, #32]
  stp q12, q13, [sp, #64]
  stp q14, q15, [sp, #96]

  // also stash the current stack pointer on the stack
  // NOTE - you cannot directly store or load the `sp` register in arm64
  // save the native sp through x9 because str cannot use sp as data
  mov x9, sp
  // switch to new stack
  mov sp, x0
  str x9, [sp, #-16]!

  // set GOAL function pointer
  mov x20, x4 // set GOAL process
  mov x21, x4 // symbol table
  mov x22, x5 // offset
  mov x27, x6
  // call GOAL by function pointer
  blr x3

  // restore registers
  ldr x9, [sp], #16
  mov sp, x9
  ldp q8, q9, [sp]
  ldp q10, q11, [sp, #32]
  ldp q12, q13, [sp, #64]
  ldp q14, q15, [sp, #96]
  add sp, sp, #128
  ldp x22, x27, [sp], #16
  ldp x20, x21, [sp], #16
  ldp	x29, x30, [sp], #16
  ret
