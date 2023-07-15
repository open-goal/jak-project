;; GOAL Runtime assembly functions. These exist only in the x86 version of GOAL.

SECTION .text

;; Call C++ code on unix systems, from GOAL, using System V calling convention.
global _arg_call_systemv
_arg_call_systemv:
  pop rax
  push r10 ; arg 6 (OpenGOAL compiler expects this register to be saved but systemv doesn't save it)
  push r11 ; arg 7 (OpenGOAL compiler expects this register to be saved but systemv doesn't save it)

  ; xmm stuff
  sub rsp, 136 ; 128 (size for xmms) + 8 (stack alignment)
  movaps [rsp], xmm8
  movaps [rsp + 16], xmm9
  movaps [rsp + 32], xmm10
  movaps [rsp + 48], xmm11
  movaps [rsp + 64], xmm12
  movaps [rsp + 80], xmm13
  movaps [rsp + 96], xmm14
  movaps [rsp + 112], xmm15

  call rax

  movaps xmm8, [rsp]
  movaps xmm9, [rsp + 16]
  movaps xmm10, [rsp + 32]
  movaps xmm11, [rsp + 48]
  movaps xmm12, [rsp + 64]
  movaps xmm13, [rsp + 80]
  movaps xmm14, [rsp + 96]
  movaps xmm15, [rsp + 112]
  add rsp, 136 ; 128 (size for xmms) + 8 (stack alignment)

  pop r11
  pop r10
  ret


;; Call C++ code on unix systems, from GOAL. 
;; 
;; Put arguments on the stack and put a pointer to this array in the first arg.
;; this function pushes all 8 OpenGOAL registers into a stack array.
;; then it calls the function pointed to by rax with a pointer to this array.
;; it returns the return value of the called function.
global _stack_call_systemv
_stack_call_systemv:
  pop rax

  sub rsp, 136 ; 128 (size for xmms) + 8 (stack alignment)
  movaps [rsp], xmm8
  movaps [rsp + 16], xmm9
  movaps [rsp + 32], xmm10
  movaps [rsp + 48], xmm11
  movaps [rsp + 64], xmm12
  movaps [rsp + 80], xmm13
  movaps [rsp + 96], xmm14
  movaps [rsp + 112], xmm15

  ; create stack array of arguments
  push r11
  push r10
  push r9
  push r8
  push rcx
  push rdx
  push rsi
  push rdi

  ; set first argument
  mov rdi, rsp
  ; call function
  call rax
  ; restore arguments (probably don't need to really do this...)
  pop rdi
  pop rsi
  pop rdx
  pop rcx
  pop r8
  pop r9
  pop r10
  pop r11

  movaps xmm8, [rsp]
  movaps xmm9, [rsp + 16]
  movaps xmm10, [rsp + 32]
  movaps xmm11, [rsp + 48]
  movaps xmm12, [rsp + 64]
  movaps xmm13, [rsp + 80]
  movaps xmm14, [rsp + 96]
  movaps xmm15, [rsp + 112]

  add rsp, 136 ; 128 (size for xmms) + 8 (stack alignment)
  ; return!
  ret

;; Call c++ code through mips2c.
;; GOAL will call a dynamically generated trampoline.
;; The trampoline will have pushed the exec function and stack offset onto the stack
global _mips2c_call_systemv
_mips2c_call_systemv:
  ;; grab the address to call and put it in xmm0
  sub rsp, 8
  movaps xmm0, [rsp + 16]
  ;; grab the stack offset
  mov rax, [rsp + 8]

  ;; first, save xmms
  sub rsp, 128
  movaps [rsp], xmm8
  movaps [rsp + 16], xmm9
  movaps [rsp + 32], xmm10
  movaps [rsp + 48], xmm11
  movaps [rsp + 64], xmm12
  movaps [rsp + 80], xmm13
  movaps [rsp + 96], xmm14
  movaps [rsp + 112], xmm15

  push r10
  push r11

  ;; oof
  sub rsp, 1280
  mov [rsp + 64], rdi ;; arg0
  mov [rsp + 80], rsi ;; arg1
  mov [rsp + 96], rdx ;; arg2
  mov [rsp + 112], rcx ;; arg3
  mov [rsp + 128], r8 ;; arg4
  mov [rsp + 144], r9 ;; arg5
  mov [rsp + 160], r10 ;; arg6
  mov [rsp + 176], r11 ;; arg7
  mov [rsp + 352], r13 ;; s6 (pp)
  mov [rsp + 368], r14 ;; s7 (st)

  mov rdi, rsp
  sub rdi, r15
  mov [rsp + 464], rdi ;; mip2c code's MIPS stack

  mov rdi, rsp


  sub rsp, rax ;; allocate space on the stack for GOAL fake stack
  push rax     ;; and remember this so we can find our way back
  sub rsp, 8

  movq rax, xmm0

  call rax ;; call!

  ;; unallocate
  add rsp, 8
  pop rax
  add rsp, rax

  mov rax, [rsp + 32]

  add rsp, 1280
  pop r11
  pop r10

  movaps xmm8, [rsp]
  movaps xmm9, [rsp + 16]
  movaps xmm10, [rsp + 32]
  movaps xmm11, [rsp + 48]
  movaps xmm12, [rsp + 64]
  movaps xmm13, [rsp + 80]
  movaps xmm14, [rsp + 96]
  movaps xmm15, [rsp + 112]
  add rsp, 152 ;; 128 for xmm's + 16 for the stuff pushed by trampoline + 8 for stack alignment undo

  ret

global _mips2c_call_windows
_mips2c_call_windows:
  ;; grab the address to call and put it in xmm0
  sub rsp, 8
  movaps xmm0, [rsp + 16]
  ;; grab the stack offset
  mov rax, [rsp + 8]

  ;; first, save xmms
  sub rsp, 128
  movaps [rsp], xmm8
  movaps [rsp + 16], xmm9
  movaps [rsp + 32], xmm10
  movaps [rsp + 48], xmm11
  movaps [rsp + 64], xmm12
  movaps [rsp + 80], xmm13
  movaps [rsp + 96], xmm14
  movaps [rsp + 112], xmm15

  push r10
  push r11

  ;; oof
  sub rsp, 1280
  mov [rsp + 64], rdi ;; arg0
  mov [rsp + 80], rsi ;; arg1
  mov [rsp + 96], rdx ;; arg2
  mov [rsp + 112], rcx ;; arg3
  mov [rsp + 128], r8 ;; arg4
  mov [rsp + 144], r9 ;; arg5
  mov [rsp + 160], r10 ;; arg6
  mov [rsp + 176], r11 ;; arg7
  mov [rsp + 352], r13 ;; s6 (pp)
  mov [rsp + 368], r14 ;; s7 (st)

  mov rdi, rsp
  sub rdi, r15
  mov [rsp + 464], rdi ;; mip2c code's MIPS stack

  mov rcx, rsp

  sub rsp, rax ;; allocate space on the stack for GOAL fake stack
  push rax     ;; and remember this so we can find our way back
  push rax

  movq rax, xmm0

  sub rsp, 32
  call rax ;; call!
  add rsp, 32

  ;; unallocate
  pop rax
  pop rax
  add rsp, rax

  mov rax, [rsp + 32]

  add rsp, 1280
  pop r11
  pop r10

  movaps xmm8, [rsp]
  movaps xmm9, [rsp + 16]
  movaps xmm10, [rsp + 32]
  movaps xmm11, [rsp + 48]
  movaps xmm12, [rsp + 64]
  movaps xmm13, [rsp + 80]
  movaps xmm14, [rsp + 96]
  movaps xmm15, [rsp + 112]
  add rsp, 152 ;; 128 for xmm's + 16 for the stuff pushed by trampoline + 8 for stack alignment undo

  ret


;; Call C++ code on windows, from GOAL. Put arguments on the stack and put a pointer to this array in the first arg.
global _stack_call_win32
_stack_call_win32:
  pop rax
  ; to make sure the stack frame is aligned
  sub rsp, 8

  sub rsp, 128
  movaps [rsp], xmm8
  movaps [rsp + 16], xmm9
  movaps [rsp + 32], xmm10
  movaps [rsp + 48], xmm11
  movaps [rsp + 64], xmm12
  movaps [rsp + 80], xmm13
  movaps [rsp + 96], xmm14
  movaps [rsp + 112], xmm15

  ; push all registers and create the register array on the stack
  push r11
  push r10
  push r9
  push r8
  push rcx
  push rdx
  push rsi
  push rdi

  ; set the first argument register to the stack argument array
  mov rcx, rsp
  sub rsp, 32

  ; call C function to do format, result will go in RAX
  call rax
  add rsp, 32

  ; restore
  ; (note - this could probably just be add rsp 72, we don't care about the value of these register)
  pop rdi
  pop rsi
  pop rdx
  pop rcx
  pop r8
  pop r9
  pop r10
  pop r11

  movaps xmm8, [rsp]
  movaps xmm9, [rsp + 16]
  movaps xmm10, [rsp + 32]
  movaps xmm11, [rsp + 48]
  movaps xmm12, [rsp + 64]
  movaps xmm13, [rsp + 80]
  movaps xmm14, [rsp + 96]
  movaps xmm15, [rsp + 112]
  add rsp, 128

  add rsp, 8
  ret

;; The _call_goal_asm function is used to call a GOAL function from C.
;; It calls on the parent stack, which is a bad idea if your stack is not already a GOAL stack.
;; It supports up to 3 arguments and a return value.
;; This should be called with the arguments:
;; - first goal arg
;; - second goal arg
;; - third goal arg
;; - address of function to call
;; - address of the symbol table
;; - GOAL memory space offset

global _call_goal_asm_systemv

_call_goal_asm_systemv:
  ;; x86 saved registers we need to modify for GOAL should be saved
  push r13
  push r14
  push r15

  ;; RDI - first arg
  ;; RSI - second arg
  ;; RDX - third arg
  ;; RCX - function pointer
  ;; R8  - st (goes in r14 and r13)
  ;; R9  - off (goes in r15)

  ;; set GOAL process
  mov r13, r8
  ;; symbol table
  mov r14, r8
  ;; offset
  mov r15, r9
  ;; call GOAL by function pointer
  call rcx

  ;; restore x86 registers.
  pop r15
  pop r14
  pop r13
  ret

global _call_goal8_asm_systemv

_call_goal8_asm_systemv:
  ;; x86 saved registers we need to modify for GOAL should be saved
  push r13
  push r14
  push r15

  ;; RDI - first arg (func)
  ;; RSI - second arg (arg array)
  ;; RDX - third arg  (0)
  ;; RCX - pp (goes in r13)
  ;; R8  - st (goes in r14)
  ;; R9  - off (goes in r15)

  ;; set GOAL function pointer
  mov r13, rcx
  ;; st
  mov r14, r8
  ;; offset
  mov r15, r9
  ;; move function to temp
  mov rax, rdi
  ;; extract arguments
  mov rdi, [rsi + 0]  ;; 0
  mov rdx, [rsi + 16] ;; 2
  mov rcx, [rsi + 24] ;; 3
  mov r8, [rsi + 32]  ;; 4
  mov r9, [rsi + 40]  ;; 5
  mov r10, [rsi + 48] ;; 6
  mov r11, [rsi + 56]  ;; 7
  mov rsi, [rsi + 8] ;; 1 (do this last)
  ;; call GOAL by function pointer
  call rax

  ;; retore x86 registers.
  pop r15
  pop r14
  pop r13
  ret

;; Call goal, but switch stacks.
global _call_goal_on_stack_asm_systemv

_call_goal_on_stack_asm_systemv:
  ;; RDI - stack pointer
  ;; RSI - unused
  ;; RDX - unused
  ;; RCX - function pointer
  ;; R8  - st (goes in r14 and r13)
  ;; R9  - off (goes in r15)

  ;; x86 saved registers we need to modify for GOAL should be saved
  push r13
  push r14
  push r15

  ;; stash current stack pointer in rsi
  mov rsi, rsp
  ;; switch to new stack
  mov rsp, rdi
  ;; back up old stack pointer
  push rsi

  ;; set GOAL function pointer
  mov r13, r8
  ;; symbol table
  mov r14, r8
  ;; offset
  mov r15, r9
  ;; call GOAL by function pointer
  call rcx

  ;; get old stack pointer
  pop rsi
  mov rsp, rsi

  ;; retore x86 registers.
  pop r15
  pop r14
  pop r13
  ret

;; The _call_goal_asm function is used to call a GOAL function from C.
;; It supports up to 3 arguments and a return value.
;; This should be called with the arguments:
;; - first goal arg
;; - second goal arg
;; - third goal arg
;; - address of function to call
;; - address of the symbol table
;; - GOAL memory space offset

global _call_goal_asm_win32

_call_goal_asm_win32:
  push rdx    ; 8
  push rbx    ; 16
  push rbp    ; 24
  push rsi    ; 32
  push rdi    ; 40
  push r8     ; 48
  push r9     ; 56
  push r10    ; 64
  push r11    ; 72
  push r12    ; 80
  push r13    ; 88
  push r14    ; 96
  push r15    ; 104

  sub rsp, 16
  movups [rsp], xmm6
  sub rsp, 16
  movups [rsp], xmm7
  
  mov rdi, rcx ;; rdi is GOAL first argument, rcx is windows first argument
  mov rsi, rdx ;; rsi is GOAL second argument, rdx is windows second argument
  mov rdx, r8  ;; rdx is GOAL third argument, r8 is windows third argument
  mov r15, [rsp + 184] ;; offset
  mov r14, [rsp + 176] ;; symbol table
  mov r13, r14 ;; r13 is GOAL process, set to #f (same as symbol table) TODO: verify on PS2
  
  call r9 ;; r9 is GOAL t9, r9 is windows fourth argument (not sure this is correct. maybe rax instead?)

  movups xmm7, [rsp]
  add rsp, 16
  movups xmm6, [rsp]
  add rsp, 16
  
  pop r15
  pop r14
  pop r13
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  pop rdx
  
  ret

global _call_goal8_asm_win32

_call_goal8_asm_win32:
  push rdx    ; 8
  push rbx    ; 16
  push rbp    ; 24
  push rsi    ; 32
  push rdi    ; 40
  push r8     ; 48
  push r9     ; 56
  push r10    ; 64
  push r11    ; 72
  push r12    ; 80
  push r13    ; 88
  push r14    ; 96
  push r15    ; 104

  sub rsp, 16
  movups [rsp], xmm6
  sub rsp, 16
  movups [rsp], xmm7

  mov r13, r9 ;; pp
  mov r15, [rsp + 184] ;; symbol table
  mov r14, [rsp + 176] ;; offset
  mov rax, rcx ;; func temp
  mov rsi, rdx ;; arg table
  mov rdi, [rsi + 0]  ;; 0
  mov rdx, [rsi + 16] ;; 2
  mov rcx, [rsi + 24] ;; 3
  mov r8, [rsi + 32]  ;; 4
  mov r9, [rsi + 40]  ;; 5
  mov r10, [rsi + 48] ;; 6
  mov r11, [rsi + 56]  ;; 7
  mov rsi, [rsi + 8] ;; 1 (do this last)
  ;; call GOAL by function pointer
  call rax

  movups xmm7, [rsp]
  add rsp, 16
  movups xmm6, [rsp]
  add rsp, 16

  pop r15
  pop r14
  pop r13
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  pop rdx

  ret

global _call_goal_on_stack_asm_win32

_call_goal_on_stack_asm_win32:
  ;; arg0 (rcx) stack
  ;; arg1 (rdx) fp
  ;; arg2 (r8) st
  ;; arg3 (r9) off
  push rdx    ; 8
  push rbx    ; 16
  push rbp    ; 24
  push rsi    ; 32
  push rdi    ; 40
  push r8     ; 48
  push r9     ; 56
  push r10    ; 64
  push r11    ; 72
  push r12    ; 80
  push r13    ; 88
  push r14    ; 96
  push r15    ; 104


  sub rsp, 160
  movaps [rsp], xmm8
  movaps [rsp + 16], xmm9
  movaps [rsp + 32], xmm10
  movaps [rsp + 48], xmm11
  movaps [rsp + 64], xmm12
  movaps [rsp + 80], xmm13
  movaps [rsp + 96], xmm14
  movaps [rsp + 112], xmm15
  movaps [rsp + 128], xmm7
  movaps [rsp + 144], xmm6

  ;; stack swap
  mov rsi, rsp
  mov rsp, rcx
  push rsi

  mov r13, r8  ;; pp
  mov r14, r8  ;; st
  mov r15, r9  ;; offset

  call rdx

  ;; restore stack
  pop rsi
  mov rsp, rsi

  movaps xmm8, [rsp]
  movaps xmm9, [rsp + 16]
  movaps xmm10, [rsp + 32]
  movaps xmm11, [rsp + 48]
  movaps xmm12, [rsp + 64]
  movaps xmm13, [rsp + 80]
  movaps xmm14, [rsp + 96]
  movaps xmm15, [rsp + 112]
  movaps xmm7, [rsp + 128]
  movaps xmm6, [rsp + 144]
  add rsp, 160

  pop r15
  pop r14
  pop r13
  pop r12
  pop r11
  pop r10
  pop r9
  pop r8
  pop rdi
  pop rsi
  pop rbp
  pop rbx
  pop rdx

  ret
