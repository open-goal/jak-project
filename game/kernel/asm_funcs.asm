;;;;;;;;;;;;;;;;;;;;
;; asm_funcs.nasm ;;
;;;;;;;;;;;;;;;;;;;;

;; GOAL Runtime assembly functions. These exist only in the x86 version of GOAL.

SECTION .TEXT

;; this function pushes all 8 OpenGOAL registers into a stack array.
;; then it calls the function pointed to by rax with a pointer to this array.
;; it returns the return value of the called function.
global _stack_call_linux
_stack_call_linux:
  pop rax
  ; align stack
  sub rsp, 8
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
  ; restore stack
  add rsp, 8
  ; return!
  ret

;; windows implementation of stack_call
global _stack_call_win32
_stack_call_win32:
  pop rax
  ; to make sure the stack frame is aligned
  sub rsp, 8

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
  add rsp, 8
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

global _call_goal_asm_linux

_call_goal_asm_linux:
  ;; x86 saved registers we need to modify for GOAL should be saved
  push r13
  push r14
  push r15

  ;; RDI - first arg
  ;; RSI - second arg
  ;; RDX - third arg
  ;; RCX - function pointer (goes in r13)
  ;; R8  - st (goes in r14)
  ;; R9  - off (goes in r15)

  ;; set GOAL function pointer
  mov r13, rcx
  ;; offset
  mov r14, r8
  ;; symbol table
  mov r15, r9
  ;; call GOAL by function pointer
  call r13

  ;; retore x86 registers.
  pop r15
  pop r14
  pop r13
  ret

global _call_goal_on_stack_asm_linux

_call_goal_on_stack_asm_linux:
  ;; RDI - stack pointer
  ;; RSI - unused
  ;; RDX - unused
  ;; RCX - function pointer (goes in r13)
  ;; R8  - st (goes in r14)
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
  mov r13, rcx
  ;; offset
  mov r14, r8
  ;; symbol table
  mov r15, r9
  ;; call GOAL by function pointer
  call r13

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
  mov r13, r9  ;; r13 is GOAL fp, r9 is windows fourth argument
  mov r15, [rsp + 184] ;; symbol table
  mov r14, [rsp + 176] ;; offset
  
  call r13

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

  sub rsp, 16
  movups [rsp], xmm6
  sub rsp, 16
  movups [rsp], xmm7

  ;; stack swap
  mov rsi, rsp
  mov rsp, rcx
  push rsi

  mov r13, rdx ;; fp
  mov r14, r8  ;; st
  mov r15, r9  ;; offset

  call r13

  ;; restore stack
  pop rsi
  mov rsp, rsi

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