;;;;;;;;;;;;;;;;;;;;
;; asm_funcs.nasm ;;
;;;;;;;;;;;;;;;;;;;;

;; GOAL Runtime assembly functions. These exist only in the x86 version of GOAL.

;; declaration of the extern "C" function format_impl
extern format_impl

SECTION .TEXT

;; This _format function which will be exported to the GOAL symbol table at runtime start as "_format"
;; This function accepts 8 GOAL arguments and puts them on the stack, then calls format_impl and passes
;; a pointer to this array of GOAL arguments as the argument. The reason for this is that GOAL and
;; the standard System V ABI used in Linux are different for 8 argument function calls.

global _format
_format:
  ; GOAL will call with regs  RDI, RSI, RDX, RCX, R8, R9, R10, R11

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
  mov rdi, rsp

  ; call C function to do format, result will go in RAX
  call format_impl

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
;; NOTE: calling format has a _lot_ of indirection...
;; symbol table lookup to find the GOAL "format" symbol value
;; run the GOAL-to-C trampoline (on GOAL heap) to jump to this _format
;; run this wrapper to call the real format_impl




;; The _call_goal_asm function is used to call a GOAL function from C.
;; It supports up to 3 arguments and a return value.
;; This should be called with the arguments:
;; - first goal arg
;; - second goal arg
;; - third goal arg
;; - address of function to call
;; - address of the symbol table
;; - GOAL memory space offset

global _call_goal_asm

_call_goal_asm:
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
  mov r15, r8
  ;; symbol table
  mov r14, r9
  ;; call GOAL by function pointer
  call r13

  ;; retore x86 registers.
  pop r15
  pop r14
  pop r13
  ret