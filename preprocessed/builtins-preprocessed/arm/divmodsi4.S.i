# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divmodsi4.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divmodsi4.S" 2
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divmodsi4.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 17 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divmodsi4.S" 2







 .syntax unified
 .text
  .thumb ;

@ int __divmodsi4(int divident, int divisor, int *remainder)
@ Calculate the quotient and remainder of the (signed) division. The return
@ value is the quotient, the remainder is placed in the variable.

 .p2align 3
.thumb ; ; .globl __divmodsi4 ; .type __divmodsi4,%function ; .hidden __divmodsi4 ; .thumb_func ; __divmodsi4:

 tst r1, r1
 beq .L_divzero
 mov r3, r0
 sdiv r0, r3, r1
 mls r1, r0, r1, r3
 str r1, [r2]
 bx lr
.L_divzero:
 mov r0, #0
 bx lr
# 68 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divmodsi4.S"
.size __divmodsi4, . - __divmodsi4
