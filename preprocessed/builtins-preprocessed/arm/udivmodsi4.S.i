# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivmodsi4.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivmodsi4.S" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivmodsi4.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivmodsi4.S" 2

 .syntax unified
 .text
 .thumb ;

@ unsigned int __udivmodsi4(unsigned int divident, unsigned int divisor,
@ unsigned int *remainder)
@ Calculate the quotient and remainder of the (unsigned) division. The return
@ value is the quotient, the remainder is placed in the variable.

 .p2align 2
.thumb ; ; .globl __udivmodsi4 ; .type __udivmodsi4,%function ; .hidden __udivmodsi4 ; .thumb_func ; __udivmodsi4:

 tst r1, r1
 beq .L_divby0
 mov r3, r0
 udiv r0, r3, r1
 mls r1, r0, r1, r3
 str r1, [r2]
 bx lr
# 169 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivmodsi4.S"
.L_divby0:
 mov r0, #0

 b __aeabi_idiv0




.size __udivmodsi4, . - __udivmodsi4
