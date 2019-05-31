# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivsi3.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivsi3.S" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivsi3.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivsi3.S" 2

 .syntax unified
 .text

.thumb ;

 .p2align 2
.globl __aeabi_uidiv ; .type __aeabi_uidiv,%function ; .hidden __aeabi_uidiv ; ; .set __aeabi_uidiv, __udivsi3 ;

@ unsigned int __udivsi3(unsigned int divident, unsigned int divisor)
@ Calculate and return the quotient of the (unsigned) division.

.thumb ; ; .globl __udivsi3 ; .type __udivsi3,%function ; .hidden __udivsi3 ; .thumb_func ; __udivsi3:

 tst r1, r1
 beq .L_divby0
 udiv r0, r0, r1
 bx lr

.L_divby0:
 mov r0, #0

 b __aeabi_idiv0
# 261 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/udivsi3.S"
.size __udivsi3, . - __udivsi3
