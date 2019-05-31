# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/umodsi3.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/umodsi3.S" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/umodsi3.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/umodsi3.S" 2

 .syntax unified
 .text
 .thumb ;

@ unsigned int __umodsi3(unsigned int divident, unsigned int divisor)
@ Calculate and return the remainder of the (unsigned) division.

 .p2align 2
.thumb ; ; .globl __umodsi3 ; .type __umodsi3,%function ; .hidden __umodsi3 ; .thumb_func ; __umodsi3:

 tst r1, r1
 beq .L_divby0
 udiv r2, r0, r1
 mls r0, r2, r1, r0
 bx lr
# 147 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/umodsi3.S"
.L_divby0:
 mov r0, #0

 b __aeabi_idiv0




.size __umodsi3, . - __umodsi3
