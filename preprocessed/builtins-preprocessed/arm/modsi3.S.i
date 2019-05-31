# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/modsi3.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/modsi3.S" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/modsi3.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/modsi3.S" 2







 .syntax unified
 .text
 .thumb ;

@ int __modsi3(int divident, int divisor)
@ Calculate and return the remainder of the (signed) division.

 .p2align 3
.thumb ; ; .globl __modsi3 ; .type __modsi3,%function ; .hidden __modsi3 ; .thumb_func ; __modsi3:

 tst r1, r1
 beq .L_divzero
 sdiv r2, r0, r1
 mls r0, r2, r1, r0
 bx lr
.L_divzero:
 mov r0, #0
 bx lr
# 57 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/modsi3.S"
.size __modsi3, . - __modsi3
