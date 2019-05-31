# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divsi3.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divsi3.S" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divsi3.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divsi3.S" 2







   .syntax unified
   .text
   .thumb ;

 .p2align 3

.globl __aeabi_idiv ; .type __aeabi_idiv,%function ; .hidden __aeabi_idiv ; ; .set __aeabi_idiv, __divsi3 ;

@ int __divsi3(int divident, int divisor)
@ Calculate and return the quotient of the (signed) division.

.thumb ; ; .globl __divsi3 ; .type __divsi3,%function ; .hidden __divsi3 ; .thumb_func ; __divsi3:

   tst r1,r1
   beq .L_divzero
   sdiv r0, r0, r1
   bx lr
.L_divzero:
   mov r0,#0
   bx lr
# 79 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/divsi3.S"
.size __divsi3, . - __divsi3
