# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzdi2.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzdi2.S" 2
# 14 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzdi2.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzdi2.S" 2

 .syntax unified
 .text
 .thumb ;

 .p2align 2
.thumb ; ; .globl __clzdi2 ; .type __clzdi2,%function ; .hidden __clzdi2 ; .thumb_func ; __clzdi2:
# 30 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzdi2.S"
 cmp r1, 0
 itee ne
 clzne r0, r1
 clzeq r0, r0
 addeq r0, r0, 32

 bx lr
# 90 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzdi2.S"
.size __clzdi2, . - __clzdi2
