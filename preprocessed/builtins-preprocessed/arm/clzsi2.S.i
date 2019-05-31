# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzsi2.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzsi2.S" 2
# 14 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzsi2.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzsi2.S" 2

 .syntax unified
 .text
 .thumb ;

 .p2align 2
.thumb ; ; .globl __clzsi2 ; .type __clzsi2,%function ; .hidden __clzsi2 ; .thumb_func ; __clzsi2:

 clz r0, r0
 bx lr
# 70 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/clzsi2.S"
.size __clzsi2, . - __clzsi2
