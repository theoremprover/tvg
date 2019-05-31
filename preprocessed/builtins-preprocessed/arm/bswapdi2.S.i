# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapdi2.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapdi2.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapdi2.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapdi2.S" 2

 .syntax unified
 .text
 .thumb ;






 .p2align 2
.thumb ; ; .globl __bswapdi2 ; .type __bswapdi2,%function ; .hidden __bswapdi2 ; .thumb_func ; __bswapdi2:
# 36 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapdi2.S"
    rev r2, r0
    rev r0, r1

    mov r1, r2
    bx lr
.size __bswapdi2, . - __bswapdi2
