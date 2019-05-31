# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapsi2.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapsi2.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapsi2.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/bswapsi2.S" 2

 .syntax unified
 .text
 .thumb ;






 .p2align 2
.thumb ; ; .globl __bswapsi2 ; .type __bswapsi2,%function ; .hidden __bswapsi2 ; .thumb_func ; __bswapsi2:







    rev r0, r0

    bx lr
.size __bswapsi2, . - __bswapsi2
