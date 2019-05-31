# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switchu8.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switchu8.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switchu8.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switchu8.S" 2
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switchu8.S"
 .text
 .syntax unified





 .p2align 2
.thumb ; ; .globl __switchu8 ; .type __switchu8,%function ; .hidden __switchu8 ; .thumb_func ; __switchu8:
 ldrb ip, [lr, #-1]
 cmp r0, ip
 ite lo
 ldrblo r0, [lr, r0]
 ldrbhs r0, [lr, ip]
 add ip, lr, r0, lsl #1
 bx ip
.size __switchu8, . - __switchu8
