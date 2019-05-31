# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch8.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch8.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch8.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch8.S" 2
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch8.S"
 .text
 .syntax unified





 .p2align 2
.thumb ; ; .globl __switch8 ; .type __switch8,%function ; .hidden __switch8 ; .thumb_func ; __switch8:
 ldrb ip, [lr, #-1]
 cmp r0, ip
 ite lo
 ldrsblo r0, [lr, r0]
 ldrsbhs r0, [lr, ip]
 add ip, lr, r0, lsl #1
 bx ip
.size __switch8, . - __switch8
