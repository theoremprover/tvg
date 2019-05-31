# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch16.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch16.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch16.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch16.S" 2
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch16.S"
 .text
 .syntax unified





 .p2align 2
.thumb ; ; .globl __switch16 ; .type __switch16,%function ; .hidden __switch16 ; .thumb_func ; __switch16:
 ldrh ip, [lr, #-1]
 cmp r0, ip
 add r0, lr, r0, lsl #1
 add ip, lr, ip, lsl #1
 ite lo
 ldrshlo r0, [r0, #1]
 ldrshhs r0, [ip, #1]
 add ip, lr, r0, lsl #1
 bx ip
.size __switch16, . - __switch16
