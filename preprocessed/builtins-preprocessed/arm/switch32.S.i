# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch32.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch32.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch32.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch32.S" 2
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/switch32.S"
 .text
 .syntax unified





 .p2align 2
.thumb ; ; .globl __switch32 ; .type __switch32,%function ; .hidden __switch32 ; .thumb_func ; __switch32:
 ldr ip, [lr, #-1]
 cmp r0, ip
 add r0, lr, r0, lsl #2
 add ip, lr, ip, lsl #2
 ite lo
 ldrlo r0, [r0, #3]
 ldrhs r0, [ip, #3]
 add ip, lr, r0
 bx ip
.size __switch32, . - __switch32
