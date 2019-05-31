# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uldivmod.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uldivmod.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uldivmod.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uldivmod.S" 2
# 23 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uldivmod.S"
        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_uldivmod ; .type __aeabi_uldivmod,%function ; .hidden __aeabi_uldivmod ; .thumb_func ; __aeabi_uldivmod:
        push {r6, lr}
        sub sp, sp, #16
        add r6, sp, #8
        str r6, [sp]
# 38 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uldivmod.S"
        bl __udivmoddi4
        ldr r2, [sp, #8]
        ldr r3, [sp, #12]
        add sp, sp, #16
        pop {r6, pc}
.size __aeabi_uldivmod, . - __aeabi_uldivmod
