# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_ldivmod.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_ldivmod.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_ldivmod.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_ldivmod.S" 2
# 23 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_ldivmod.S"
        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_ldivmod ; .type __aeabi_ldivmod,%function ; .hidden __aeabi_ldivmod ; .thumb_func ; __aeabi_ldivmod:
        push {r6, lr}
        sub sp, sp, #16
        add r6, sp, #8
        str r6, [sp]
# 38 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_ldivmod.S"
        bl __divmoddi4
        ldr r2, [sp, #8]
        ldr r3, [sp, #12]
        add sp, sp, #16
        pop {r6, pc}
.size __aeabi_ldivmod, . - __aeabi_ldivmod
