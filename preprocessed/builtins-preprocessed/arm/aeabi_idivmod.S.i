# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_idivmod.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_idivmod.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_idivmod.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_idivmod.S" 2
# 22 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_idivmod.S"
        .syntax unified
        .text
        .thumb ;
        .p2align 2
.thumb ; ; .globl __aeabi_idivmod ; .type __aeabi_idivmod,%function ; .hidden __aeabi_idivmod ; .thumb_func ; __aeabi_idivmod:
# 35 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_idivmod.S"
        push { lr }
        sub sp, sp, #4
        mov r2, sp





        bl __divmodsi4
        ldr r1, [sp]
        add sp, sp, #4
        pop { pc }

.size __aeabi_idivmod, . - __aeabi_idivmod
