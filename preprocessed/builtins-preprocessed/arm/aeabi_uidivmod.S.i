# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uidivmod.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uidivmod.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uidivmod.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uidivmod.S" 2
# 23 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uidivmod.S"
        .syntax unified
        .text
        .thumb ;
        .p2align 2
.thumb ; ; .globl __aeabi_uidivmod ; .type __aeabi_uidivmod,%function ; .hidden __aeabi_uidivmod ; .thumb_func ; __aeabi_uidivmod:
# 42 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_uidivmod.S"
        push { lr }
        sub sp, sp, #4
        mov r2, sp





        bl __udivmodsi4
        ldr r1, [sp]
        add sp, sp, #4
        pop { pc }

.size __aeabi_uidivmod, . - __aeabi_uidivmod
