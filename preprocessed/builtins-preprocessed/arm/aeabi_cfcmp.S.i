# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S" 2
# 27 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S"
        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_cfcmpeq ; .type __aeabi_cfcmpeq,%function ; .hidden __aeabi_cfcmpeq ; .thumb_func ; __aeabi_cfcmpeq:
        push {r0-r3, lr}
        bl __aeabi_cfcmpeq_check_nan
        cmp r0, #1
# 46 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S"
        pop {r0-r3, lr}




        it ne
        bne __aeabi_cfcmple


        mov ip, #(1 << 29)
        msr APSR_nzcvq, ip



        bx lr

.size __aeabi_cfcmpeq, . - __aeabi_cfcmpeq
# 75 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S"
        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_cfcmple ; .type __aeabi_cfcmple,%function ; .hidden __aeabi_cfcmple ; .thumb_func ; __aeabi_cfcmple:


        push {r0-r3, lr}

        bl __aeabi_fcmplt
        cmp r0, #1
# 106 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cfcmp.S"
        itt eq
        moveq ip, #0
        beq 1f

        ldm sp, {r0-r3}
        bl __aeabi_fcmpeq
        cmp r0, #1
        ite eq
        moveq ip, #((1 << 29) | (1 << 30))
        movne ip, #((1 << 29))

1:



        msr CPSR_f, ip

        pop {r0-r3}
        pop {pc}

.size __aeabi_cfcmple, . - __aeabi_cfcmple





        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_cfrcmple ; .type __aeabi_cfrcmple,%function ; .hidden __aeabi_cfrcmple ; .thumb_func ; __aeabi_cfrcmple:

        mov ip, r0
        mov r0, r1
        mov r1, ip

        b __aeabi_cfcmple
.size __aeabi_cfrcmple, . - __aeabi_cfrcmple
