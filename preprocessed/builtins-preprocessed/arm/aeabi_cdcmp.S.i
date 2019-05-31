# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S" 2
# 27 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S"
        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_cdcmpeq ; .type __aeabi_cdcmpeq,%function ; .hidden __aeabi_cdcmpeq ; .thumb_func ; __aeabi_cdcmpeq:
        push {r0-r3, lr}
        bl __aeabi_cdcmpeq_check_nan
        cmp r0, #1
# 46 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S"
        pop {r0-r3, lr}




        it ne
        bne __aeabi_cdcmple


        mov ip, #(1 << 29)
        msr APSR_nzcvq, ip



        bx lr

.size __aeabi_cdcmpeq, . - __aeabi_cdcmpeq
# 75 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S"
        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_cdcmple ; .type __aeabi_cdcmple,%function ; .hidden __aeabi_cdcmple ; .thumb_func ; __aeabi_cdcmple:


        push {r0-r3, lr}

        bl __aeabi_dcmplt
        cmp r0, #1
# 106 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_cdcmp.S"
        itt eq
        moveq ip, #0
        beq 1f

        ldm sp, {r0-r3}
        bl __aeabi_dcmpeq
        cmp r0, #1
        ite eq
        moveq ip, #((1 << 29) | (1 << 30))
        movne ip, #((1 << 29))

1:



        msr CPSR_f, ip

        pop {r0-r3}
        pop {pc}

.size __aeabi_cdcmple, . - __aeabi_cdcmple





        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_cdrcmple ; .type __aeabi_cdrcmple,%function ; .hidden __aeabi_cdrcmple ; .thumb_func ; __aeabi_cdrcmple:

        mov ip, r0
        mov r0, r2
        mov r2, ip


        mov ip, r1
        mov r1, r3
        mov r3, ip

        b __aeabi_cdcmple
.size __aeabi_cdrcmple, . - __aeabi_cdrcmple
