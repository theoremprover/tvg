# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S" 2
# 40 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 41 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S" 2
 .syntax unified
    .text
    .thumb ;

@ int __eqsf2(float a, float b)

    .p2align 2
.thumb ; ; .globl __eqsf2 ; .type __eqsf2,%function ; .hidden __eqsf2 ; .thumb_func ; __eqsf2:
# 60 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    mov r2, r0, lsl #1
    mov r3, r1, lsl #1
# 73 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    orrs r12, r2, r3, lsr #1
# 83 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    it ne
    eorsne r12, r0, r1
# 96 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    it pl
    subspl r0, r2, r3
# 121 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    it lo
    mvnlo r0, r1, asr #31
# 138 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    it hi
    movhi r0, r1, asr #31
# 148 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    it ne
    orrne r0, r0, #1
# 167 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    cmp r2, #0xff000000
    ite ls
    cmpls r3, #0xff000000
    movhi r0, #1
    bx lr

.size __eqsf2, . - __eqsf2

.globl __lesf2 ; .type __lesf2,%function ; .hidden __lesf2 ; ; .set __lesf2, __eqsf2 ;
.globl __ltsf2 ; .type __ltsf2,%function ; .hidden __ltsf2 ; ; .set __ltsf2, __eqsf2 ;
.globl __nesf2 ; .type __nesf2,%function ; .hidden __nesf2 ; ; .set __nesf2, __eqsf2 ;

@ int __gtsf2(float a, float b)

    .p2align 2
.thumb ; ; .globl __gtsf2 ; .type __gtsf2,%function ; .hidden __gtsf2 ; .thumb_func ; __gtsf2:
# 230 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    mov r2, r0, lsl #1
    mov r3, r1, lsl #1
    orrs r12, r2, r3, lsr #1
    it ne
    eorsne r12, r0, r1
    it pl
    subspl r0, r2, r3
    it lo
    mvnlo r0, r1, asr #31
    it hi
    movhi r0, r1, asr #31
    it ne
    orrne r0, r0, #1
    cmp r2, #0xff000000
    ite ls
    cmpls r3, #0xff000000
    movhi r0, #-1
    bx lr

.size __gtsf2, . - __gtsf2

.globl __gesf2 ; .type __gesf2,%function ; .hidden __gesf2 ; ; .set __gesf2, __gtsf2 ;

@ int __unordsf2(float a, float b)

    .p2align 2
.thumb ; ; .globl __unordsf2 ; .type __unordsf2,%function ; .hidden __unordsf2 ; .thumb_func ; __unordsf2:






    lsls r2, r0, #1
    lsls r3, r1, #1
    movs r0, #0
# 277 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
    cmp r2, #0xff000000
    ite ls
    cmpls r3, #0xff000000
    movhi r0, #1

    bx lr
.size __unordsf2, . - __unordsf2
# 292 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/comparesf2.S"
.globl __aeabi_fcmpun ; .type __aeabi_fcmpun,%function ; .hidden __aeabi_fcmpun ; ; .set __aeabi_fcmpun, __unordsf2 ;
