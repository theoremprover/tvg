# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memset.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memset.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memset.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memset.S" 2




        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_memset ; .type __aeabi_memset,%function ; .hidden __aeabi_memset ; .thumb_func ; __aeabi_memset:
        mov r3, r1
        mov r1, r2
        mov r2, r3





        b memset

.size __aeabi_memset, . - __aeabi_memset

.globl __aeabi_memset4 ; .type __aeabi_memset4,%function ; .hidden __aeabi_memset4 ; ; .set __aeabi_memset4, __aeabi_memset ;
.globl __aeabi_memset8 ; .type __aeabi_memset8,%function ; .hidden __aeabi_memset8 ; ; .set __aeabi_memset8, __aeabi_memset ;

        .p2align 2
.thumb ; ; .globl __aeabi_memclr ; .type __aeabi_memclr,%function ; .hidden __aeabi_memclr ; .thumb_func ; __aeabi_memclr:
        mov r2, r1
        movs r1, #0





        b memset

.size __aeabi_memclr, . - __aeabi_memclr

.globl __aeabi_memclr4 ; .type __aeabi_memclr4,%function ; .hidden __aeabi_memclr4 ; ; .set __aeabi_memclr4, __aeabi_memclr ;
.globl __aeabi_memclr8 ; .type __aeabi_memclr8,%function ; .hidden __aeabi_memclr8 ; ; .set __aeabi_memclr8, __aeabi_memclr ;
