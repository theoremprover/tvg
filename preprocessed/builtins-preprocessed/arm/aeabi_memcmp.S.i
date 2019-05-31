# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memcmp.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memcmp.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memcmp.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_memcmp.S" 2



        .syntax unified
        .p2align 2
.thumb ; ; .globl __aeabi_memcmp ; .type __aeabi_memcmp,%function ; .hidden __aeabi_memcmp ; .thumb_func ; __aeabi_memcmp:





        b memcmp

.size __aeabi_memcmp, . - __aeabi_memcmp

.globl __aeabi_memcmp4 ; .type __aeabi_memcmp4,%function ; .hidden __aeabi_memcmp4 ; ; .set __aeabi_memcmp4, __aeabi_memcmp ;
.globl __aeabi_memcmp8 ; .type __aeabi_memcmp8,%function ; .hidden __aeabi_memcmp8 ; ; .set __aeabi_memcmp8, __aeabi_memcmp ;
