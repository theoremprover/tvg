# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_fcmp.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_fcmp.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_fcmp.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_fcmp.S" 2
# 45 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_fcmp.S"
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_fcmpeq ; .type __aeabi_fcmpeq,%function ; .hidden __aeabi_fcmpeq ; .thumb_func ; __aeabi_fcmpeq: push { r4, lr } ; ; bl __eqsf2 ; cmp r0, #0 ; beq 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_fcmpeq, . - __aeabi_fcmpeq
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_fcmplt ; .type __aeabi_fcmplt,%function ; .hidden __aeabi_fcmplt ; .thumb_func ; __aeabi_fcmplt: push { r4, lr } ; ; bl __ltsf2 ; cmp r0, #0 ; blt 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_fcmplt, . - __aeabi_fcmplt
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_fcmple ; .type __aeabi_fcmple,%function ; .hidden __aeabi_fcmple ; .thumb_func ; __aeabi_fcmple: push { r4, lr } ; ; bl __lesf2 ; cmp r0, #0 ; ble 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_fcmple, . - __aeabi_fcmple
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_fcmpge ; .type __aeabi_fcmpge,%function ; .hidden __aeabi_fcmpge ; .thumb_func ; __aeabi_fcmpge: push { r4, lr } ; ; bl __gesf2 ; cmp r0, #0 ; bge 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_fcmpge, . - __aeabi_fcmpge
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_fcmpgt ; .type __aeabi_fcmpgt,%function ; .hidden __aeabi_fcmpgt ; .thumb_func ; __aeabi_fcmpgt: push { r4, lr } ; ; bl __gtsf2 ; cmp r0, #0 ; bgt 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_fcmpgt, . - __aeabi_fcmpgt
