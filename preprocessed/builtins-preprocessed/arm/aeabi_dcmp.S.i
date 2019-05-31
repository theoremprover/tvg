# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_dcmp.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_dcmp.S" 2
# 10 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_dcmp.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 11 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_dcmp.S" 2
# 45 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/aeabi_dcmp.S"
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_dcmpeq ; .type __aeabi_dcmpeq,%function ; .hidden __aeabi_dcmpeq ; .thumb_func ; __aeabi_dcmpeq: push { r4, lr } ; ; bl __eqdf2 ; cmp r0, #0 ; beq 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_dcmpeq, . - __aeabi_dcmpeq
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_dcmplt ; .type __aeabi_dcmplt,%function ; .hidden __aeabi_dcmplt ; .thumb_func ; __aeabi_dcmplt: push { r4, lr } ; ; bl __ltdf2 ; cmp r0, #0 ; blt 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_dcmplt, . - __aeabi_dcmplt
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_dcmple ; .type __aeabi_dcmple,%function ; .hidden __aeabi_dcmple ; .thumb_func ; __aeabi_dcmple: push { r4, lr } ; ; bl __ledf2 ; cmp r0, #0 ; ble 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_dcmple, . - __aeabi_dcmple
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_dcmpge ; .type __aeabi_dcmpge,%function ; .hidden __aeabi_dcmpge ; .thumb_func ; __aeabi_dcmpge: push { r4, lr } ; ; bl __gedf2 ; cmp r0, #0 ; bge 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_dcmpge, . - __aeabi_dcmpge
.syntax unified ; .p2align 2 ; .thumb ; ; .globl __aeabi_dcmpgt ; .type __aeabi_dcmpgt,%function ; .hidden __aeabi_dcmpgt ; .thumb_func ; __aeabi_dcmpgt: push { r4, lr } ; ; bl __gtdf2 ; cmp r0, #0 ; bgt 1f ; movs r0, #0 ; pop { r4, pc } ; 1: ; movs r0, #1 ; pop { r4, pc } ; .size __aeabi_dcmpgt, . - __aeabi_dcmpgt
