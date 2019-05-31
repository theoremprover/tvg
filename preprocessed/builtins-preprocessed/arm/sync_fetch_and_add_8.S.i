# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_add_8.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_add_8.S" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_add_8.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync-ops.h" 1
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync-ops.h"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 17 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync-ops.h" 2
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_add_8.S" 2






.p2align 2 ; .thumb ; .syntax unified ; .thumb ; ; .globl __sync_fetch_and_add_8 ; .type __sync_fetch_and_add_8,%function ; .hidden __sync_fetch_and_add_8 ; ; .thumb_func ; __sync_fetch_and_add_8: push {r4, r5, r6, lr} ; dmb ; mov r12, r0 ; .L_tryatomic_add_8: ldrexd r0, r1, [r12] ; adds r4, r0, r2 ; adc r5, r1, r3 ; strexd r6, r4, r5, [r12] ; cmp r6, #0 ; bne .L_tryatomic_add_8 ; dmb ; pop {r4, r5, r6, pc}
