# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_or_4.S"
# 1 "<built-in>" 1
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_or_4.S" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_or_4.S"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync-ops.h" 1
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync-ops.h"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/../assembly.h" 1
# 17 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync-ops.h" 2
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/arm/sync_fetch_and_or_4.S" 2



.p2align 2 ; .thumb ; .syntax unified ; .thumb ; ; .globl __sync_fetch_and_or_4 ; .type __sync_fetch_and_or_4,%function ; .hidden __sync_fetch_and_or_4 ; ; .thumb_func ; __sync_fetch_and_or_4: dmb ; mov r12, r0 ; .L_tryatomic_or_4: ldrex r0, [r12] ; orr r2, r0, r1 ; strex r3, r2, [r12] ; cmp r3, #0 ; bne .L_tryatomic_or_4 ; dmb ; bx lr
