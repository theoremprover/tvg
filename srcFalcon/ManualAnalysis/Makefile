

GCC_VER = 4.3.2
GCC = /home/utils/gcc-${GCC_VER}/bin/gcc

FUNC_FLAGS = -DL_udivdi3 -DL_divdi3 -DL_umoddi3 -DL_moddi3
#FUNC_FLAGS = -DL_udivmoddi4

CFLAGS = -m32 -O2 -std=gnu99 -I . $(FUNC_FLAGS)
CFLAGS += -DIN_GCC -DCROSS_DIRECTORY_STRUCTURE -DIN_LIBGCC2 -D__GCC_FLOAT_NOT_NEEDED -Dinhibit_libc -DHAVE_CC_TLS 

# for debug macros
CFLAGS += -g3 -dM -save-temps

LDFLAGS = -Wl,-M -Wl,-Map=link.log

FILES = main.c libgcc2.c

build:
	$(GCC) $(CFLAGS) $(FILES) $(LDFLAGS) -o a.out
