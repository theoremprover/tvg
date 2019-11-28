# 1 "chroot_canon.c"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/git/elf//"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/build-x86_64-poky-linux/libc-modules.h" 1
# 1 "<command-line>" 2
# 1 "./../include/libc-symbols.h" 1
# 85 "./../include/libc-symbols.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/build-x86_64-poky-linux/config.h" 1
# 86 "./../include/libc-symbols.h" 2
# 847 "./../include/libc-symbols.h"
# 1 "../sysdeps/generic/symbol-hacks.h" 1
# 848 "./../include/libc-symbols.h" 2
# 1 "<command-line>" 2
# 1 "chroot_canon.c"
# 18 "chroot_canon.c"
# 1 "../include/stdlib.h" 1



# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 151 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 3 4

# 151 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 3 4
typedef long int ptrdiff_t;
# 221 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 3 4
typedef long unsigned int size_t;
# 336 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 3 4
typedef int wchar_t;
# 435 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 3 4
typedef struct {
  long long __max_align_ll __attribute__((__aligned__(__alignof__(long long))));
  long double __max_align_ld __attribute__((__aligned__(__alignof__(long double))));
# 446 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 3 4
} max_align_t;
# 5 "../include/stdlib.h" 2

# 1 "../stdlib/stdlib.h" 1
# 25 "../stdlib/stdlib.h"
# 1 "../bits/libc-header-start.h" 1
# 33 "../bits/libc-header-start.h"
# 1 "../include/features.h" 1
# 406 "../include/features.h"
# 1 "../include/stdc-predef.h" 1
# 407 "../include/features.h" 2
# 428 "../include/features.h"
# 1 "../include/sys/cdefs.h" 1


# 1 "../misc/sys/cdefs.h" 1
# 442 "../misc/sys/cdefs.h"
# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 443 "../misc/sys/cdefs.h" 2
# 1 "../sysdeps/ieee754/ldbl-96/bits/long-double.h" 1
# 444 "../misc/sys/cdefs.h" 2
# 4 "../include/sys/cdefs.h" 2
# 12 "../include/sys/cdefs.h"

# 12 "../include/sys/cdefs.h"
extern void __chk_fail (void) __attribute__ ((__noreturn__));


# 429 "../include/features.h" 2
# 452 "../include/features.h"
# 1 "../include/gnu/stubs.h" 1
# 453 "../include/features.h" 2
# 34 "../bits/libc-header-start.h" 2
# 26 "../stdlib/stdlib.h" 2





# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 32 "../stdlib/stdlib.h" 2







# 1 "../sysdeps/unix/sysv/linux/bits/waitflags.h" 1
# 40 "../stdlib/stdlib.h" 2
# 1 "../bits/waitstatus.h" 1
# 41 "../stdlib/stdlib.h" 2
# 55 "../stdlib/stdlib.h"
# 1 "../sysdeps/x86/bits/floatn.h" 1
# 119 "../sysdeps/x86/bits/floatn.h"
# 1 "../bits/floatn-common.h" 1
# 24 "../bits/floatn-common.h"
# 1 "../sysdeps/ieee754/ldbl-96/bits/long-double.h" 1
# 25 "../bits/floatn-common.h" 2
# 120 "../sysdeps/x86/bits/floatn.h" 2
# 56 "../stdlib/stdlib.h" 2


typedef struct
  {
    int quot;
    int rem;
  } div_t;



typedef struct
  {
    long int quot;
    long int rem;
  } ldiv_t;





__extension__ typedef struct
  {
    long long int quot;
    long long int rem;
  } lldiv_t;
# 97 "../stdlib/stdlib.h"
extern size_t __ctype_get_mb_cur_max (void) __attribute__ ((__nothrow__ )) ;



extern double atof (const char *__nptr)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;

extern int atoi (const char *__nptr)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;

extern long int atol (const char *__nptr)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;



__extension__ extern long long int atoll (const char *__nptr)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;



extern double strtod (const char *__restrict __nptr,
        char **__restrict __endptr)
     __attribute__ ((__nothrow__ )) ;



extern float strtof (const char *__restrict __nptr,
       char **__restrict __endptr) __attribute__ ((__nothrow__ )) ;

extern long double strtold (const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__ )) ;
# 140 "../stdlib/stdlib.h"
extern _Float32 strtof32 (const char *__restrict __nptr,
     char **__restrict __endptr)
     __attribute__ ((__nothrow__ )) ;



extern _Float64 strtof64 (const char *__restrict __nptr,
     char **__restrict __endptr)
     __attribute__ ((__nothrow__ )) ;



extern _Float128 strtof128 (const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__ )) ;



extern _Float32x strtof32x (const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__ )) ;



extern _Float64x strtof64x (const char *__restrict __nptr,
       char **__restrict __endptr)
     __attribute__ ((__nothrow__ )) ;
# 176 "../stdlib/stdlib.h"
extern long int strtol (const char *__restrict __nptr,
   char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ )) ;

extern unsigned long int strtoul (const char *__restrict __nptr,
      char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ )) ;



__extension__
extern long long int strtoq (const char *__restrict __nptr,
        char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ )) ;

__extension__
extern unsigned long long int strtouq (const char *__restrict __nptr,
           char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ )) ;




__extension__
extern long long int strtoll (const char *__restrict __nptr,
         char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ )) ;

__extension__
extern unsigned long long int strtoull (const char *__restrict __nptr,
     char **__restrict __endptr, int __base)
     __attribute__ ((__nothrow__ )) ;




extern int strfromd (char *__dest, size_t __size, const char *__format,
       double __f)
     __attribute__ ((__nothrow__ )) ;

extern int strfromf (char *__dest, size_t __size, const char *__format,
       float __f)
     __attribute__ ((__nothrow__ )) ;

extern int strfroml (char *__dest, size_t __size, const char *__format,
       long double __f)
     __attribute__ ((__nothrow__ )) ;
# 232 "../stdlib/stdlib.h"
extern int strfromf32 (char *__dest, size_t __size, const char * __format,
         _Float32 __f)
     __attribute__ ((__nothrow__ )) ;



extern int strfromf64 (char *__dest, size_t __size, const char * __format,
         _Float64 __f)
     __attribute__ ((__nothrow__ )) ;



extern int strfromf128 (char *__dest, size_t __size, const char * __format,
   _Float128 __f)
     __attribute__ ((__nothrow__ )) ;



extern int strfromf32x (char *__dest, size_t __size, const char * __format,
   _Float32x __f)
     __attribute__ ((__nothrow__ )) ;



extern int strfromf64x (char *__dest, size_t __size, const char * __format,
   _Float64x __f)
     __attribute__ ((__nothrow__ )) ;
# 272 "../stdlib/stdlib.h"
# 1 "../include/bits/types/locale_t.h" 1
# 1 "../locale/bits/types/locale_t.h" 1
# 22 "../locale/bits/types/locale_t.h"
# 1 "../include/bits/types/__locale_t.h" 1
# 1 "../locale/bits/types/__locale_t.h" 1
# 28 "../locale/bits/types/__locale_t.h"
struct __locale_struct
{

  struct __locale_data *__locales[13];


  const unsigned short int *__ctype_b;
  const int *__ctype_tolower;
  const int *__ctype_toupper;


  const char *__names[13];
};

typedef struct __locale_struct *__locale_t;
# 1 "../include/bits/types/__locale_t.h" 2
# 23 "../locale/bits/types/locale_t.h" 2

typedef __locale_t locale_t;
# 1 "../include/bits/types/locale_t.h" 2
# 273 "../stdlib/stdlib.h" 2

extern long int strtol_l (const char *__restrict __nptr,
     char **__restrict __endptr, int __base,
     locale_t __loc) __attribute__ ((__nothrow__ )) ;

extern unsigned long int strtoul_l (const char *__restrict __nptr,
        char **__restrict __endptr,
        int __base, locale_t __loc)
     __attribute__ ((__nothrow__ )) ;

__extension__
extern long long int strtoll_l (const char *__restrict __nptr,
    char **__restrict __endptr, int __base,
    locale_t __loc)
     __attribute__ ((__nothrow__ )) ;

__extension__
extern unsigned long long int strtoull_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       int __base, locale_t __loc)
     __attribute__ ((__nothrow__ )) ;

extern double strtod_l (const char *__restrict __nptr,
   char **__restrict __endptr, locale_t __loc)
     __attribute__ ((__nothrow__ )) ;

extern float strtof_l (const char *__restrict __nptr,
         char **__restrict __endptr, locale_t __loc)
     __attribute__ ((__nothrow__ )) ;

extern long double strtold_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     __attribute__ ((__nothrow__ )) ;
# 316 "../stdlib/stdlib.h"
extern _Float32 strtof32_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       locale_t __loc)
     __attribute__ ((__nothrow__ )) ;



extern _Float64 strtof64_l (const char *__restrict __nptr,
       char **__restrict __endptr,
       locale_t __loc)
     __attribute__ ((__nothrow__ )) ;



extern _Float128 strtof128_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     __attribute__ ((__nothrow__ )) ;



extern _Float32x strtof32x_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     __attribute__ ((__nothrow__ )) ;



extern _Float64x strtof64x_l (const char *__restrict __nptr,
         char **__restrict __endptr,
         locale_t __loc)
     __attribute__ ((__nothrow__ )) ;
# 360 "../stdlib/stdlib.h"
extern __inline int
__attribute__ ((__nothrow__ )) atoi (const char *__nptr)
{
  return (int) strtol (__nptr, (char **) 
# 363 "../stdlib/stdlib.h" 3 4
                                        ((void *)0)
# 363 "../stdlib/stdlib.h"
                                            , 10);
}
extern __inline long int
__attribute__ ((__nothrow__ )) atol (const char *__nptr)
{
  return strtol (__nptr, (char **) 
# 368 "../stdlib/stdlib.h" 3 4
                                  ((void *)0)
# 368 "../stdlib/stdlib.h"
                                      , 10);
}


__extension__ extern __inline long long int
__attribute__ ((__nothrow__ )) atoll (const char *__nptr)
{
  return strtoll (__nptr, (char **) 
# 375 "../stdlib/stdlib.h" 3 4
                                   ((void *)0)
# 375 "../stdlib/stdlib.h"
                                       , 10);
}
# 385 "../stdlib/stdlib.h"
extern char *l64a (long int __n) __attribute__ ((__nothrow__ )) ;


extern long int a64l (const char *__s)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;




# 1 "../include/sys/types.h" 1
# 1 "../posix/sys/types.h" 1
# 27 "../posix/sys/types.h"


# 1 "../include/bits/types.h" 1
# 1 "../posix/bits/types.h" 1
# 27 "../posix/bits/types.h"
# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 28 "../posix/bits/types.h" 2


typedef unsigned char __u_char;
typedef unsigned short int __u_short;
typedef unsigned int __u_int;
typedef unsigned long int __u_long;


typedef signed char __int8_t;
typedef unsigned char __uint8_t;
typedef signed short int __int16_t;
typedef unsigned short int __uint16_t;
typedef signed int __int32_t;
typedef unsigned int __uint32_t;

typedef signed long int __int64_t;
typedef unsigned long int __uint64_t;






typedef __int8_t __int_least8_t;
typedef __uint8_t __uint_least8_t;
typedef __int16_t __int_least16_t;
typedef __uint16_t __uint_least16_t;
typedef __int32_t __int_least32_t;
typedef __uint32_t __uint_least32_t;
typedef __int64_t __int_least64_t;
typedef __uint64_t __uint_least64_t;



typedef long int __quad_t;
typedef unsigned long int __u_quad_t;







typedef long int __intmax_t;
typedef unsigned long int __uintmax_t;
# 140 "../posix/bits/types.h"
# 1 "../sysdeps/unix/sysv/linux/x86/bits/typesizes.h" 1
# 141 "../posix/bits/types.h" 2


typedef unsigned long int __dev_t;
typedef unsigned int __uid_t;
typedef unsigned int __gid_t;
typedef unsigned long int __ino_t;
typedef unsigned long int __ino64_t;
typedef unsigned int __mode_t;
typedef unsigned long int __nlink_t;
typedef long int __off_t;
typedef long int __off64_t;
typedef int __pid_t;
typedef struct { int __val[2]; } __fsid_t;
typedef long int __clock_t;
typedef unsigned long int __rlim_t;
typedef unsigned long int __rlim64_t;
typedef unsigned int __id_t;
typedef long int __time_t;
typedef unsigned int __useconds_t;
typedef long int __suseconds_t;

typedef int __daddr_t;
typedef int __key_t;


typedef int __clockid_t;


typedef void * __timer_t;


typedef long int __blksize_t;




typedef long int __blkcnt_t;
typedef long int __blkcnt64_t;


typedef unsigned long int __fsblkcnt_t;
typedef unsigned long int __fsblkcnt64_t;


typedef unsigned long int __fsfilcnt_t;
typedef unsigned long int __fsfilcnt64_t;


typedef long int __fsword_t;

typedef long int __ssize_t;


typedef long int __syscall_slong_t;

typedef unsigned long int __syscall_ulong_t;



typedef __off64_t __loff_t;
typedef char *__caddr_t;


typedef long int __intptr_t;


typedef unsigned int __socklen_t;




typedef int __sig_atomic_t;
# 1 "../include/bits/types.h" 2
# 30 "../posix/sys/types.h" 2



typedef __u_char u_char;
typedef __u_short u_short;
typedef __u_int u_int;
typedef __u_long u_long;
typedef __quad_t quad_t;
typedef __u_quad_t u_quad_t;
typedef __fsid_t fsid_t;


typedef __loff_t loff_t;




typedef __ino_t ino_t;






typedef __ino64_t ino64_t;




typedef __dev_t dev_t;




typedef __gid_t gid_t;




typedef __mode_t mode_t;




typedef __nlink_t nlink_t;




typedef __uid_t uid_t;





typedef __off_t off_t;






typedef __off64_t off64_t;




typedef __pid_t pid_t;





typedef __id_t id_t;




typedef __ssize_t ssize_t;





typedef __daddr_t daddr_t;
typedef __caddr_t caddr_t;





typedef __key_t key_t;




# 1 "../include/bits/types/clock_t.h" 1
# 1 "../time/bits/types/clock_t.h" 1



# 1 "../include/bits/types.h" 1
# 5 "../time/bits/types/clock_t.h" 2


typedef __clock_t clock_t;
# 1 "../include/bits/types/clock_t.h" 2
# 127 "../posix/sys/types.h" 2

# 1 "../include/bits/types/clockid_t.h" 1
# 1 "../time/bits/types/clockid_t.h" 1



# 1 "../include/bits/types.h" 1
# 5 "../time/bits/types/clockid_t.h" 2


typedef __clockid_t clockid_t;
# 1 "../include/bits/types/clockid_t.h" 2
# 129 "../posix/sys/types.h" 2
# 1 "../include/bits/types/time_t.h" 1
# 1 "../time/bits/types/time_t.h" 1



# 1 "../include/bits/types.h" 1
# 5 "../time/bits/types/time_t.h" 2


typedef __time_t time_t;
# 1 "../include/bits/types/time_t.h" 2
# 130 "../posix/sys/types.h" 2
# 1 "../include/bits/types/timer_t.h" 1
# 1 "../time/bits/types/timer_t.h" 1



# 1 "../include/bits/types.h" 1
# 5 "../time/bits/types/timer_t.h" 2


typedef __timer_t timer_t;
# 1 "../include/bits/types/timer_t.h" 2
# 131 "../posix/sys/types.h" 2



typedef __useconds_t useconds_t;



typedef __suseconds_t suseconds_t;





# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 145 "../posix/sys/types.h" 2



typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;




# 1 "../bits/stdint-intn.h" 1
# 22 "../bits/stdint-intn.h"
# 1 "../include/bits/types.h" 1
# 23 "../bits/stdint-intn.h" 2

typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;
# 156 "../posix/sys/types.h" 2
# 177 "../posix/sys/types.h"
typedef unsigned int u_int8_t __attribute__ ((__mode__ (__QI__)));
typedef unsigned int u_int16_t __attribute__ ((__mode__ (__HI__)));
typedef unsigned int u_int32_t __attribute__ ((__mode__ (__SI__)));
typedef unsigned int u_int64_t __attribute__ ((__mode__ (__DI__)));

typedef int register_t __attribute__ ((__mode__ (__word__)));
# 193 "../posix/sys/types.h"
# 1 "../include/endian.h" 1
# 1 "../string/endian.h" 1
# 36 "../string/endian.h"
# 1 "../sysdeps/x86/bits/endian.h" 1
# 37 "../string/endian.h" 2
# 60 "../string/endian.h"
# 1 "../bits/byteswap.h" 1
# 27 "../bits/byteswap.h"
# 1 "../include/bits/types.h" 1
# 28 "../bits/byteswap.h" 2





static __inline __uint16_t
__bswap_16 (__uint16_t __bsx)
{

  return __builtin_bswap16 (__bsx);



}






static __inline __uint32_t
__bswap_32 (__uint32_t __bsx)
{

  return __builtin_bswap32 (__bsx);



}
# 69 "../bits/byteswap.h"
__extension__ static __inline __uint64_t
__bswap_64 (__uint64_t __bsx)
{

  return __builtin_bswap64 (__bsx);



}
# 61 "../string/endian.h" 2
# 1 "../bits/uintn-identity.h" 1
# 26 "../bits/uintn-identity.h"
# 1 "../include/bits/types.h" 1
# 27 "../bits/uintn-identity.h" 2





static __inline __uint16_t
__uint16_identity (__uint16_t __x)
{
  return __x;
}

static __inline __uint32_t
__uint32_identity (__uint32_t __x)
{
  return __x;
}

static __inline __uint64_t
__uint64_identity (__uint64_t __x)
{
  return __x;
}
# 62 "../string/endian.h" 2
# 2 "../include/endian.h" 2
# 194 "../posix/sys/types.h" 2


# 1 "../include/sys/select.h" 1

# 1 "../misc/sys/select.h" 1
# 27 "../misc/sys/select.h"
# 1 "../include/bits/types.h" 1
# 28 "../misc/sys/select.h" 2


# 1 "../sysdeps/x86/bits/select.h" 1
# 22 "../sysdeps/x86/bits/select.h"
# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 23 "../sysdeps/x86/bits/select.h" 2
# 31 "../misc/sys/select.h" 2


# 1 "../include/bits/types/sigset_t.h" 1
# 1 "../signal/bits/types/sigset_t.h" 1



# 1 "../sysdeps/unix/sysv/linux/bits/types/__sigset_t.h" 1




typedef struct
{
  unsigned long int __val[(1024 / (8 * sizeof (unsigned long int)))];
} __sigset_t;
# 5 "../signal/bits/types/sigset_t.h" 2


typedef __sigset_t sigset_t;
# 1 "../include/bits/types/sigset_t.h" 2
# 34 "../misc/sys/select.h" 2


# 1 "../include/bits/types/time_t.h" 1
# 37 "../misc/sys/select.h" 2
# 1 "../include/bits/types/struct_timeval.h" 1
# 1 "../time/bits/types/struct_timeval.h" 1



# 1 "../include/bits/types.h" 1
# 5 "../time/bits/types/struct_timeval.h" 2



struct timeval
{
  __time_t tv_sec;
  __suseconds_t tv_usec;
};
# 1 "../include/bits/types/struct_timeval.h" 2
# 38 "../misc/sys/select.h" 2

# 1 "../include/bits/types/struct_timespec.h" 1
# 1 "../time/bits/types/struct_timespec.h" 1




# 1 "../include/bits/types.h" 1
# 6 "../time/bits/types/struct_timespec.h" 2



struct timespec
{
  __time_t tv_sec;
  __syscall_slong_t tv_nsec;
};
# 1 "../include/bits/types/struct_timespec.h" 2
# 40 "../misc/sys/select.h" 2
# 49 "../misc/sys/select.h"
typedef long int __fd_mask;
# 59 "../misc/sys/select.h"
typedef struct
  {



    __fd_mask fds_bits[1024 / (8 * (int) sizeof (__fd_mask))];





  } fd_set;






typedef __fd_mask fd_mask;
# 91 "../misc/sys/select.h"

# 101 "../misc/sys/select.h"
extern int select (int __nfds, fd_set *__restrict __readfds,
     fd_set *__restrict __writefds,
     fd_set *__restrict __exceptfds,
     struct timeval *__restrict __timeout);
# 113 "../misc/sys/select.h"
extern int pselect (int __nfds, fd_set *__restrict __readfds,
      fd_set *__restrict __writefds,
      fd_set *__restrict __exceptfds,
      const struct timespec *__restrict __timeout,
      const __sigset_t *__restrict __sigmask);
# 126 "../misc/sys/select.h"

# 3 "../include/sys/select.h" 2



extern int __pselect (int __nfds, fd_set *__readfds,
        fd_set *__writefds, fd_set *__exceptfds,
        const struct timespec *__timeout,
        const __sigset_t *__sigmask);

extern int __select (int __nfds, fd_set *__restrict __readfds,
       fd_set *__restrict __writefds,
       fd_set *__restrict __exceptfds,
       struct timeval *__restrict __timeout);

# 197 "../posix/sys/types.h" 2





typedef __blksize_t blksize_t;






typedef __blkcnt_t blkcnt_t;



typedef __fsblkcnt_t fsblkcnt_t;



typedef __fsfilcnt_t fsfilcnt_t;
# 236 "../posix/sys/types.h"
typedef __blkcnt64_t blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;





# 1 "../sysdeps/nptl/bits/pthreadtypes.h" 1
# 23 "../sysdeps/nptl/bits/pthreadtypes.h"
# 1 "../sysdeps/nptl/bits/thread-shared-types.h" 1
# 77 "../sysdeps/nptl/bits/thread-shared-types.h"
# 1 "../sysdeps/x86/nptl/bits/pthreadtypes-arch.h" 1
# 21 "../sysdeps/x86/nptl/bits/pthreadtypes-arch.h"
# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 22 "../sysdeps/x86/nptl/bits/pthreadtypes-arch.h" 2
# 65 "../sysdeps/x86/nptl/bits/pthreadtypes-arch.h"
struct __pthread_rwlock_arch_t
{
  unsigned int __readers;
  unsigned int __writers;
  unsigned int __wrphase_futex;
  unsigned int __writers_futex;
  unsigned int __pad3;
  unsigned int __pad4;

  int __cur_writer;
  int __shared;
  signed char __rwelision;




  unsigned char __pad1[7];


  unsigned long int __pad2;


  unsigned int __flags;
# 99 "../sysdeps/x86/nptl/bits/pthreadtypes-arch.h"
};
# 78 "../sysdeps/nptl/bits/thread-shared-types.h" 2




typedef struct __pthread_internal_list
{
  struct __pthread_internal_list *__prev;
  struct __pthread_internal_list *__next;
} __pthread_list_t;
# 118 "../sysdeps/nptl/bits/thread-shared-types.h"
struct __pthread_mutex_s
{
  int __lock ;
  unsigned int __count;
  int __owner;

  unsigned int __nusers;
# 148 "../sysdeps/nptl/bits/thread-shared-types.h"
  int __kind;
 




  short __spins; short __elision;
  __pthread_list_t __list;
# 165 "../sysdeps/nptl/bits/thread-shared-types.h"
 
};




struct __pthread_cond_s
{
  __extension__ union
  {
    __extension__ unsigned long long int __wseq;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __wseq32;
  };
  __extension__ union
  {
    __extension__ unsigned long long int __g1_start;
    struct
    {
      unsigned int __low;
      unsigned int __high;
    } __g1_start32;
  };
  unsigned int __g_refs[2] ;
  unsigned int __g_size[2];
  unsigned int __g1_orig_size;
  unsigned int __wrefs;
  unsigned int __g_signals[2];
};
# 24 "../sysdeps/nptl/bits/pthreadtypes.h" 2



typedef unsigned long int pthread_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_mutexattr_t;




typedef union
{
  char __size[4];
  int __align;
} pthread_condattr_t;



typedef unsigned int pthread_key_t;



typedef int pthread_once_t;


union pthread_attr_t
{
  char __size[56];
  long int __align;
};

typedef union pthread_attr_t pthread_attr_t;




typedef union
{
  struct __pthread_mutex_s __data;
  char __size[40];
  long int __align;
} pthread_mutex_t;


typedef union
{
  struct __pthread_cond_s __data;
  char __size[48];
  __extension__ long long int __align;
} pthread_cond_t;





typedef union
{
  struct __pthread_rwlock_arch_t __data;
  char __size[56];
  long int __align;
} pthread_rwlock_t;

typedef union
{
  char __size[8];
  long int __align;
} pthread_rwlockattr_t;





typedef volatile int pthread_spinlock_t;




typedef union
{
  char __size[32];
  long int __align;
} pthread_barrier_t;

typedef union
{
  char __size[4];
  int __align;
} pthread_barrierattr_t;
# 245 "../posix/sys/types.h" 2



# 1 "../include/sys/types.h" 2
# 395 "../stdlib/stdlib.h" 2






extern long int random (void) __attribute__ ((__nothrow__ ));


extern void srandom (unsigned int __seed) __attribute__ ((__nothrow__ ));





extern char *initstate (unsigned int __seed, char *__statebuf,
   size_t __statelen) __attribute__ ((__nothrow__ )) ;



extern char *setstate (char *__statebuf) __attribute__ ((__nothrow__ )) ;







struct random_data
  {
    int32_t *fptr;
    int32_t *rptr;
    int32_t *state;
    int rand_type;
    int rand_deg;
    int rand_sep;
    int32_t *end_ptr;
  };

extern int random_r (struct random_data *__restrict __buf,
       int32_t *__restrict __result) __attribute__ ((__nothrow__ )) ;

extern int srandom_r (unsigned int __seed, struct random_data *__buf)
     __attribute__ ((__nothrow__ )) ;

extern int initstate_r (unsigned int __seed, char *__restrict __statebuf,
   size_t __statelen,
   struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ )) ;

extern int setstate_r (char *__restrict __statebuf,
         struct random_data *__restrict __buf)
     __attribute__ ((__nothrow__ )) ;





extern int rand (void) __attribute__ ((__nothrow__ ));

extern void srand (unsigned int __seed) __attribute__ ((__nothrow__ ));



extern int rand_r (unsigned int *__seed) __attribute__ ((__nothrow__ ));







extern double drand48 (void) __attribute__ ((__nothrow__ ));
extern double erand48 (unsigned short int __xsubi[3]) __attribute__ ((__nothrow__ )) ;


extern long int lrand48 (void) __attribute__ ((__nothrow__ ));
extern long int nrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ )) ;


extern long int mrand48 (void) __attribute__ ((__nothrow__ ));
extern long int jrand48 (unsigned short int __xsubi[3])
     __attribute__ ((__nothrow__ )) ;


extern void srand48 (long int __seedval) __attribute__ ((__nothrow__ ));
extern unsigned short int *seed48 (unsigned short int __seed16v[3])
     __attribute__ ((__nothrow__ )) ;
extern void lcong48 (unsigned short int __param[7]) __attribute__ ((__nothrow__ )) ;





struct drand48_data
  {
    unsigned short int __x[3];
    unsigned short int __old_x[3];
    unsigned short int __c;
    unsigned short int __init;
    __extension__ unsigned long long int __a;

  };


extern int drand48_r (struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ )) ;
extern int erand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        double *__restrict __result) __attribute__ ((__nothrow__ )) ;


extern int lrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ )) ;
extern int nrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ )) ;


extern int mrand48_r (struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ )) ;
extern int jrand48_r (unsigned short int __xsubi[3],
        struct drand48_data *__restrict __buffer,
        long int *__restrict __result)
     __attribute__ ((__nothrow__ )) ;


extern int srand48_r (long int __seedval, struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ )) ;

extern int seed48_r (unsigned short int __seed16v[3],
       struct drand48_data *__buffer) __attribute__ ((__nothrow__ )) ;

extern int lcong48_r (unsigned short int __param[7],
        struct drand48_data *__buffer)
     __attribute__ ((__nothrow__ )) ;




extern void *malloc (size_t __size) __attribute__ ((__nothrow__ )) __attribute__ ((__malloc__)) ;

extern void *calloc (size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__ )) __attribute__ ((__malloc__)) ;






extern void *realloc (void *__ptr, size_t __size)
     __attribute__ ((__nothrow__ )) __attribute__ ((__warn_unused_result__));







extern void *reallocarray (void *__ptr, size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__ )) __attribute__ ((__warn_unused_result__));



extern void free (void *__ptr) __attribute__ ((__nothrow__ ));


# 1 "../include/alloca.h" 1


# 1 "../stdlib/alloca.h" 1
# 24 "../stdlib/alloca.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 25 "../stdlib/alloca.h" 2







extern void *alloca (size_t __size) __attribute__ ((__nothrow__ ));






# 4 "../include/alloca.h" 2



# 1 "../include/stackinfo.h" 1
# 24 "../include/stackinfo.h"
# 1 "../sysdeps/x86_64/stackinfo.h" 1
# 24 "../sysdeps/x86_64/stackinfo.h"
# 1 "../include/elf.h" 1

# 1 "../elf/elf.h" 1
# 24 "../elf/elf.h"




# 1 "../sysdeps/generic/stdint.h" 1
# 26 "../sysdeps/generic/stdint.h"
# 1 "../bits/libc-header-start.h" 1
# 27 "../sysdeps/generic/stdint.h" 2
# 1 "../include/bits/types.h" 1
# 28 "../sysdeps/generic/stdint.h" 2
# 1 "../bits/wchar.h" 1
# 29 "../sysdeps/generic/stdint.h" 2
# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 30 "../sysdeps/generic/stdint.h" 2







# 1 "../bits/stdint-uintn.h" 1
# 22 "../bits/stdint-uintn.h"
# 1 "../include/bits/types.h" 1
# 23 "../bits/stdint-uintn.h" 2

typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;
# 38 "../sysdeps/generic/stdint.h" 2





typedef __int_least8_t int_least8_t;
typedef __int_least16_t int_least16_t;
typedef __int_least32_t int_least32_t;
typedef __int_least64_t int_least64_t;


typedef __uint_least8_t uint_least8_t;
typedef __uint_least16_t uint_least16_t;
typedef __uint_least32_t uint_least32_t;
typedef __uint_least64_t uint_least64_t;





typedef signed char int_fast8_t;

typedef long int int_fast16_t;
typedef long int int_fast32_t;
typedef long int int_fast64_t;
# 71 "../sysdeps/generic/stdint.h"
typedef unsigned char uint_fast8_t;

typedef unsigned long int uint_fast16_t;
typedef unsigned long int uint_fast32_t;
typedef unsigned long int uint_fast64_t;
# 87 "../sysdeps/generic/stdint.h"
typedef long int intptr_t;


typedef unsigned long int uintptr_t;
# 101 "../sysdeps/generic/stdint.h"
typedef __intmax_t intmax_t;
typedef __uintmax_t uintmax_t;
# 29 "../elf/elf.h" 2


typedef uint16_t Elf32_Half;
typedef uint16_t Elf64_Half;


typedef uint32_t Elf32_Word;
typedef int32_t Elf32_Sword;
typedef uint32_t Elf64_Word;
typedef int32_t Elf64_Sword;


typedef uint64_t Elf32_Xword;
typedef int64_t Elf32_Sxword;
typedef uint64_t Elf64_Xword;
typedef int64_t Elf64_Sxword;


typedef uint32_t Elf32_Addr;
typedef uint64_t Elf64_Addr;


typedef uint32_t Elf32_Off;
typedef uint64_t Elf64_Off;


typedef uint16_t Elf32_Section;
typedef uint16_t Elf64_Section;


typedef Elf32_Half Elf32_Versym;
typedef Elf64_Half Elf64_Versym;






typedef struct
{
  unsigned char e_ident[(16)];
  Elf32_Half e_type;
  Elf32_Half e_machine;
  Elf32_Word e_version;
  Elf32_Addr e_entry;
  Elf32_Off e_phoff;
  Elf32_Off e_shoff;
  Elf32_Word e_flags;
  Elf32_Half e_ehsize;
  Elf32_Half e_phentsize;
  Elf32_Half e_phnum;
  Elf32_Half e_shentsize;
  Elf32_Half e_shnum;
  Elf32_Half e_shstrndx;
} Elf32_Ehdr;

typedef struct
{
  unsigned char e_ident[(16)];
  Elf64_Half e_type;
  Elf64_Half e_machine;
  Elf64_Word e_version;
  Elf64_Addr e_entry;
  Elf64_Off e_phoff;
  Elf64_Off e_shoff;
  Elf64_Word e_flags;
  Elf64_Half e_ehsize;
  Elf64_Half e_phentsize;
  Elf64_Half e_phnum;
  Elf64_Half e_shentsize;
  Elf64_Half e_shnum;
  Elf64_Half e_shstrndx;
} Elf64_Ehdr;
# 384 "../elf/elf.h"
typedef struct
{
  Elf32_Word sh_name;
  Elf32_Word sh_type;
  Elf32_Word sh_flags;
  Elf32_Addr sh_addr;
  Elf32_Off sh_offset;
  Elf32_Word sh_size;
  Elf32_Word sh_link;
  Elf32_Word sh_info;
  Elf32_Word sh_addralign;
  Elf32_Word sh_entsize;
} Elf32_Shdr;

typedef struct
{
  Elf64_Word sh_name;
  Elf64_Word sh_type;
  Elf64_Xword sh_flags;
  Elf64_Addr sh_addr;
  Elf64_Off sh_offset;
  Elf64_Xword sh_size;
  Elf64_Word sh_link;
  Elf64_Word sh_info;
  Elf64_Xword sh_addralign;
  Elf64_Xword sh_entsize;
} Elf64_Shdr;
# 491 "../elf/elf.h"
typedef struct
{
  Elf32_Word ch_type;
  Elf32_Word ch_size;
  Elf32_Word ch_addralign;
} Elf32_Chdr;

typedef struct
{
  Elf64_Word ch_type;
  Elf64_Word ch_reserved;
  Elf64_Xword ch_size;
  Elf64_Xword ch_addralign;
} Elf64_Chdr;
# 518 "../elf/elf.h"
typedef struct
{
  Elf32_Word st_name;
  Elf32_Addr st_value;
  Elf32_Word st_size;
  unsigned char st_info;
  unsigned char st_other;
  Elf32_Section st_shndx;
} Elf32_Sym;

typedef struct
{
  Elf64_Word st_name;
  unsigned char st_info;
  unsigned char st_other;
  Elf64_Section st_shndx;
  Elf64_Addr st_value;
  Elf64_Xword st_size;
} Elf64_Sym;




typedef struct
{
  Elf32_Half si_boundto;
  Elf32_Half si_flags;
} Elf32_Syminfo;

typedef struct
{
  Elf64_Half si_boundto;
  Elf64_Half si_flags;
} Elf64_Syminfo;
# 633 "../elf/elf.h"
typedef struct
{
  Elf32_Addr r_offset;
  Elf32_Word r_info;
} Elf32_Rel;






typedef struct
{
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
} Elf64_Rel;



typedef struct
{
  Elf32_Addr r_offset;
  Elf32_Word r_info;
  Elf32_Sword r_addend;
} Elf32_Rela;

typedef struct
{
  Elf64_Addr r_offset;
  Elf64_Xword r_info;
  Elf64_Sxword r_addend;
} Elf64_Rela;
# 678 "../elf/elf.h"
typedef struct
{
  Elf32_Word p_type;
  Elf32_Off p_offset;
  Elf32_Addr p_vaddr;
  Elf32_Addr p_paddr;
  Elf32_Word p_filesz;
  Elf32_Word p_memsz;
  Elf32_Word p_flags;
  Elf32_Word p_align;
} Elf32_Phdr;

typedef struct
{
  Elf64_Word p_type;
  Elf64_Word p_flags;
  Elf64_Off p_offset;
  Elf64_Addr p_vaddr;
  Elf64_Addr p_paddr;
  Elf64_Xword p_filesz;
  Elf64_Xword p_memsz;
  Elf64_Xword p_align;
} Elf64_Phdr;
# 819 "../elf/elf.h"
typedef struct
{
  Elf32_Sword d_tag;
  union
    {
      Elf32_Word d_val;
      Elf32_Addr d_ptr;
    } d_un;
} Elf32_Dyn;

typedef struct
{
  Elf64_Sxword d_tag;
  union
    {
      Elf64_Xword d_val;
      Elf64_Addr d_ptr;
    } d_un;
} Elf64_Dyn;
# 998 "../elf/elf.h"
typedef struct
{
  Elf32_Half vd_version;
  Elf32_Half vd_flags;
  Elf32_Half vd_ndx;
  Elf32_Half vd_cnt;
  Elf32_Word vd_hash;
  Elf32_Word vd_aux;
  Elf32_Word vd_next;

} Elf32_Verdef;

typedef struct
{
  Elf64_Half vd_version;
  Elf64_Half vd_flags;
  Elf64_Half vd_ndx;
  Elf64_Half vd_cnt;
  Elf64_Word vd_hash;
  Elf64_Word vd_aux;
  Elf64_Word vd_next;

} Elf64_Verdef;
# 1040 "../elf/elf.h"
typedef struct
{
  Elf32_Word vda_name;
  Elf32_Word vda_next;

} Elf32_Verdaux;

typedef struct
{
  Elf64_Word vda_name;
  Elf64_Word vda_next;

} Elf64_Verdaux;




typedef struct
{
  Elf32_Half vn_version;
  Elf32_Half vn_cnt;
  Elf32_Word vn_file;

  Elf32_Word vn_aux;
  Elf32_Word vn_next;

} Elf32_Verneed;

typedef struct
{
  Elf64_Half vn_version;
  Elf64_Half vn_cnt;
  Elf64_Word vn_file;

  Elf64_Word vn_aux;
  Elf64_Word vn_next;

} Elf64_Verneed;
# 1087 "../elf/elf.h"
typedef struct
{
  Elf32_Word vna_hash;
  Elf32_Half vna_flags;
  Elf32_Half vna_other;
  Elf32_Word vna_name;
  Elf32_Word vna_next;

} Elf32_Vernaux;

typedef struct
{
  Elf64_Word vna_hash;
  Elf64_Half vna_flags;
  Elf64_Half vna_other;
  Elf64_Word vna_name;
  Elf64_Word vna_next;

} Elf64_Vernaux;
# 1121 "../elf/elf.h"
typedef struct
{
  uint32_t a_type;
  union
    {
      uint32_t a_val;



    } a_un;
} Elf32_auxv_t;

typedef struct
{
  uint64_t a_type;
  union
    {
      uint64_t a_val;



    } a_un;
} Elf64_auxv_t;
# 1220 "../elf/elf.h"
typedef struct
{
  Elf32_Word n_namesz;
  Elf32_Word n_descsz;
  Elf32_Word n_type;
} Elf32_Nhdr;

typedef struct
{
  Elf64_Word n_namesz;
  Elf64_Word n_descsz;
  Elf64_Word n_type;
} Elf64_Nhdr;
# 1340 "../elf/elf.h"
typedef struct
{
  Elf32_Xword m_value;
  Elf32_Word m_info;
  Elf32_Word m_poffset;
  Elf32_Half m_repeat;
  Elf32_Half m_stride;
} Elf32_Move;

typedef struct
{
  Elf64_Xword m_value;
  Elf64_Xword m_info;
  Elf64_Xword m_poffset;
  Elf64_Half m_repeat;
  Elf64_Half m_stride;
} Elf64_Move;
# 1729 "../elf/elf.h"
typedef union
{
  struct
    {
      Elf32_Word gt_current_g_value;
      Elf32_Word gt_unused;
    } gt_header;
  struct
    {
      Elf32_Word gt_g_value;
      Elf32_Word gt_bytes;
    } gt_entry;
} Elf32_gptab;



typedef struct
{
  Elf32_Word ri_gprmask;
  Elf32_Word ri_cprmask[4];
  Elf32_Sword ri_gp_value;
} Elf32_RegInfo;



typedef struct
{
  unsigned char kind;

  unsigned char size;
  Elf32_Section section;

  Elf32_Word info;
} Elf_Options;
# 1805 "../elf/elf.h"
typedef struct
{
  Elf32_Word hwp_flags1;
  Elf32_Word hwp_flags2;
} Elf_Options_Hw;
# 1971 "../elf/elf.h"
typedef struct
{
  Elf32_Word l_name;
  Elf32_Word l_time_stamp;
  Elf32_Word l_checksum;
  Elf32_Word l_version;
  Elf32_Word l_flags;
} Elf32_Lib;

typedef struct
{
  Elf64_Word l_name;
  Elf64_Word l_time_stamp;
  Elf64_Word l_checksum;
  Elf64_Word l_version;
  Elf64_Word l_flags;
} Elf64_Lib;
# 2002 "../elf/elf.h"
typedef Elf32_Addr Elf32_Conflict;

typedef struct
{

  Elf32_Half version;

  unsigned char isa_level;

  unsigned char isa_rev;

  unsigned char gpr_size;

  unsigned char cpr1_size;

  unsigned char cpr2_size;

  unsigned char fp_abi;

  Elf32_Word isa_ext;

  Elf32_Word ases;

  Elf32_Word flags1;
  Elf32_Word flags2;
} Elf_MIPS_ABIFlags_v0;
# 2078 "../elf/elf.h"
enum
{

  Val_GNU_MIPS_ABI_FP_ANY = 0,

  Val_GNU_MIPS_ABI_FP_DOUBLE = 1,

  Val_GNU_MIPS_ABI_FP_SINGLE = 2,

  Val_GNU_MIPS_ABI_FP_SOFT = 3,

  Val_GNU_MIPS_ABI_FP_OLD_64 = 4,

  Val_GNU_MIPS_ABI_FP_XX = 5,

  Val_GNU_MIPS_ABI_FP_64 = 6,

  Val_GNU_MIPS_ABI_FP_64A = 7,

  Val_GNU_MIPS_ABI_FP_MAX = 7
};
# 3935 "../elf/elf.h"

# 3 "../include/elf.h" 2



# 1 "../include/libc-pointer-arith.h" 1
# 7 "../include/elf.h" 2
# 20 "../include/elf.h"
# 1 "../sysdeps/generic/dl-dtprocnum.h" 1
# 21 "../include/elf.h" 2
# 25 "../sysdeps/x86_64/stackinfo.h" 2
# 25 "../include/stackinfo.h" 2
# 8 "../include/alloca.h" 2




extern void *__alloca (size_t __size);





extern int __libc_use_alloca (size_t size) __attribute__ ((const));
extern int __libc_alloca_cutoff (size_t size) __attribute__ ((const));




# 1 "../sysdeps/pthread/allocalim.h" 1
# 19 "../sysdeps/pthread/allocalim.h"
# 1 "../include/alloca.h" 1
# 20 "../sysdeps/pthread/allocalim.h" 2
# 1 "../include/limits.h" 1
# 26 "../include/limits.h"
# 1 "../bits/libc-header-start.h" 1
# 27 "../include/limits.h" 2
# 124 "../include/limits.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include-fixed/limits.h" 1 3 4
# 125 "../include/limits.h" 2
# 183 "../include/limits.h"
# 1 "../include/bits/posix1_lim.h" 1
# 1 "../posix/bits/posix1_lim.h" 1
# 27 "../posix/bits/posix1_lim.h"
# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 28 "../posix/bits/posix1_lim.h" 2
# 161 "../posix/bits/posix1_lim.h"
# 1 "../sysdeps/unix/sysv/linux/bits/local_lim.h" 1
# 38 "../sysdeps/unix/sysv/linux/bits/local_lim.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/linux/limits.h" 1 3 4
# 39 "../sysdeps/unix/sysv/linux/bits/local_lim.h" 2
# 162 "../posix/bits/posix1_lim.h" 2
# 1 "../include/bits/posix1_lim.h" 2
# 184 "../include/limits.h" 2



# 1 "../include/bits/posix2_lim.h" 1
# 1 "../posix/bits/posix2_lim.h" 1
# 1 "../include/bits/posix2_lim.h" 2
# 188 "../include/limits.h" 2



# 1 "../include/bits/xopen_lim.h" 1
# 64 "../include/bits/xopen_lim.h"
# 1 "../sysdeps/unix/sysv/linux/bits/uio_lim.h" 1
# 65 "../include/bits/xopen_lim.h" 2
# 192 "../include/limits.h" 2
# 21 "../sysdeps/pthread/allocalim.h" 2


extern __inline __attribute__ ((__always_inline__))
int
__libc_use_alloca (size_t size)
{
  return (__builtin_expect ((__libc_alloca_cutoff (size)), 1)

          || __builtin_expect ((size <= 16384 / 4), 1)

   );
}
# 25 "../include/alloca.h" 2
# 567 "../stdlib/stdlib.h" 2





extern void *valloc (size_t __size) __attribute__ ((__nothrow__ )) __attribute__ ((__malloc__)) ;




extern int posix_memalign (void **__memptr, size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__ )) ;




extern void *aligned_alloc (size_t __alignment, size_t __size)
     __attribute__ ((__nothrow__ )) __attribute__ ((__malloc__)) __attribute__ ((__alloc_size__ (2))) ;



extern void abort (void) __attribute__ ((__nothrow__ )) __attribute__ ((__noreturn__));



extern int atexit (void (*__func) (void)) __attribute__ ((__nothrow__ )) ;







extern int at_quick_exit (void (*__func) (void)) __attribute__ ((__nothrow__ )) ;






extern int on_exit (void (*__func) (int __status, void *__arg), void *__arg)
     __attribute__ ((__nothrow__ )) ;





extern void exit (int __status) __attribute__ ((__nothrow__ )) __attribute__ ((__noreturn__));





extern void quick_exit (int __status) __attribute__ ((__nothrow__ )) __attribute__ ((__noreturn__));





extern void _Exit (int __status) __attribute__ ((__nothrow__ )) __attribute__ ((__noreturn__));




extern char *getenv (const char *__name) __attribute__ ((__nothrow__ )) ;




extern char *secure_getenv (const char *__name)
     __attribute__ ((__nothrow__ )) ;






extern int putenv (char *__string) __attribute__ ((__nothrow__ )) ;





extern int setenv (const char *__name, const char *__value, int __replace)
     __attribute__ ((__nothrow__ )) ;


extern int unsetenv (const char *__name) __attribute__ ((__nothrow__ )) ;






extern int clearenv (void) __attribute__ ((__nothrow__ ));
# 672 "../stdlib/stdlib.h"
extern char *mktemp (char *__template) __attribute__ ((__nothrow__ )) ;
# 685 "../stdlib/stdlib.h"
extern int mkstemp (char *__template) ;
# 695 "../stdlib/stdlib.h"
extern int mkstemp64 (char *__template) ;
# 707 "../stdlib/stdlib.h"
extern int mkstemps (char *__template, int __suffixlen) ;
# 717 "../stdlib/stdlib.h"
extern int mkstemps64 (char *__template, int __suffixlen)
     ;
# 728 "../stdlib/stdlib.h"
extern char *mkdtemp (char *__template) __attribute__ ((__nothrow__ )) ;
# 739 "../stdlib/stdlib.h"
extern int mkostemp (char *__template, int __flags) ;
# 749 "../stdlib/stdlib.h"
extern int mkostemp64 (char *__template, int __flags) ;
# 759 "../stdlib/stdlib.h"
extern int mkostemps (char *__template, int __suffixlen, int __flags)
     ;
# 771 "../stdlib/stdlib.h"
extern int mkostemps64 (char *__template, int __suffixlen, int __flags)
     ;
# 781 "../stdlib/stdlib.h"
extern int system (const char *__command) ;





extern char *canonicalize_file_name (const char *__name)
     __attribute__ ((__nothrow__ )) ;
# 797 "../stdlib/stdlib.h"
extern char *realpath (const char *__restrict __name,
         char *__restrict __resolved) __attribute__ ((__nothrow__ )) ;






typedef int (*__compar_fn_t) (const void *, const void *);


typedef __compar_fn_t comparison_fn_t;



typedef int (*__compar_d_fn_t) (const void *, const void *, void *);




extern void *bsearch (const void *__key, const void *__base,
        size_t __nmemb, size_t __size, __compar_fn_t __compar)
     ;


# 1 "../bits/stdlib-bsearch.h" 1
# 19 "../bits/stdlib-bsearch.h"
extern __inline void *
bsearch (const void *__key, const void *__base, size_t __nmemb, size_t __size,
  __compar_fn_t __compar)
{
  size_t __l, __u, __idx;
  const void *__p;
  int __comparison;

  __l = 0;
  __u = __nmemb;
  while (__l < __u)
    {
      __idx = (__l + __u) / 2;
      __p = (void *) (((const char *) __base) + (__idx * __size));
      __comparison = (*__compar) (__key, __p);
      if (__comparison < 0)
 __u = __idx;
      else if (__comparison > 0)
 __l = __idx + 1;
      else
 return (void *) __p;
    }

  return 
# 42 "../bits/stdlib-bsearch.h" 3 4
        ((void *)0)
# 42 "../bits/stdlib-bsearch.h"
            ;
}
# 823 "../stdlib/stdlib.h" 2




extern void qsort (void *__base, size_t __nmemb, size_t __size,
     __compar_fn_t __compar) ;

extern void qsort_r (void *__base, size_t __nmemb, size_t __size,
       __compar_d_fn_t __compar, void *__arg)
  ;




extern int abs (int __x) __attribute__ ((__nothrow__ )) __attribute__ ((__const__)) ;
extern long int labs (long int __x) __attribute__ ((__nothrow__ )) __attribute__ ((__const__)) ;


__extension__ extern long long int llabs (long long int __x)
     __attribute__ ((__nothrow__ )) __attribute__ ((__const__)) ;






extern div_t div (int __numer, int __denom)
     __attribute__ ((__nothrow__ )) __attribute__ ((__const__)) ;
extern ldiv_t ldiv (long int __numer, long int __denom)
     __attribute__ ((__nothrow__ )) __attribute__ ((__const__)) ;


__extension__ extern lldiv_t lldiv (long long int __numer,
        long long int __denom)
     __attribute__ ((__nothrow__ )) __attribute__ ((__const__)) ;
# 869 "../stdlib/stdlib.h"
extern char *ecvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ )) ;




extern char *fcvt (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign) __attribute__ ((__nothrow__ )) ;




extern char *gcvt (double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ )) ;




extern char *qecvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ )) ;
extern char *qfcvt (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign)
     __attribute__ ((__nothrow__ )) ;
extern char *qgcvt (long double __value, int __ndigit, char *__buf)
     __attribute__ ((__nothrow__ )) ;




extern int ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ )) ;
extern int fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
     int *__restrict __sign, char *__restrict __buf,
     size_t __len) __attribute__ ((__nothrow__ )) ;

extern int qecvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ )) ;
extern int qfcvt_r (long double __value, int __ndigit,
      int *__restrict __decpt, int *__restrict __sign,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ )) ;





extern int mblen (const char *__s, size_t __n) __attribute__ ((__nothrow__ ));


extern int mbtowc (wchar_t *__restrict __pwc,
     const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ ));


extern int wctomb (char *__s, wchar_t __wchar) __attribute__ ((__nothrow__ ));



extern size_t mbstowcs (wchar_t *__restrict __pwcs,
   const char *__restrict __s, size_t __n) __attribute__ ((__nothrow__ ));

extern size_t wcstombs (char *__restrict __s,
   const wchar_t *__restrict __pwcs, size_t __n)
     __attribute__ ((__nothrow__ ));







extern int rpmatch (const char *__response) __attribute__ ((__nothrow__ )) ;
# 954 "../stdlib/stdlib.h"
extern int getsubopt (char **__restrict __optionp,
        char *const *__restrict __tokens,
        char **__restrict __valuep)
     __attribute__ ((__nothrow__ )) ;







extern int posix_openpt (int __oflag) ;







extern int grantpt (int __fd) __attribute__ ((__nothrow__ ));



extern int unlockpt (int __fd) __attribute__ ((__nothrow__ ));




extern char *ptsname (int __fd) __attribute__ ((__nothrow__ )) ;






extern int ptsname_r (int __fd, char *__buf, size_t __buflen)
     __attribute__ ((__nothrow__ )) ;


extern int getpt (void);






extern int getloadavg (double __loadavg[], int __nelem)
     __attribute__ ((__nothrow__ )) ;
# 1010 "../stdlib/stdlib.h"
# 1 "../include/bits/stdlib-float.h" 1





# 1 "../stdlib/bits/stdlib-float.h" 1
# 24 "../stdlib/bits/stdlib-float.h"
extern __inline double
__attribute__ ((__nothrow__ )) atof (const char *__nptr)
{
  return strtod (__nptr, (char **) 
# 27 "../stdlib/bits/stdlib-float.h" 3 4
                                  ((void *)0)
# 27 "../stdlib/bits/stdlib-float.h"
                                      );
}
# 7 "../include/bits/stdlib-float.h" 2
# 1011 "../stdlib/stdlib.h" 2
# 1020 "../stdlib/stdlib.h"

# 7 "../include/stdlib.h" 2



# 1 "../include/sys/stat.h" 1

# 1 "../io/sys/stat.h" 1
# 27 "../io/sys/stat.h"
# 1 "../include/bits/types.h" 1
# 28 "../io/sys/stat.h" 2


# 1 "../include/bits/types/struct_timespec.h" 1
# 31 "../io/sys/stat.h" 2






# 1 "../include/bits/types/time_t.h" 1
# 38 "../io/sys/stat.h" 2
# 99 "../io/sys/stat.h"


# 1 "../sysdeps/unix/sysv/linux/x86/bits/stat.h" 1
# 46 "../sysdeps/unix/sysv/linux/x86/bits/stat.h"
struct stat
  {
    __dev_t st_dev;




    __ino_t st_ino;







    __nlink_t st_nlink;
    __mode_t st_mode;

    __uid_t st_uid;
    __gid_t st_gid;

    int __pad0;

    __dev_t st_rdev;




    __off_t st_size;



    __blksize_t st_blksize;

    __blkcnt_t st_blocks;
# 91 "../sysdeps/unix/sysv/linux/x86/bits/stat.h"
    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
# 106 "../sysdeps/unix/sysv/linux/x86/bits/stat.h"
    __syscall_slong_t __glibc_reserved[3];
# 115 "../sysdeps/unix/sysv/linux/x86/bits/stat.h"
  };



struct stat64
  {
    __dev_t st_dev;

    __ino64_t st_ino;
    __nlink_t st_nlink;
    __mode_t st_mode;






    __uid_t st_uid;
    __gid_t st_gid;

    int __pad0;
    __dev_t st_rdev;
    __off_t st_size;





    __blksize_t st_blksize;
    __blkcnt64_t st_blocks;







    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
# 164 "../sysdeps/unix/sysv/linux/x86/bits/stat.h"
    __syscall_slong_t __glibc_reserved[3];



  };
# 102 "../io/sys/stat.h" 2
# 205 "../io/sys/stat.h"
extern int stat (const char *__restrict __file,
   struct stat *__restrict __buf) __attribute__ ((__nothrow__ )) ;



extern int fstat (int __fd, struct stat *__buf) __attribute__ ((__nothrow__ )) ;
# 224 "../io/sys/stat.h"
extern int stat64 (const char *__restrict __file,
     struct stat64 *__restrict __buf) __attribute__ ((__nothrow__ )) ;
extern int fstat64 (int __fd, struct stat64 *__buf) __attribute__ ((__nothrow__ )) ;







extern int fstatat (int __fd, const char *__restrict __file,
      struct stat *__restrict __buf, int __flag)
     __attribute__ ((__nothrow__ )) ;
# 249 "../io/sys/stat.h"
extern int fstatat64 (int __fd, const char *__restrict __file,
        struct stat64 *__restrict __buf, int __flag)
     __attribute__ ((__nothrow__ )) ;







extern int lstat (const char *__restrict __file,
    struct stat *__restrict __buf) __attribute__ ((__nothrow__ )) ;
# 272 "../io/sys/stat.h"
extern int lstat64 (const char *__restrict __file,
      struct stat64 *__restrict __buf)
     __attribute__ ((__nothrow__ )) ;





extern int chmod (const char *__file, __mode_t __mode)
     __attribute__ ((__nothrow__ )) ;





extern int lchmod (const char *__file, __mode_t __mode)
     __attribute__ ((__nothrow__ )) ;




extern int fchmod (int __fd, __mode_t __mode) __attribute__ ((__nothrow__ ));





extern int fchmodat (int __fd, const char *__file, __mode_t __mode,
       int __flag)
     __attribute__ ((__nothrow__ )) ;






extern __mode_t umask (__mode_t __mask) __attribute__ ((__nothrow__ ));




extern __mode_t getumask (void) __attribute__ ((__nothrow__ ));



extern int mkdir (const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__ )) ;





extern int mkdirat (int __fd, const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__ )) ;






extern int mknod (const char *__path, __mode_t __mode, __dev_t __dev)
     __attribute__ ((__nothrow__ )) ;





extern int mknodat (int __fd, const char *__path, __mode_t __mode,
      __dev_t __dev) __attribute__ ((__nothrow__ )) ;





extern int mkfifo (const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__ )) ;





extern int mkfifoat (int __fd, const char *__path, __mode_t __mode)
     __attribute__ ((__nothrow__ )) ;





extern int utimensat (int __fd, const char *__path,
        const struct timespec __times[2],
        int __flags)
     __attribute__ ((__nothrow__ )) ;




extern int futimens (int __fd, const struct timespec __times[2]) __attribute__ ((__nothrow__ ));
# 395 "../io/sys/stat.h"
extern int __fxstat (int __ver, int __fildes, struct stat *__stat_buf)
     __attribute__ ((__nothrow__ )) ;
extern int __xstat (int __ver, const char *__filename,
      struct stat *__stat_buf) __attribute__ ((__nothrow__ )) ;
extern int __lxstat (int __ver, const char *__filename,
       struct stat *__stat_buf) __attribute__ ((__nothrow__ )) ;
extern int __fxstatat (int __ver, int __fildes, const char *__filename,
         struct stat *__stat_buf, int __flag)
     __attribute__ ((__nothrow__ )) ;
# 428 "../io/sys/stat.h"
extern int __fxstat64 (int __ver, int __fildes, struct stat64 *__stat_buf)
     __attribute__ ((__nothrow__ )) ;
extern int __xstat64 (int __ver, const char *__filename,
        struct stat64 *__stat_buf) __attribute__ ((__nothrow__ )) ;
extern int __lxstat64 (int __ver, const char *__filename,
         struct stat64 *__stat_buf) __attribute__ ((__nothrow__ )) ;
extern int __fxstatat64 (int __ver, int __fildes, const char *__filename,
    struct stat64 *__stat_buf, int __flag)
     __attribute__ ((__nothrow__ )) ;

extern int __xmknod (int __ver, const char *__path, __mode_t __mode,
       __dev_t *__dev) __attribute__ ((__nothrow__ )) ;

extern int __xmknodat (int __ver, int __fd, const char *__path,
         __mode_t __mode, __dev_t *__dev)
     __attribute__ ((__nothrow__ )) ;


# 1 "../include/bits/statx.h" 1
# 1 "../io/bits/statx.h" 1
# 25 "../io/bits/statx.h"
struct statx_timestamp
{
  __int64_t tv_sec;
  __uint32_t tv_nsec;
  __int32_t __statx_timestamp_pad1[1];
};





struct statx
{
  __uint32_t stx_mask;
  __uint32_t stx_blksize;
  __uint64_t stx_attributes;
  __uint32_t stx_nlink;
  __uint32_t stx_uid;
  __uint32_t stx_gid;
  __uint16_t stx_mode;
  __uint16_t __statx_pad1[1];
  __uint64_t stx_ino;
  __uint64_t stx_size;
  __uint64_t stx_blocks;
  __uint64_t stx_attributes_mask;
  struct statx_timestamp stx_atime;
  struct statx_timestamp stx_btime;
  struct statx_timestamp stx_ctime;
  struct statx_timestamp stx_mtime;
  __uint32_t stx_rdev_major;
  __uint32_t stx_rdev_minor;
  __uint32_t stx_dev_major;
  __uint32_t stx_dev_minor;
  __uint64_t __statx_pad2[14];
};
# 84 "../io/bits/statx.h"



int statx (int __dirfd, const char *__restrict __path, int __flags,
           unsigned int __mask, struct statx *__restrict __buf)
  __attribute__ ((__nothrow__ )) ;


# 1 "../include/bits/statx.h" 2
# 447 "../io/sys/stat.h" 2





extern __inline int
__attribute__ ((__nothrow__ )) stat (const char *__path, struct stat *__statbuf)
{
  return __xstat (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__ )) lstat (const char *__path, struct stat *__statbuf)
{
  return __lxstat (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__ )) fstat (int __fd, struct stat *__statbuf)
{
  return __fxstat (1, __fd, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__ )) fstatat (int __fd, const char *__filename, struct stat *__statbuf, int __flag)

{
  return __fxstatat (1, __fd, __filename, __statbuf, __flag);
}



extern __inline int
__attribute__ ((__nothrow__ )) mknod (const char *__path, __mode_t __mode, __dev_t __dev)
{
  return __xmknod (0, __path, __mode, &__dev);
}



extern __inline int
__attribute__ ((__nothrow__ )) mknodat (int __fd, const char *__path, __mode_t __mode, __dev_t __dev)

{
  return __xmknodat (0, __fd, __path, __mode, &__dev);
}





extern __inline int
__attribute__ ((__nothrow__ )) stat64 (const char *__path, struct stat64 *__statbuf)
{
  return __xstat64 (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__ )) lstat64 (const char *__path, struct stat64 *__statbuf)
{
  return __lxstat64 (1, __path, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__ )) fstat64 (int __fd, struct stat64 *__statbuf)
{
  return __fxstat64 (1, __fd, __statbuf);
}


extern __inline int
__attribute__ ((__nothrow__ )) fstatat64 (int __fd, const char *__filename, struct stat64 *__statbuf, int __flag)

{
  return __fxstatat64 (1, __fd, __filename, __statbuf, __flag);
}







# 3 "../include/sys/stat.h" 2



extern int __stat (const char *__file, struct stat *__buf);
extern int __fstat (int __fd, struct stat *__buf);
extern int __lstat (const char *__file, struct stat *__buf);
extern int __chmod (const char *__file, __mode_t __mode);

extern int __fchmod (int __fd, __mode_t __mode);
extern __mode_t __umask (__mode_t __mask);
extern int __mkdir (const char *__path, __mode_t __mode);

extern int __mknod (const char *__path,
      __mode_t __mode, __dev_t __dev);
# 25 "../include/sys/stat.h"
extern __inline__ int __stat (const char *__path, struct stat *__statbuf)
{
  return __xstat (1, __path, __statbuf);
}

extern __inline__ int __mknod (const char *__path, __mode_t __mode,
          __dev_t __dev)
{
  return __xmknod (0, __path, __mode, &__dev);
}




# 11 "../include/stdlib.h" 2

extern __typeof (strtol_l) __strtol_l;
extern __typeof (strtoul_l) __strtoul_l;
extern __typeof (strtoll_l) __strtoll_l;
extern __typeof (strtoull_l) __strtoull_l;
extern __typeof (strtod_l) __strtod_l;
extern __typeof (strtof_l) __strtof_l;
extern __typeof (strtold_l) __strtold_l;











extern __typeof (secure_getenv) __libc_secure_getenv;



extern __typeof (qsort_r) __qsort_r;




extern long int __random (void) ;
extern void __srandom (unsigned int __seed);
extern char *__initstate (unsigned int __seed, char *__statebuf,
     size_t __statelen);
extern char *__setstate (char *__statebuf);
extern int __random_r (struct random_data *__buf, int32_t *__result)
     ;
extern int __srandom_r (unsigned int __seed, struct random_data *__buf)
     ;
extern int __initstate_r (unsigned int __seed, char *__statebuf,
     size_t __statelen, struct random_data *__buf)
     ;
extern int __setstate_r (char *__statebuf, struct random_data *__buf)
     ;
extern int __rand_r (unsigned int *__seed);
extern int __erand48_r (unsigned short int __xsubi[3],
   struct drand48_data *__buffer, double *__result)
     ;
extern int __nrand48_r (unsigned short int __xsubi[3],
   struct drand48_data *__buffer,
   long int *__result) ;
extern int __jrand48_r (unsigned short int __xsubi[3],
   struct drand48_data *__buffer,
   long int *__result) ;
extern int __srand48_r (long int __seedval,
   struct drand48_data *__buffer) ;
extern int __seed48_r (unsigned short int __seed16v[3],
         struct drand48_data *__buffer) ;
extern int __lcong48_r (unsigned short int __param[7],
   struct drand48_data *__buffer) ;


extern int __drand48_iterate (unsigned short int __xsubi[3],
         struct drand48_data *__buffer)
     ;


extern struct drand48_data __libc_drand48_data ;

extern int __setenv (const char *__name, const char *__value, int __replace)
     ;
extern int __unsetenv (const char *__name) ;
extern int __clearenv (void) ;
extern char *__mktemp (char *__template) __attribute__ ((__nothrow__ )) ;
extern char *__canonicalize_file_name (const char *__name);
extern char *__realpath (const char *__name, char *__resolved);

extern int __ptsname_r (int __fd, char *__buf, size_t __buflen)
     ;

extern int __ptsname_internal (int fd, char *buf, size_t buflen,
          struct stat64 *stp) ;

extern int __getpt (void);
extern int __posix_openpt (int __oflag) ;

extern int __add_to_environ (const char *name, const char *value,
        const char *combines, int replace)
     ;
extern void _quicksort (void *const pbase, size_t total_elems,
   size_t size, __compar_d_fn_t cmp, void *arg);

extern int __on_exit (void (*__func) (int __status, void *__arg), void *__arg);

extern int __cxa_atexit (void (*func) (void *), void *arg, void *d);
;

extern int __cxa_thread_atexit_impl (void (*func) (void *), void *arg,
         void *d);
extern void __call_tls_dtors (void)

  __attribute__ ((weak))

  ;


extern void __cxa_finalize (void *d);

extern int __posix_memalign (void **memptr, size_t alignment, size_t size);

extern void *__libc_memalign (size_t alignment, size_t size)
     __attribute__ ((__malloc__));

extern void *__libc_reallocarray (void *__ptr, size_t __nmemb, size_t __size)
     __attribute__ ((__nothrow__ )) __attribute__ ((__warn_unused_result__));


extern int __libc_system (const char *line);


extern double __strtod_internal (const char *__restrict __nptr,
     char **__restrict __endptr, int __group)
     __attribute__ ((__nothrow__ )) ;
extern float __strtof_internal (const char *__restrict __nptr,
    char **__restrict __endptr, int __group)
     __attribute__ ((__nothrow__ )) ;
extern long double __strtold_internal (const char *__restrict __nptr,
           char **__restrict __endptr,
           int __group)
     __attribute__ ((__nothrow__ )) ;
extern long int __strtol_internal (const char *__restrict __nptr,
       char **__restrict __endptr,
       int __base, int __group)
     __attribute__ ((__nothrow__ )) ;
extern unsigned long int __strtoul_internal (const char *__restrict __nptr,
          char **__restrict __endptr,
          int __base, int __group)
     __attribute__ ((__nothrow__ )) ;
__extension__
extern long long int __strtoll_internal (const char *__restrict __nptr,
      char **__restrict __endptr,
      int __base, int __group)
     __attribute__ ((__nothrow__ )) ;
__extension__
extern unsigned long long int __strtoull_internal (const char *
         __restrict __nptr,
         char **__restrict __endptr,
         int __base, int __group)
     __attribute__ ((__nothrow__ )) ;








extern double ____strtod_l_internal (const char *__restrict __nptr,
         char **__restrict __endptr, int __group,
         locale_t __loc);
extern float ____strtof_l_internal (const char *__restrict __nptr,
        char **__restrict __endptr, int __group,
        locale_t __loc);
extern long double ____strtold_l_internal (const char *__restrict __nptr,
        char **__restrict __endptr,
        int __group, locale_t __loc);
extern long int ____strtol_l_internal (const char *__restrict __nptr,
           char **__restrict __endptr,
           int __base, int __group,
           locale_t __loc);
extern unsigned long int ____strtoul_l_internal (const char *
       __restrict __nptr,
       char **__restrict __endptr,
       int __base, int __group,
       locale_t __loc);
__extension__
extern long long int ____strtoll_l_internal (const char *__restrict __nptr,
          char **__restrict __endptr,
          int __base, int __group,
          locale_t __loc);
__extension__
extern unsigned long long int ____strtoull_l_internal (const char *
             __restrict __nptr,
             char **
             __restrict __endptr,
             int __base, int __group,
             locale_t __loc);



















extern float __strtof_nan (const char *, char **, char);
extern double __strtod_nan (const char *, char **, char);
extern long double __strtold_nan (const char *, char **, char);
extern float __wcstof_nan (const wchar_t *, wchar_t **, wchar_t);
extern double __wcstod_nan (const wchar_t *, wchar_t **, wchar_t);
extern long double __wcstold_nan (const wchar_t *, wchar_t **, wchar_t);












extern __typeof (strtof128_l) __strtof128_l;




extern _Float128 __strtof128_nan (const char *, char **, char);
extern _Float128 __wcstof128_nan (const wchar_t *, wchar_t **, wchar_t);




extern _Float128 __strtof128_internal (const char *__restrict __nptr,
           char **__restrict __endptr,
           int __group);


extern _Float128 ____strtof128_l_internal (const char *__restrict __nptr,
        char **__restrict __endptr,
        int __group, locale_t __loc);




extern char *__ecvt (double __value, int __ndigit, int *__restrict __decpt,
       int *__restrict __sign);
extern char *__fcvt (double __value, int __ndigit, int *__restrict __decpt,
       int *__restrict __sign);
extern char *__gcvt (double __value, int __ndigit, char *__buf);
extern int __ecvt_r (double __value, int __ndigit, int *__restrict __decpt,
       int *__restrict __sign, char *__restrict __buf,
       size_t __len);

extern int __fcvt_r (double __value, int __ndigit, int *__restrict __decpt,
       int *__restrict __sign, char *__restrict __buf,
       size_t __len);

extern char *__qecvt (long double __value, int __ndigit,
        int *__restrict __decpt, int *__restrict __sign);
extern char *__qfcvt (long double __value, int __ndigit,
        int *__restrict __decpt, int *__restrict __sign);
extern char *__qgcvt (long double __value, int __ndigit, char *__buf);
extern int __qecvt_r (long double __value, int __ndigit,
        int *__restrict __decpt, int *__restrict __sign,
        char *__restrict __buf, size_t __len);

extern int __qfcvt_r (long double __value, int __ndigit,
        int *__restrict __decpt, int *__restrict __sign,
        char *__restrict __buf, size_t __len);







extern void *__default_morecore (ptrdiff_t) __attribute__ ((__nothrow__ ));


struct abort_msg_s
{
  unsigned int size;
  char msg[0];
};
extern struct abort_msg_s *__abort_msg;

# 19 "chroot_canon.c" 2
# 1 "../include/string.h" 1





# 1 "../include/sys/types.h" 1
# 7 "../include/string.h" 2

extern void *__memccpy (void *__dest, const void *__src,
   int __c, size_t __n);

extern size_t __strnlen (const char *__string, size_t __maxlen)
     __attribute__ ((__pure__));

extern char *__strsep (char **__stringp, const char *__delim);


extern int __strverscmp (const char *__s1, const char *__s2)
     __attribute__ ((__pure__));

extern int __strncasecmp (const char *__s1, const char *__s2,
     size_t __n)
     __attribute__ ((__pure__));

extern int __strcasecmp (const char *__s1, const char *__s2)
     __attribute__ ((__pure__));

extern char *__strcasestr (const char *__haystack, const char *__needle)
     __attribute__ ((__pure__));

extern char *__strdup (const char *__string)
     __attribute__ ((__malloc__));
extern char *__strndup (const char *__string, size_t __n)
     __attribute__ ((__malloc__));

extern void *__rawmemchr (const void *__s, int __c)
     __attribute__ ((__pure__));

extern char *__strchrnul (const char *__s, int __c)
     __attribute__ ((__pure__));

extern void *__memrchr (const void *__s, int __c, size_t __n)
     __attribute__ ((__pure__));

extern void *__memchr (const void *__s, int __c, size_t __n)
     __attribute__ ((__pure__));

extern void __bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ )) ;

extern int __ffs (int __i) __attribute__ ((const));

extern char *__strerror_r (int __errnum, char *__buf, size_t __buflen);


void __strerror_thread_freeres (void) ;


# 1 "../sysdeps/x86/string_private.h" 1
# 58 "../include/string.h" 2


# 1 "../string/string.h" 1
# 26 "../string/string.h"
# 1 "../bits/libc-header-start.h" 1
# 27 "../string/string.h" 2






# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 34 "../string/string.h" 2
# 42 "../string/string.h"
extern void *memcpy (void *__restrict __dest, const void *__restrict __src,
       size_t __n) __attribute__ ((__nothrow__ )) ;


extern void *memmove (void *__dest, const void *__src, size_t __n)
     __attribute__ ((__nothrow__ )) ;





extern void *memccpy (void *__restrict __dest, const void *__restrict __src,
        int __c, size_t __n)
     __attribute__ ((__nothrow__ )) ;




extern void *memset (void *__s, int __c, size_t __n) __attribute__ ((__nothrow__ )) ;


extern int memcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 90 "../string/string.h"
extern void *memchr (const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 103 "../string/string.h"
extern void *rawmemchr (const void *__s, int __c)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 114 "../string/string.h"
extern void *memrchr (const void *__s, int __c, size_t __n)
      __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;





extern char *strcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ )) ;

extern char *strncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ )) ;


extern char *strcat (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ )) ;

extern char *strncat (char *__restrict __dest, const char *__restrict __src,
        size_t __n) __attribute__ ((__nothrow__ )) ;


extern int strcmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;

extern int strncmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;


extern int strcoll (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;

extern size_t strxfrm (char *__restrict __dest,
         const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ )) ;



# 1 "../include/bits/types/locale_t.h" 1
# 153 "../string/string.h" 2


extern int strcoll_l (const char *__s1, const char *__s2, locale_t __l)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;


extern size_t strxfrm_l (char *__dest, const char *__src, size_t __n,
    locale_t __l) __attribute__ ((__nothrow__ )) ;





extern char *strdup (const char *__s)
     __attribute__ ((__nothrow__ )) __attribute__ ((__malloc__)) ;






extern char *strndup (const char *__string, size_t __n)
     __attribute__ ((__nothrow__ )) __attribute__ ((__malloc__)) ;
# 225 "../string/string.h"
extern char *strchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 252 "../string/string.h"
extern char *strrchr (const char *__s, int __c)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 265 "../string/string.h"
extern char *strchrnul (const char *__s, int __c)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;





extern size_t strcspn (const char *__s, const char *__reject)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;


extern size_t strspn (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 302 "../string/string.h"
extern char *strpbrk (const char *__s, const char *__accept)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 329 "../string/string.h"
extern char *strstr (const char *__haystack, const char *__needle)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;




extern char *strtok (char *__restrict __s, const char *__restrict __delim)
     __attribute__ ((__nothrow__ )) ;



extern char *__strtok_r (char *__restrict __s,
    const char *__restrict __delim,
    char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ )) ;

extern char *strtok_r (char *__restrict __s, const char *__restrict __delim,
         char **__restrict __save_ptr)
     __attribute__ ((__nothrow__ )) ;
# 359 "../string/string.h"
extern char *strcasestr (const char *__haystack, const char *__needle)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;







extern void *memmem (const void *__haystack, size_t __haystacklen,
       const void *__needle, size_t __needlelen)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;



extern void *__mempcpy (void *__restrict __dest,
   const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ )) ;
extern void *mempcpy (void *__restrict __dest,
        const void *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ )) ;




extern size_t strlen (const char *__s)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;




extern size_t strnlen (const char *__string, size_t __maxlen)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;




extern char *strerror (int __errnum) __attribute__ ((__nothrow__ ));
# 420 "../string/string.h"
extern char *strerror_r (int __errnum, char *__buf, size_t __buflen)
     __attribute__ ((__nothrow__ )) ;





extern char *strerror_l (int __errnum, locale_t __l) __attribute__ ((__nothrow__ ));



# 1 "../include/strings.h" 1
# 1 "../string/strings.h" 1
# 23 "../string/strings.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 24 "../string/strings.h" 2










extern int bcmp (const void *__s1, const void *__s2, size_t __n)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;


extern void bcopy (const void *__src, void *__dest, size_t __n)
  __attribute__ ((__nothrow__ )) ;


extern void bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ )) ;
# 68 "../string/strings.h"
extern char *index (const char *__s, int __c)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;
# 96 "../string/strings.h"
extern char *rindex (const char *__s, int __c)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;






extern int ffs (int __i) __attribute__ ((__nothrow__ )) __attribute__ ((__const__));





extern int ffsl (long int __l) __attribute__ ((__nothrow__ )) __attribute__ ((__const__));
__extension__ extern int ffsll (long long int __ll)
     __attribute__ ((__nothrow__ )) __attribute__ ((__const__));



extern int strcasecmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;


extern int strncasecmp (const char *__s1, const char *__s2, size_t __n)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;



# 1 "../include/bits/types/locale_t.h" 1
# 126 "../string/strings.h" 2


extern int strcasecmp_l (const char *__s1, const char *__s2, locale_t __loc)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;



extern int strncasecmp_l (const char *__s1, const char *__s2,
     size_t __n, locale_t __loc)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;



# 1 "../include/strings.h" 2
# 432 "../string/string.h" 2



extern void explicit_bzero (void *__s, size_t __n) __attribute__ ((__nothrow__ )) ;



extern char *strsep (char **__restrict __stringp,
       const char *__restrict __delim)
     __attribute__ ((__nothrow__ )) ;




extern char *strsignal (int __sig) __attribute__ ((__nothrow__ ));


extern char *__stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ )) ;
extern char *stpcpy (char *__restrict __dest, const char *__restrict __src)
     __attribute__ ((__nothrow__ )) ;



extern char *__stpncpy (char *__restrict __dest,
   const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ )) ;
extern char *stpncpy (char *__restrict __dest,
        const char *__restrict __src, size_t __n)
     __attribute__ ((__nothrow__ )) ;




extern int strverscmp (const char *__s1, const char *__s2)
     __attribute__ ((__nothrow__ )) __attribute__ ((__pure__)) ;


extern char *strfry (char *__string) __attribute__ ((__nothrow__ )) ;


extern void *memfrob (void *__s, size_t __n) __attribute__ ((__nothrow__ )) ;
# 486 "../string/string.h"
extern char *basename (const char *__filename) __attribute__ ((__nothrow__ )) ;
# 498 "../string/string.h"

# 61 "../include/string.h" 2


extern __typeof (strcoll_l) __strcoll_l;
extern __typeof (strxfrm_l) __strxfrm_l;
extern __typeof (strcasecmp_l) __strcasecmp_l;
extern __typeof (strncasecmp_l) __strncasecmp_l;
# 82 "../include/string.h"













extern __typeof (strncat) __strncat;






extern char *__basename (const char *__filename) __attribute__ ((__nothrow__ )) ;





extern char *__strsep_g (char **__stringp, const char *__delim);




extern __typeof (memmem) __memmem;


# 128 "../include/string.h"




















# 173 "../include/string.h"
extern __typeof (mempcpy) mempcpy __asm__ ("__mempcpy");
extern __typeof (stpcpy) stpcpy __asm__ ("__stpcpy");


extern void *__memcpy_chk (void *__restrict __dest,
      const void *__restrict __src, size_t __len,
      size_t __destlen) __attribute__ ((__nothrow__ ));
extern void *__memmove_chk (void *__dest, const void *__src, size_t __len,
       size_t __destlen) __attribute__ ((__nothrow__ ));
extern void *__mempcpy_chk (void *__restrict __dest,
       const void *__restrict __src, size_t __len,
       size_t __destlen) __attribute__ ((__nothrow__ ));
extern void *__memset_chk (void *__dest, int __ch, size_t __len,
      size_t __destlen) __attribute__ ((__nothrow__ ));
extern char *__strcpy_chk (char *__restrict __dest,
      const char *__restrict __src,
      size_t __destlen) __attribute__ ((__nothrow__ ));
extern char *__stpcpy_chk (char *__restrict __dest,
      const char *__restrict __src,
      size_t __destlen) __attribute__ ((__nothrow__ ));
extern char *__strncpy_chk (char *__restrict __dest,
       const char *__restrict __src,
       size_t __len, size_t __destlen) __attribute__ ((__nothrow__ ));
extern char *__strcat_chk (char *__restrict __dest,
      const char *__restrict __src,
      size_t __destlen) __attribute__ ((__nothrow__ ));
extern char *__strncat_chk (char *__restrict __dest,
       const char *__restrict __src,
       size_t __len, size_t __destlen) __attribute__ ((__nothrow__ ));
# 20 "chroot_canon.c" 2
# 1 "../include/unistd.h" 1

# 1 "../posix/unistd.h" 1
# 27 "../posix/unistd.h"

# 202 "../posix/unistd.h"
# 1 "../sysdeps/unix/sysv/linux/bits/posix_opt.h" 1
# 203 "../posix/unistd.h" 2



# 1 "../sysdeps/unix/sysv/linux/x86/bits/environments.h" 1
# 22 "../sysdeps/unix/sysv/linux/x86/bits/environments.h"
# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 23 "../sysdeps/unix/sysv/linux/x86/bits/environments.h" 2
# 207 "../posix/unistd.h" 2
# 217 "../posix/unistd.h"
# 1 "../include/bits/types.h" 1
# 218 "../posix/unistd.h" 2
# 226 "../posix/unistd.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 227 "../posix/unistd.h" 2
# 274 "../posix/unistd.h"
typedef __socklen_t socklen_t;
# 287 "../posix/unistd.h"
extern int access (const char *__name, int __type) __attribute__ ((__nothrow__ )) ;




extern int euidaccess (const char *__name, int __type)
     __attribute__ ((__nothrow__ )) ;


extern int eaccess (const char *__name, int __type)
     __attribute__ ((__nothrow__ )) ;






extern int faccessat (int __fd, const char *__file, int __type, int __flag)
     __attribute__ ((__nothrow__ )) ;
# 334 "../posix/unistd.h"
extern __off_t lseek (int __fd, __off_t __offset, int __whence) __attribute__ ((__nothrow__ ));
# 345 "../posix/unistd.h"
extern __off64_t lseek64 (int __fd, __off64_t __offset, int __whence)
     __attribute__ ((__nothrow__ ));






extern int close (int __fd);






extern ssize_t read (int __fd, void *__buf, size_t __nbytes) ;





extern ssize_t write (int __fd, const void *__buf, size_t __n) ;
# 376 "../posix/unistd.h"
extern ssize_t pread (int __fd, void *__buf, size_t __nbytes,
        __off_t __offset) ;






extern ssize_t pwrite (int __fd, const void *__buf, size_t __n,
         __off_t __offset) ;
# 404 "../posix/unistd.h"
extern ssize_t pread64 (int __fd, void *__buf, size_t __nbytes,
   __off64_t __offset) ;


extern ssize_t pwrite64 (int __fd, const void *__buf, size_t __n,
    __off64_t __offset) ;







extern int pipe (int __pipedes[2]) __attribute__ ((__nothrow__ )) ;




extern int pipe2 (int __pipedes[2], int __flags) __attribute__ ((__nothrow__ )) ;
# 432 "../posix/unistd.h"
extern unsigned int alarm (unsigned int __seconds) __attribute__ ((__nothrow__ ));
# 444 "../posix/unistd.h"
extern unsigned int sleep (unsigned int __seconds);







extern __useconds_t ualarm (__useconds_t __value, __useconds_t __interval)
     __attribute__ ((__nothrow__ ));






extern int usleep (__useconds_t __useconds);
# 469 "../posix/unistd.h"
extern int pause (void);



extern int chown (const char *__file, __uid_t __owner, __gid_t __group)
     __attribute__ ((__nothrow__ )) ;



extern int fchown (int __fd, __uid_t __owner, __gid_t __group) __attribute__ ((__nothrow__ )) ;




extern int lchown (const char *__file, __uid_t __owner, __gid_t __group)
     __attribute__ ((__nothrow__ )) ;






extern int fchownat (int __fd, const char *__file, __uid_t __owner,
       __gid_t __group, int __flag)
     __attribute__ ((__nothrow__ )) ;



extern int chdir (const char *__path) __attribute__ ((__nothrow__ )) ;



extern int fchdir (int __fd) __attribute__ ((__nothrow__ )) ;
# 511 "../posix/unistd.h"
extern char *getcwd (char *__buf, size_t __size) __attribute__ ((__nothrow__ )) ;





extern char *get_current_dir_name (void) __attribute__ ((__nothrow__ ));







extern char *getwd (char *__buf)
     __attribute__ ((__nothrow__ )) __attribute__ ((__deprecated__)) ;




extern int dup (int __fd) __attribute__ ((__nothrow__ )) ;


extern int dup2 (int __fd, int __fd2) __attribute__ ((__nothrow__ ));




extern int dup3 (int __fd, int __fd2, int __flags) __attribute__ ((__nothrow__ ));



extern char **__environ;

extern char **environ;





extern int execve (const char *__path, char *const __argv[],
     char *const __envp[]) __attribute__ ((__nothrow__ )) ;




extern int fexecve (int __fd, char *const __argv[], char *const __envp[])
     __attribute__ ((__nothrow__ )) ;




extern int execv (const char *__path, char *const __argv[])
     __attribute__ ((__nothrow__ )) ;



extern int execle (const char *__path, const char *__arg, ...)
     __attribute__ ((__nothrow__ )) ;



extern int execl (const char *__path, const char *__arg, ...)
     __attribute__ ((__nothrow__ )) ;



extern int execvp (const char *__file, char *const __argv[])
     __attribute__ ((__nothrow__ )) ;




extern int execlp (const char *__file, const char *__arg, ...)
     __attribute__ ((__nothrow__ )) ;




extern int execvpe (const char *__file, char *const __argv[],
      char *const __envp[])
     __attribute__ ((__nothrow__ )) ;





extern int nice (int __inc) __attribute__ ((__nothrow__ )) ;




extern void _exit (int __status) __attribute__ ((__noreturn__));





# 1 "../bits/confname.h" 1
# 24 "../bits/confname.h"
enum
  {
    _PC_LINK_MAX,

    _PC_MAX_CANON,

    _PC_MAX_INPUT,

    _PC_NAME_MAX,

    _PC_PATH_MAX,

    _PC_PIPE_BUF,

    _PC_CHOWN_RESTRICTED,

    _PC_NO_TRUNC,

    _PC_VDISABLE,

    _PC_SYNC_IO,

    _PC_ASYNC_IO,

    _PC_PRIO_IO,

    _PC_SOCK_MAXBUF,

    _PC_FILESIZEBITS,

    _PC_REC_INCR_XFER_SIZE,

    _PC_REC_MAX_XFER_SIZE,

    _PC_REC_MIN_XFER_SIZE,

    _PC_REC_XFER_ALIGN,

    _PC_ALLOC_SIZE_MIN,

    _PC_SYMLINK_MAX,

    _PC_2_SYMLINKS

  };


enum
  {
    _SC_ARG_MAX,

    _SC_CHILD_MAX,

    _SC_CLK_TCK,

    _SC_NGROUPS_MAX,

    _SC_OPEN_MAX,

    _SC_STREAM_MAX,

    _SC_TZNAME_MAX,

    _SC_JOB_CONTROL,

    _SC_SAVED_IDS,

    _SC_REALTIME_SIGNALS,

    _SC_PRIORITY_SCHEDULING,

    _SC_TIMERS,

    _SC_ASYNCHRONOUS_IO,

    _SC_PRIORITIZED_IO,

    _SC_SYNCHRONIZED_IO,

    _SC_FSYNC,

    _SC_MAPPED_FILES,

    _SC_MEMLOCK,

    _SC_MEMLOCK_RANGE,

    _SC_MEMORY_PROTECTION,

    _SC_MESSAGE_PASSING,

    _SC_SEMAPHORES,

    _SC_SHARED_MEMORY_OBJECTS,

    _SC_AIO_LISTIO_MAX,

    _SC_AIO_MAX,

    _SC_AIO_PRIO_DELTA_MAX,

    _SC_DELAYTIMER_MAX,

    _SC_MQ_OPEN_MAX,

    _SC_MQ_PRIO_MAX,

    _SC_VERSION,

    _SC_PAGESIZE,


    _SC_RTSIG_MAX,

    _SC_SEM_NSEMS_MAX,

    _SC_SEM_VALUE_MAX,

    _SC_SIGQUEUE_MAX,

    _SC_TIMER_MAX,




    _SC_BC_BASE_MAX,

    _SC_BC_DIM_MAX,

    _SC_BC_SCALE_MAX,

    _SC_BC_STRING_MAX,

    _SC_COLL_WEIGHTS_MAX,

    _SC_EQUIV_CLASS_MAX,

    _SC_EXPR_NEST_MAX,

    _SC_LINE_MAX,

    _SC_RE_DUP_MAX,

    _SC_CHARCLASS_NAME_MAX,


    _SC_2_VERSION,

    _SC_2_C_BIND,

    _SC_2_C_DEV,

    _SC_2_FORT_DEV,

    _SC_2_FORT_RUN,

    _SC_2_SW_DEV,

    _SC_2_LOCALEDEF,


    _SC_PII,

    _SC_PII_XTI,

    _SC_PII_SOCKET,

    _SC_PII_INTERNET,

    _SC_PII_OSI,

    _SC_POLL,

    _SC_SELECT,

    _SC_UIO_MAXIOV,

    _SC_IOV_MAX = _SC_UIO_MAXIOV,

    _SC_PII_INTERNET_STREAM,

    _SC_PII_INTERNET_DGRAM,

    _SC_PII_OSI_COTS,

    _SC_PII_OSI_CLTS,

    _SC_PII_OSI_M,

    _SC_T_IOV_MAX,



    _SC_THREADS,

    _SC_THREAD_SAFE_FUNCTIONS,

    _SC_GETGR_R_SIZE_MAX,

    _SC_GETPW_R_SIZE_MAX,

    _SC_LOGIN_NAME_MAX,

    _SC_TTY_NAME_MAX,

    _SC_THREAD_DESTRUCTOR_ITERATIONS,

    _SC_THREAD_KEYS_MAX,

    _SC_THREAD_STACK_MIN,

    _SC_THREAD_THREADS_MAX,

    _SC_THREAD_ATTR_STACKADDR,

    _SC_THREAD_ATTR_STACKSIZE,

    _SC_THREAD_PRIORITY_SCHEDULING,

    _SC_THREAD_PRIO_INHERIT,

    _SC_THREAD_PRIO_PROTECT,

    _SC_THREAD_PROCESS_SHARED,


    _SC_NPROCESSORS_CONF,

    _SC_NPROCESSORS_ONLN,

    _SC_PHYS_PAGES,

    _SC_AVPHYS_PAGES,

    _SC_ATEXIT_MAX,

    _SC_PASS_MAX,


    _SC_XOPEN_VERSION,

    _SC_XOPEN_XCU_VERSION,

    _SC_XOPEN_UNIX,

    _SC_XOPEN_CRYPT,

    _SC_XOPEN_ENH_I18N,

    _SC_XOPEN_SHM,


    _SC_2_CHAR_TERM,

    _SC_2_C_VERSION,

    _SC_2_UPE,


    _SC_XOPEN_XPG2,

    _SC_XOPEN_XPG3,

    _SC_XOPEN_XPG4,


    _SC_CHAR_BIT,

    _SC_CHAR_MAX,

    _SC_CHAR_MIN,

    _SC_INT_MAX,

    _SC_INT_MIN,

    _SC_LONG_BIT,

    _SC_WORD_BIT,

    _SC_MB_LEN_MAX,

    _SC_NZERO,

    _SC_SSIZE_MAX,

    _SC_SCHAR_MAX,

    _SC_SCHAR_MIN,

    _SC_SHRT_MAX,

    _SC_SHRT_MIN,

    _SC_UCHAR_MAX,

    _SC_UINT_MAX,

    _SC_ULONG_MAX,

    _SC_USHRT_MAX,


    _SC_NL_ARGMAX,

    _SC_NL_LANGMAX,

    _SC_NL_MSGMAX,

    _SC_NL_NMAX,

    _SC_NL_SETMAX,

    _SC_NL_TEXTMAX,


    _SC_XBS5_ILP32_OFF32,

    _SC_XBS5_ILP32_OFFBIG,

    _SC_XBS5_LP64_OFF64,

    _SC_XBS5_LPBIG_OFFBIG,


    _SC_XOPEN_LEGACY,

    _SC_XOPEN_REALTIME,

    _SC_XOPEN_REALTIME_THREADS,


    _SC_ADVISORY_INFO,

    _SC_BARRIERS,

    _SC_BASE,

    _SC_C_LANG_SUPPORT,

    _SC_C_LANG_SUPPORT_R,

    _SC_CLOCK_SELECTION,

    _SC_CPUTIME,

    _SC_THREAD_CPUTIME,

    _SC_DEVICE_IO,

    _SC_DEVICE_SPECIFIC,

    _SC_DEVICE_SPECIFIC_R,

    _SC_FD_MGMT,

    _SC_FIFO,

    _SC_PIPE,

    _SC_FILE_ATTRIBUTES,

    _SC_FILE_LOCKING,

    _SC_FILE_SYSTEM,

    _SC_MONOTONIC_CLOCK,

    _SC_MULTI_PROCESS,

    _SC_SINGLE_PROCESS,

    _SC_NETWORKING,

    _SC_READER_WRITER_LOCKS,

    _SC_SPIN_LOCKS,

    _SC_REGEXP,

    _SC_REGEX_VERSION,

    _SC_SHELL,

    _SC_SIGNALS,

    _SC_SPAWN,

    _SC_SPORADIC_SERVER,

    _SC_THREAD_SPORADIC_SERVER,

    _SC_SYSTEM_DATABASE,

    _SC_SYSTEM_DATABASE_R,

    _SC_TIMEOUTS,

    _SC_TYPED_MEMORY_OBJECTS,

    _SC_USER_GROUPS,

    _SC_USER_GROUPS_R,

    _SC_2_PBS,

    _SC_2_PBS_ACCOUNTING,

    _SC_2_PBS_LOCATE,

    _SC_2_PBS_MESSAGE,

    _SC_2_PBS_TRACK,

    _SC_SYMLOOP_MAX,

    _SC_STREAMS,

    _SC_2_PBS_CHECKPOINT,


    _SC_V6_ILP32_OFF32,

    _SC_V6_ILP32_OFFBIG,

    _SC_V6_LP64_OFF64,

    _SC_V6_LPBIG_OFFBIG,


    _SC_HOST_NAME_MAX,

    _SC_TRACE,

    _SC_TRACE_EVENT_FILTER,

    _SC_TRACE_INHERIT,

    _SC_TRACE_LOG,


    _SC_LEVEL1_ICACHE_SIZE,

    _SC_LEVEL1_ICACHE_ASSOC,

    _SC_LEVEL1_ICACHE_LINESIZE,

    _SC_LEVEL1_DCACHE_SIZE,

    _SC_LEVEL1_DCACHE_ASSOC,

    _SC_LEVEL1_DCACHE_LINESIZE,

    _SC_LEVEL2_CACHE_SIZE,

    _SC_LEVEL2_CACHE_ASSOC,

    _SC_LEVEL2_CACHE_LINESIZE,

    _SC_LEVEL3_CACHE_SIZE,

    _SC_LEVEL3_CACHE_ASSOC,

    _SC_LEVEL3_CACHE_LINESIZE,

    _SC_LEVEL4_CACHE_SIZE,

    _SC_LEVEL4_CACHE_ASSOC,

    _SC_LEVEL4_CACHE_LINESIZE,



    _SC_IPV6 = _SC_LEVEL1_ICACHE_SIZE + 50,

    _SC_RAW_SOCKETS,


    _SC_V7_ILP32_OFF32,

    _SC_V7_ILP32_OFFBIG,

    _SC_V7_LP64_OFF64,

    _SC_V7_LPBIG_OFFBIG,


    _SC_SS_REPL_MAX,


    _SC_TRACE_EVENT_NAME_MAX,

    _SC_TRACE_NAME_MAX,

    _SC_TRACE_SYS_MAX,

    _SC_TRACE_USER_EVENT_MAX,


    _SC_XOPEN_STREAMS,


    _SC_THREAD_ROBUST_PRIO_INHERIT,

    _SC_THREAD_ROBUST_PRIO_PROTECT

  };


enum
  {
    _CS_PATH,


    _CS_V6_WIDTH_RESTRICTED_ENVS,



    _CS_GNU_LIBC_VERSION,

    _CS_GNU_LIBPTHREAD_VERSION,


    _CS_V5_WIDTH_RESTRICTED_ENVS,



    _CS_V7_WIDTH_RESTRICTED_ENVS,



    _CS_LFS_CFLAGS = 1000,

    _CS_LFS_LDFLAGS,

    _CS_LFS_LIBS,

    _CS_LFS_LINTFLAGS,

    _CS_LFS64_CFLAGS,

    _CS_LFS64_LDFLAGS,

    _CS_LFS64_LIBS,

    _CS_LFS64_LINTFLAGS,


    _CS_XBS5_ILP32_OFF32_CFLAGS = 1100,

    _CS_XBS5_ILP32_OFF32_LDFLAGS,

    _CS_XBS5_ILP32_OFF32_LIBS,

    _CS_XBS5_ILP32_OFF32_LINTFLAGS,

    _CS_XBS5_ILP32_OFFBIG_CFLAGS,

    _CS_XBS5_ILP32_OFFBIG_LDFLAGS,

    _CS_XBS5_ILP32_OFFBIG_LIBS,

    _CS_XBS5_ILP32_OFFBIG_LINTFLAGS,

    _CS_XBS5_LP64_OFF64_CFLAGS,

    _CS_XBS5_LP64_OFF64_LDFLAGS,

    _CS_XBS5_LP64_OFF64_LIBS,

    _CS_XBS5_LP64_OFF64_LINTFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_CFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_LDFLAGS,

    _CS_XBS5_LPBIG_OFFBIG_LIBS,

    _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS,


    _CS_POSIX_V6_ILP32_OFF32_CFLAGS,

    _CS_POSIX_V6_ILP32_OFF32_LDFLAGS,

    _CS_POSIX_V6_ILP32_OFF32_LIBS,

    _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS,

    _CS_POSIX_V6_ILP32_OFFBIG_LIBS,

    _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS,

    _CS_POSIX_V6_LP64_OFF64_CFLAGS,

    _CS_POSIX_V6_LP64_OFF64_LDFLAGS,

    _CS_POSIX_V6_LP64_OFF64_LIBS,

    _CS_POSIX_V6_LP64_OFF64_LINTFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LIBS,

    _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS,


    _CS_POSIX_V7_ILP32_OFF32_CFLAGS,

    _CS_POSIX_V7_ILP32_OFF32_LDFLAGS,

    _CS_POSIX_V7_ILP32_OFF32_LIBS,

    _CS_POSIX_V7_ILP32_OFF32_LINTFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_CFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_LDFLAGS,

    _CS_POSIX_V7_ILP32_OFFBIG_LIBS,

    _CS_POSIX_V7_ILP32_OFFBIG_LINTFLAGS,

    _CS_POSIX_V7_LP64_OFF64_CFLAGS,

    _CS_POSIX_V7_LP64_OFF64_LDFLAGS,

    _CS_POSIX_V7_LP64_OFF64_LIBS,

    _CS_POSIX_V7_LP64_OFF64_LINTFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_CFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LDFLAGS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LIBS,

    _CS_POSIX_V7_LPBIG_OFFBIG_LINTFLAGS,


    _CS_V6_ENV,

    _CS_V7_ENV

  };
# 610 "../posix/unistd.h" 2


extern long int pathconf (const char *__path, int __name)
     __attribute__ ((__nothrow__ )) ;


extern long int fpathconf (int __fd, int __name) __attribute__ ((__nothrow__ ));


extern long int sysconf (int __name) __attribute__ ((__nothrow__ ));



extern size_t confstr (int __name, char *__buf, size_t __len) __attribute__ ((__nothrow__ ));




extern __pid_t getpid (void) __attribute__ ((__nothrow__ ));


extern __pid_t getppid (void) __attribute__ ((__nothrow__ ));


extern __pid_t getpgrp (void) __attribute__ ((__nothrow__ ));


extern __pid_t __getpgid (__pid_t __pid) __attribute__ ((__nothrow__ ));

extern __pid_t getpgid (__pid_t __pid) __attribute__ ((__nothrow__ ));






extern int setpgid (__pid_t __pid, __pid_t __pgid) __attribute__ ((__nothrow__ ));
# 660 "../posix/unistd.h"
extern int setpgrp (void) __attribute__ ((__nothrow__ ));






extern __pid_t setsid (void) __attribute__ ((__nothrow__ ));



extern __pid_t getsid (__pid_t __pid) __attribute__ ((__nothrow__ ));



extern __uid_t getuid (void) __attribute__ ((__nothrow__ ));


extern __uid_t geteuid (void) __attribute__ ((__nothrow__ ));


extern __gid_t getgid (void) __attribute__ ((__nothrow__ ));


extern __gid_t getegid (void) __attribute__ ((__nothrow__ ));




extern int getgroups (int __size, __gid_t __list[]) __attribute__ ((__nothrow__ )) ;



extern int group_member (__gid_t __gid) __attribute__ ((__nothrow__ ));






extern int setuid (__uid_t __uid) __attribute__ ((__nothrow__ )) ;




extern int setreuid (__uid_t __ruid, __uid_t __euid) __attribute__ ((__nothrow__ )) ;




extern int seteuid (__uid_t __uid) __attribute__ ((__nothrow__ )) ;






extern int setgid (__gid_t __gid) __attribute__ ((__nothrow__ )) ;




extern int setregid (__gid_t __rgid, __gid_t __egid) __attribute__ ((__nothrow__ )) ;




extern int setegid (__gid_t __gid) __attribute__ ((__nothrow__ )) ;





extern int getresuid (__uid_t *__ruid, __uid_t *__euid, __uid_t *__suid)
     __attribute__ ((__nothrow__ ));



extern int getresgid (__gid_t *__rgid, __gid_t *__egid, __gid_t *__sgid)
     __attribute__ ((__nothrow__ ));



extern int setresuid (__uid_t __ruid, __uid_t __euid, __uid_t __suid)
     __attribute__ ((__nothrow__ )) ;



extern int setresgid (__gid_t __rgid, __gid_t __egid, __gid_t __sgid)
     __attribute__ ((__nothrow__ )) ;






extern __pid_t fork (void) __attribute__ ((__nothrow__));







extern __pid_t vfork (void) __attribute__ ((__nothrow__ ));





extern char *ttyname (int __fd) __attribute__ ((__nothrow__ ));



extern int ttyname_r (int __fd, char *__buf, size_t __buflen)
     __attribute__ ((__nothrow__ )) ;



extern int isatty (int __fd) __attribute__ ((__nothrow__ ));




extern int ttyslot (void) __attribute__ ((__nothrow__ ));




extern int link (const char *__from, const char *__to)
     __attribute__ ((__nothrow__ )) ;




extern int linkat (int __fromfd, const char *__from, int __tofd,
     const char *__to, int __flags)
     __attribute__ ((__nothrow__ )) ;




extern int symlink (const char *__from, const char *__to)
     __attribute__ ((__nothrow__ )) ;




extern ssize_t readlink (const char *__restrict __path,
    char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ )) ;




extern int symlinkat (const char *__from, int __tofd,
        const char *__to) __attribute__ ((__nothrow__ )) ;


extern ssize_t readlinkat (int __fd, const char *__restrict __path,
      char *__restrict __buf, size_t __len)
     __attribute__ ((__nothrow__ )) ;



extern int unlink (const char *__name) __attribute__ ((__nothrow__ )) ;



extern int unlinkat (int __fd, const char *__name, int __flag)
     __attribute__ ((__nothrow__ )) ;



extern int rmdir (const char *__path) __attribute__ ((__nothrow__ )) ;



extern __pid_t tcgetpgrp (int __fd) __attribute__ ((__nothrow__ ));


extern int tcsetpgrp (int __fd, __pid_t __pgrp_id) __attribute__ ((__nothrow__ ));






extern char *getlogin (void);







extern int getlogin_r (char *__name, size_t __name_len) ;




extern int setlogin (const char *__name) __attribute__ ((__nothrow__ )) ;







# 1 "../include/bits/getopt_posix.h" 1
# 1 "../posix/bits/getopt_posix.h" 1
# 27 "../posix/bits/getopt_posix.h"
# 1 "../include/bits/getopt_core.h" 1
# 1 "../posix/bits/getopt_core.h" 1
# 28 "../posix/bits/getopt_core.h"








extern char *optarg;
# 50 "../posix/bits/getopt_core.h"
extern int optind;




extern int opterr;



extern int optopt;
# 91 "../posix/bits/getopt_core.h"
extern int getopt (int ___argc, char *const *___argv, const char *__shortopts)
       __attribute__ ((__nothrow__ )) ;


# 1 "../include/bits/getopt_core.h" 2
# 28 "../posix/bits/getopt_posix.h" 2


# 49 "../posix/bits/getopt_posix.h"

# 1 "../include/bits/getopt_posix.h" 2
# 870 "../posix/unistd.h" 2







extern int gethostname (char *__name, size_t __len) __attribute__ ((__nothrow__ )) ;






extern int sethostname (const char *__name, size_t __len)
     __attribute__ ((__nothrow__ )) ;



extern int sethostid (long int __id) __attribute__ ((__nothrow__ )) ;





extern int getdomainname (char *__name, size_t __len)
     __attribute__ ((__nothrow__ )) ;
extern int setdomainname (const char *__name, size_t __len)
     __attribute__ ((__nothrow__ )) ;





extern int vhangup (void) __attribute__ ((__nothrow__ ));


extern int revoke (const char *__file) __attribute__ ((__nothrow__ )) ;







extern int profil (unsigned short int *__sample_buffer, size_t __size,
     size_t __offset, unsigned int __scale)
     __attribute__ ((__nothrow__ )) ;





extern int acct (const char *__name) __attribute__ ((__nothrow__ ));



extern char *getusershell (void) __attribute__ ((__nothrow__ ));
extern void endusershell (void) __attribute__ ((__nothrow__ ));
extern void setusershell (void) __attribute__ ((__nothrow__ ));





extern int daemon (int __nochdir, int __noclose) __attribute__ ((__nothrow__ )) ;






extern int chroot (const char *__path) __attribute__ ((__nothrow__ )) ;



extern char *getpass (const char *__prompt) ;







extern int fsync (int __fd);





extern int syncfs (int __fd) __attribute__ ((__nothrow__ ));






extern long int gethostid (void);


extern void sync (void) __attribute__ ((__nothrow__ ));





extern int getpagesize (void) __attribute__ ((__nothrow__ )) __attribute__ ((__const__));




extern int getdtablesize (void) __attribute__ ((__nothrow__ ));
# 991 "../posix/unistd.h"
extern int truncate (const char *__file, __off_t __length)
     __attribute__ ((__nothrow__ )) ;
# 1003 "../posix/unistd.h"
extern int truncate64 (const char *__file, __off64_t __length)
     __attribute__ ((__nothrow__ )) ;
# 1014 "../posix/unistd.h"
extern int ftruncate (int __fd, __off_t __length) __attribute__ ((__nothrow__ )) ;
# 1024 "../posix/unistd.h"
extern int ftruncate64 (int __fd, __off64_t __length) __attribute__ ((__nothrow__ )) ;
# 1035 "../posix/unistd.h"
extern int brk (void *__addr) __attribute__ ((__nothrow__ )) ;





extern void *sbrk (intptr_t __delta) __attribute__ ((__nothrow__ ));
# 1056 "../posix/unistd.h"
extern long int syscall (long int __sysno, ...) __attribute__ ((__nothrow__ ));
# 1079 "../posix/unistd.h"
extern int lockf (int __fd, int __cmd, __off_t __len) ;
# 1089 "../posix/unistd.h"
extern int lockf64 (int __fd, int __cmd, __off64_t __len) ;
# 1107 "../posix/unistd.h"
ssize_t copy_file_range (int __infd, __off64_t *__pinoff,
    int __outfd, __off64_t *__poutoff,
    size_t __length, unsigned int __flags);





extern int fdatasync (int __fildes);
# 1124 "../posix/unistd.h"
extern char *crypt (const char *__key, const char *__salt)
     __attribute__ ((__nothrow__ )) ;







extern void swab (const void *__restrict __from, void *__restrict __to,
    ssize_t __n) __attribute__ ((__nothrow__ )) ;
# 1161 "../posix/unistd.h"
int getentropy (void *__buffer, size_t __length) ;








# 3 "../include/unistd.h" 2














extern __typeof (getlogin_r) __getlogin_r ;








extern int __access (const char *__name, int __type) ;
extern int __euidaccess (const char *__name, int __type);
extern int __faccessat (int __fd, const char *__file, int __type, int __flag);
extern int __faccessat_noerrno (int __fd, const char *__file, int __type,
           int __flag);
extern __off64_t __lseek64 (int __fd, __off64_t __offset, int __whence)
     ;
extern __off_t __lseek (int __fd, __off_t __offset, int __whence);

extern __off_t __libc_lseek (int __fd, __off_t __offset, int __whence);
extern __off64_t __libc_lseek64 (int __fd, __off64_t __offset, int __whence);
extern ssize_t __pread (int __fd, void *__buf, size_t __nbytes,
   __off_t __offset);
;
extern ssize_t __libc_pread (int __fd, void *__buf, size_t __nbytes,
        __off_t __offset);
extern ssize_t __pread64 (int __fd, void *__buf, size_t __nbytes,
     __off64_t __offset);
;
extern ssize_t __libc_pread64 (int __fd, void *__buf, size_t __nbytes,
          __off64_t __offset) ;
extern ssize_t __pwrite (int __fd, const void *__buf, size_t __n,
    __off_t __offset);

extern ssize_t __libc_pwrite (int __fd, const void *__buf, size_t __n,
         __off_t __offset);
extern ssize_t __pwrite64 (int __fd, const void *__buf, size_t __n,
      __off64_t __offset);

extern ssize_t __libc_pwrite64 (int __fd, const void *__buf, size_t __n,
    __off64_t __offset) ;
extern ssize_t __libc_read (int __fd, void *__buf, size_t __n);


extern ssize_t __libc_write (int __fd, const void *__buf, size_t __n);


extern int __pipe (int __pipedes[2]);

extern int __pipe2 (int __pipedes[2], int __flags) ;
extern unsigned int __sleep (unsigned int __seconds) ;
extern int __chown (const char *__file,
      __uid_t __owner, __gid_t __group);

extern int __fchown (int __fd,
       __uid_t __owner, __gid_t __group);
extern int __lchown (const char *__file, __uid_t __owner,
       __gid_t __group);
extern int __chdir (const char *__path) ;
extern int __fchdir (int __fd) ;
extern char *__getcwd (char *__buf, size_t __size) ;
extern int __rmdir (const char *__path) ;
extern int __execvpe (const char *file, char *const argv[],
        char *const envp[]) ;
extern int __execvpex (const char *file, char *const argv[],
         char *const envp[]) ;







char *__canonicalize_directory_name_internal (const char *__thisdir,
           char *__buf,
           size_t __size) ;

extern int __dup (int __fd);

extern int __dup2 (int __fd, int __fd2);

extern int __dup3 (int __fd, int __fd2, int flags);

extern int __execve (const char *__path, char *const __argv[],
       char *const __envp[]) ;
extern long int __pathconf (const char *__path, int __name);
extern long int __fpathconf (int __fd, int __name);
extern long int __sysconf (int __name);

extern __pid_t __getpid (void);

extern __pid_t __getppid (void);
extern __pid_t __setsid (void) ;
extern __uid_t __getuid (void) ;
extern __uid_t __geteuid (void) ;
extern __gid_t __getgid (void) ;
extern __gid_t __getegid (void) ;
extern int __getgroups (int __size, __gid_t __list[]) ;

extern int __group_member (__gid_t __gid) ;
extern int __setuid (__uid_t __uid);
extern int __setreuid (__uid_t __ruid, __uid_t __euid);
extern int __setgid (__gid_t __gid);
extern int __setpgid (__pid_t __pid, __pid_t __pgid);

extern int __setregid (__gid_t __rgid, __gid_t __egid);
extern int __getresuid (__uid_t *__ruid, __uid_t *__euid, __uid_t *__suid);
extern int __getresgid (__gid_t *__rgid, __gid_t *__egid, __gid_t *__sgid);
extern int __setresuid (__uid_t __ruid, __uid_t __euid, __uid_t __suid);
extern int __setresgid (__gid_t __rgid, __gid_t __egid, __gid_t __sgid);




extern __pid_t __vfork (void);

extern int __ttyname_r (int __fd, char *__buf, size_t __buflen)
     ;
extern int __isatty (int __fd) ;
extern int __link (const char *__from, const char *__to);
extern int __symlink (const char *__from, const char *__to);
extern ssize_t __readlink (const char *__path, char *__buf, size_t __len)
     ;
extern int __unlink (const char *__name) ;
extern int __gethostname (char *__name, size_t __len) ;
extern int __revoke (const char *__file);
extern int __profil (unsigned short int *__sample_buffer, size_t __size,
       size_t __offset, unsigned int __scale)
     ;
extern int __getdtablesize (void) ;
extern int __brk (void *__addr) ;
extern int __close (int __fd);

extern int __libc_close (int __fd);
extern ssize_t __read (int __fd, void *__buf, size_t __nbytes);

extern ssize_t __write (int __fd, const void *__buf, size_t __n);

extern __pid_t __fork (void);

extern int __getpagesize (void) __attribute__ ((__const__));

extern int __ftruncate (int __fd, __off_t __length) ;
extern int __ftruncate64 (int __fd, __off64_t __length) ;
extern int __truncate (const char *path, __off_t __length);
extern void *__sbrk (intptr_t __delta);








extern int __libc_enable_secure __attribute__ ((section (".data.rel.ro")));
extern int __libc_enable_secure_decided;




extern void __libc_check_standard_fds (void) ;



extern __pid_t __libc_fork (void);



extern int __libc_pause (void);

extern int __getlogin_r_loginuid (char *name, size_t namesize)
     ;
# 21 "chroot_canon.c" 2
# 1 "../include/limits.h" 1
# 183 "../include/limits.h"
# 1 "../include/bits/posix1_lim.h" 1
# 184 "../include/limits.h" 2



# 1 "../include/bits/posix2_lim.h" 1
# 188 "../include/limits.h" 2
# 22 "chroot_canon.c" 2

# 1 "../include/errno.h" 1

# 1 "../stdlib/errno.h" 1
# 28 "../stdlib/errno.h"
# 1 "../sysdeps/unix/sysv/linux/bits/errno.h" 1
# 26 "../sysdeps/unix/sysv/linux/bits/errno.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/linux/errno.h" 1 3 4
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm/errno.h" 1 3 4
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm-generic/errno.h" 1 3 4




# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm-generic/errno-base.h" 1 3 4
# 6 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm-generic/errno.h" 2 3 4
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm/errno.h" 2 3 4
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/linux/errno.h" 2 3 4
# 27 "../sysdeps/unix/sysv/linux/bits/errno.h" 2
# 29 "../stdlib/errno.h" 2








extern int *__errno_location (void) __attribute__ ((__nothrow__ )) __attribute__ ((__const__));







extern char *program_invocation_name;
extern char *program_invocation_short_name;

# 1 "../bits/types/error_t.h" 1
# 22 "../bits/types/error_t.h"
typedef int error_t;
# 49 "../stdlib/errno.h" 2




# 3 "../include/errno.h" 2
# 39 "../include/errno.h"
extern int *__errno_location (void) __attribute__ ((__nothrow__ )) __attribute__ ((__const__))



;

# 24 "chroot_canon.c" 2
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 25 "chroot_canon.c" 2


# 1 "../sysdeps/generic/eloop-threshold.h" 1
# 22 "../sysdeps/generic/eloop-threshold.h"
# 1 "../include/limits.h" 1
# 183 "../include/limits.h"
# 1 "../include/bits/posix1_lim.h" 1
# 184 "../include/limits.h" 2



# 1 "../include/bits/posix2_lim.h" 1
# 188 "../include/limits.h" 2
# 23 "../sysdeps/generic/eloop-threshold.h" 2
# 1 "../include/sys/param.h" 1
# 1 "../misc/sys/param.h" 1
# 23 "../misc/sys/param.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 24 "../misc/sys/param.h" 2

# 1 "../include/sys/types.h" 1
# 26 "../misc/sys/param.h" 2
# 1 "../include/limits.h" 1
# 183 "../include/limits.h"
# 1 "../include/bits/posix1_lim.h" 1
# 184 "../include/limits.h" 2



# 1 "../include/bits/posix2_lim.h" 1
# 188 "../include/limits.h" 2
# 27 "../misc/sys/param.h" 2
# 1 "../include/endian.h" 1
# 28 "../misc/sys/param.h" 2
# 1 "../include/signal.h" 1

# 1 "../signal/signal.h" 1
# 27 "../signal/signal.h"


# 1 "../include/bits/types.h" 1
# 30 "../signal/signal.h" 2
# 1 "../sysdeps/unix/sysv/linux/bits/signum.h" 1
# 26 "../sysdeps/unix/sysv/linux/bits/signum.h"
# 1 "../bits/signum-generic.h" 1
# 27 "../sysdeps/unix/sysv/linux/bits/signum.h" 2
# 31 "../signal/signal.h" 2

# 1 "../include/bits/types/sig_atomic_t.h" 1
# 1 "../signal/bits/types/sig_atomic_t.h" 1



# 1 "../include/bits/types.h" 1
# 5 "../signal/bits/types/sig_atomic_t.h" 2



typedef __sig_atomic_t sig_atomic_t;
# 1 "../include/bits/types/sig_atomic_t.h" 2
# 33 "../signal/signal.h" 2


# 1 "../include/bits/types/sigset_t.h" 1
# 36 "../signal/signal.h" 2
# 53 "../signal/signal.h"
# 1 "../include/bits/types/struct_timespec.h" 1
# 54 "../signal/signal.h" 2



# 1 "../sysdeps/unix/sysv/linux/bits/types/siginfo_t.h" 1



# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 5 "../sysdeps/unix/sysv/linux/bits/types/siginfo_t.h" 2
# 1 "../include/bits/types.h" 1
# 6 "../sysdeps/unix/sysv/linux/bits/types/siginfo_t.h" 2
# 1 "../include/bits/types/__sigval_t.h" 1
# 1 "../signal/bits/types/__sigval_t.h" 1
# 24 "../signal/bits/types/__sigval_t.h"
union sigval
{
  int sival_int;
  void *sival_ptr;
};

typedef union sigval __sigval_t;
# 1 "../include/bits/types/__sigval_t.h" 2
# 7 "../sysdeps/unix/sysv/linux/bits/types/siginfo_t.h" 2
# 16 "../sysdeps/unix/sysv/linux/bits/types/siginfo_t.h"
# 1 "../sysdeps/unix/sysv/linux/x86/bits/siginfo-arch.h" 1
# 17 "../sysdeps/unix/sysv/linux/bits/types/siginfo_t.h" 2
# 36 "../sysdeps/unix/sysv/linux/bits/types/siginfo_t.h"
typedef struct
  {
    int si_signo;

    int si_errno;

    int si_code;





    int __pad0;


    union
      {
 int _pad[((128 / sizeof (int)) - 4)];


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
   } _kill;


 struct
   {
     int si_tid;
     int si_overrun;
     __sigval_t si_sigval;
   } _timer;


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     __sigval_t si_sigval;
   } _rt;


 struct
   {
     __pid_t si_pid;
     __uid_t si_uid;
     int si_status;
     __clock_t si_utime;
     __clock_t si_stime;
   } _sigchld;


 struct
   {
     void *si_addr;
    
     short int si_addr_lsb;
     union
       {

  struct
    {
      void *_lower;
      void *_upper;
    } _addr_bnd;

  __uint32_t _pkey;
       } _bounds;
   } _sigfault;


 struct
   {
     long int si_band;
     int si_fd;
   } _sigpoll;



 struct
   {
     void *_call_addr;
     int _syscall;
     unsigned int _arch;
   } _sigsys;

      } _sifields;
  } siginfo_t ;
# 58 "../signal/signal.h" 2
# 1 "../sysdeps/unix/sysv/linux/bits/siginfo-consts.h" 1
# 35 "../sysdeps/unix/sysv/linux/bits/siginfo-consts.h"
enum
{
  SI_ASYNCNL = -60,
  SI_TKILL = -6,
  SI_SIGIO,

  SI_ASYNCIO,
  SI_MESGQ,
  SI_TIMER,





  SI_QUEUE,
  SI_USER,
  SI_KERNEL = 0x80
# 63 "../sysdeps/unix/sysv/linux/bits/siginfo-consts.h"
};




enum
{
  ILL_ILLOPC = 1,

  ILL_ILLOPN,

  ILL_ILLADR,

  ILL_ILLTRP,

  ILL_PRVOPC,

  ILL_PRVREG,

  ILL_COPROC,

  ILL_BADSTK

};


enum
{
  FPE_INTDIV = 1,

  FPE_INTOVF,

  FPE_FLTDIV,

  FPE_FLTOVF,

  FPE_FLTUND,

  FPE_FLTRES,

  FPE_FLTINV,

  FPE_FLTSUB

};


enum
{
  SEGV_MAPERR = 1,

  SEGV_ACCERR,

  SEGV_BNDERR,

  SEGV_PKUERR

};


enum
{
  BUS_ADRALN = 1,

  BUS_ADRERR,

  BUS_OBJERR,

  BUS_MCEERR_AR,

  BUS_MCEERR_AO

};




enum
{
  TRAP_BRKPT = 1,

  TRAP_TRACE,

  TRAP_BRANCH,

  TRAP_HWBKPT

};




enum
{
  CLD_EXITED = 1,

  CLD_KILLED,

  CLD_DUMPED,

  CLD_TRAPPED,

  CLD_STOPPED,

  CLD_CONTINUED

};


enum
{
  POLL_IN = 1,

  POLL_OUT,

  POLL_MSG,

  POLL_ERR,

  POLL_PRI,

  POLL_HUP

};





# 1 "../sysdeps/unix/sysv/linux/bits/siginfo-consts-arch.h" 1
# 193 "../sysdeps/unix/sysv/linux/bits/siginfo-consts.h" 2
# 59 "../signal/signal.h" 2



# 1 "../include/bits/types/sigval_t.h" 1
# 1 "../signal/bits/types/sigval_t.h" 1



# 1 "../include/bits/types/__sigval_t.h" 1
# 5 "../signal/bits/types/sigval_t.h" 2
# 16 "../signal/bits/types/sigval_t.h"
typedef __sigval_t sigval_t;
# 1 "../include/bits/types/sigval_t.h" 2
# 63 "../signal/signal.h" 2



# 1 "../sysdeps/unix/sysv/linux/bits/types/sigevent_t.h" 1



# 1 "../sysdeps/x86/bits/wordsize.h" 1
# 5 "../sysdeps/unix/sysv/linux/bits/types/sigevent_t.h" 2
# 1 "../include/bits/types.h" 1
# 6 "../sysdeps/unix/sysv/linux/bits/types/sigevent_t.h" 2
# 1 "../include/bits/types/__sigval_t.h" 1
# 7 "../sysdeps/unix/sysv/linux/bits/types/sigevent_t.h" 2
# 22 "../sysdeps/unix/sysv/linux/bits/types/sigevent_t.h"
typedef struct sigevent
  {
    __sigval_t sigev_value;
    int sigev_signo;
    int sigev_notify;

    union
      {
 int _pad[((64 / sizeof (int)) - 4)];



 __pid_t _tid;

 struct
   {
     void (*_function) (__sigval_t);
     pthread_attr_t *_attribute;
   } _sigev_thread;
      } _sigev_un;
  } sigevent_t;
# 67 "../signal/signal.h" 2
# 1 "../sysdeps/unix/sysv/linux/bits/sigevent-consts.h" 1
# 27 "../sysdeps/unix/sysv/linux/bits/sigevent-consts.h"
enum
{
  SIGEV_SIGNAL = 0,

  SIGEV_NONE,

  SIGEV_THREAD,


  SIGEV_THREAD_ID = 4


};
# 68 "../signal/signal.h" 2




typedef void (*__sighandler_t) (int);




extern __sighandler_t __sysv_signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__ ));

extern __sighandler_t sysv_signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__ ));






extern __sighandler_t signal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__ ));
# 112 "../signal/signal.h"
extern int kill (__pid_t __pid, int __sig) __attribute__ ((__nothrow__ ));






extern int killpg (__pid_t __pgrp, int __sig) __attribute__ ((__nothrow__ ));



extern int raise (int __sig) __attribute__ ((__nothrow__ ));



extern __sighandler_t ssignal (int __sig, __sighandler_t __handler)
     __attribute__ ((__nothrow__ ));
extern int gsignal (int __sig) __attribute__ ((__nothrow__ ));




extern void psignal (int __sig, const char *__s);


extern void psiginfo (const siginfo_t *__pinfo, const char *__s);
# 151 "../signal/signal.h"
extern int sigpause (int __sig) __asm__ ("__xpg_sigpause");
# 170 "../signal/signal.h"
extern int sigblock (int __mask) __attribute__ ((__nothrow__ )) __attribute__ ((__deprecated__));


extern int sigsetmask (int __mask) __attribute__ ((__nothrow__ )) __attribute__ ((__deprecated__));


extern int siggetmask (void) __attribute__ ((__nothrow__ )) __attribute__ ((__deprecated__));
# 185 "../signal/signal.h"
typedef __sighandler_t sighandler_t;




typedef __sighandler_t sig_t;





extern int sigemptyset (sigset_t *__set) __attribute__ ((__nothrow__ )) ;


extern int sigfillset (sigset_t *__set) __attribute__ ((__nothrow__ )) ;


extern int sigaddset (sigset_t *__set, int __signo) __attribute__ ((__nothrow__ )) ;


extern int sigdelset (sigset_t *__set, int __signo) __attribute__ ((__nothrow__ )) ;


extern int sigismember (const sigset_t *__set, int __signo)
     __attribute__ ((__nothrow__ )) ;



extern int sigisemptyset (const sigset_t *__set) __attribute__ ((__nothrow__ )) ;


extern int sigandset (sigset_t *__set, const sigset_t *__left,
        const sigset_t *__right) __attribute__ ((__nothrow__ )) ;


extern int sigorset (sigset_t *__set, const sigset_t *__left,
       const sigset_t *__right) __attribute__ ((__nothrow__ )) ;




# 1 "../sysdeps/unix/sysv/linux/bits/sigaction.h" 1
# 27 "../sysdeps/unix/sysv/linux/bits/sigaction.h"
struct sigaction
  {


    union
      {

 __sighandler_t sa_handler;

 void (*sa_sigaction) (int, siginfo_t *, void *);
      }
    __sigaction_handler;







    __sigset_t sa_mask;


    int sa_flags;


    void (*sa_restorer) (void);
  };
# 227 "../signal/signal.h" 2


extern int sigprocmask (int __how, const sigset_t *__restrict __set,
   sigset_t *__restrict __oset) __attribute__ ((__nothrow__ ));






extern int sigsuspend (const sigset_t *__set) ;


extern int sigaction (int __sig, const struct sigaction *__restrict __act,
        struct sigaction *__restrict __oact) __attribute__ ((__nothrow__ ));


extern int sigpending (sigset_t *__set) __attribute__ ((__nothrow__ )) ;







extern int sigwait (const sigset_t *__restrict __set, int *__restrict __sig)
     ;







extern int sigwaitinfo (const sigset_t *__restrict __set,
   siginfo_t *__restrict __info) ;






extern int sigtimedwait (const sigset_t *__restrict __set,
    siginfo_t *__restrict __info,
    const struct timespec *__restrict __timeout)
     ;



extern int sigqueue (__pid_t __pid, int __sig, const union sigval __val)
     __attribute__ ((__nothrow__ ));
# 286 "../signal/signal.h"
extern const char *const _sys_siglist[(64 + 1)];
extern const char *const sys_siglist[(64 + 1)];



# 1 "../sysdeps/unix/sysv/linux/x86/bits/sigcontext.h" 1
# 25 "../sysdeps/unix/sysv/linux/x86/bits/sigcontext.h"
# 1 "../include/bits/types.h" 1
# 26 "../sysdeps/unix/sysv/linux/x86/bits/sigcontext.h" 2





struct _fpx_sw_bytes
{
  __uint32_t magic1;
  __uint32_t extended_size;
  __uint64_t xstate_bv;
  __uint32_t xstate_size;
  __uint32_t __glibc_reserved1[7];
};

struct _fpreg
{
  unsigned short significand[4];
  unsigned short exponent;
};

struct _fpxreg
{
  unsigned short significand[4];
  unsigned short exponent;
  unsigned short __glibc_reserved1[3];
};

struct _xmmreg
{
  __uint32_t element[4];
};
# 123 "../sysdeps/unix/sysv/linux/x86/bits/sigcontext.h"
struct _fpstate
{

  __uint16_t cwd;
  __uint16_t swd;
  __uint16_t ftw;
  __uint16_t fop;
  __uint64_t rip;
  __uint64_t rdp;
  __uint32_t mxcsr;
  __uint32_t mxcr_mask;
  struct _fpxreg _st[8];
  struct _xmmreg _xmm[16];
  __uint32_t __glibc_reserved1[24];
};

struct sigcontext
{
  __uint64_t r8;
  __uint64_t r9;
  __uint64_t r10;
  __uint64_t r11;
  __uint64_t r12;
  __uint64_t r13;
  __uint64_t r14;
  __uint64_t r15;
  __uint64_t rdi;
  __uint64_t rsi;
  __uint64_t rbp;
  __uint64_t rbx;
  __uint64_t rdx;
  __uint64_t rax;
  __uint64_t rcx;
  __uint64_t rsp;
  __uint64_t rip;
  __uint64_t eflags;
  unsigned short cs;
  unsigned short gs;
  unsigned short fs;
  unsigned short __pad0;
  __uint64_t err;
  __uint64_t trapno;
  __uint64_t oldmask;
  __uint64_t cr2;
  __extension__ union
    {
      struct _fpstate * fpstate;
      __uint64_t __fpstate_word;
    };
  __uint64_t __reserved1 [8];
};



struct _xsave_hdr
{
  __uint64_t xstate_bv;
  __uint64_t __glibc_reserved1[2];
  __uint64_t __glibc_reserved2[5];
};

struct _ymmh_state
{
  __uint32_t ymmh_space[64];
};

struct _xstate
{
  struct _fpstate fpstate;
  struct _xsave_hdr xstate_hdr;
  struct _ymmh_state ymmh;
};
# 292 "../signal/signal.h" 2


extern int sigreturn (struct sigcontext *__scp) __attribute__ ((__nothrow__ ));






# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 302 "../signal/signal.h" 2

# 1 "../sysdeps/unix/sysv/linux/bits/types/stack_t.h" 1
# 23 "../sysdeps/unix/sysv/linux/bits/types/stack_t.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 24 "../sysdeps/unix/sysv/linux/bits/types/stack_t.h" 2


typedef struct
  {
    void *ss_sp;
    int ss_flags;
    size_t ss_size;
  } stack_t;
# 304 "../signal/signal.h" 2


# 1 "../sysdeps/unix/sysv/linux/x86/sys/ucontext.h" 1
# 23 "../sysdeps/unix/sysv/linux/x86/sys/ucontext.h"
# 1 "../include/bits/types.h" 1
# 24 "../sysdeps/unix/sysv/linux/x86/sys/ucontext.h" 2
# 1 "../include/bits/types/sigset_t.h" 1
# 25 "../sysdeps/unix/sysv/linux/x86/sys/ucontext.h" 2
# 37 "../sysdeps/unix/sysv/linux/x86/sys/ucontext.h"
__extension__ typedef long long int greg_t;
# 46 "../sysdeps/unix/sysv/linux/x86/sys/ucontext.h"
typedef greg_t gregset_t[23];



enum
{
  REG_R8 = 0,

  REG_R9,

  REG_R10,

  REG_R11,

  REG_R12,

  REG_R13,

  REG_R14,

  REG_R15,

  REG_RDI,

  REG_RSI,

  REG_RBP,

  REG_RBX,

  REG_RDX,

  REG_RAX,

  REG_RCX,

  REG_RSP,

  REG_RIP,

  REG_EFL,

  REG_CSGSFS,

  REG_ERR,

  REG_TRAPNO,

  REG_OLDMASK,

  REG_CR2

};


struct _libc_fpxreg
{
  unsigned short int significand[4];
  unsigned short int exponent;
  unsigned short int __glibc_reserved1[3];
};

struct _libc_xmmreg
{
  __uint32_t element[4];
};

struct _libc_fpstate
{

  __uint16_t cwd;
  __uint16_t swd;
  __uint16_t ftw;
  __uint16_t fop;
  __uint64_t rip;
  __uint64_t rdp;
  __uint32_t mxcsr;
  __uint32_t mxcr_mask;
  struct _libc_fpxreg _st[8];
  struct _libc_xmmreg _xmm[16];
  __uint32_t __glibc_reserved1[24];
};


typedef struct _libc_fpstate *fpregset_t;


typedef struct
  {
    gregset_t gregs;

    fpregset_t fpregs;
    __extension__ unsigned long long __reserved1 [8];
} mcontext_t;


typedef struct ucontext_t
  {
    unsigned long int uc_flags;
    struct ucontext_t *uc_link;
    stack_t uc_stack;
    mcontext_t uc_mcontext;
    sigset_t uc_sigmask;
    struct _libc_fpstate __fpregs_mem;
    __extension__ unsigned long long int __ssp[4];
  } ucontext_t;
# 307 "../signal/signal.h" 2







extern int siginterrupt (int __sig, int __interrupt) __attribute__ ((__nothrow__ ));

# 1 "../sysdeps/unix/sysv/linux/bits/sigstack.h" 1
# 317 "../signal/signal.h" 2
# 1 "../sysdeps/unix/sysv/linux/bits/ss_flags.h" 1
# 27 "../sysdeps/unix/sysv/linux/bits/ss_flags.h"
enum
{
  SS_ONSTACK = 1,

  SS_DISABLE

};
# 318 "../signal/signal.h" 2



extern int sigaltstack (const stack_t *__restrict __ss,
   stack_t *__restrict __oss) __attribute__ ((__nothrow__ ));




# 1 "../include/bits/types/struct_sigstack.h" 1
# 1 "../signal/bits/types/struct_sigstack.h" 1
# 23 "../signal/bits/types/struct_sigstack.h"
struct sigstack
  {
    void *ss_sp;
    int ss_onstack;
  };
# 1 "../include/bits/types/struct_sigstack.h" 2
# 328 "../signal/signal.h" 2







extern int sigstack (struct sigstack *__ss, struct sigstack *__oss)
     __attribute__ ((__nothrow__ )) __attribute__ ((__deprecated__));






extern int sighold (int __sig) __attribute__ ((__nothrow__ ));


extern int sigrelse (int __sig) __attribute__ ((__nothrow__ ));


extern int sigignore (int __sig) __attribute__ ((__nothrow__ ));


extern __sighandler_t sigset (int __sig, __sighandler_t __disp) __attribute__ ((__nothrow__ ));






# 1 "../sysdeps/pthread/bits/sigthread.h" 1
# 31 "../sysdeps/pthread/bits/sigthread.h"
extern int pthread_sigmask (int __how,
       const __sigset_t *__restrict __newmask,
       __sigset_t *__restrict __oldmask)__attribute__ ((__nothrow__ ));


extern int pthread_kill (pthread_t __threadid, int __signo) __attribute__ ((__nothrow__ ));



extern int pthread_sigqueue (pthread_t __threadid, int __signo,
        const union sigval __value) __attribute__ ((__nothrow__ ));
# 360 "../signal/signal.h" 2






extern int __libc_current_sigrtmin (void) __attribute__ ((__nothrow__ ));

extern int __libc_current_sigrtmax (void) __attribute__ ((__nothrow__ ));





# 3 "../include/signal.h" 2







extern int __sigpause (int sig_or_mask, int is_sig);







extern __sighandler_t __bsd_signal (int __sig, __sighandler_t __handler);
extern int __kill (__pid_t __pid, int __sig);

extern int __sigaction (int __sig, const struct sigaction *__restrict __act,
   struct sigaction *__restrict __oact);

extern int __sigblock (int __mask);

extern int __sigsetmask (int __mask);
extern int __sigprocmask (int __how,
     const sigset_t *__set, sigset_t *__oset);

extern int __sigsuspend (const sigset_t *__set);

extern int __sigwait (const sigset_t *__set, int *__sig);

extern int __sigwaitinfo (const sigset_t *__set, siginfo_t *__info);

extern int __sigtimedwait (const sigset_t *__set, siginfo_t *__info,
      const struct timespec *__timeout);

extern int __sigqueue (__pid_t __pid, int __sig,
         const union sigval __val);

extern int __sigreturn (struct sigcontext *__scp);

extern int __sigaltstack (const stack_t *__ss,
     stack_t *__oss);

extern int __libc_sigaction (int sig, const struct sigaction *act,
        struct sigaction *oact);


extern int __default_sigpause (int mask);
extern int __xpg_sigpause (int sig);


extern int __libc_allocate_rtsig (int __high);
# 29 "../misc/sys/param.h" 2


# 1 "../sysdeps/unix/sysv/linux/bits/param.h" 1
# 28 "../sysdeps/unix/sysv/linux/bits/param.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/linux/param.h" 1 3 4




# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm/param.h" 1 3 4
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm-generic/param.h" 1 3 4
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/asm/param.h" 2 3 4
# 6 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot/usr/include/linux/param.h" 2 3 4
# 29 "../sysdeps/unix/sysv/linux/bits/param.h" 2
# 32 "../misc/sys/param.h" 2
# 1 "../include/sys/param.h" 2
# 24 "../sysdeps/generic/eloop-threshold.h" 2
# 51 "../sysdeps/generic/eloop-threshold.h"
static inline unsigned int __attribute__ ((const))
__eloop_threshold (void)
{







  static long int sysconf_symloop_max;
  if (sysconf_symloop_max == 0)
    sysconf_symloop_max = __sysconf (_SC_SYMLOOP_MAX);
  const unsigned int symloop_max = (sysconf_symloop_max <= 0
                                    ? 8
                                    : sysconf_symloop_max);


  return (((symloop_max)>(40))?(symloop_max):(40));
}
# 28 "chroot_canon.c" 2
# 1 "../sysdeps/unix/sysv/linux/x86_64/ldconfig.h" 1
# 18 "../sysdeps/unix/sysv/linux/x86_64/ldconfig.h"
# 1 "../sysdeps/generic/ldconfig.h" 1
# 52 "../sysdeps/generic/ldconfig.h"
extern void print_cache (const char *cache_name);

extern void init_cache (void);

extern void save_cache (const char *cache_name);

extern void add_to_cache (const char *path, const char *lib, int flags,
     unsigned int osversion, uint64_t hwcap);

extern void init_aux_cache (void);

extern void load_aux_cache (const char *aux_cache_name);

extern int search_aux_cache (struct stat64 *stat_buf, int *flags,
        unsigned int *osversion, char **soname);

extern void add_to_aux_cache (struct stat64 *stat_buf, int flags,
         unsigned int osversion, const char *soname);

extern void save_aux_cache (const char *aux_cache_name);


extern int process_file (const char *real_file_name, const char *file_name,
    const char *lib, int *flag, unsigned int *osversion,
    char **soname, int is_link, struct stat64 *stat_buf);

extern char *implicit_soname (const char *lib, int flag);


extern int process_elf_file (const char *file_name, const char *lib, int *flag,
        unsigned int *osversion, char **soname,
        void *file_contents, size_t file_length);


extern char *chroot_canon (const char *chroot, const char *name);


extern int opt_verbose;

extern int opt_format;


# 1 "../include/programs/xmalloc.h" 1
# 22 "../include/programs/xmalloc.h"
# 1 "/home/flaig/EB_2.1/build/tmp/work/corei7-64-poky-linux/glibc/2.28-r0/recipe-sysroot-native/usr/lib/x86_64-poky-linux/gcc/x86_64-poky-linux/8.3.0/include/stddef.h" 1 3 4
# 23 "../include/programs/xmalloc.h" 2


extern void *xmalloc (size_t n)
  __attribute__ ((__malloc__)) __attribute__ ((__alloc_size__ (1)));
extern void *xcalloc (size_t n, size_t s)
  __attribute__ ((__malloc__)) __attribute__ ((__alloc_size__ (1, 2)));
extern void *xrealloc (void *o, size_t n)
  __attribute__ ((__malloc__)) __attribute__ ((__alloc_size__ (2)));
extern char *xstrdup (const char *) __attribute__ ((__malloc__));
# 95 "../sysdeps/generic/ldconfig.h" 2
# 19 "../sysdeps/unix/sysv/linux/x86_64/ldconfig.h" 2
# 29 "chroot_canon.c" 2
# 40 "chroot_canon.c"
char *
chroot_canon (const char *chroot, const char *name)
{
  char *rpath;
  char *dest;
  char *extra_buf = 
# 45 "chroot_canon.c" 3 4
                   ((void *)0)
# 45 "chroot_canon.c"
                       ;
  char *rpath_root;
  const char *start;
  const char *end;
  const char *rpath_limit;
  int num_links = 0;
  size_t chroot_len = strlen (chroot);

  if (chroot_len < 1)
    {
      ((*__errno_location ()) = (
# 55 "chroot_canon.c" 3 4
     22
# 55 "chroot_canon.c"
     ));
      return 
# 56 "chroot_canon.c" 3 4
            ((void *)0)
# 56 "chroot_canon.c"
                ;
    }

  rpath = xmalloc (chroot_len + 
# 59 "chroot_canon.c" 3 4
                               4096
# 59 "chroot_canon.c"
                                       );

  rpath_limit = rpath + chroot_len + 
# 61 "chroot_canon.c" 3 4
                                    4096
# 61 "chroot_canon.c"
                                            ;

  rpath_root = (char *) mempcpy (rpath, chroot, chroot_len) - 1;
  if (*rpath_root != '/')
    *++rpath_root = '/';
  dest = rpath_root + 1;

  for (start = end = name; *start; start = end)
    {
      struct stat64 st;


      while (*start == '/')
 ++start;


      for (end = start; *end && *end != '/'; ++end)
                ;

      if (end - start == 0)
 break;
      else if (end - start == 1 && start[0] == '.')
              ;
      else if (end - start == 2 && start[0] == '.' && start[1] == '.')
 {

   if (dest > rpath_root + 1)
     while ((--dest)[-1] != '/');
 }
      else
 {
   size_t new_size;

   if (dest[-1] != '/')
     *dest++ = '/';

   if (dest + (end - start) >= rpath_limit)
     {
       ptrdiff_t dest_offset = dest - rpath;
       char *new_rpath;

       new_size = rpath_limit - rpath;
       if (end - start + 1 > 
# 103 "chroot_canon.c" 3 4
                            4096
# 103 "chroot_canon.c"
                                    )
  new_size += end - start + 1;
       else
  new_size += 
# 106 "chroot_canon.c" 3 4
             4096
# 106 "chroot_canon.c"
                     ;
       new_rpath = (char *) xrealloc (rpath, new_size);
       rpath = new_rpath;
       rpath_limit = rpath + new_size;

       dest = rpath + dest_offset;
     }

   dest = mempcpy (dest, start, end - start);
   *dest = '\0';

   if (__lxstat64 (1, rpath, &st) < 0)
     {
       if (*end == '\0')
  goto done;
       goto error;
     }

   if (((((st.st_mode)) & 0170000) == (0120000)))
     {
       char *buf = __builtin_alloca (
# 126 "chroot_canon.c" 3 4
                  4096
# 126 "chroot_canon.c"
                  );
       size_t len;

       if (++num_links > __eloop_threshold ())
  {
    ((*__errno_location ()) = (
# 131 "chroot_canon.c" 3 4
   40
# 131 "chroot_canon.c"
   ));
    goto error;
  }

       ssize_t n = readlink (rpath, buf, 
# 135 "chroot_canon.c" 3 4
                                        4096 
# 135 "chroot_canon.c"
                                                 - 1);
       if (n < 0)
  {
    if (*end == '\0')
      goto done;
    goto error;
  }
       buf[n] = '\0';

       if (!extra_buf)
  extra_buf = __builtin_alloca (
# 145 "chroot_canon.c" 3 4
             4096
# 145 "chroot_canon.c"
             );

       len = strlen (end);
       if (len >= 
# 148 "chroot_canon.c" 3 4
                 4096 
# 148 "chroot_canon.c"
                          - n)
  {
    ((*__errno_location ()) = (
# 150 "chroot_canon.c" 3 4
   36
# 150 "chroot_canon.c"
   ));
    goto error;
  }


       memmove (&extra_buf[n], end, len + 1);
       name = end = memcpy (extra_buf, buf, n);

       if (buf[0] == '/')
  dest = rpath_root + 1;
       else

  if (dest > rpath_root + 1)
    while ((--dest)[-1] != '/');
     }
 }
    }
 done:
  if (dest > rpath_root + 1 && dest[-1] == '/')
    --dest;
  *dest = '\0';

  return rpath;

 error:
  free (rpath);
  return 
# 176 "chroot_canon.c" 3 4
        ((void *)0)
# 176 "chroot_canon.c"
            ;
}
