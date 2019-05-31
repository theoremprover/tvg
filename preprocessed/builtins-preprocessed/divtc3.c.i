# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/divtc3.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 369 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/divtc3.c" 2
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/divtc3.c"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 1
# 71 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h"
# 1 "/toolchain/arm/include/limits.h" 1 3




# 1 "/toolchain/arm/include/yvals.h" 1 3


# 1 "/toolchain/arm/include/xkeycheck.h" 1 3
# 4 "/toolchain/arm/include/yvals.h" 2 3
# 462 "/toolchain/arm/include/yvals.h" 3
typedef long _Int32t;
typedef unsigned long _Uint32t;



  typedef int _Ptrdifft;
# 477 "/toolchain/arm/include/yvals.h" 3
typedef unsigned int _Sizet;
# 1151 "/toolchain/arm/include/yvals.h" 3
# 1 "/toolchain/arm/include/stdarg.h" 1 3
# 10 "/toolchain/arm/include/stdarg.h" 3
typedef __builtin_va_list va_list;
# 1152 "/toolchain/arm/include/yvals.h" 2 3
# 1278 "/toolchain/arm/include/yvals.h" 3
typedef long long _Longlong;
typedef unsigned long long _ULonglong;
# 1337 "/toolchain/arm/include/yvals.h" 3
typedef unsigned int _Wchart;
typedef unsigned int _Wintt;
# 1371 "/toolchain/arm/include/yvals.h" 3
typedef va_list _Va_list;
# 1394 "/toolchain/arm/include/yvals.h" 3
void _Atexit(void (*)(void));
# 1409 "/toolchain/arm/include/yvals.h" 3
typedef char _Sysch_t;
# 6 "/toolchain/arm/include/limits.h" 2 3
# 72 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2
# 1 "/toolchain/arm/include/stdint.h" 1 3
# 25 "/toolchain/arm/include/stdint.h" 3
typedef signed char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int int64_t;

typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef long long unsigned int uint64_t;

typedef signed char int_least8_t;
typedef short int_least16_t;
typedef int int_least32_t;
typedef long long int int_least64_t;

typedef unsigned char uint_least8_t;
typedef unsigned short uint_least16_t;
typedef unsigned int uint_least32_t;
typedef long long unsigned int uint_least64_t;

typedef signed char int_fast8_t;
typedef short int_fast16_t;
typedef int int_fast32_t;
typedef long long int int_fast64_t;

typedef unsigned char uint_fast8_t;
typedef unsigned short uint_fast16_t;
typedef unsigned int uint_fast32_t;
typedef long long unsigned int uint_fast64_t;

typedef int intptr_t;
typedef unsigned int uintptr_t;

typedef long long int intmax_t;
typedef long long unsigned int uintmax_t;
# 73 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2
# 1 "/toolchain/arm/include/stdbool.h" 1 3
# 74 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2
# 1 "/toolchain/arm/include/float.h" 1 3



# 1 "/toolchain/arm/include/ymath.h" 1 3



# 1 "/toolchain/arm/include/yvals.h" 1 3
# 5 "/toolchain/arm/include/ymath.h" 2 3
# 126 "/toolchain/arm/include/ymath.h" 3
void _Feraise(int);

typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } _Dconst;


double _Cosh(double, double);
short _Dtest(double *);
double _Sinh(double, double);
double _Divide(double, double);
short _Exp(double *, double, long);
double _Log(double, int);
double _Recip(double);
double _Sin(double, unsigned int);
double _Sinx(double, unsigned int, int);

extern const _Dconst _Denorm, _Hugeval, _Inf,
 _Nan, _Snan;


float _FCosh(float, float);
short _FDtest(float *);
float _FSinh(float, float);
float _FDivide(float, float);
short _FExp(float *, float, long);
float _FLog(float, int);
float _FRecip(float);
float _FSin(float, unsigned int);
float _FSinx(float, unsigned int, int);

extern const _Dconst _FDenorm, _FInf, _FNan, _FSnan;


long double _LCosh(long double, long double);
short _LDtest(long double *);
long double _LSinh(long double, long double);
long double _LDivide(long double, long double);
short _LExp(long double *, long double, long);
long double _LLog(long double, int);
long double _LRecip(long double);
long double _LSin(long double, unsigned int);
long double _LSinx(long double, unsigned int, int);

extern const _Dconst _LDenorm, _LInf, _LNan, _LSnan;
# 5 "/toolchain/arm/include/float.h" 2 3
# 75 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2



# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h" 1
# 21 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_endianness.h" 1
# 22 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h" 2





typedef int si_int;
typedef unsigned su_int;

typedef long long di_int;
typedef unsigned long long du_int;

typedef union
{
    di_int all;
    struct
    {

        su_int low;
        si_int high;




    }s;
} dwords;

typedef union
{
    du_int all;
    struct
    {

        su_int low;
        su_int high;




    }s;
} udwords;
# 117 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_types.h"
typedef union
{
    su_int u;
    float f;
} float_bits;

typedef union
{
    udwords u;
    double f;
} double_bits;

typedef struct
{

    udwords low;
    udwords high;




} uqwords;

typedef union
{
    uqwords u;
    long double f;
} long_double_bits;


typedef float _Complex Fcomplex;
typedef double _Complex Dcomplex;
typedef long double _Complex Lcomplex;
# 79 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2


# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_util.h" 1
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_util.h"
__attribute__((noreturn)) void compilerrt_abort_impl(const char *file, int line,
                                    const char *function);
# 82 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2

                si_int __paritysi2(si_int a);
                si_int __paritydi2(di_int a);

                di_int __divdi3(di_int a, di_int b);
                si_int __divsi3(si_int a, si_int b);
                su_int __udivsi3(su_int n, su_int d);

                su_int __udivmodsi4(su_int a, su_int b, su_int* rem);
                du_int __udivmoddi4(du_int a, du_int b, du_int* rem);
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/divtc3.c" 2
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_math.h" 1
# 17 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/divtc3.c" 2



                Lcomplex
__divtc3(long double __a, long double __b, long double __c, long double __d)
{
    int __ilogbw = 0;
    long double __logbw = __builtin_logbl((__builtin_fmaxl((__builtin_fabsl((__c))), (__builtin_fabsl((__d))))));
    if (__builtin_isfinite((__logbw)))
    {
        __ilogbw = (int)__logbw;
        __c = __builtin_scalbnl((__c), (-__ilogbw));
        __d = __builtin_scalbnl((__d), (-__ilogbw));
    }
    long double __denom = __c * __c + __d * __d;
    Lcomplex z;
    __real__(z) = __builtin_scalbnl(((__a * __c + __b * __d) / __denom), (-__ilogbw));
    __imag__(z) = __builtin_scalbnl(((__b * __c - __a * __d) / __denom), (-__ilogbw));
    if (__builtin_isnan((__real__(z))) && __builtin_isnan((__imag__(z))))
    {
        if ((__denom == 0.0) && (!__builtin_isnan((__a)) || !__builtin_isnan((__b))))
        {
            __real__(z) = __builtin_copysignl((__builtin_huge_valf()), (__c)) * __a;
            __imag__(z) = __builtin_copysignl((__builtin_huge_valf()), (__c)) * __b;
        }
        else if ((__builtin_isinf((__a)) || __builtin_isinf((__b))) &&
                 __builtin_isfinite((__c)) && __builtin_isfinite((__d)))
        {
            __a = __builtin_copysignl((__builtin_isinf((__a)) ? 1.0 : 0.0), (__a));
            __b = __builtin_copysignl((__builtin_isinf((__b)) ? 1.0 : 0.0), (__b));
            __real__(z) = __builtin_huge_valf() * (__a * __c + __b * __d);
            __imag__(z) = __builtin_huge_valf() * (__b * __c - __a * __d);
        }
        else if (__builtin_isinf((__logbw)) && __logbw > 0.0 &&
                 __builtin_isfinite((__a)) && __builtin_isfinite((__b)))
        {
            __c = __builtin_copysignl((__builtin_isinf((__c)) ? 1.0 : 0.0), (__c));
            __d = __builtin_copysignl((__builtin_isinf((__d)) ? 1.0 : 0.0), (__d));
            __real__(z) = 0.0 * (__a * __c + __b * __d);
            __imag__(z) = 0.0 * (__b * __c - __a * __d);
        }
    }
    return z;
}
