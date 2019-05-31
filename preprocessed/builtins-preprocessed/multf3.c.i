# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/multf3.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 369 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/multf3.c" 2
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/multf3.c"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h" 1
# 24 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h"
# 1 "/toolchain/arm/include/stdint.h" 1 3




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
# 6 "/toolchain/arm/include/stdint.h" 2 3
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
# 25 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h" 2
# 1 "/toolchain/arm/include/stdbool.h" 1 3
# 26 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h" 2
# 1 "/toolchain/arm/include/limits.h" 1 3
# 27 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h" 2
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 1
# 72 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h"
# 1 "/toolchain/arm/include/stdint.h" 1 3
# 73 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/int_lib.h" 2

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
# 28 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h" 2
# 17 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/multf3.c" 2

