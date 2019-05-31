# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/truncdfhf2.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 369 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/truncdfhf2.c" 2
# 12 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/truncdfhf2.c"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc_impl.inc" 1
# 40 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc_impl.inc"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc.h" 1
# 17 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc.h"
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
# 18 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc.h" 2








typedef double src_t;
typedef uint64_t src_rep_t;

static const int srcSigBits = 52;
# 54 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc.h"
typedef uint16_t dst_t;
typedef uint16_t dst_rep_t;

static const int dstSigBits = 10;
# 66 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc.h"
static __inline src_rep_t srcToRep(src_t x) {
    const union { src_t f; src_rep_t i; } rep = {.f = x};
    return rep.i;
}

static __inline dst_t dstFromRep(dst_rep_t x) {
    const union { dst_t f; dst_rep_t i; } rep = {.i = x};
    return rep.f;
}
# 41 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_trunc_impl.inc" 2

static __inline dst_t __truncXfYf2__(src_t a) {


    const int srcBits = sizeof(src_t)*8;
    const int srcExpBits = srcBits - srcSigBits - 1;
    const int srcInfExp = (1 << srcExpBits) - 1;
    const int srcExpBias = srcInfExp >> 1;

    const src_rep_t srcMinNormal = 1ULL << srcSigBits;
    const src_rep_t srcSignificandMask = srcMinNormal - 1;
    const src_rep_t srcInfinity = (src_rep_t)srcInfExp << srcSigBits;
    const src_rep_t srcSignMask = 1ULL << (srcSigBits + srcExpBits);
    const src_rep_t srcAbsMask = srcSignMask - 1;
    const src_rep_t roundMask = (1ULL << (srcSigBits - dstSigBits)) - 1;
    const src_rep_t halfway = 1ULL << (srcSigBits - dstSigBits - 1);
    const src_rep_t srcQNaN = 1ULL << (srcSigBits - 1);
    const src_rep_t srcNaNCode = srcQNaN - 1;

    const int dstBits = sizeof(dst_t)*8;
    const int dstExpBits = dstBits - dstSigBits - 1;
    const int dstInfExp = (1 << dstExpBits) - 1;
    const int dstExpBias = dstInfExp >> 1;

    const int underflowExponent = srcExpBias + 1 - dstExpBias;
    const int overflowExponent = srcExpBias + dstInfExp - dstExpBias;
    const src_rep_t underflow = (src_rep_t)underflowExponent << srcSigBits;
    const src_rep_t overflow = (src_rep_t)overflowExponent << srcSigBits;

    const dst_rep_t dstQNaN = 1 << (dstSigBits - 1);
    const dst_rep_t dstNaNCode = dstQNaN - 1;


    const src_rep_t aRep = srcToRep(a);
    const src_rep_t aAbs = aRep & srcAbsMask;
    const src_rep_t sign = aRep & srcSignMask;
    dst_rep_t absResult;

    if (aAbs - underflow < aAbs - overflow) {



        absResult = aAbs >> (srcSigBits - dstSigBits);
        absResult -= (dst_rep_t)(srcExpBias - dstExpBias) << dstSigBits;

        const src_rep_t roundBits = aAbs & roundMask;

        if (roundBits > halfway)
            absResult++;

        else if (roundBits == halfway)
            absResult += absResult & 1;
    }
    else if (aAbs > srcInfinity) {



        absResult = (dst_rep_t)dstInfExp << dstSigBits;
        absResult |= dstQNaN;
        absResult |= ((aAbs & srcNaNCode) >> (srcSigBits - dstSigBits)) & dstNaNCode;
    }
    else if (aAbs >= overflow) {

        absResult = (dst_rep_t)dstInfExp << dstSigBits;
    }
    else {



        const int aExp = aAbs >> srcSigBits;
        const int shift = srcExpBias - dstExpBias - aExp + 1;

        const src_rep_t significand = (aRep & srcSignificandMask) | srcMinNormal;


        if (shift > srcSigBits) {
            absResult = 0;
        } else {
            const _Bool sticky = significand << (srcBits - shift);
            src_rep_t denormalizedSignificand = significand >> shift | sticky;
            absResult = denormalizedSignificand >> (srcSigBits - dstSigBits);
            const src_rep_t roundBits = denormalizedSignificand & roundMask;

            if (roundBits > halfway)
                absResult++;

            else if (roundBits == halfway)
                absResult += absResult & 1;
        }
    }


    const dst_rep_t result = absResult | sign >> (srcBits - dstBits);
    return dstFromRep(result);
}
# 13 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/truncdfhf2.c" 2

                uint16_t __truncdfhf2(double a) {
    return __truncXfYf2__(a);
}



__attribute__((__pcs__("aapcs"))) uint16_t __aeabi_d2h(double a) {
  return __truncdfhf2(a);
}
