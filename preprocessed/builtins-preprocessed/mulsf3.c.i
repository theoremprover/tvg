# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/mulsf3.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 369 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/mulsf3.c" 2
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/mulsf3.c"
# 1 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_mul_impl.inc" 1
# 15 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_mul_impl.inc"
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
# 43 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h"
typedef uint32_t rep_t;
typedef int32_t srep_t;
typedef float fp_t;



static __inline int rep_clz(rep_t a) {
    return __builtin_clz(a);
}


static __inline void wideMultiply(rep_t a, rep_t b, rep_t *hi, rep_t *lo) {
    const uint64_t product = (uint64_t)a*b;
    *hi = product >> 32;
    *lo = product;
}
                fp_t __addsf3(fp_t a, fp_t b);
# 231 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_lib.h"
static __inline rep_t toRep(fp_t x) {
    const union { fp_t f; rep_t i; } rep = {.f = x};
    return rep.i;
}

static __inline fp_t fromRep(rep_t x) {
    const union { fp_t f; rep_t i; } rep = {.i = x};
    return rep.f;
}

static __inline int normalize(rep_t *significand) {
    const int shift = rep_clz(*significand) - rep_clz((1U << 23));
    *significand <<= shift;
    return 1 - shift;
}

static __inline void wideLeftShift(rep_t *hi, rep_t *lo, int count) {
    *hi = *hi << count | *lo >> ((sizeof(rep_t)*8) - count);
    *lo = *lo << count;
}

static __inline void wideRightShiftWithSticky(rep_t *hi, rep_t *lo, unsigned int count) {
    if (count < (sizeof(rep_t)*8)) {
        const _Bool sticky = *lo << ((sizeof(rep_t)*8) - count);
        *lo = *hi << ((sizeof(rep_t)*8) - count) | *lo >> count | sticky;
        *hi = *hi >> count;
    }
    else if (count < 2*(sizeof(rep_t)*8)) {
        const _Bool sticky = *hi << (2*(sizeof(rep_t)*8) - count) | *lo;
        *lo = *hi >> (count - (sizeof(rep_t)*8)) | sticky;
        *hi = 0;
    } else {
        const _Bool sticky = *hi | *lo;
        *lo = sticky;
        *hi = 0;
    }
}
# 16 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/fp_mul_impl.inc" 2

static __inline fp_t __mulXf3__(fp_t a, fp_t b) {
    const unsigned int aExponent = toRep(a) >> 23 & ((1 << ((sizeof(rep_t)*8) - 23 - 1)) - 1);
    const unsigned int bExponent = toRep(b) >> 23 & ((1 << ((sizeof(rep_t)*8) - 23 - 1)) - 1);
    const rep_t productSign = (toRep(a) ^ toRep(b)) & (1U << (23 + ((sizeof(rep_t)*8) - 23 - 1)));

    rep_t aSignificand = toRep(a) & ((1U << 23) - 1U);
    rep_t bSignificand = toRep(b) & ((1U << 23) - 1U);
    int scale = 0;


    if (aExponent-1U >= ((1 << ((sizeof(rep_t)*8) - 23 - 1)) - 1)-1U || bExponent-1U >= ((1 << ((sizeof(rep_t)*8) - 23 - 1)) - 1)-1U) {

        const rep_t aAbs = toRep(a) & ((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U);
        const rep_t bAbs = toRep(b) & ((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U);


        if (aAbs > (((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U) ^ ((1U << 23) - 1U))) return fromRep(toRep(a) | ((1U << 23) >> 1));

        if (bAbs > (((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U) ^ ((1U << 23) - 1U))) return fromRep(toRep(b) | ((1U << 23) >> 1));

        if (aAbs == (((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U) ^ ((1U << 23) - 1U))) {

            if (bAbs) return fromRep(aAbs | productSign);

            else return fromRep(((((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U) ^ ((1U << 23) - 1U)) | ((1U << 23) >> 1)));
        }

        if (bAbs == (((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U) ^ ((1U << 23) - 1U))) {

            if (aAbs) return fromRep(bAbs | productSign);

            else return fromRep(((((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U) ^ ((1U << 23) - 1U)) | ((1U << 23) >> 1)));
        }


        if (!aAbs) return fromRep(productSign);

        if (!bAbs) return fromRep(productSign);




        if (aAbs < (1U << 23)) scale += normalize(&aSignificand);
        if (bAbs < (1U << 23)) scale += normalize(&bSignificand);
    }




    aSignificand |= (1U << 23);
    bSignificand |= (1U << 23);






    rep_t productHi, productLo;
    wideMultiply(aSignificand, bSignificand << ((sizeof(rep_t)*8) - 23 - 1),
                 &productHi, &productLo);

    int productExponent = aExponent + bExponent - (((1 << ((sizeof(rep_t)*8) - 23 - 1)) - 1) >> 1) + scale;


    if (productHi & (1U << 23)) productExponent++;
    else wideLeftShift(&productHi, &productLo, 1);


    if (productExponent >= ((1 << ((sizeof(rep_t)*8) - 23 - 1)) - 1)) return fromRep((((1U << (23 + ((sizeof(rep_t)*8) - 23 - 1))) - 1U) ^ ((1U << 23) - 1U)) | productSign);

    if (productExponent <= 0) {






        const unsigned int shift = 1U - (unsigned int)productExponent;
        if (shift >= (sizeof(rep_t)*8)) return fromRep(productSign);



        wideRightShiftWithSticky(&productHi, &productLo, shift);
    }
    else {

        productHi &= ((1U << 23) - 1U);
        productHi |= (rep_t)productExponent << 23;
    }


    productHi |= productSign;




    if (productLo > (1U << (23 + ((sizeof(rep_t)*8) - 23 - 1)))) productHi++;
    if (productLo == (1U << (23 + ((sizeof(rep_t)*8) - 23 - 1)))) productHi += productHi & 1;
    return fromRep(productHi);
}
# 17 "/arm-libs/library-src/llvm-project/compiler-rt/lib/builtins/mulsf3.c" 2

                fp_t __mulsf3(fp_t a, fp_t b) {
    return __mulXf3__(a, b);
}



__attribute__((__pcs__("aapcs"))) fp_t __aeabi_fmul(fp_t a, fp_t b) {
  return __mulsf3(a, b);
}
