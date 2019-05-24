# 1 "/arm-libs/library-src/dinkum/source/./fegetround.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./fegetround.c" 2

# 1 "/toolchain/arm/include/fenv.h" 1 3




# 1 "/toolchain/arm/include/ymath.h" 1 3



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
# 6 "/toolchain/arm/include/fenv.h" 2 3
# 144 "/toolchain/arm/include/fenv.h" 3
typedef unsigned long fexcept_t;
typedef unsigned long fenv_t;
# 272 "/toolchain/arm/include/fenv.h" 3
int feclearexcept(int);
int fegetexceptflag(fexcept_t *, int);
int feraiseexcept(int);
int fesetexceptflag(const fexcept_t *, int);
int fetestexcept(int);
int fegetround(void);
int fesetround(int);
int fegetenv(fenv_t *);
int feholdexcept(fenv_t *);
int fesetenv(const fenv_t *);
int feupdateenv(const fenv_t *);


fexcept_t fegettrapenable(void);
int fesettrapenable(fexcept_t);


double _Force_raise(int except);


extern const fenv_t _Fenv0;
# 3 "/arm-libs/library-src/dinkum/source/./fegetround.c" 2
# 1 "/toolchain/arm/include/float.h" 1 3
# 4 "/arm-libs/library-src/dinkum/source/./fegetround.c" 2
# 24 "/arm-libs/library-src/dinkum/source/./fegetround.c"
int (fegetround)()
 {
  fexcept_t control;

  fegetenv(&control);
  return ((int)(control >> 22) & 0x03u);
  }
