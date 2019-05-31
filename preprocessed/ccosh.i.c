# 1 "/arm-libs/library-src/dinkum/source/./ccosh.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./ccosh.c" 2

# 1 "/arm-libs/library-src/dinkum/source/./xxdftype.h" 1

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
# 3 "/arm-libs/library-src/dinkum/source/./xxdftype.h" 2
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
# 4 "/arm-libs/library-src/dinkum/source/./xxdftype.h" 2
# 3 "/arm-libs/library-src/dinkum/source/./ccosh.c" 2
# 1 "/arm-libs/library-src/dinkum/source/./xxccosh.h" 1

# 1 "/toolchain/arm/include/complex.h" 1 3



# 1 "/toolchain/arm/include/yvals.h" 1 3
# 5 "/toolchain/arm/include/complex.h" 2 3
# 87 "/toolchain/arm/include/complex.h" 3
typedef double _Complex _Dcomplex;
typedef float _Complex _Fcomplex;
typedef long double _Complex _Lcomplex;
# 161 "/toolchain/arm/include/complex.h" 3
double cimag(_Dcomplex);
double creal(_Dcomplex);
float cimagf(_Fcomplex);
float crealf(_Fcomplex);
long double cimagl(_Lcomplex);
long double creall(_Lcomplex);
# 186 "/toolchain/arm/include/complex.h" 3
double cabs(_Dcomplex);
_Dcomplex cacos(_Dcomplex);
_Dcomplex cacosh(_Dcomplex);
double carg(_Dcomplex);
_Dcomplex casin(_Dcomplex);
_Dcomplex casinh(_Dcomplex);
_Dcomplex catan(_Dcomplex);
_Dcomplex catanh(_Dcomplex);
_Dcomplex ccos(_Dcomplex);
_Dcomplex ccosh(_Dcomplex);
_Dcomplex cexp(_Dcomplex);


_Dcomplex clog10(_Dcomplex);
_Dcomplex conj(_Dcomplex);
_Dcomplex cpow(_Dcomplex, _Dcomplex);
_Dcomplex cproj(_Dcomplex);

_Dcomplex csin(_Dcomplex);
_Dcomplex csinh(_Dcomplex);
_Dcomplex csqrt(_Dcomplex);
_Dcomplex ctan(_Dcomplex);
_Dcomplex ctanh(_Dcomplex);
double norm(_Dcomplex);

float cabsf(_Fcomplex);
_Fcomplex cacosf(_Fcomplex);
_Fcomplex cacoshf(_Fcomplex);
float cargf(_Fcomplex);
_Fcomplex casinf(_Fcomplex);
_Fcomplex casinhf(_Fcomplex);
_Fcomplex catanf(_Fcomplex);
_Fcomplex catanhf(_Fcomplex);
_Fcomplex ccosf(_Fcomplex);
_Fcomplex ccoshf(_Fcomplex);
_Fcomplex cexpf(_Fcomplex);

_Fcomplex clogf(_Fcomplex);
_Fcomplex clog10f(_Fcomplex);
_Fcomplex conjf(_Fcomplex);
_Fcomplex cpowf(_Fcomplex, _Fcomplex);
_Fcomplex cprojf(_Fcomplex);

_Fcomplex csinf(_Fcomplex);
_Fcomplex csinhf(_Fcomplex);
_Fcomplex csqrtf(_Fcomplex);
_Fcomplex ctanf(_Fcomplex);
_Fcomplex ctanhf(_Fcomplex);
float normf(_Fcomplex);

long double cabsl(_Lcomplex);
_Lcomplex cacosl(_Lcomplex);
_Lcomplex cacoshl(_Lcomplex);
long double cargl(_Lcomplex);
_Lcomplex casinl(_Lcomplex);
_Lcomplex casinhl(_Lcomplex);
_Lcomplex catanl(_Lcomplex);
_Lcomplex catanhl(_Lcomplex);
_Lcomplex ccosl(_Lcomplex);
_Lcomplex ccoshl(_Lcomplex);
_Lcomplex cexpl(_Lcomplex);

_Lcomplex clogl(_Lcomplex);
_Lcomplex clog10l(_Lcomplex);
_Lcomplex conjl(_Lcomplex);
_Lcomplex cpowl(_Lcomplex, _Lcomplex);
_Lcomplex cprojl(_Lcomplex);

_Lcomplex csinl(_Lcomplex);
_Lcomplex csinhl(_Lcomplex);
_Lcomplex csqrtl(_Lcomplex);
_Lcomplex ctanl(_Lcomplex);
_Lcomplex ctanhl(_Lcomplex);
long double norml(_Lcomplex);

_Dcomplex (_Cbuild)(double, double);
_Dcomplex (_Cmulcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Cmulcr)(_Dcomplex, double);
_Dcomplex (_Cdivcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Cdivcr)(_Dcomplex, double);
_Dcomplex (_Caddcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Caddcr)(_Dcomplex, double);
_Dcomplex (_Csubcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Csubcr)(_Dcomplex, double);

_Fcomplex (_FCbuild)(float, float);
_Fcomplex (_FCmulcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCmulcr)(_Fcomplex, float);
_Fcomplex (_FCdivcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCdivcr)(_Fcomplex, float);
_Fcomplex (_FCaddcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCaddcr)(_Fcomplex, float);
_Fcomplex (_FCsubcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCsubcr)(_Fcomplex, float);

_Lcomplex (_LCbuild)(long double, long double);
_Lcomplex (_LCmulcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCmulcr)(_Lcomplex, long double);
_Lcomplex (_LCdivcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCdivcr)(_Lcomplex, long double);
_Lcomplex (_LCaddcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCaddcr)(_Lcomplex, long double);
_Lcomplex (_LCsubcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCsubcr)(_Lcomplex, long double);
# 908 "/toolchain/arm/include/complex.h" 3
_Dcomplex clog(_Dcomplex);
# 3 "/arm-libs/library-src/dinkum/source/./xxccosh.h" 2
# 1 "/toolchain/arm/include/xmath.h" 1 3




# 1 "/toolchain/arm/include/errno.h" 1 3
# 490 "/toolchain/arm/include/errno.h" 3
extern int _Errno;
# 507 "/toolchain/arm/include/errno.h" 3
typedef int errno_t;
# 6 "/toolchain/arm/include/xmath.h" 2 3
# 1 "/toolchain/arm/include/math.h" 1 3
# 39 "/toolchain/arm/include/math.h" 3
typedef float float_t;
typedef double double_t;
# 100 "/toolchain/arm/include/math.h" 3
int _FFpcomp(float, float);
int _Fpcomp(double, double);
int _LFpcomp(long double, long double);

int _FDclass(float);
int _Dclass(double);
int _LDclass(long double);

int _FDsign(float);
int _Dsign(double);
int _LDsign(long double);
# 253 "/toolchain/arm/include/math.h" 3
static double __attribute__((__overloadable__)) __tg_promote(int);
static double __attribute__((__overloadable__)) __tg_promote(unsigned int);
static double __attribute__((__overloadable__)) __tg_promote(long);
static double __attribute__((__overloadable__)) __tg_promote(unsigned long);
static double __attribute__((__overloadable__)) __tg_promote(long long);
static double __attribute__((__overloadable__)) __tg_promote(unsigned long long);
static float __attribute__((__overloadable__)) __tg_promote(float);
static double __attribute__((__overloadable__)) __tg_promote(double);
static long double __attribute__((__overloadable__)) __tg_promote(long double);

static float _Complex __attribute__((__overloadable__)) __tg_promote(float _Complex);
static double _Complex __attribute__((__overloadable__)) __tg_promote(double _Complex);
static long double _Complex __attribute__((__overloadable__)) __tg_promote(long double _Complex);

# 293 "/toolchain/arm/include/math.h" 3
static int __attribute__((__overloadable__, __always_inline__)) _Tg_Fpcomp(float _Left, float _Right) {return (_FFpcomp(_Left, _Right));} static int __attribute__((__overloadable__, __always_inline__)) _Tg_Fpcomp(double _Left, double _Right) {return (_Fpcomp(_Left, _Right));} static int __attribute__((__overloadable__, __always_inline__)) _Tg_Fpcomp(long double _Left, long double _Right) {return (_LFpcomp(_Left, _Right));}



static float __attribute__((__overloadable__, __always_inline__)) _Tg_Dclass(float _Left) {return (_FDclass(_Left));} static double __attribute__((__overloadable__, __always_inline__)) _Tg_Dclass(double _Left) {return (_Dclass(_Left));} static long double __attribute__((__overloadable__, __always_inline__)) _Tg_Dclass(long double _Left) {return (_LDclass(_Left));}



static float __attribute__((__overloadable__, __always_inline__)) _Tg_Dsign(float _Left) {return (_FDsign(_Left));} static double __attribute__((__overloadable__, __always_inline__)) _Tg_Dsign(double _Left) {return (_Dsign(_Left));} static long double __attribute__((__overloadable__, __always_inline__)) _Tg_Dsign(long double _Left) {return (_LDsign(_Left));}
# 434 "/toolchain/arm/include/math.h" 3
double acos(double);
double asin(double);
double atan(double);
double atan2(double, double);
double ceil(double);
double exp(double);
double fabs(double);
double floor(double);
double fmod(double, double);
double frexp(double, int *);
double ldexp(double, int);
double modf(double, double *);
double pow(double, double);
double sqrt(double);
double tan(double);
double tanh(double);


double acosh(double);
double asinh(double);
double atanh(double);
double cbrt(double);
double copysign(double, double);
double erf(double);
double erfc(double);
double exp2(double);
double expm1(double);
double fdim(double, double);
double fma(double, double, double);
double fmax(double, double);
double fmin(double, double);
double hypot(double, double);
int ilogb(double);
double lgamma(double);
long long llrint(double);
long long llround(double);
double log1p(double);
double logb(double);
long lrint(double);
long lround(double);
double nan(const char *);
double nearbyint(double);
double nextafter(double, double);
double nexttoward(double, long double);
double remainder(double, double);
double remquo(double, double, int *);
double rint(double);
double round(double);
double scalbn(double, int);
double scalbln(double, long);
double tgamma(double);
double trunc(double);



float acosf(float);
float asinf(float);
float atanf(float);
float atan2f(float, float);
float ceilf(float);
float expf(float);
float fabsf(float);
float floorf(float);
float fmodf(float, float);
float frexpf(float, int *);
float ldexpf(float, int);
float modff(float, float *);
float powf(float, float);
float sqrtf(float);
float tanf(float);
float tanhf(float);


float acoshf(float);
float asinhf(float);
float atanhf(float);
float cbrtf(float);
float copysignf(float, float);
float erff(float);
float erfcf(float);
float expm1f(float);
float exp2f(float);
float fdimf(float, float);
float fmaf(float, float, float);
float fmaxf(float, float);
float fminf(float, float);
float hypotf(float, float);
int ilogbf(float);
float lgammaf(float);
long long llrintf(float);
long long llroundf(float);
float log1pf(float);
float logbf(float);
long lrintf(float);
long lroundf(float);
float nanf(const char *);
float nearbyintf(float);
float nextafterf(float, float);
float nexttowardf(float, long double);
float remainderf(float, float);
float remquof(float, float, int *);
float rintf(float);
float roundf(float);
float scalbnf(float, int);
float scalblnf(float, long);
float tgammaf(float);
float truncf(float);



long double acosl(long double);
long double asinl(long double);
long double atanl(long double);
long double atan2l(long double, long double);
long double ceill(long double);
long double expl(long double);
long double fabsl(long double);
long double floorl(long double);
long double fmodl(long double, long double);
long double frexpl(long double, int *);
long double ldexpl(long double, int);
long double modfl(long double, long double *);
long double powl(long double, long double);
long double sqrtl(long double);
long double tanl(long double);
long double tanhl(long double);


long double acoshl(long double);
long double asinhl(long double);
long double atanhl(long double);
long double cbrtl(long double);
long double copysignl(long double, long double);
long double erfl(long double);
long double erfcl(long double);
long double exp2l(long double);
long double expm1l(long double);
long double fdiml(long double, long double);
long double fmal(long double, long double, long double);
long double fmaxl(long double, long double);
long double fminl(long double, long double);
long double hypotl(long double, long double);
int ilogbl(long double);
long double lgammal(long double);
long long llrintl(long double);
long long llroundl(long double);
long double log1pl(long double);
long double logbl(long double);
long lrintl(long double);
long lroundl(long double);
long double nanl(const char *);
long double nearbyintl(long double);
long double nextafterl(long double, long double);
long double nexttowardl(long double, long double);
long double remainderl(long double, long double);
long double remquol(long double, long double, int *);
long double rintl(long double);
long double roundl(long double);
long double scalbnl(long double, int);
long double scalblnl(long double, long);
long double tgammal(long double);
long double truncl(long double);
# 1309 "/toolchain/arm/include/math.h" 3
double cos(double);
double cosh(double);
double log(double);
double log10(double);
double sin(double);
double sinh(double);


double log2(double);




float cosf(float);
float coshf(float);
float logf(float);
float log10f(float);
float sinf(float);
float sinhf(float);


float log2f(float);




long double cosl(long double);
long double coshl(long double);
long double logl(long double);
long double log10l(long double);
long double sinl(long double);
long double sinhl(long double);


long double log2l(long double);
# 7 "/toolchain/arm/include/xmath.h" 2 3
# 1 "/toolchain/arm/include/stddef.h" 1 3
# 41 "/toolchain/arm/include/stddef.h" 3
typedef _Ptrdifft ptrdiff_t;
# 51 "/toolchain/arm/include/stddef.h" 3
typedef _Sizet size_t;





typedef _Wchart wchar_t;
# 72 "/toolchain/arm/include/stddef.h" 3
typedef size_t rsize_t;
# 8 "/toolchain/arm/include/xmath.h" 2 3
# 132 "/toolchain/arm/include/xmath.h" 3
int _Stopfx(const char **, char **);
int _Stoflt(const char *, const char *, char **,
 long[], int);
int _Stoxflt(const char *, const char *, char **,
 long[], int);
int _WStopfx(const wchar_t **, wchar_t **);
int _WStoflt(const wchar_t *, const wchar_t *, wchar_t **,
 long[], int);
int _WStoxflt(const wchar_t *, const wchar_t *, wchar_t **,
 long[], int);


typedef union
 {
 unsigned short _Sh[8];
 double _Val;
 } _Dval;

unsigned short *_Plsw(double *);
unsigned short *_Pmsw(double *);

double _Atan(double, int);
short _Dscalex(double *, long, int);
double _Hypot(double, double, int *);

short _Dint(double *, short);
short _Dnorm(_Dval *);
short _Dscale(double *, long);
short _Dunscale(short *, double *);

double _Poly(double, const double *, int);

extern const _Dconst _Eps, _Rteps;
extern const double _Zero;

double _Xp_getw(const double *, int);
double *_Xp_setn(double *, int, long);
double *_Xp_setw(double *, int, double);
double *_Xp_addh(double *, int, double);
double *_Xp_mulh(double *, int, double);
double *_Xp_movx(double *, int, const double *);
double *_Xp_addx(double *, int,
 const double *, int);
double *_Xp_subx(double *, int,
 const double *, int);
double *_Xp_ldexpx(double *, int, int);
double *_Xp_mulx(double *, int,
 const double *, int, double *);
double *_Xp_invx(double *, int, double *);
double *_Xp_sqrtx(double *, int, double *);


typedef union
 {
 unsigned short _Sh[8];
 float _Val;
 } _Fval;

unsigned short *_FPlsw(float *);
unsigned short *_FPmsw(float *);

float _FAtan(float, int);
short _FDscalex(float *, long, int);
float _FHypot(float, float, int *);

short _FDint(float *, short);
short _FDnorm(_Fval *);
short _FDscale(float *, long);
short _FDunscale(short *, float *);
# 209 "/toolchain/arm/include/xmath.h" 3
float _FPoly(float, const float *, int);


extern const _Dconst _FEps, _FRteps;
extern const float _FZero;

float _FXp_getw(const float *, int);
float *_FXp_setn(float *, int, long);
float *_FXp_setw(float *, int, float);
float *_FXp_addh(float *, int, float);
float *_FXp_mulh(float *, int, float);
float *_FXp_movx(float *, int, const float *);
float *_FXp_addx(float *, int,
 const float *, int);
float *_FXp_subx(float *, int,
 const float *, int);
float *_FXp_ldexpx(float *, int, int);
float *_FXp_mulx(float *, int,
 const float *, int, float *);
float *_FXp_invx(float *, int, float *);
float *_FXp_sqrtx(float *, int, float *);


typedef union
 {
 unsigned short _Sh[8];
 long double _Val;
 } _Lval;





unsigned short *_LPlsw(long double *);
unsigned short *_LPmsw(long double *);

long double _LAtan(long double, int);
short _LDscalex(long double *, long, int);
long double _LHypot(long double, long double, int *);

short _LDint(long double *, short);
short _LDnorm(_Lval *);
short _LDscale(long double *, long);
short _LDunscale(short *, long double *);
long double _LPoly(long double, const long double *, int);

extern const _Dconst _LEps, _LRteps;
extern const long double _LZero;

long double _LXp_getw(const long double *, int);
long double *_LXp_setn(long double *, int, long);
long double *_LXp_setw(long double *, int, long double);
long double *_LXp_addh(long double *, int, long double);
long double *_LXp_mulh(long double *, int, long double);
long double *_LXp_movx(long double *, int,
 const long double *);
long double *_LXp_addx(long double *, int,
 const long double *, int);
long double *_LXp_subx(long double *, int,
 const long double *, int);
long double *_LXp_ldexpx(long double *, int, int);
long double *_LXp_mulx(long double *, int,
 const long double *, int, long double *);
long double *_LXp_invx(long double *, int, long double *);
long double *_LXp_sqrtx(long double *, int, long double *);
# 4 "/arm-libs/library-src/dinkum/source/./xxccosh.h" 2


_Dcomplex (ccosh)(_Dcomplex x)
 {
 double re = creal(x);
 double im = cimag(x);
 short errx = _Dtest(&re);
 short erry = _Dtest(&im);

 if (errx == 2)
  if (erry == 0)
   return (_Cbuild(_Nan._Double, im));
  else
   return (_Cbuild(_Nan._Double, _Nan._Double));
 else if (erry == 1)
  {
  _Feraise(0x01);
  if (errx == 0)
   return (_Cbuild(_Nan._Double,
    im < 0.0 ? -_Zero : 0.0));
  else if (errx == 1)
   return (_Cbuild(_Inf._Double,
    _Nan._Double));
  else
   return (_Cbuild(_Nan._Double, _Nan._Double));
  }
 else if (erry == 2)
  if (errx == 0)
   return (_Cbuild(_Nan._Double, re));
  else if (errx == 1)
   return (_Cbuild(_Inf._Double,
    _Nan._Double));
  else
   return (_Cbuild(_Nan._Double, _Nan._Double));
 else if (errx == 1)
  if (erry == 0)
   return (_Cbuild(_Inf._Double,
    re < 0.0 ? -im : im));
  else
   return (_Cbuild(_Inf._Double * cos(im),
    _Inf._Double * sin(im)));
 else
  return (_Cbuild(_Cosh(re, cos(im)),
   _Sinh(re, sin(im))));
 }
# 4 "/arm-libs/library-src/dinkum/source/./ccosh.c" 2

