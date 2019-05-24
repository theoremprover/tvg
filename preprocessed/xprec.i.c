# 1 "/arm-libs/library-src/dinkum/source/./xprec.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./xprec.c" 2

# 1 "/toolchain/arm/include/xmath.h" 1 3



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
# 5 "/toolchain/arm/include/xmath.h" 2 3
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
# 3 "/arm-libs/library-src/dinkum/source/./xprec.c" 2
# 1 "/arm-libs/library-src/dinkum/source/./xxdftype.h" 1

# 1 "/toolchain/arm/include/yvals.h" 1 3
# 3 "/arm-libs/library-src/dinkum/source/./xxdftype.h" 2
# 1 "/toolchain/arm/include/float.h" 1 3
# 4 "/arm-libs/library-src/dinkum/source/./xxdftype.h" 2
# 4 "/arm-libs/library-src/dinkum/source/./xprec.c" 2

# 1 "/arm-libs/library-src/dinkum/source/./xxxprec.h" 1

# 1 "/toolchain/arm/include/string.h" 1 3
# 29 "/toolchain/arm/include/string.h" 3
int memcmp(const void *, const void *, size_t);
void *memcpy(void *, const void *, size_t);
void *memset(void *, int, size_t);
char *strcat(char *, const char *);
int strcmp(const char *, const char *);
char *strcpy(char *, const char *);
size_t strlen(const char *);

void *memmove(void *, const void *, size_t);
int strcoll(const char *, const char *);
size_t strcspn(const char *, const char *);
char *strerror(int);
char *strncat(char *, const char *, size_t);
int strncmp(const char *, const char *, size_t);
char *strncpy(char *, const char *, size_t);
size_t strspn(const char *, const char *);
char *strtok(char *, const char *);
size_t strxfrm(char *, const char *, size_t);


char *strdup(const char *);
int strcasecmp(const char *, const char *);
int strncasecmp(const char *, const char *, size_t);
char *strtok_r(char *, const char *, char **);
# 94 "/toolchain/arm/include/string.h" 3
char *strchr(const char *, int);
char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
char *strstr(const char *, const char *);
# 118 "/toolchain/arm/include/string.h" 3
void *memchr(const void *, int, size_t);
# 137 "/toolchain/arm/include/string.h" 3
errno_t memcpy_s(void *, rsize_t,
 const void *, rsize_t);
errno_t memmove_s(void *, rsize_t,
 const void *, rsize_t);

errno_t strcpy_s(char *, rsize_t,
 const char *);
errno_t strncpy_s(char *, rsize_t,
 const char *, rsize_t);
errno_t strcat_s(char *, rsize_t,
 const char *);
errno_t strncat_s(char *, rsize_t,
 const char *, rsize_t);
char *strtok_s(char *, rsize_t *,
 const char *, char **);

errno_t strerror_s(char *, rsize_t, errno_t);
size_t strerrorlen_s(errno_t);

size_t strnlen_s(const char *, size_t);
# 3 "/arm-libs/library-src/dinkum/source/./xxxprec.h" 2
# 33 "/arm-libs/library-src/dinkum/source/./xxxprec.h"
double _Xp_getw(const double *p, int n)
 {
 if (n == 0)
  return (0.0);
 else if (n == 1 || p[0] == 0.0 || p[1] == 0.0)
  return (p[0]);
 else if (n == 2 || p[2] == 0.0)
  return (p[0] + p[1]);
 else
  {
  double p01 = p[0] + p[1];
  double p2 = p[2];

  if (4 <= n)
   p2 += p[3];
  if (p01 - p[0] == p[1])
   return (p01 + p2);
  else
   return (p[0] + (p[1] + p2));
  }
 }

double *_Xp_setw(double *p, int n, double x)
 {
 double x0 = x;
 short errx, xexp;

 if (n <= 0)
  ;
 else if (n == 1 || (errx = _Dunscale(&xexp, &x0)) == 0)
  p[0] = x0;
 else if (0 < errx)
  {
  p[0] = x0;
  p[1] = 0.0;
  }
 else
  {
  _Dint(&x0, (53 / 2));
  _Dscale(&x0, xexp);

  p[0] = x0;
  p[1] = x - x0;
  if ((53 & 1) != 0 && 2 < n && p[1] != 0.0)
   {
   x = p[1];
   _Dunscale(&xexp, &p[1]);
   _Dint(&p[1], (53 / 2));
   _Dscale(&p[1], xexp);
   p[2] = x - p[1];
   if (3 < n && p[2] != 0.0)
    p[3] = 0.0;
   }
  else if (2 < n)
   p[2] = 0.0;
  }
 return (p);
 }

double *_Xp_addh(double *p, int n, double x0)
 {
 double xscaled = x0;
 short errx, xexp;

 if (n == 0)
  ;
 else if (0 < (errx = _Dunscale(&xexp, &xscaled)))
  if (errx == 2 || (errx = _Dtest(&p[0])) <= 0)
   p[0] = x0;
  else if (errx == 2 || ((*_Pmsw(&(x0))) & ((unsigned short)0x8000)) == ((*_Pmsw(&(p[0]))) & ((unsigned short)0x8000)))
   ;
  else
   {
   _Feraise(0x01);
   p[0] = _Nan._Double;
   if (1 < n)
    p[1] = 0.0;
   }
 else if (errx < 0)
  {
  long prevexp = (2 * 1024);
  int k;

  for (k = 0; k < n; )
   {
   double yscaled = p[k];
   int mybits = (53 / 2);
   short yexp;
   long diff;

   if (0 < (errx = _Dunscale(&yexp, &yscaled)))
    break;
   else if (errx == 0)
    {
    p[k] = x0;
    if (k + 1 < n)
     p[k + 1] = 0.0;
    break;
    }
   else if ((diff = (long)yexp - xexp) <= -mybits
    && x0 != 0.0)
    {
    int j;

    for (j = k; ++j < n && p[j] != 0.0; )
     ;
    if (j < n - 1)
     ++j;
    else if (j == n)
     --j;
    for (; k < j; --j)
     p[j] = p[j - 1];
    p[k] = x0;
    x0 = 0.0;
    }
   else if (mybits <= diff && x0 != 0.0)
    {
    prevexp = yexp;
    ++k;
    }
   else
    {
    if ((p[k] += x0) == 0.0)
     {
     {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0; ) ; p[n - 1] = 0.0;}
     if (p[k] == 0.0)
      break;
     }
    x0 = p[k];
    _Dunscale(&xexp, &x0);
    if (prevexp - mybits < xexp)
     {
     _Dint(&x0, (short)(xexp - (prevexp - mybits)));
     _Dscale(&x0, xexp);
     if ((p[k] -= x0) == 0.0)
      {
      {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0; ) ; p[n - 1] = 0.0;}
      }
     if (--k == 0)
      prevexp = (2 * 1024);
     else
      {
      xscaled = p[k - 1];
      _Dunscale(&yexp, &xscaled);
      prevexp = yexp;
      }
     }
    else if (k + 1 == n)
     break;
    else
     {
     x0 = p[k];
     _Dunscale(&yexp, &p[k]);
     _Dint(&p[k], (53 / 2));
     _Dscale(&p[k], yexp);
     x0 -= p[k];
     prevexp = yexp;

     xscaled = x0 != 0.0 ? x0 : p[k];
     _Dunscale(&xexp, &xscaled);
     ++k;
     }
    }
   }
  }
 return (p);
 }

double *_Xp_mulh(double *p, int n, double x0)
 {
 short errx;
 int j, k;
 double buf[4];

 if (0 < n)
  {
  buf[0] = p[0] * x0;
  if (0 <= (errx = _Dtest(&buf[0])))
   {
   if (errx == 2)
    _Feraise(0x01);
   p[0] = buf[0];
   if (0 < errx && 1 < n)
    p[1] = 0.0;
   return (p);
   }
  p[0] = 0.0;
  }

 for (j = 1, k = 0; k < n; ++k, --j)
  {
  for (; j < 4; ++j)
   if (k + j < n && p[k + j] != 0.0)
    {
    buf[j] = p[k + j] * x0;
    p[k + j] = 0.0;
    }
   else
    {
    buf[j] = 0.0;
    j = 2 * 4;
    break;
    }

  if (buf[0] == 0.0)
   break;
  else
   {
   int i = 0;
   double y0 = buf[0];
   short xexp;

   _Dunscale(&xexp, &y0);
   _Dint(&y0, (53 / 2));
   _Dscale(&y0, xexp);
   _Xp_addh(p, n, y0);
   _Xp_addh(p, n, buf[0] - y0);

   for (; ++i < j; )
    if ((buf[i - 1] = buf[i]) == 0.0)
     break;
   }
  }
 return (p);
 }

double *_Xp_setn(double *p, int n, long x)
 {

 _Xp_setw(p, n, (double)x);







 return (p);
 }

double *_Xp_movx(double *p, int n, const double *q)
 {
 memcpy(p, q, n * sizeof (double));
 return (p);
 }

double *_Xp_addx(double *p, int n,
 const double *q, int m)
 {
 int k;

 for (k = 0; k < m && q[k] != 0.0; ++k)
  _Xp_addh(p, n, q[k]);
 return (p);
 }
# 301 "/arm-libs/library-src/dinkum/source/./xxxprec.h"
double *_Xp_subx(double *p, int n,
 const double *q, int m)
 {
 int k;

 for (k = 0; k < m && q[k] != 0.0; ++k)
  _Xp_addh(p, n, -q[k]);
 return (p);
 }

double *_Xp_ldexpx(double *p, int n, int m)
 {
 int k;

 for (k = 0; k < n; ++k)
  {
  p[k] = ldexp(p[k], m);
  if (p[k] == 0.0)
   break;
  }
 return (p);
 }

double *_Xp_mulx(double *p, int n,
 const double *q, int m,
 double *ptemp2)
 {
 if (n == 0 || m == 0)
  ;
 else if (q[0] == 0.0 || q[1] == 0.0)
  _Xp_mulh(p, n, q[0]);
 else
  {
  double *px = ptemp2;
  double *pac = ptemp2 + n;
  int j;

  _Xp_movx(px, n, p);
  _Xp_mulh(p, n, q[0]);
  for (j = 1; j < m && q[j] != 0.0; ++j)
   {
   _Xp_movx(pac, n, px);
   _Xp_mulh(pac, n, q[j]);
   _Xp_addx(p, n, pac, n);
   }
  }
 return (p);
 }
# 378 "/arm-libs/library-src/dinkum/source/./xxxprec.h"
double *_Xp_invx(double *p, int n, double *ptemp4)
 {
 short errx;

 if (n == 0)
  ;
 else if (0 <= (errx = _Dtest(&p[0])))
  {
  if (errx == 1)
   p[0] = 0.0;
  else if (errx == 0)
   p[0] = _Inf._Double;

  }
 else
  {
  double *pac = ptemp4;
  double *py = ptemp4 + n;
  double *ptemp2 = py + n;
  double x0 = p[0];
  int k;

  _Xp_movx(py, n, p);
  _Xp_mulh(py, n, -1.0);

  if (1 < n)
   x0 += p[1];
  _Xp_setw(p, n, (1.0 / (x0)));

  for (k = 1; k < n; k <<= 1)
   {
   _Xp_movx(pac, n, p);
   _Xp_mulx(pac, n, py, n, ptemp2);
   _Xp_addh(pac, n, 1.0);
   _Xp_mulx(pac, n, p, n, ptemp2);
   _Xp_addx(p, n, pac, n);
   }
  }
 return (p);
 }

double *_Xp_sqrtx(double *p, int n, double *ptemp4)
 {
 if (n == 0)
  ;
 else if (0 <= _Dtest(&p[0]) || p[0] < 0.0)
  {
  if (p[0] < 0.0)
   {
   _Feraise(0x01);
   p[0] = _Nan._Double;
   }
  }
 else
  {
  double *pac = ptemp4;
  double *py = ptemp4 + n;
  double *ptemp2 = py + n;
  double x0 = p[0];
  int k;

  if (1 < n)
   x0 += p[1];
  _Xp_setw(py, n, (1.0 / (sqrt(x0))));

  for (k = 2; k < n; k <<= 1)
   {
   _Xp_movx(pac, n, py);
   _Xp_mulh(pac, n, -0.5);
   _Xp_mulx(pac, n, p, n, ptemp2);
   _Xp_mulx(pac, n, py, n, ptemp2);
   _Xp_addh(pac, n, 1.5);
   _Xp_mulx(py, n, pac, n, ptemp2);
   }
  _Xp_mulx(p, n, py, n, ptemp2);
  }
 return (p);
 }
# 6 "/arm-libs/library-src/dinkum/source/./xprec.c" 2

