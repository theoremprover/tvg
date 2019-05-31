# 1 "/arm-libs/library-src/dinkum/source/./pow.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./pow.c" 2

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
# 3 "/arm-libs/library-src/dinkum/source/./pow.c" 2
# 1 "/arm-libs/library-src/dinkum/source/./xxpow.h" 1

# 1 "/toolchain/arm/include/string.h" 1 3
# 24 "/toolchain/arm/include/string.h" 3
typedef _Sizet size_t;




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
# 129 "/toolchain/arm/include/string.h" 3
typedef int errno_t;




typedef size_t rsize_t;


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
# 3 "/arm-libs/library-src/dinkum/source/./xxpow.h" 2
# 1 "/toolchain/arm/include/xmath.h" 1 3




# 1 "/toolchain/arm/include/errno.h" 1 3
# 490 "/toolchain/arm/include/errno.h" 3
extern int _Errno;
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
# 57 "/toolchain/arm/include/stddef.h" 3
typedef _Wchart wchar_t;
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
# 4 "/arm-libs/library-src/dinkum/source/./xxpow.h" 2






double _Logpoly(double w);







# 1 "/arm-libs/library-src/dinkum/source/./xxpow.hx" 1
# 7482 "/arm-libs/library-src/dinkum/source/./xxpow.hx"
static const double log2e[] = {
 (double)(48408812.0 / 67108864.0L * 2),
 (double)(43368831.0 / 67108864.0L / 67108864.0L * 2),
 (double)(3076001.0 / 67108864.0L / 67108864.0L / 67108864.0L * 2),
 (double)(67031145.0 / 67108864.0L / 67108864.0L / 67108864.0L
  / 67108864.0L * 2),
 };



static const double scale = (double)(1 << 0);

static const double lnbias[][4] = {
 {
 -33554432.0 * 2 / 67108864.0L,
 -0.0 * 2 / (67108864.0L * 67108864.0L),
 -0.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -32064812.0 * 2 / 67108864.0L,
 -36291593.0 * 2 / (67108864.0L * 67108864.0L),
 -59567263.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -30619666.0 * 2 / 67108864.0L,
 -2733927.0 * 2 / (67108864.0L * 67108864.0L),
 -19949173.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -29216413.0 * 2 / 67108864.0L,
 -53631073.0 * 2 / (67108864.0L * 67108864.0L),
 -52065808.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -27852695.0 * 2 / 67108864.0L,
 -6308503.0 * 2 / (67108864.0L * 67108864.0L),
 -35133437.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -26526343.0 * 2 / 67108864.0L,
 -19181878.0 * 2 / (67108864.0L * 67108864.0L),
 -50595768.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -25235365.0 * 2 / 67108864.0L,
 -7330175.0 * 2 / (67108864.0L * 67108864.0L),
 -24581577.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -23977922.0 * 2 / 67108864.0L,
 -43925272.0 * 2 / (67108864.0L * 67108864.0L),
 -54451491.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -22752317.0 * 2 / 67108864.0L,
 -42359950.0 * 2 / (67108864.0L * 67108864.0L),
 -41999210.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -21556977.0 * 2 / 67108864.0L,
 -38559332.0 * 2 / (67108864.0L * 67108864.0L),
 -13908310.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -20390443.0 * 2 / 67108864.0L,
 -47979806.0 * 2 / (67108864.0L * 67108864.0L),
 -61187749.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -19251360.0 * 2 / 67108864.0L,
 -18411435.0 * 2 / (67108864.0L * 67108864.0L),
 -61732296.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -18138464.0 * 2 / 67108864.0L,
 -66691774.0 * 2 / (67108864.0L * 67108864.0L),
 -8446112.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -17050580.0 * 2 / 67108864.0L,
 -48668454.0 * 2 / (67108864.0L * 67108864.0L),
 -10023783.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -15986607.0 * 2 / 67108864.0L,
 -63598934.0 * 2 / (67108864.0L * 67108864.0L),
 -27753896.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -14945518.0 * 2 / 67108864.0L,
 -5934076.0 * 2 / (67108864.0L * 67108864.0L),
 -35408620.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -13926347.0 * 2 / 67108864.0L,
 -36708683.0 * 2 / (67108864.0L * 67108864.0L),
 -51121150.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -12928192.0 * 2 / 67108864.0L,
 -22542246.0 * 2 / (67108864.0L * 67108864.0L),
 -20133197.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -11950203.0 * 2 / 67108864.0L,
 -17611037.0 * 2 / (67108864.0L * 67108864.0L),
 -16889556.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -10991581.0 * 2 / 67108864.0L,
 -39442611.0 * 2 / (67108864.0L * 67108864.0L),
 -3961460.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -10051575.0 * 2 / 67108864.0L,
 -7216589.0 * 2 / (67108864.0L * 67108864.0L),
 -3330340.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -9129474.0 * 2 / 67108864.0L,
 -40664619.0 * 2 / (67108864.0L * 67108864.0L),
 -3742343.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -8224610.0 * 2 / 67108864.0L,
 -43017187.0 * 2 / (67108864.0L * 67108864.0L),
 -19145723.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -7336350.0 * 2 / 67108864.0L,
 -41942860.0 * 2 / (67108864.0L * 67108864.0L),
 -50445322.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -6464096.0 * 2 / 67108864.0L,
 -11271123.0 * 2 / (67108864.0L * 67108864.0L),
 -10066598.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -5607280.0 * 2 / 67108864.0L,
 -44038859.0 * 2 / (67108864.0L * 67108864.0L),
 -8593864.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -4765367.0 * 2 / 67108864.0L,
 -2798409.0 * 2 / (67108864.0L * 67108864.0L),
 -28619918.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -3937845.0 * 2 / 67108864.0L,
 -55023594.0 * 2 / (67108864.0L * 67108864.0L),
 -46962292.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -3124233.0 * 2 / 67108864.0L,
 -11959770.0 * 2 / (67108864.0L * 67108864.0L),
 -26011496.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -2324069.0 * 2 / 67108864.0L,
 -19923620.0 * 2 / (67108864.0L * 67108864.0L),
 -60922053.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -1536916.0 * 2 / 67108864.0L,
 -52913507.0 * 2 / (67108864.0L * 67108864.0L),
 -22404700.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -762359.0 * 2 / 67108864.0L,
 -17579626.0 * 2 / (67108864.0L * 67108864.0L),
 -45200036.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -0.0 * 2 / 67108864.0L,
 -0.0 * 2 / (67108864.0L * 67108864.0L),
 -0.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},
 };
# 18 "/arm-libs/library-src/dinkum/source/./xxpow.h" 2

double (_Pow)(double x, double y, short *pex)
 {
 double x1;
 double yi = y;
 double z;

 long zexp = 0;
 short xexp;
 short neg;

 if (y == 1.0)
  return (x);

 short errx = _Dunscale(&xexp, &x);
 const short erry = _Dint(&yi, 0);

 static const double invln2 = 1.4426950408889634073599246810018922;
 static const double ln2 = 0.69314718055994530941723212145817657;
 static const double rthalf = 0.70710678118654752440084436210484904;
 static const double maxexp = (double)(32768);







 if (pex != 0)
  *pex = 0;
 if ((erry == 0 && y == 0.0)
  || (errx < 0 && xexp == 1
   && (x == 0.5 || (erry == 1 && x == -0.5))))
  return (1.0);
 else if (0 <= errx || 0 < erry)
  {
  if (errx == 2)
   return (x);
  else if (erry == 2)
   return (y);
  else if (errx == 1)
   if (!((*_Pmsw(&(x))) & ((unsigned short)0x8000)))
    return (((*_Pmsw(&(y))) & ((unsigned short)0x8000)) ? 0.0 : _Inf._Double);
   else if (!((*_Pmsw(&(y))) & ((unsigned short)0x8000)))
    return (erry == 0 && _Dint(&yi, -1) < 0
     ? -_Inf._Double
     : _Inf._Double);
   else
    return (erry == 0 && _Dint(&yi, -1) < 0
     ? -_Zero : 0.0);
  else if (erry == 1)
   if (!((*_Pmsw(&(y))) & ((unsigned short)0x8000)))
    return (xexp <= 0 ? 0.0 : _Inf._Double);
   else
    return (xexp <= 0 ? _Inf._Double : 0.0);
  else
   if (!((*_Pmsw(&(y))) & ((unsigned short)0x8000)))
    return (erry == 0 && _Dint(&yi, -1) < 0 && ((*_Pmsw(&(x))) & ((unsigned short)0x8000))
     ? -_Zero : 0.0);
   else
    {
    _Feraise(0x02);
    return (erry == 0 && _Dint(&yi, -1) < 0 && ((*_Pmsw(&(x))) & ((unsigned short)0x8000))
     ? -_Inf._Double : _Inf._Double);
    }
  }
 else if (((*_Pmsw(&(x))) & ((unsigned short)0x8000)) && erry < 0)
  {
  _Feraise(0x01);
  return (_Nan._Double);
  }

 if (0.0 < x)
  neg = 0;
 else
  {
  ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
  neg = _Dint(&yi, -1);
  }


  {
 double z, w;

 if (x < rthalf)
  {
  x *= 2.0;
  --xexp;
  }

 z = ((x - 1.0) / (x + 1.0));
 w = z * z;

 x -= 1.0;
 x1 = z * (w * _Logpoly(w) - x);

 yi = (double)xexp + (x + x1) * invln2;
  }
# 145 "/arm-libs/library-src/dinkum/source/./xxpow.h"
 z = y * yi;
 if (z < -maxexp)
  errx = 0;
 else if (-15.0 <= z && z <= 15.0
  && -40.0 < y && y < 40.0)
  {
  zexp = (long)(z < 0.0 ? z - 0.5 : z + 0.5);
  z = y * (x + x1);


  z += (y * (double)xexp - (double)zexp) * ln2;






  errx = -1;
  }
 else if (maxexp < z)
  errx = 1;
 else
  {
  double xpx1[2], xpx[4], xpy[4], xpz[4];
  int i;

  _Xp_setw(xpx, 4, x);
  _Xp_setw(xpx1, 2, x1);
  _Xp_addx(xpx, 4, xpx1, 2);

  if (xpx[0] == 0.0)
   _Xp_setw(xpy, 4, 0.0);
  else
   {
   memcpy(xpy, log2e, sizeof (xpy));
   _Xp_mulh(xpy, 4, xpx[0]);
   for (i = 1; i < 4 && xpx[i] != 0.0; ++i)
    {
    double xpw[4];

    memcpy(xpw, log2e, sizeof (xpw));
    _Xp_mulh(xpw, 4, xpx[i]);
    _Xp_addx(xpy, 4, xpw, 4);
    }
   }
# 198 "/arm-libs/library-src/dinkum/source/./xxpow.h"
  _Xp_addh(xpy, 4, (double)xexp);

  _Xp_setw(xpx, 2, y);
  memcpy(xpz, xpy, sizeof (xpz));
  _Xp_mulh(xpz, 4, xpx[0]);

  if (xpx[1] != 0.0)
   {
   double xpw[4];

   memcpy(xpw, xpy, sizeof (xpw));
   _Xp_mulh(xpw, 4, xpx[1]);
   _Xp_addx(xpz, 4, xpw, 4);
   }

  x = xpz[0];
  if (xpz[0] != 0.0 && xpz[1] != 0.0)
   x += xpz[1] + xpz[2];
  _Dint(&x, 0);
  _Xp_addh(xpz, 4, -x);
  z = _Xp_getw(xpz, 4);
  z *= ln2;
  zexp = (long)x;
  errx = -1;
  }

 if (errx < 0)
  {
  if (pex != 0)
   {
   *pex = zexp;
   zexp = 0;
   }
  errx = _Exp(&z, 1.0, zexp);
   }
 switch (errx)
  {
 case 0:
  z = 0.0;
  _Feraise(0x08);
  break;

 case 1:
  if (z < 0.0)
   {
   z = 0.0;
   _Feraise(0x08);
   }
  else
   {
   z = _Inf._Double;
   _Feraise(0x04);
   }
  }

 if (neg)
  ((*_Pmsw(&(z))) ^= ((unsigned short)0x8000));
 return (z);
 }

double (pow)(double x, double y)
 {
 return (_Pow(x, y, 0));
 }
# 4 "/arm-libs/library-src/dinkum/source/./pow.c" 2

