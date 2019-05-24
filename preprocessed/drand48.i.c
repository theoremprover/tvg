# 1 "/arm-libs/library-src/dinkum/source/./drand48.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./drand48.c" 2

# 1 "/toolchain/arm/include/math.h" 1 3



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
# 5 "/toolchain/arm/include/math.h" 2 3
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
# 3 "/arm-libs/library-src/dinkum/source/./drand48.c" 2
# 1 "/toolchain/arm/include/stdlib.h" 1 3
# 40 "/toolchain/arm/include/stdlib.h" 3
typedef _Sizet size_t;





typedef _Wchart wchar_t;


typedef struct
 {
 int quot;
 int rem;
 } div_t;

typedef struct
 {
 long quot;
 long rem;
 } ldiv_t;


typedef struct
 {
 _Longlong quot;
 _Longlong rem;
 } _Lldiv_t;



typedef _Lldiv_t lldiv_t;




void exit(int) __attribute__((__noreturn__));

void _Exit(int) __attribute__((__noreturn__));
char *getenv(const char *);
int system(const char *);



int abs(int);
long labs(long);

void abort(void) __attribute__((__noreturn__));
void *calloc(size_t, size_t);
div_t div(int, int);
void free(void *);
ldiv_t ldiv(long, long);
void *malloc(size_t);
int mblen(const char *, size_t);
size_t mbstowcs(wchar_t *,
 const char *, size_t);
int mbtowc(wchar_t *, const char *, size_t);
int rand(void);
void srand(unsigned int);
void *realloc(void *, size_t);
long strtol(const char *, char **, int);
size_t wcstombs(char *,
 const wchar_t *, size_t);
int wctomb(char *, wchar_t);


void lcong48(unsigned short [7]);
unsigned short *seed48(unsigned short [3]);
void srand48(long);

double drand48(void);
long lrand48(void);
long mrand48(void);

double erand48(unsigned short [3]);
long nrand48(unsigned short [3]);
long jrand48(unsigned short [3]);

int getopt(int, char *const *, const char *);
void *memalign(size_t, size_t);
char *mktemp(char *);
int putenv(char *);
int rand_r(unsigned *);
char *tempnam(const char *, const char *);

extern char *optarg;
extern int optind, opterr, optopt;




extern float _Stofx(const char *, char **,
 long, int *);
extern double _Stodx(const char *, char **,
 long, int *);
extern long double _Stoldx(const char *, char **,
 long, int *);
extern long _Stolx(const char *, char **,
 int, int *);
extern unsigned long _Stoulx(const char *, char **,
 int, int *);
extern long long _Stollx(const char *, char **,
 int, int *);
extern unsigned long long _Stoullx(const char *, char **,
 int, int *);



unsigned long _Stoul(const char *, char **, int);
float _Stof(const char *, char **, long);
double _Stod(const char *, char **, long);
long double _Stold(const char *, char **, long);
_Longlong _Stoll(const char *, char **, int);
_ULonglong _Stoull(const char *, char **, int);

float _Stofx(const char *, char **, long, int *);
double _Stodx(const char *, char **, long, int *);
long double _Stoldx(const char *, char **, long, int *);
long _Stolx(const char *, char **, int, int *);
unsigned long _Stoulx(const char *, char **, int, int *);
_Longlong _Stollx(const char *, char **, int, int *);
_ULonglong _Stoullx(const char *, char **, int, int *);

size_t _Getmbcurmax(void);


_Longlong llabs(_Longlong);
lldiv_t lldiv(_Longlong, _Longlong);
# 311 "/toolchain/arm/include/stdlib.h" 3
typedef int _Cmpfun(const void *, const void *);

      int atexit(void (*)(void)) ;
void *bsearch(const void *, const void *,
 size_t, size_t, _Cmpfun *);
void qsort(void *, size_t, size_t, _Cmpfun *);

double atof(const char *);
int atoi(const char *);
long atol(const char *);
double strtod(const char *, char **);
unsigned long strtoul(const char *,
 char **, int);
# 332 "/toolchain/arm/include/stdlib.h" 3
_Longlong atoll(const char *);
float strtof(const char *,
 char **);
long double strtold(const char *,
 char **);
_Longlong strtoll(const char *,
 char **, int);
_ULonglong strtoull(const char *,
 char **, int);
# 360 "/toolchain/arm/include/stdlib.h" 3
int (_Fail_s)(const char *, size_t);



typedef int errno_t;




typedef size_t rsize_t;


typedef void (*constraint_handler_t)(const char *,
 void *, errno_t);

constraint_handler_t set_constraint_handler_s(
 constraint_handler_t);
void abort_handler_s(const char *,
 void *, errno_t);
void ignore_handler_s(const char *,
 void *, errno_t);

errno_t getenv_s(size_t *, char *,
 rsize_t, const char *);
# 417 "/toolchain/arm/include/stdlib.h" 3
typedef int _Cmpfun_s(const void *, const void *, void *);

void *bsearch_s(const void *, const void *,
 rsize_t, rsize_t, _Cmpfun_s *, void *);
errno_t qsort_s(void *,
 rsize_t, rsize_t, _Cmpfun_s *, void *);




errno_t wctomb_s(int *,
 char *, rsize_t,
 wchar_t);
errno_t mbstowcs_s(size_t *,
 wchar_t *, rsize_t,
 const char *, rsize_t);
errno_t wcstombs_s(size_t *,
 char *, rsize_t,
 const wchar_t *, rsize_t);
# 4 "/arm-libs/library-src/dinkum/source/./drand48.c" 2
# 15 "/arm-libs/library-src/dinkum/source/./drand48.c"
static unsigned long long xval = 0x330e;
static unsigned long long aval = ((unsigned long long)0x5deec << 16) | 0xe66d;
static unsigned short cval = 0x000b;

static const unsigned long long mask48 = ((unsigned long long)0xffffffff << 16) | 0xffff;
static const unsigned long long xval0 = 0x330e;
static const unsigned long long aval0 = ((unsigned long long)0x5deec << 16) | 0xe66d;
static const unsigned short cval0 = 0x000b;

void (lcong48)(unsigned short par[6 + 1])
 {
 xval = ((((unsigned long long)par[2] << 16) | par[1]) << 16) | par[0];
 aval = ((((unsigned long long)par[5] << 16) | par[4]) << 16) | par[3];
 cval = par[6];
 }

unsigned short *(seed48)(unsigned short par[2 + 1])
 {
 static unsigned short par0[2 + 1];

 xval = ((((unsigned long long)par[2] << 16) | par[1]) << 16) | par[0];
 aval = aval0;
 cval = cval0;

 par0[0] = par[0];
 par0[1] = par[1];
 par0[2] = par[2];
 return (&par0[0]);
 }

void (srand48)(long seed)
 {
 xval = ((unsigned long long)seed << 16) | xval0;
 aval = aval0;
 cval = cval0;
 }

double (drand48)()
 {
 xval = (aval * xval + cval) & mask48;
 return (ldexp((unsigned long)(xval >> 16), -32));
 }

double (erand48)(unsigned short par[2 + 1])
 {
 unsigned long long xv = ((((unsigned long long)par[2] << 16) | par[1]) << 16) | par[0];

 xv = (aval * xv + cval) & mask48;
 par[0] = (unsigned short)xv;
 par[1] = (unsigned short)(xv >> 16);
 par[2] = (unsigned short)(xv >> 32);
 return (ldexp((unsigned long)(xv >> 16), -32));
 }

long (lrand48)()
 {
 xval = (aval * xval + cval) & mask48;
 return ((long)((unsigned long)(xval >> 17) & 0x7fffffff));
 }

long (nrand48)(unsigned short par[2 + 1])
 {
 unsigned long long xv = ((((unsigned long long)par[2] << 16) | par[1]) << 16) | par[0];

 xv = (aval * xv + cval) & mask48;
 par[0] = (unsigned short)xv;
 par[1] = (unsigned short)(xv >> 16);
 par[2] = (unsigned short)(xv >> 32);
 return ((long)((unsigned long)(xv >> 17) & 0x7fffffff));
 }

long (mrand48)()
 {
 xval = (aval * xval + cval) & mask48;
 return ((long)(unsigned long)(xval >> 16));
 }

long (jrand48)(unsigned short par[2 + 1])
 {
 unsigned long long xv = ((((unsigned long long)par[2] << 16) | par[1]) << 16) | par[0];

 xv = (aval * xv + cval) & mask48;
 par[0] = (unsigned short)xv;
 par[1] = (unsigned short)(xv >> 16);
 par[2] = (unsigned short)(xv >> 32);
 return ((long)(unsigned long)(xv >> 16));
 }
