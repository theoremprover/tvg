








typedef long int _Int32t;
typedef unsigned long int _Uint32t;







typedef long int _Ptrdifft;






typedef long unsigned int _Sizet;











  typedef __builtin_va_list va_list;

typedef long long _Longlong;
typedef unsigned long long _ULonglong;
typedef int _Wchart;
typedef unsigned int _Wintt;
typedef va_list _Va_list;





void _Atexit(void (*)(void));

typedef char _Sysch_t;



typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } __attribute__ ((__may_alias__)) _Dconst;


void _Feraise(int);


double _Cosh(double, double);



short _Dtest(double *);

short _Exp(double *, double, long);
double _Log(double, int);
double _Sin(double, unsigned int);
double _Sinh(double, double);
extern _Dconst _Denorm, _Hugeval, _Inf,
 _Nan, _Snan;


float _FCosh(float, float);
short _FDtest(float *);
short _FExp(float *, float, long);
float _FLog(float, int);
float _FSin(float, unsigned int);
float _FSinh(float, float);
extern _Dconst _FDenorm, _FInf, _FNan, _FSnan;


long double _LCosh(long double, long double);
short _LDtest(long double *);
short _LExp(long double *, long double, long);
long double _LLog(long double, int);
long double _LSin(long double, unsigned int);
long double _LSinh(long double, long double);
extern _Dconst _LDenorm, _LInf, _LNan, _LSnan;













extern int __attribute__((fardata)) _Errno;







typedef int errno_t;













typedef float float_t;
typedef double double_t;

int _FFpcomp(float, float);
int _Fpcomp(double, double);
int _LFpcomp(long double, long double);

int _FDclass(float);
int _Dclass(double);
int _LDclass(long double);

int _FDsign(float);
int _Dsign(double);
int _LDsign(long double);



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
_Longlong llrint(double);
_Longlong llround(double);
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
_Longlong llrintf(float);
_Longlong llroundf(float);
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
_Longlong llrintl(long double);
_Longlong llroundl(long double);
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




double cos(double);
double cosh(double);
double log(double);
double log10(double);
double sin(double);
double sinh(double);


void sincos (double, double *, double *);
double log2(double);







float cosf(float);
float coshf(float);
float logf(float);
float log10f(float);
float sinf(float);
float sinhf(float);


void sincosf (float, float *, float *);
float log2f(float);







long double cosl(long double);
long double coshl(long double);
long double logl(long double);
long double log10l(long double);
long double sinl(long double);
long double sinhl(long double);


void sincosl(long double, long double *, long double *);
long double log2l(long double);











typedef _Ptrdifft ptrdiff_t;
typedef _Sizet size_t;





typedef _Wchart wchar_t;
typedef size_t rsize_t;









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
 unsigned short _Sh[sizeof(double)/sizeof(short)];
 double _Val;
 } __attribute__ ((__may_alias__)) _Dval;


double _Atan(double, int);






short _Dint(double *, short);
short _Dnorm(_Dval *);
short _Dscale(double *, long);
short _Dunscale(short *, double *);

double _Hypot(double, double, int *);
double _Poly(double, const double *, int);
extern _Dconst _Eps, _Rteps;
extern double _Xbig, _Zero;

double _Xp_getw(double *, int);
double *_Xp_setn(double *, int, long);
double *_Xp_setw(double *, int, double);
double *_Xp_addh(double *, int, double);
double *_Xp_mulh(double *, int, double);
double *_Xp_movx(double *, int, double *);
double *_Xp_addx(double *, int, double *, int);
double *_Xp_subx(double *, int, double *, int);
double *_Xp_ldexpx(double *, int, int);
double *_Xp_mulx(double *, int, double *, int, double *);
double *_Xp_invx(double *, int, double *);
double *_Xp_sqrtx(double *, int, double *);


typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;

float _FAtan(float, int);
short _FDint(float *, short);
short _FDnorm(_Fval *);
short _FDscale(float *, long);
short _FDunscale(short *, float *);
float _FHypot(float, float, int *);
float _FPoly(float, const float *, int);
extern _Dconst _FEps, _FRteps;
extern float _FXbig, _FZero;

float _FXp_getw(float *, int);
float *_FXp_setn(float *, int, long);
float *_FXp_setw(float *, int, float);
float *_FXp_addh(float *, int, float);
float *_FXp_mulh(float *, int, float);
float *_FXp_movx(float *, int, float *);
float *_FXp_addx(float *, int, float *, int);
float *_FXp_subx(float *, int, float *, int);
float *_FXp_ldexpx(float *, int, int);
float *_FXp_mulx(float *, int, float *, int, float *);
float *_FXp_invx(float *, int, float *);
float *_FXp_sqrtx(float *, int, float *);


typedef union
 {
 unsigned short _Sh[sizeof(long double)/sizeof(short)];
 long double _Val;
 } __attribute__ ((__may_alias__)) _Lval;

long double _LAtan(long double, int);
short _LDint(long double *, short);
short _LDnorm(_Lval *);
short _LDscale(long double *, long);
short _LDunscale(short *, long double *);
long double _LHypot(long double, long double, int *);
long double _LPoly(long double, const long double *, int);
extern _Dconst _LEps, _LRteps;
extern long double _LXbig, _LZero;

long double _LXp_getw(long double *, int);
long double *_LXp_setn(long double *, int, long);
long double *_LXp_setw(long double *, int, long double);
long double *_LXp_addh(long double *, int, long double);
long double *_LXp_mulh(long double *, int, long double);
long double *_LXp_movx(long double *, int, long double *);
long double *_LXp_addx(long double *, int,
 long double *, int);
long double *_LXp_subx(long double *, int,
 long double *, int);
long double *_LXp_ldexpx(long double *, int, int);
long double *_LXp_mulx(long double *, int, long double *,
 int, long double *);
long double *_LXp_invx(long double *, int, long double *);
long double *_LXp_sqrtx(long double *, int, long double *);








int _Fltrounds(void);










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






char *strchr(const char *, int);
char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
char *strstr(const char *, const char *);


void *memchr(const void *, int, size_t);







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






float _FXp_getw(float *, int);
float *_FXp_setw(float *, int, float);
float *_FXp_mulh(float *, int, float);
float *_FXp_addx(float *, int, float *, int);
static const float c[] = {
 (float)(3216.0F * 8),
 (float)(4058.0F * 8 / 4096.0L),
 (float)(2594.0F * 8 / (4096.0L * 4096.0L)),
 (float)(360.0F * 8 / ((4096.0L * 4096.0L) * 4096.0L)),
 (float)(3107.0F * 8 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L))),
 };

static const float b[][(sizeof c / sizeof c[0])] = {
 {
 (float)(-3299.0F * 2 / 4096.0L),
 (float)(2032.0F * 2 / (4096.0L * 4096.0L)),
 (float)(553.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(2275.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(2403.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(2434.0F * 4 / 4096.0L),
 (float)(1228.0F * 4 / (4096.0L * 4096.0L)),
 (float)(3674.0F * 4 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(3023.0F * 4 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(962.0F * 4 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(-1831.0F * 2 / 4096.0L),
 (float)(621.0F * 2 / (4096.0L * 4096.0L)),
 (float)(2511.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(2994.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(61.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(-2726.0F * 4 / 4096.0L),
 (float)(3148.0F * 4 / (4096.0L * 4096.0L)),
 (float)(371.0F * 4 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(3704.0F * 4 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(2247.0F * 4 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(-2790.0F * 4 / 4096.0L),
 (float)(3216.0F * 4 / (4096.0L * 4096.0L)),
 (float)(170.0F * 4 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(2288.0F * 4 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(1274.0F * 4 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(265.0F * 2 / 4096.0L),
 (float)(3910.0F * 2 / (4096.0L * 4096.0L)),
 (float)(2109.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(592.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(561.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(3744.0F * 2 / 4096.0L),
 (float)(2415.0F * 2 / (4096.0L * 4096.0L)),
 (float)(635.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(1011.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(3900.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(3192.0F * 4 / 4096.0L),
 (float)(3374.0F * 4 / (4096.0L * 4096.0L)),
 (float)(490.0F * 4 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(2409.0F * 4 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(2573.0F * 4 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(494.0F * 2 / 4096.0L),
 (float)(1635.0F * 2 / (4096.0L * 4096.0L)),
 (float)(2440.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(1888.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(1235.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(-2114.0F * 2 / 4096.0L),
 (float)(2415.0F * 2 / (4096.0L * 4096.0L)),
 (float)(802.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(2030.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(3474.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(-579.0F * 2 / 4096.0L),
 (float)(1708.0F * 2 / (4096.0L * 4096.0L)),
 (float)(2269.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(2968.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(2501.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(3149.0F * 4 / 4096.0L),
 (float)(710.0F * 4 / (4096.0L * 4096.0L)),
 (float)(526.0F * 4 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(3616.0F * 4 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(1499.0F * 4 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(3881.0F * 2 / 4096.0L),
 (float)(1341.0F * 2 / (4096.0L * 4096.0L)),
 (float)(816.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(32.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(305.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(2786.0F * 2 / 4096.0L),
 (float)(2599.0F * 2 / (4096.0L * 4096.0L)),
 (float)(3838.0F * 2 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(3593.0F * 2 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(1902.0F * 2 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},

 {
 (float)(2820.0F * 4 / 4096.0L),
 (float)(1030.0F * 4 / (4096.0L * 4096.0L)),
 (float)(1226.0F * 4 / (4096.0L * 4096.0L) / 4096.0L),
 (float)(2488.0F * 4 / (4096.0L * 4096.0L) / (4096.0L * 4096.0L)),
 (float)(64.0F * 4 / ((4096.0L * 4096.0L) * (4096.0L * 4096.0L)) / 4096.0L)},
 };

static const float huge_rad = (float)(0.63661977236758134307553505349005744L
 * (4096.0L * 4096.0L) / 4096.0L);
static const float inv2pi = 0.15915494309189533576888376337251436F;
static const float twobypi = 0.63661977236758134307553505349005745F;

static int get_acsize(float x)
 {
 int acsize;
 short xexp;

 _FDunscale(&xexp, &x);
 acsize = 2 * (xexp / 24) + 4;
 return (acsize <= (sizeof c / sizeof c[0]) ? acsize
  : (int)(sizeof c / sizeof c[0]));
 }

unsigned int _FQuad(float *px)
 {
 float x = *px;
 float g;

 if (-huge_rad < x && x < huge_rad)
  {
  g = x * twobypi;
  if (0.0F <= g)
   g += 0.5F;
  else
   g -= 0.5F;
  _FDint(&g, 0);
  if (g != 0.0F)
   {
   float g4 = g * 0.25F / 4096.0L;

   x = ((((x - g4 * c[0]) - g4 * c[1]) - g4 * c[2])
    - g4 * c[3]) - g4 * c[4];
   }
  *px = x;
  }
 else
  {
  float xpy[(sizeof c / sizeof c[0])], xpz[(sizeof c / sizeof c[0])], xpx[2];
  int acsize = (sizeof c / sizeof c[0]);
  short xexp;

  g = x;
  _FDunscale(&xexp, &g);
  if (xexp < 24 + 1 + (1 << 3))
   _FXp_setw(xpz, acsize, x);
  else
   {
   xexp = (xexp - (24 + 1)) >> 3;
   _FDscale(&x, -(xexp << 3));
   _FXp_setw(xpx, 2, x);

   memcpy(xpz, &b[xexp - 1][0], (sizeof c / sizeof c[0]) * sizeof (b[0][0]));
   _FXp_mulh(xpz, acsize, xpx[0]);
   if (xpx[1] != 0.0F)
    {
    memcpy(xpy, &b[xexp - 1][0], (sizeof c / sizeof c[0]) * sizeof (b[0][0]));
    _FXp_mulh(xpy, acsize, xpx[1]);
    _FXp_addx(xpz, acsize, xpy, acsize);
    }
   }
  for (; xpz[0] < -huge_rad || huge_rad < xpz[0]; )
   {
   g = (xpz[0] + xpz[1]) * inv2pi;
   _FDint(&g, 0);

   _FXp_setw(xpx, 2, -g / 4096.0L);
   memcpy(xpy, c, acsize * sizeof (c[0]));
   _FXp_mulh(xpy, acsize, xpx[0]);

   if (xpx[1] != 0.0F)
    {
    float xpw[(sizeof c / sizeof c[0])];

    acsize = get_acsize(xpz[0]);
    memcpy(xpw, c, acsize * sizeof (c[0]));
    _FXp_mulh(xpw, acsize, xpx[1]);
    _FXp_addx(xpy, acsize, xpw, acsize);
    }
   _FXp_addx(xpz, acsize, xpy, acsize);
   }

  g = (xpz[0] + xpz[1]) * twobypi;
  if (0.0F <= g)
   g += 0.5F;
  else
   g -= 0.5F;
  _FDint(&g, 0);

  if (g != 0.0F)
   {
   float xpw[(sizeof c / sizeof c[0])];

   acsize = get_acsize(xpz[0]);
   memcpy(xpw, c, acsize * sizeof (c[0]));
   _FXp_mulh(xpw, acsize, -g * 0.25F / 4096.0L);
   _FXp_addx(xpz, acsize, xpw, acsize);
   }
  *px = _FXp_getw(xpz, acsize);
  }

 if (g < -0x7fffffffL || 0x7fffffffL < g)
  g = fmodf(g, (float)0x7fffffffL + 1.0F);
 return ((unsigned int)(long)g & 0x3);
 }

static const float pi = 3.14159265358979323846264338327950287F;
static const float piby4 = 0.78539816339744830961566084581987572F;

unsigned int _FQuadph(float *px, float phase)
 {
 unsigned int qoff = _FQuad(px);
 float ph0 = phase;
 float ans;

 _FDint(&ph0, 1);
 phase -= ph0;

 if (ph0 < -0x7fffffffL / 2 || 0x7fffffffL / 2 < ph0)
  ph0 = fmodf(ph0, (float)(0x7fffffffL / 2) + 1.0F);
 qoff += ((unsigned int)(long)(ph0 * 2.0F)) & 0x3;

 ans = *px + phase * pi;
 if (piby4 <= ans)
  {
  phase -= 0.5F;
  ++qoff;
  *px += phase * pi;
  }
 else if (ans <= -piby4)
  {
  phase += 0.5F;
  --qoff;
  *px += phase * pi;
  }
 else
  *px = ans;

 return (qoff);
 }

