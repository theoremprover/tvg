



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



int _Fltrounds(void);








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






char *strchr(const char *, int);
char *strpbrk(const char *, const char *);
char *strrchr(const char *, int);
char *strstr(const char *, const char *);


void *memchr(const void *, int, size_t);










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
typedef _Wchart wchar_t;






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






long double _LLogpoly(long double w);

long double _LXp_getw(long double *, int);
long double *_LXp_setw(long double *, int, long double);
long double *_LXp_addh(long double *, int, long double);
long double *_LXp_mulh(long double *, int, long double);
long double *_LXp_addx(long double *, int, long double *, int);
static const long double log2e[] = {
 (long double)(48408812.0L / 67108864.0L * 2),
 (long double)(43368831.0L / 67108864.0L / 67108864.0L * 2),
 (long double)(3076001.0L / 67108864.0L / 67108864.0L / 67108864.0L * 2),
 (long double)(67031145.0L / 67108864.0L / 67108864.0L / 67108864.0L
  / 67108864.0L * 2),
 };



static long double scale = (long double)(1 << 6);

static long double lnbias[][4] = {
 {
 -33554432.0L * 2 / 67108864.0L,
 -0.0L * 2 / (67108864.0L * 67108864.0L),
 -0.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -32064812.0L * 2 / 67108864.0L,
 -36291593.0L * 2 / (67108864.0L * 67108864.0L),
 -59567263.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -30619666.0L * 2 / 67108864.0L,
 -2733927.0L * 2 / (67108864.0L * 67108864.0L),
 -19949173.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -29216413.0L * 2 / 67108864.0L,
 -53631073.0L * 2 / (67108864.0L * 67108864.0L),
 -52065808.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -27852695.0L * 2 / 67108864.0L,
 -6308503.0L * 2 / (67108864.0L * 67108864.0L),
 -35133437.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -26526343.0L * 2 / 67108864.0L,
 -19181878.0L * 2 / (67108864.0L * 67108864.0L),
 -50595768.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -25235365.0L * 2 / 67108864.0L,
 -7330175.0L * 2 / (67108864.0L * 67108864.0L),
 -24581577.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -23977922.0L * 2 / 67108864.0L,
 -43925272.0L * 2 / (67108864.0L * 67108864.0L),
 -54451491.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -22752317.0L * 2 / 67108864.0L,
 -42359950.0L * 2 / (67108864.0L * 67108864.0L),
 -41999210.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -21556977.0L * 2 / 67108864.0L,
 -38559332.0L * 2 / (67108864.0L * 67108864.0L),
 -13908310.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -20390443.0L * 2 / 67108864.0L,
 -47979806.0L * 2 / (67108864.0L * 67108864.0L),
 -61187749.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -19251360.0L * 2 / 67108864.0L,
 -18411435.0L * 2 / (67108864.0L * 67108864.0L),
 -61732296.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -18138464.0L * 2 / 67108864.0L,
 -66691774.0L * 2 / (67108864.0L * 67108864.0L),
 -8446112.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -17050580.0L * 2 / 67108864.0L,
 -48668454.0L * 2 / (67108864.0L * 67108864.0L),
 -10023783.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -15986607.0L * 2 / 67108864.0L,
 -63598934.0L * 2 / (67108864.0L * 67108864.0L),
 -27753896.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -14945518.0L * 2 / 67108864.0L,
 -5934076.0L * 2 / (67108864.0L * 67108864.0L),
 -35408620.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -13926347.0L * 2 / 67108864.0L,
 -36708683.0L * 2 / (67108864.0L * 67108864.0L),
 -51121150.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -12928192.0L * 2 / 67108864.0L,
 -22542246.0L * 2 / (67108864.0L * 67108864.0L),
 -20133197.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -11950203.0L * 2 / 67108864.0L,
 -17611037.0L * 2 / (67108864.0L * 67108864.0L),
 -16889556.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -10991581.0L * 2 / 67108864.0L,
 -39442611.0L * 2 / (67108864.0L * 67108864.0L),
 -3961460.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -10051575.0L * 2 / 67108864.0L,
 -7216589.0L * 2 / (67108864.0L * 67108864.0L),
 -3330340.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -9129474.0L * 2 / 67108864.0L,
 -40664619.0L * 2 / (67108864.0L * 67108864.0L),
 -3742343.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -8224610.0L * 2 / 67108864.0L,
 -43017187.0L * 2 / (67108864.0L * 67108864.0L),
 -19145723.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -7336350.0L * 2 / 67108864.0L,
 -41942860.0L * 2 / (67108864.0L * 67108864.0L),
 -50445322.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -6464096.0L * 2 / 67108864.0L,
 -11271123.0L * 2 / (67108864.0L * 67108864.0L),
 -10066598.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -5607280.0L * 2 / 67108864.0L,
 -44038859.0L * 2 / (67108864.0L * 67108864.0L),
 -8593864.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -4765367.0L * 2 / 67108864.0L,
 -2798409.0L * 2 / (67108864.0L * 67108864.0L),
 -28619918.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -3937845.0L * 2 / 67108864.0L,
 -55023594.0L * 2 / (67108864.0L * 67108864.0L),
 -46962292.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -3124233.0L * 2 / 67108864.0L,
 -11959770.0L * 2 / (67108864.0L * 67108864.0L),
 -26011496.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -2324069.0L * 2 / 67108864.0L,
 -19923620.0L * 2 / (67108864.0L * 67108864.0L),
 -60922053.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -1536916.0L * 2 / 67108864.0L,
 -52913507.0L * 2 / (67108864.0L * 67108864.0L),
 -22404700.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -762359.0L * 2 / 67108864.0L,
 -17579626.0L * 2 / (67108864.0L * 67108864.0L),
 -45200036.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},

 {
 -0.0L * 2 / 67108864.0L,
 -0.0L * 2 / (67108864.0L * 67108864.0L),
 -0.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L},
 };

long double (_LPow)(long double x, long double y, short *pex)
 {
 long double x1;
 long double yi = y;
 long double z;
 long double *plna = 0;

 long zexp = 0;
 short xexp;
 short neg;

 if (y == 1.0L)
  return (x);

 short errx = _LDunscale(&xexp, &x);
 const short erry = _LDint(&yi, 0);

 static const long double ln2 = 0.69314718055994530941723212145817657L;
 static const long double rthalf = 0.70710678118654752440084436210484904L;
 static const long double maxexp = (long double)(32768);

 if (pex != 0)
  *pex = 0;
 if (erry == 0 && y == 0.0L
  || errx < 0 && xexp == 1
   && (x == 0.5L || erry == 1 && x == -0.5L))
  return (1.0L);
 else if (0 <= errx || 0 < erry)
  {
  if (errx == 2)
   return (x);
  else if (erry == 2)
   return (y);
  else if (errx == 1)
   if (!(((_Lval *)(char *)&(x))->_Sh[3] & ((unsigned short)0x8000)))
    return ((((_Lval *)(char *)&(y))->_Sh[3] & ((unsigned short)0x8000)) ? 0.0L : _LInf._Long_double);
   else if (!(((_Lval *)(char *)&(y))->_Sh[3] & ((unsigned short)0x8000)))
    return (erry == 0 && _LDint(&yi, -1) < 0
     ? -_LInf._Long_double
     : _LInf._Long_double);
   else
    return (erry == 0 && _LDint(&yi, -1) < 0
     ? -_LZero : 0.0L);
  else if (erry == 1)
   if (!(((_Lval *)(char *)&(y))->_Sh[3] & ((unsigned short)0x8000)))
    return (xexp <= 0 ? 0.0L : _LInf._Long_double);
   else
    return (xexp <= 0 ? _LInf._Long_double : 0.0L);
  else
   if (!(((_Lval *)(char *)&(y))->_Sh[3] & ((unsigned short)0x8000)))
    return (erry == 0 && _LDint(&yi, -1) < 0 && (((_Lval *)(char *)&(x))->_Sh[3] & ((unsigned short)0x8000))
     ? -_LZero : 0.0L);
   else
    {
    _Feraise(0x04);
    return (erry == 0 && _LDint(&yi, -1) < 0 && (((_Lval *)(char *)&(x))->_Sh[3] & ((unsigned short)0x8000))
     ? -_LInf._Long_double : _LInf._Long_double);
    }
  }
 else if ((((_Lval *)(char *)&(x))->_Sh[3] & ((unsigned short)0x8000)) && erry < 0)
  {
  _Feraise(0x01);
  return (_LNan._Long_double);
  }

 if (0.0L < x)
  neg = 0;
 else
  {
  x = -x;
  neg = _LDint(&yi, -1);
  }
  {
 int k;
 long double bias, z, w;

 if (xexp == 1 && x < rthalf)
  {
  k = 0;
  bias = 1.0L;
  x *= 2.0L;

  }
 else
  {
  k = (int)(x * scale + 0.5L) - (1 << (6 - 1));
  bias = (long double)k / scale + 0.5L;
  }

 plna = &lnbias[k][0];
 z = (x - bias) / (x + bias);
 w = z * z;

 x -= bias;
 x1 = z * (w * _LLogpoly(w) - x + 2.0L * (1.0L - bias));

 yi = (x + x1) / ln2;
 yi += plna[0] + (long double)xexp;
  }


 z = y * yi;
 if (z < -maxexp)
  errx = 0;
 else if (-15.0L <= z && z <= 15.0L
  && -40.0L < y && y < 40.0L)
  {
  zexp = (long)(z < 0.0L ? z - 0.5L : z + 0.5L);
  z = y * (x + x1);

  if (plna == 0)
   z += (y * (long double)xexp - (long double)zexp) * ln2;
  else
   {
   z += y * (plna[1] + plna[2]) * ln2;
   z += (y * ((long double)xexp + plna[0]) - (long double)zexp) * ln2;
   }
  errx = -1;
  }
 else if (maxexp < z)
  errx = 1;
 else
  {
  long double xpx1[2], xpx[4], xpy[4], xpz[4];
  int i;

  _LXp_setw(xpx, 4, x);
  _LXp_setw(xpx1, 2, x1);
  _LXp_addx(xpx, 4, xpx1, 2);

  if (xpx[0] == 0.0L)
   _LXp_setw(xpy, 4, 0.0L);
  else
   {
   memcpy(xpy, log2e, sizeof (xpy));
   _LXp_mulh(xpy, 4, xpx[0]);
   for (i = 1; i < 4 && xpx[i] != 0.0L; ++i)
    {
    long double xpw[4];

    memcpy(xpw, log2e, sizeof (xpw));
    _LXp_mulh(xpw, 4, xpx[i]);
    _LXp_addx(xpy, 4, xpw, 4);
    }
   }
  if (plna)
   _LXp_addx(xpy, 4, plna, 3);
  _LXp_addh(xpy, 4, (long double)xexp);

  _LXp_setw(xpx, 2, y);
  memcpy(xpz, xpy, sizeof (xpz));
  _LXp_mulh(xpz, 4, xpx[0]);

  if (xpx[1] != 0.0L)
   {
   long double xpw[4];

   memcpy(xpw, xpy, sizeof (xpw));
   _LXp_mulh(xpw, 4, xpx[1]);
   _LXp_addx(xpz, 4, xpw, 4);
   }

  x = xpz[0];
  if (xpz[0] != 0 && xpz[1] != 0)
   x += xpz[1] + xpz[2];
  _LDint(&x, 0);
  _LXp_addh(xpz, 4, -x);
  z = _LXp_getw(xpz, 4);
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
  errx = _LExp(&z, 1.0L, zexp);
   }
 switch (errx)
  {
 case 0:
  z = 0.0L;
  _Feraise(0x10);
  break;

 case 1:
  z = _LInf._Long_double;
  _Feraise(0x08);
  }
 return (neg != 0 ? -z : z);
  }

long double (powl)(long double x, long double y)
 {
 return (_LPow(x, y, 0));
 }

