








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






long double _LXp_getw(long double *, int);
long double *_LXp_setw(long double *, int, long double);
long double *_LXp_mulh(long double *, int, long double);
long double *_LXp_addx(long double *, int, long double *, int);
static const long double c[] = {
 (long double)(52707178.0L * 8),
 (long double)(35788428.0L * 8 / 67108864.0L),
 (long double)(9253169.0L * 8 / (67108864.0L * 67108864.0L)),
 (long double)(40012672.0L * 8 / ((67108864.0L * 67108864.0L) * 67108864.0L)),
 (long double)(57701188.0L * 8 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L))),
 (long double)(43001056.0L * 8 / (((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) * 67108864.0L)),
 };

static const long double b[][(sizeof c / sizeof c[0])]={
 {
 (long double)(-44650192.0L * 4 / 67108864.0L),
 (long double)(24373128.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(52217969.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(32495948.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(52430789.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(10049912.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(52311224.0L * 4 / 67108864.0L),
 (long double)(32151194.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(3521788.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(17658793.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(12653430.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(28545187.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(51596056.0L * 4 / 67108864.0L),
 (long double)(34529797.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(57427415.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(47345698.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(19105487.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(33321368.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(45168630.0L * 2 / 67108864.0L),
 (long double)(26184515.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(34707788.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(8089061.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(57195156.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(2619245.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(65114761.0L * 2 / 67108864.0L),
 (long double)(20027062.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(47489540.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(14214764.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(59230352.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(31139624.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(52534019.0L * 4 / 67108864.0L),
 (long double)(47861617.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(57586889.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(3460398.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(36832902.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(41682469.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-6849158.0L * 2 / 67108864.0L),
 (long double)(17949024.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(45979062.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(9676238.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(39833771.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(34623173.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(45206937.0L * 4 / 67108864.0L),
 (long double)(30064030.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(1277106.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(31481331.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(32310796.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(1285356.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-45881600.0L * 4 / 67108864.0L),
 (long double)(65668541.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(35499664.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(66210623.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(58493317.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(65982999.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(1294181.0L * 2 / 67108864.0L),
 (long double)(64808977.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(54574838.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(55770472.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(10000946.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(4603453.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-51696412.0L * 4 / 67108864.0L),
 (long double)(16119248.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(25386904.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(11185286.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(56753234.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(29265903.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(52319465.0L * 2 / 67108864.0L),
 (long double)(34308466.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(21037701.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(49304840.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(8546391.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(10934690.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(36475572.0L * 2 / 67108864.0L),
 (long double)(15585858.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(7042195.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(63308505.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(34738113.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(43227007.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-57543755.0L * 2 / 67108864.0L),
 (long double)(13151969.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(61014092.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(51172192.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(36610653.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(11303136.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(62328609.0L * 2 / 67108864.0L),
 (long double)(17373473.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(20271818.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(2732297.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(44166545.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(13628803.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(48097582.0L * 2 / 67108864.0L),
 (long double)(63049219.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(54727889.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(30569368.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(45941495.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(33618616.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-34138911.0L * 4 / 67108864.0L),
 (long double)(472447.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(24191272.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(66788033.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(42935532.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(43807839.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(64560050.0L * 2 / 67108864.0L),
 (long double)(12046811.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(9847201.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(48880213.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(61123341.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(34876678.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(39872682.0L * 4 / 67108864.0L),
 (long double)(22873682.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(31384146.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(15814545.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(47398232.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(41988367.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(32224897.0L * 2 / 67108864.0L),
 (long double)(52604533.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(21898454.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(52106995.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(4539170.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(13183912.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-13594073.0L * 2 / 67108864.0L),
 (long double)(12725775.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(28801379.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(18128404.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(44774589.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(60130533.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-46124738.0L * 2 / 67108864.0L),
 (long double)(21613885.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(34957299.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(53333399.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(36591729.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(62362664.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-6995674.0L * 2 / 67108864.0L),
 (long double)(54175033.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(22625417.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(23179700.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(52525623.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(17345860.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-3462232.0L * 2 / 67108864.0L),
 (long double)(46003782.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(45190672.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(51960393.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(2449619.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(8368730.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-34168926.0L * 2 / 67108864.0L),
 (long double)(14513990.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(63122172.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(36035659.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(1080650.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(47601380.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-30221623.0L * 2 / 67108864.0L),
 (long double)(39344933.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(48788887.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(35265370.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(2271449.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(43393681.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-44514277.0L * 2 / 67108864.0L),
 (long double)(34044011.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(63424890.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(19238758.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(52815139.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(23109538.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-36385577.0L * 2 / 67108864.0L),
 (long double)(57863133.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(20694677.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(52649708.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(48587469.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(7820136.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-36403504.0L * 4 / 67108864.0L),
 (long double)(64031972.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(61650298.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(32999103.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(37147537.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(10436430.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(38413979.0L * 4 / 67108864.0L),
 (long double)(22582941.0L * 4 / (67108864.0L * 67108864.0L)),
 (long double)(1611179.0L * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(21108706.0L * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(27622625.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(37960282.0L * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},

 {
 (long double)(-4138705.0L * 2 / 67108864.0L),
 (long double)(41933462.0L * 2 / (67108864.0L * 67108864.0L)),
 (long double)(51111950.0L * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (long double)(51269386.0L * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (long double)(59561043.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (long double)(6069291.0L * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 };

static const long double huge_rad = (long double)(0.63661977236758134307553505349005744L
 * (67108864.0L * 67108864.0L * 2) / 67108864.0L);
static const long double inv2pi = 0.15915494309189533576888376337251436L;
static const long double twobypi = 0.63661977236758134307553505349005745L;

static int get_acsize(long double x)
 {
 int acsize;
 short xexp;

 _LDunscale(&xexp, &x);
 acsize = 2 * (xexp / 53) + 5;
 return (acsize <= (sizeof c / sizeof c[0]) ? acsize
  : (int)(sizeof c / sizeof c[0]));
 }

unsigned int _LQuad(long double *px)
 {
 long double x = *px;
 long double g;

 if (-huge_rad < x && x < huge_rad)
  {
  g = x * twobypi;
  if (0.0L <= g)
   g += 0.5L;
  else
   g -= 0.5L;
  _LDint(&g, 0);
  if (g != 0.0L)
   {
   long double g4 = g * 0.25L / 67108864.0L;

   x = ((((x - g4 * c[0]) - g4 * c[1]) - g4 * c[2])
    - g4 * c[3]) - g4 * c[4];
   }
  *px = x;
  }
 else
  {
  long double xpy[(sizeof c / sizeof c[0])], xpz[(sizeof c / sizeof c[0])], xpx[2];
  int acsize = (sizeof c / sizeof c[0]);
  short xexp;

  g = x;
  _LDunscale(&xexp, &g);
  if (xexp < 53 + 1 + (1 << 5))
   _LXp_setw(xpz, acsize, x);
  else
   {
   xexp = (xexp - (53 + 1)) >> 5;
   _LDscale(&x, -(xexp << 5));
   _LXp_setw(xpx, 2, x);

   memcpy(xpz, &b[xexp - 1][0], (sizeof c / sizeof c[0]) * sizeof (b[0][0]));
   _LXp_mulh(xpz, acsize, xpx[0]);
   if (xpx[1] != 0.0L)
    {
    memcpy(xpy, &b[xexp - 1][0], (sizeof c / sizeof c[0]) * sizeof (b[0][0]));
    _LXp_mulh(xpy, acsize, xpx[1]);
    _LXp_addx(xpz, acsize, xpy, acsize);
    }
   }
  for (; xpz[0] < -huge_rad || huge_rad < xpz[0]; )
   {
   g = (xpz[0] + xpz[1]) * inv2pi;
   _LDint(&g, 0);

   _LXp_setw(xpx, 2, -g / 67108864.0L);
   memcpy(xpy, c, acsize * sizeof (c[0]));
   _LXp_mulh(xpy, acsize, xpx[0]);

   if (xpx[1] != 0.0L)
    {
    long double xpw[(sizeof c / sizeof c[0])];

    acsize = get_acsize(xpz[0]);
    memcpy(xpw, c, acsize * sizeof (c[0]));
    _LXp_mulh(xpw, acsize, xpx[1]);
    _LXp_addx(xpy, acsize, xpw, acsize);
    }
   _LXp_addx(xpz, acsize, xpy, acsize);
   }

  g = (xpz[0] + xpz[1]) * twobypi;
  if (0.0L <= g)
   g += 0.5L;
  else
   g -= 0.5L;
  _LDint(&g, 0);

  if (g != 0.0L)
   {
   long double xpw[(sizeof c / sizeof c[0])];

   acsize = get_acsize(xpz[0]);
   memcpy(xpw, c, acsize * sizeof (c[0]));
   _LXp_mulh(xpw, acsize, -g * 0.25L / 67108864.0L);
   _LXp_addx(xpz, acsize, xpw, acsize);
   }
  *px = _LXp_getw(xpz, acsize);
  }

 if (g < -0x7fffffffL || 0x7fffffffL < g)
  g = fmodl(g, (long double)0x7fffffffL + 1.0L);
 return ((unsigned int)(long)g & 0x3);
 }

static const long double pi = 3.14159265358979323846264338327950287L;
static const long double piby4 = 0.78539816339744830961566084581987572L;

unsigned int _LQuadph(long double *px, long double phase)
 {
 unsigned int qoff = _LQuad(px);
 long double ph0 = phase;
 long double ans;

 _LDint(&ph0, 1);
 phase -= ph0;

 if (ph0 < -0x7fffffffL / 2 || 0x7fffffffL / 2 < ph0)
  ph0 = fmodl(ph0, (long double)(0x7fffffffL / 2) + 1.0L);
 qoff += ((unsigned int)(long)(ph0 * 2.0L)) & 0x3;

 ans = *px + phase * pi;
 if (piby4 <= ans)
  {
  phase -= 0.5L;
  ++qoff;
  *px += phase * pi;
  }
 else if (ans <= -piby4)
  {
  phase += 0.5L;
  --qoff;
  *px += phase * pi;
  }
 else
  *px = ans;

 return (qoff);
 }
