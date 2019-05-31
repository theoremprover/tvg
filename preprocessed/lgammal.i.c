# 1 "/arm-libs/library-src/dinkum/source/./lgammal.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./lgammal.c" 2

# 1 "/arm-libs/library-src/dinkum/source/./xxlftype.h" 1

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
# 3 "/arm-libs/library-src/dinkum/source/./xxlftype.h" 2
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
# 4 "/arm-libs/library-src/dinkum/source/./xxlftype.h" 2
# 3 "/arm-libs/library-src/dinkum/source/./lgammal.c" 2
# 1 "/arm-libs/library-src/dinkum/source/./xxlgamma.h" 1

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
# 3 "/arm-libs/library-src/dinkum/source/./xxlgamma.h" 2


long double _LTgamma(long double *px, short *pex);
extern const long double _LGamma_big;
# 272 "/arm-libs/library-src/dinkum/source/./xxlgamma.h"
static const long double c[] = {
 0.00079365079365079364L,
 -0.00277777777777777777L,
 0.08333333333333333333L,
 };



static const long double approx_num[] = {
 0.66454129398121535842e-6L,
 -0.37089172292679657345e-5L,
 0.14370364744985202372e-4L,
 -0.49032574258831844388e-4L,
 0.16172652190598458749e-3L,
 -0.54911719631636623561e-3L,
 0.20525849632769452598e-2L,
 -0.94072115542101477245e-2L,
 0.68944965881171283469e-1L,
 0.45072592886235283741L,
 0.34855136383920749813L,
 -0.25793049510317640659L,
 -0.11473652220196855901L,
 };

static const long double approx_den[] = {
 0.38904024299563881796L,
 1.25390477499242941917L,
 0.99562329726629917468L,
 };

static const long double approx_mid = 3039929748475085.0L
  / 2251799813685248.0L;



struct RootApprox_d53 {
 long double from, to;
 long double zero1, zero2;
 int size;
 };

static const struct RootApprox_d53 rootapprox[] = {
 {-7.00023L, -7.00017L,
 -7881522651463199.0L / 9007199254740992.0L * 8.0L,
 634929214277839.0L / 9007199254740992.0L
  / 281474976710656.0L,
 5},

 {-6.99988L, -6.99976L,
 -7881075865650928.0L / 9007199254740992.0L * 8.0L,
 3423712940645379.0L / 9007199254740992.0L
  / 9007199254740992.0L / 4.0L,
 5},

 {-6.00176L, -6.00137L,
 -6756959143951501.0L / 9007199254740992.0L * 8.0L,
 2602576852966713.0L / 9007199254740992.0L
  / 4503599627370496.0L,
 25},

 {-5.9987L, -5.9985L,
 -6753831603008526.0L / 9007199254740992.0L * 8.0L,
 5648571687346535.0L / 9007199254740992.0L
  / 1125899906842624.0L,
 5},

 {-5.00992L, -5.0066L,
 -5638752369161946.0L / 9007199254740992.0L * 8.0L,
 8561733107791203.0L / 9007199254740992.0L
  / 1125899906842624.0L,
 22},

 {-4.9916L, -4.991L,
 -5619979645807357.0L / 9007199254740992.0L * 8.0L,
 6155472696447695.0L / 9007199254740992.0L
  / 4503599627370496.0L,
 13},

 {-4.05L, -4.0338L,
 -4547917119067521.0L / 9007199254740992.0L * 8.0L,
 4288511774695039.0L / 9007199254740992.0L
  / 2251799813685248.0L,
 25},

 {-3.957L, -3.943L,
 -8906530933714918.0L / 9007199254740992.0L * 4.0L,
 8601666998595293.0L / 9007199254740992.0L
  / 2251799813685248.0L,
 27},

 {-3.177L, -3.121L,
 -7078714858690992.0L / 9007199254740992.0L * 4.0L,
 2290973324514835.0L / 9007199254740992.0L
  / 1125899906842624.0L,
 23},

 {-2.821L, -2.66L,
 -6187231271966977.0L / 9007199254740992.0L * 4.0L,
 7346564866404229.0L / 9007199254740992.0L
  / 9007199254740992.0L,
 32},

 {-2.66L, -2.28L,
 -5532727847745645.0L / 9007199254740992.0L * 4.0L,
 4127608264937587.0L / 9007199254740992.0L
  / 1125899906842624.0L,
 42},
 };

static const long double coeffs[] = {

 161568434353282.84645L,
 42725890801.879194201L,
 12710958.833951394096L,
 5044.0299411108291964L,
 -0.12175258490193364201e-27L,


 161052333144473.79179L,
 -42623489764.758116460L,
 12690641.166047179245L,
 -5035.9673737681254144L,
 -0.20581818739894356439e-29L,


 .16702384292615087643e68L,
 .24143708154808177927e65L,
 .34966424302971768883e62L,
 .50745402854220778545e59L,
 .73812191351926529454e56L,
 .10763328342636469567e54L,
 .15738733442238088969e51L,
 .23085296615878999430e48L,
 .33978679185880898173e45L,
 .50208507521853188549e42L,
 .74521678897734857594e39L,
 .11117558137197869630e37L,
 .16684515921763651638e34L,
 .25214146210905917811e31L,
 .38421918575710539199e28L,
 .59139634091676227619e25L,
 .92166532952423812779e22L,
 14591747070346194018.L,
 23582843991458177.616L,
 39203019565186.829430L,
 67884656938.770961215L,
 125386984.94090366666L,
 260548.41030309396963L,
 723.73662992528012397L,
 0.80774867792891736990e-30L,


 66486633581.993339903L,
 -123445282.15783688645L,
 257851.58963956261716L,
 -716.24543042754729207L,
 0.24868202361581571484e-29L,


 .29343034918502210617e43L,
 .25320330005878996474e41L,
 .21903866733887263598e39L,
 .19001020060889468230e37L,
 .16533908593556384527e35L,
 .14437084659298575148e33L,
 .12655615127215832952e31L,
 .11143497358009343437e29L,
 .98623685989765983922e26L,
 .87804822306338075083e24L,
 .78719433736243702136e22L,
 71162251237406755435.L,
 649803732092997587.50L,
 6007721002863435.3097L,
 56425671356242.343690L,
 541001609074.77377555L,
 5335250743.3843846461L,
 54807486.356655854741L,
 600556.16845281092210L,
 7404.7624322286821683L,
 123.36218456335339335L,
 -0.42308313888395786116e-29L,


 .62407079930079160892e24L,
 -.57564468262543762995e22L,
 53540109714248596276.L,
 -503000968987169457.12L,
 4784685740359657.1393L,
 -46235700276520.071504L,
 456096042262.19758599L,
 -4627747171.7392495881L,
 48911582.709059044464L,
 -551419.99785128835644L,
 6995.2359548940638411L,
 -116.53578161624363080L,
 -0.11238146637063361009e-30L,


 .21776066140073500912e33L,
 .89441324408859604541e31L,
 .36806012174521234933e30L,
 .15177405599732660536e29L,
 .62728113723849412533e27L,
 .25990462732887708421e26L,
 .10798675638117843503e25L,
 .45005901869396575263e23L,
 .18822347903091117223e22L,
 79027439114890969640.L,
 3332855778798614561.3L,
 141278668508269686.59L,
 6024404000960524.0985L,
 258689045256062.42226L,
 11200724416202.002935L,
 489867910498.20017235L,
 21692364960.713309683L,
 975830163.53821450990L,
 44812215.950155709152L,
 2116669.4175775350565L,
 104145.55520145541151L,
 5465.6918207771342500L,
 324.25532293784715720L,
 26.790480886140593262L,
 0.96342376492948458900e-31L,


 .47379169342948857155e34L,
 -.22028444342134064193e33L,
 .10258305810079954444e32L,
 -.47854424074312096897e30L,
 .22366101714185069414e29L,
 -.10475065040607433448e28L,
 .49171003748270800414e26L,
 -.23139209334600083497e25L,
 .10919246178382310542e24L,
 -.51686757445722645660e22L,
 .24551117958856703321e21L,
 -11707469715963823368.L,
 560775863730503686.54L,
 -26998338793041111.225L,
 1307561713653824.5426L,
 -63769616182266.903880L,
 3135952925293.2922916L,
 -155772242416.57216815L,
 7834398183.7383184021L,
 -400276998.18746365804L,
 20877114.726503329247L,
 -1119991.5016655221355L,
 62588.081918766060559L,
 -3730.6047156806125108L,
 251.71468258688940014L,
 -20.725060845803705677L,
 -0.43412455472202368124e-30L,


 159019845254241963.38L,
 23919458772922345.047L,
 3606095996440261.2531L,
 545017333309682.24398L,
 82601521376228.045059L,
 12557646866390.601851L,
 1915727973453.9403584L,
 293399385882.33045051L,
 45135583357.515278640L,
 6979115396.6713459663L,
 1085573221.5700050825L,
 170037345.82174312364L,
 26855525.055575888049L,
 4284377.4436459796216L,
 692049.60228977461199L,
 113559.66423713516225L,
 19023.061984356358371L,
 3277.1937854953417450L,
 588.89074223800134673L,
 112.26898629717600257L,
 25.831338372387958936L,
 7.7818846581313508722L,
 -0.10431866962787154032e-30L,


 -111757730480459360.89L,
 29138361921333636.227L,
 -7605635543471837.3634L,
 1987570752795063.8740L,
 -520072613705173.85733L,
 136270397145885.24852L,
 -35758721374667.469826L,
 9398485347630.6486690L,
 -2474505336668.4851125L,
 652740666207.52570348L,
 -172540549493.28646964L,
 45711723541.795551153L,
 -12140906400.864337912L,
 3233548126.0760565808L,
 -863873252.92932095220L,
 231593359.80352937599L,
 -62330684.832407414773L,
 16850484.270697366178L,
 -4578716.5945817174816L,
 1251567.4465251002019L,
 -344497.59343483289168L,
 95617.451643349703165L,
 -26804.578420618108710L,
 7610.3979866583287295L,
 -2193.1127764806237429L,
 646.90599971292827571L,
 -194.76615530344621664L,
 62.627282713513713193L,
 -20.095134916842602581L,
 9.5751894757096666708L,
 -1.9143501856115988164L,
 0.63609282465313460000e-32L,


 2134602288151.7325973L,
 1001825705477.8754020L,
 468557430102.87333136L,
 220358151446.77905511L,
 103107284381.27778465L,
 48612189337.342165762L,
 22750853238.533041144L,
 10759909800.736022102L,
 5035014000.2995260339L,
 2390700936.0723962024L,
 1117953985.0661735268L,
 533518885.54834525763L,
 249118974.61233805307L,
 119674503.86711416381L,
 55731833.611605620914L,
 27007615.310435627948L,
 12522259.656340321558L,
 6139385.8152913544239L,
 2827023.6968710568926L,
 1407979.5933782963805L,
 641568.32774174789445L,
 326433.70222156329746L,
 146431.99829213393467L,
 76721.036023984288730L,
 33630.457055794339177L,
 18347.170699977025256L,
 7776.1581023442464729L,
 4487.2184531889983114L,
 1811.2054188018712620L,
 1130.4578581775129886L,
 425.17984568438211700L,
 296.43641350761319567L,
 100.64812354551159120L,
 82.229156827042532426L,
 24.038823062292954289L,
 24.824942121894071095L,
 5.8004145665998724718L,
 8.7217825838153462180L,
 1.4112911430779799506L,
 4.8583209516339961204L,
 1.5156034480216573216L,
 0.20646555198525407000e-32L,
 };
# 1627 "/arm-libs/library-src/dinkum/source/./xxlgamma.h"
static const long double half_ln2pim1 =
 0.41893853320467274178032973640561765L;

static long double _LLgamma_big(long double x)
 {
 long double xinv = (1.0L / (x));
 long double y = _LLog(x, 0) - 1.0L;
 long double z = x - 0.5L;
 long double w = xinv * xinv;
 short xexp;

 _LDunscale(&xexp, &z);
 z *= y;
 if (_LDscale(&z, xexp) < 0)
  z += half_ln2pim1 + (((c[0] * w + c[1]) * w + c[2]) * xinv);
 else
  _Feraise(0x04);
 return (z);
 }

static const long double loge2hi = (long double)(5814539.0 / 8388608.0);
static const long double loge2lo = 1.1730463525082348212145817656807550e-7L;

static const long double pi = 3.1415926535897932384626433832795029L;

long double (lgammal)(long double x)
 {
 long double y = x;

 switch (_LDint(&y, 0))
  {
 case 2:
  return (x);

 case 1:
  return (_LInf._Long_double);

 case 0:
  if (x <= 0.0L)
   {
   _Feraise(0x02);
   return (_LInf._Long_double);
   }

 default:
  if (x < -_LGamma_big)
   return (_LLog(
    ((pi) / ((x * _LSinx(pi * (x - y), 0, 0)))), 0)
     - _LLgamma_big(-x));
  else if (_LGamma_big <= x)
   return (_LLgamma_big(x));
  else
   {
   int offset = 0;
   int i;

   for (i = 0; i < (sizeof (rootapprox) / sizeof (rootapprox[0])); offset += rootapprox[i].size, ++i)
    {
    if (rootapprox[i].from < x && x < rootapprox[i].to)
     break;
    }

   if (i < (sizeof (rootapprox) / sizeof (rootapprox[0])))
    {
    long double y = x;

    y = ((y - rootapprox[i].zero1) - rootapprox[i].zero2);
    return (_LPoly(y, &coeffs[offset],
     rootapprox[i].size - 1));
    }
   else if (1.2L < x && x < 1.5L)
    {
    long double y = x - approx_mid;

    return (((_LPoly(y, approx_num, ((sizeof (approx_num) / sizeof ((approx_num)[0])) - 1))) / (_LPoly(y, approx_den, ((sizeof (approx_den) / sizeof ((approx_den)[0])) - 1)))));

    }
   else
    {
    short xexp;
    long double z;

    y = _LTgamma(&x, &xexp);
    ((*_LPmsw(&(x))) &= ~((unsigned short)0x8000));
    x = -_LLog(x, 0);
    z = -xexp;
    x += loge2lo * z;
    x += loge2hi * z;
    x += log1pl(y);
    return (x);
    }
   }
  }
 }
# 4 "/arm-libs/library-src/dinkum/source/./lgammal.c" 2

