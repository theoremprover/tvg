# 1 "/arm-libs/library-src/dinkum/source/./lgamma.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./lgamma.c" 2

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
# 3 "/arm-libs/library-src/dinkum/source/./lgamma.c" 2
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


double _Tgamma(double *px, short *pex);
extern const double _Gamma_big;
# 272 "/arm-libs/library-src/dinkum/source/./xxlgamma.h"
static const double c[] = {
 0.00079365079365079364,
 -0.00277777777777777777,
 0.08333333333333333333,
 };



static const double approx_num[] = {
 0.66454129398121535842e-6,
 -0.37089172292679657345e-5,
 0.14370364744985202372e-4,
 -0.49032574258831844388e-4,
 0.16172652190598458749e-3,
 -0.54911719631636623561e-3,
 0.20525849632769452598e-2,
 -0.94072115542101477245e-2,
 0.68944965881171283469e-1,
 0.45072592886235283741,
 0.34855136383920749813,
 -0.25793049510317640659,
 -0.11473652220196855901,
 };

static const double approx_den[] = {
 0.38904024299563881796,
 1.25390477499242941917,
 0.99562329726629917468,
 };

static const double approx_mid = 3039929748475085.0
  / 2251799813685248.0;



struct RootApprox_d53 {
 double from, to;
 double zero1, zero2;
 int size;
 };

static const struct RootApprox_d53 rootapprox[] = {
 {-7.00023, -7.00017,
 -7881522651463199.0 / 9007199254740992.0 * 8.0,
 634929214277839.0 / 9007199254740992.0
  / 281474976710656.0,
 5},

 {-6.99988, -6.99976,
 -7881075865650928.0 / 9007199254740992.0 * 8.0,
 3423712940645379.0 / 9007199254740992.0
  / 9007199254740992.0 / 4.0,
 5},

 {-6.00176, -6.00137,
 -6756959143951501.0 / 9007199254740992.0 * 8.0,
 2602576852966713.0 / 9007199254740992.0
  / 4503599627370496.0,
 25},

 {-5.9987, -5.9985,
 -6753831603008526.0 / 9007199254740992.0 * 8.0,
 5648571687346535.0 / 9007199254740992.0
  / 1125899906842624.0,
 5},

 {-5.00992, -5.0066,
 -5638752369161946.0 / 9007199254740992.0 * 8.0,
 8561733107791203.0 / 9007199254740992.0
  / 1125899906842624.0,
 22},

 {-4.9916, -4.991,
 -5619979645807357.0 / 9007199254740992.0 * 8.0,
 6155472696447695.0 / 9007199254740992.0
  / 4503599627370496.0,
 13},

 {-4.05, -4.0338,
 -4547917119067521.0 / 9007199254740992.0 * 8.0,
 4288511774695039.0 / 9007199254740992.0
  / 2251799813685248.0,
 25},

 {-3.957, -3.943,
 -8906530933714918.0 / 9007199254740992.0 * 4.0,
 8601666998595293.0 / 9007199254740992.0
  / 2251799813685248.0,
 27},

 {-3.177, -3.121,
 -7078714858690992.0 / 9007199254740992.0 * 4.0,
 2290973324514835.0 / 9007199254740992.0
  / 1125899906842624.0,
 23},

 {-2.821, -2.66,
 -6187231271966977.0 / 9007199254740992.0 * 4.0,
 7346564866404229.0 / 9007199254740992.0
  / 9007199254740992.0,
 32},

 {-2.66, -2.28,
 -5532727847745645.0 / 9007199254740992.0 * 4.0,
 4127608264937587.0 / 9007199254740992.0
  / 1125899906842624.0,
 42},
 };

static const double coeffs[] = {

 161568434353282.84645,
 42725890801.879194201,
 12710958.833951394096,
 5044.0299411108291964,
 -0.12175258490193364201e-27,


 161052333144473.79179,
 -42623489764.758116460,
 12690641.166047179245,
 -5035.9673737681254144,
 -0.20581818739894356439e-29,


 .16702384292615087643e68,
 .24143708154808177927e65,
 .34966424302971768883e62,
 .50745402854220778545e59,
 .73812191351926529454e56,
 .10763328342636469567e54,
 .15738733442238088969e51,
 .23085296615878999430e48,
 .33978679185880898173e45,
 .50208507521853188549e42,
 .74521678897734857594e39,
 .11117558137197869630e37,
 .16684515921763651638e34,
 .25214146210905917811e31,
 .38421918575710539199e28,
 .59139634091676227619e25,
 .92166532952423812779e22,
 14591747070346194018.,
 23582843991458177.616,
 39203019565186.829430,
 67884656938.770961215,
 125386984.94090366666,
 260548.41030309396963,
 723.73662992528012397,
 0.80774867792891736990e-30,


 66486633581.993339903,
 -123445282.15783688645,
 257851.58963956261716,
 -716.24543042754729207,
 0.24868202361581571484e-29,


 .29343034918502210617e43,
 .25320330005878996474e41,
 .21903866733887263598e39,
 .19001020060889468230e37,
 .16533908593556384527e35,
 .14437084659298575148e33,
 .12655615127215832952e31,
 .11143497358009343437e29,
 .98623685989765983922e26,
 .87804822306338075083e24,
 .78719433736243702136e22,
 71162251237406755435.,
 649803732092997587.50,
 6007721002863435.3097,
 56425671356242.343690,
 541001609074.77377555,
 5335250743.3843846461,
 54807486.356655854741,
 600556.16845281092210,
 7404.7624322286821683,
 123.36218456335339335,
 -0.42308313888395786116e-29,


 .62407079930079160892e24,
 -.57564468262543762995e22,
 53540109714248596276.,
 -503000968987169457.12,
 4784685740359657.1393,
 -46235700276520.071504,
 456096042262.19758599,
 -4627747171.7392495881,
 48911582.709059044464,
 -551419.99785128835644,
 6995.2359548940638411,
 -116.53578161624363080,
 -0.11238146637063361009e-30,


 .21776066140073500912e33,
 .89441324408859604541e31,
 .36806012174521234933e30,
 .15177405599732660536e29,
 .62728113723849412533e27,
 .25990462732887708421e26,
 .10798675638117843503e25,
 .45005901869396575263e23,
 .18822347903091117223e22,
 79027439114890969640.,
 3332855778798614561.3,
 141278668508269686.59,
 6024404000960524.0985,
 258689045256062.42226,
 11200724416202.002935,
 489867910498.20017235,
 21692364960.713309683,
 975830163.53821450990,
 44812215.950155709152,
 2116669.4175775350565,
 104145.55520145541151,
 5465.6918207771342500,
 324.25532293784715720,
 26.790480886140593262,
 0.96342376492948458900e-31,


 .47379169342948857155e34,
 -.22028444342134064193e33,
 .10258305810079954444e32,
 -.47854424074312096897e30,
 .22366101714185069414e29,
 -.10475065040607433448e28,
 .49171003748270800414e26,
 -.23139209334600083497e25,
 .10919246178382310542e24,
 -.51686757445722645660e22,
 .24551117958856703321e21,
 -11707469715963823368.,
 560775863730503686.54,
 -26998338793041111.225,
 1307561713653824.5426,
 -63769616182266.903880,
 3135952925293.2922916,
 -155772242416.57216815,
 7834398183.7383184021,
 -400276998.18746365804,
 20877114.726503329247,
 -1119991.5016655221355,
 62588.081918766060559,
 -3730.6047156806125108,
 251.71468258688940014,
 -20.725060845803705677,
 -0.43412455472202368124e-30,


 159019845254241963.38,
 23919458772922345.047,
 3606095996440261.2531,
 545017333309682.24398,
 82601521376228.045059,
 12557646866390.601851,
 1915727973453.9403584,
 293399385882.33045051,
 45135583357.515278640,
 6979115396.6713459663,
 1085573221.5700050825,
 170037345.82174312364,
 26855525.055575888049,
 4284377.4436459796216,
 692049.60228977461199,
 113559.66423713516225,
 19023.061984356358371,
 3277.1937854953417450,
 588.89074223800134673,
 112.26898629717600257,
 25.831338372387958936,
 7.7818846581313508722,
 -0.10431866962787154032e-30,


 -111757730480459360.89,
 29138361921333636.227,
 -7605635543471837.3634,
 1987570752795063.8740,
 -520072613705173.85733,
 136270397145885.24852,
 -35758721374667.469826,
 9398485347630.6486690,
 -2474505336668.4851125,
 652740666207.52570348,
 -172540549493.28646964,
 45711723541.795551153,
 -12140906400.864337912,
 3233548126.0760565808,
 -863873252.92932095220,
 231593359.80352937599,
 -62330684.832407414773,
 16850484.270697366178,
 -4578716.5945817174816,
 1251567.4465251002019,
 -344497.59343483289168,
 95617.451643349703165,
 -26804.578420618108710,
 7610.3979866583287295,
 -2193.1127764806237429,
 646.90599971292827571,
 -194.76615530344621664,
 62.627282713513713193,
 -20.095134916842602581,
 9.5751894757096666708,
 -1.9143501856115988164,
 0.63609282465313460000e-32,


 2134602288151.7325973,
 1001825705477.8754020,
 468557430102.87333136,
 220358151446.77905511,
 103107284381.27778465,
 48612189337.342165762,
 22750853238.533041144,
 10759909800.736022102,
 5035014000.2995260339,
 2390700936.0723962024,
 1117953985.0661735268,
 533518885.54834525763,
 249118974.61233805307,
 119674503.86711416381,
 55731833.611605620914,
 27007615.310435627948,
 12522259.656340321558,
 6139385.8152913544239,
 2827023.6968710568926,
 1407979.5933782963805,
 641568.32774174789445,
 326433.70222156329746,
 146431.99829213393467,
 76721.036023984288730,
 33630.457055794339177,
 18347.170699977025256,
 7776.1581023442464729,
 4487.2184531889983114,
 1811.2054188018712620,
 1130.4578581775129886,
 425.17984568438211700,
 296.43641350761319567,
 100.64812354551159120,
 82.229156827042532426,
 24.038823062292954289,
 24.824942121894071095,
 5.8004145665998724718,
 8.7217825838153462180,
 1.4112911430779799506,
 4.8583209516339961204,
 1.5156034480216573216,
 0.20646555198525407000e-32,
 };
# 1627 "/arm-libs/library-src/dinkum/source/./xxlgamma.h"
static const double half_ln2pim1 =
 0.41893853320467274178032973640561765;

static double _Lgamma_big(double x)
 {
 double xinv = (1.0 / (x));
 double y = _Log(x, 0) - 1.0;
 double z = x - 0.5;
 double w = xinv * xinv;
 short xexp;

 _Dunscale(&xexp, &z);
 z *= y;
 if (_Dscale(&z, xexp) < 0)
  z += half_ln2pim1 + (((c[0] * w + c[1]) * w + c[2]) * xinv);
 else
  _Feraise(0x04);
 return (z);
 }

static const double loge2hi = (double)(5814539.0 / 8388608.0);
static const double loge2lo = 1.1730463525082348212145817656807550e-7;

static const double pi = 3.1415926535897932384626433832795029;

double (lgamma)(double x)
 {
 double y = x;

 switch (_Dint(&y, 0))
  {
 case 2:
  return (x);

 case 1:
  return (_Inf._Double);

 case 0:
  if (x <= 0.0)
   {
   _Feraise(0x02);
   return (_Inf._Double);
   }

 default:
  if (x < -_Gamma_big)
   return (_Log(
    ((pi) / ((x * _Sinx(pi * (x - y), 0, 0)))), 0)
     - _Lgamma_big(-x));
  else if (_Gamma_big <= x)
   return (_Lgamma_big(x));
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
    double y = x;

    y = ((y - rootapprox[i].zero1) - rootapprox[i].zero2);
    return (_Poly(y, &coeffs[offset],
     rootapprox[i].size - 1));
    }
   else if (1.2 < x && x < 1.5)
    {
    double y = x - approx_mid;

    return (((_Poly(y, approx_num, ((sizeof (approx_num) / sizeof ((approx_num)[0])) - 1))) / (_Poly(y, approx_den, ((sizeof (approx_den) / sizeof ((approx_den)[0])) - 1)))));

    }
   else
    {
    short xexp;
    double z;

    y = _Tgamma(&x, &xexp);
    ((*_Pmsw(&(x))) &= ~((unsigned short)0x8000));
    x = -_Log(x, 0);
    z = -xexp;
    x += loge2lo * z;
    x += loge2hi * z;
    x += log1p(y);
    return (x);
    }
   }
  }
 }
# 4 "/arm-libs/library-src/dinkum/source/./lgamma.c" 2

