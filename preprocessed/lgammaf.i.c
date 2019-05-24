# 1 "/arm-libs/library-src/dinkum/source/./lgammaf.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./lgammaf.c" 2

# 1 "/arm-libs/library-src/dinkum/source/./xxfftype.h" 1

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
# 3 "/arm-libs/library-src/dinkum/source/./xxfftype.h" 2
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
# 4 "/arm-libs/library-src/dinkum/source/./xxfftype.h" 2
# 3 "/arm-libs/library-src/dinkum/source/./lgammaf.c" 2
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


float _FTgamma(float *px, short *pex);
extern const float _FGamma_big;
# 16 "/arm-libs/library-src/dinkum/source/./xxlgamma.h"
static const float c[] = {
 0.0833333333F,
 };



static const float approx_num[] = {
 0.5360760204e-3F,
 -0.2213148498e-2F,
 0.7719251053e-2F,
 -0.2947412669e-1F,
 0.1649097161F,
 0.4654077568F,
 -0.1879605312F,
 -0.1152409017F,
 };

static const float approx_den[] = {
 0.6424155168F,
 1.0000000153F,
 };

static const float approx_mid = 11324621.0F / 8388608.0F;



struct RootApprox_f24 {
 float from, to;
 float zero1, zero2;
 int size;
 };

static const struct RootApprox_f24 rootapprox[] = {
 {-7.00023F, -7.00017F,
 -14680480.0F / 16777216.0F * 8.0F,
 1085455.0F / 16777216.0F / 2097152.0F,
 5},

 {-6.99988F, -6.99976F,
 -14679648.0F / 16777216.0F * 8.0F,
 8996753.0F / 16777216.0F / 4194304.0F,
 4},

 {-6.00176F, -6.00137F,
 -12585818.0F / 16777216.0F * 8.0F,
 3468551.0F / 16777216.0F / 524288.0F,
 11},

 {-5.9987F, -5.9985F,
 -12579992.0F / 16777216.0F * 8.0F,
 10936511.0F / 16777216.0F / 4194304.0F,
 3},

 {-5.01F, -5.0064F,
 -10502995.0F / 16777216.0F * 8.0F,
 4225609.0F / 16777216.0F / 2097152.0F,
 10},

 {-4.99266F, -4.991F,
 -10468028.0F / 16777216.0F * 8.0F,
 364821.0F / 16777216.0F / 262144.0F,
 9},

 {-4.062F, -4.0255F,
 -8471156.0F / 16777216.0F * 8.0F,
 1002709.0F / 16777216.0F / 524288.0F,
 23},

 {-3.965F, -3.953F,
 -16589707.0F / 16777216.0F * 4.0F,
 6037121.0F / 16777216.0F / 4194304.0F,
 10},

 {-3.1962F, -3.121F,
 -13185134.0F / 16777216.0F * 4.0F,
 3545701.0F / 16777216.0F / 8388608.0F,
 16},

 {-2.864F, -2.65F,
 -11524617.0F / 16777216.0F * 4.0F,
 1434663.0F / 16777216.0F / 524288.0F,
 19},

 {-2.65F, -2.265F,
 -10305509.0F / 16777216.0F * 4.0F,
 10481785.0F / 16777216.0F / 8388608.0F,
 18},

 {2.72F, 2.77F,
 11492392.0F / 16777216.0F * 4.0F,
 16106127.0F / 16777216.0F / 4194304.0F,
 4},
 };

static const float coeffs[] = {

 .1615684343e15F,
 .4272589080e11F,
 12710958.83F,
 5044.029941F,
 -0.5743202284e-11F,


 -.4262348976e11F,
 12690641.17F,
 -5035.967374F,
 0.5313011066e-13F,


 .3842191857e28F,
 .5913963410e25F,
 .9216653296e22F,
 .1459174708e20F,
 .2358284399e17F,
 .3920301958e14F,
 .6788465695e11F,
 125386984.9F,
 260548.4103F,
 723.7366299F,
 0.8310060495e-11F,


 257851.5896F,
 -716.2454305F,
 0.1671250568e-11F,


 .6498037322e18F,
 .6007721003e16F,
 .5642567136e14F,
 .5410016092e12F,
 5335250743.F,
 54807486.36F,
 600556.1685F,
 7404.762432F,
 123.3621846F,
 -0.7615545600e-12F,


 .4784685740e16F,
 -.4623570027e14F,
 .4560960423e12F,
 -4627747172.F,
 48911582.71F,
 -551419.9979F,
 6995.235955F,
 -116.5357816F,
 0.3281973185e-12F,


 .3680601218e30F,
 .1517740560e29F,
 .6272811371e27F,
 .2599046273e26F,
 .1079867565e25F,
 .4500590188e23F,
 .1882234790e22F,
 .7902743912e20F,
 .3332855779e19F,
 .1412786684e18F,
 .6024404002e16F,
 .2586890453e15F,
 .1120072441e14F,
 .4898679104e12F,
 .2169236496e11F,
 975830163.5F,
 44812215.95F,
 2116669.418F,
 104145.5552F,
 5465.691821F,
 324.2553229F,
 26.79048089F,
 0.1813014890e-13F,


 -.1557722425e12F,
 7834398184.F,
 -400276998.2F,
 20877114.73F,
 -1119991.502F,
 62588.08192F,
 -3730.604716F,
 251.7146826F,
 -20.72506084F,
 -0.4643326223e-13F,


 .2933993859e12F,
 .4513558336e11F,
 6979115397.F,
 1085573222.F,
 170037345.8F,
 26855525.06F,
 4284377.444F,
 692049.6023F,
 113559.6642F,
 19023.06198F,
 3277.193785F,
 588.8907421F,
 112.2689863F,
 25.83133838F,
 7.781884657F,
 -0.1757985417e-14F,


 3233548126.F,
 -863873252.9F,
 231593359.8F,
 -62330684.83F,
 16850484.27F,
 -4578716.595F,
 1251567.447F,
 -344497.5934F,
 95617.45164F,
 -26804.57842F,
 7610.397987F,
 -2193.112776F,
 646.9059998F,
 -194.7661553F,
 62.62728272F,
 -20.09513491F,
 9.575189476F,
 -1.914350186F,
 -0.6767913369e-15F,


 33630.45706F,
 18347.17070F,
 7776.158102F,
 4487.218453F,
 1811.205419F,
 1130.457858F,
 425.1798458F,
 296.4364134F,
 100.6481234F,
 82.22915683F,
 24.03882306F,
 24.82494211F,
 5.800414568F,
 8.721782585F,
 1.411291143F,
 4.858320951F,
 1.515603447F,
 -0.2636060628e-14F,


 -0.3172182974e-1F,
 0.2197332343F,
 0.8145158497F,
 0.4670475668F,
 };
# 1627 "/arm-libs/library-src/dinkum/source/./xxlgamma.h"
static const float half_ln2pim1 =
 0.41893853320467274178032973640561765F;

static float _FLgamma_big(float x)
 {
 float xinv = (1.0F / (x));
 float y = _FLog(x, 0) - 1.0F;
 float z = x - 0.5F;

 short xexp;

 _FDunscale(&xexp, &z);
 z *= y;
 if (_FDscale(&z, xexp) < 0)
  z += half_ln2pim1 + (c[0] * xinv);
 else
  _Feraise(0x04);
 return (z);
 }

static const float loge2hi = (float)(5814539.0 / 8388608.0);
static const float loge2lo = 1.1730463525082348212145817656807550e-7F;

static const float pi = 3.1415926535897932384626433832795029F;

float (lgammaf)(float x)
 {
 float y = x;

 switch (_FDint(&y, 0))
  {
 case 2:
  return (x);

 case 1:
  return (_FInf._Float);

 case 0:
  if (x <= 0.0F)
   {
   _Feraise(0x02);
   return (_FInf._Float);
   }

 default:
  if (x < -_FGamma_big)
   return (_FLog(
    ((pi) / ((x * _FSinx(pi * (x - y), 0, 0)))), 0)
     - _FLgamma_big(-x));
  else if (_FGamma_big <= x)
   return (_FLgamma_big(x));
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
    float y = x;

    y = ((y - rootapprox[i].zero1) - rootapprox[i].zero2);
    return (_FPoly(y, &coeffs[offset],
     rootapprox[i].size - 1));
    }
   else if (1.2F < x && x < 1.5F)
    {
    float y = x - approx_mid;

    return (((_FPoly(y, approx_num, ((sizeof (approx_num) / sizeof ((approx_num)[0])) - 1))) / (_FPoly(y, approx_den, ((sizeof (approx_den) / sizeof ((approx_den)[0])) - 1)))));

    }
   else
    {
    short xexp;
    float z;

    y = _FTgamma(&x, &xexp);
    ((*_FPmsw(&(x))) &= ~((unsigned short)0x8000));
    x = -_FLog(x, 0);
    z = -xexp;
    x += loge2lo * z;
    x += loge2hi * z;
    x += log1pf(y);
    return (x);
    }
   }
  }
 }
# 4 "/arm-libs/library-src/dinkum/source/./lgammaf.c" 2

