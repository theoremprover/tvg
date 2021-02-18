



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






float log1pf(float x);

float _FTgamma(float *px, short *pex);
extern float _FGamma_big;

struct Approx {
 float to, mid;
 const float *num;
 int nsize;
 const float *den;
 int dsize;
 };

struct RootApprox {
 const float from, to;
 const float zero1, zero2;
 const float *sr;
 int srsize;
 };
static const float c[] = {
 0.0833333333F,
 };



static const float s10[] = {
 0.5360760204e-3F,
 -0.2213148498e-2F,
 0.7719251053e-2F,
 -0.2947412669e-1F,
 0.1649097161F,
 0.4654077568F,
 -0.1879605312F,
 -0.1152409017F,
 };

static const float c10[] = {
 0.6424155168F,
 1.0000000153F,
 };

static const struct Approx approx[] = {
 {1.5F, 11324621.0F / 8388608.0F, s10, ((sizeof (s10) / sizeof ((s10)[0])) - 1), c10, ((sizeof (c10) / sizeof ((c10)[0])) - 1)},
 };



static const float sr00[] = {
 .1615684343e15F,
 .4272589080e11F,
 12710958.83F,
 5044.029941F,
 -0.5743202284e-11F,
 };

static const float sr01[] = {
 -.4262348976e11F,
 12690641.17F,
 -5035.967374F,
 0.5313011066e-13F,
 };

static const float sr02[] = {
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
 };

static const float sr03[] = {
 257851.5896F,
 -716.2454305F,
 0.1671250568e-11F,
 };

static const float sr04[] = {
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
 };

static const float sr05[] = {
 .4784685740e16F,
 -.4623570027e14F,
 .4560960423e12F,
 -4627747172.F,
 48911582.71F,
 -551419.9979F,
 6995.235955F,
 -116.5357816F,
 0.3281973185e-12F,
 };

static const float sr06[] = {
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
 };

static const float sr07[] = {
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
 };

static const float sr08[] = {
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
 };

static const float sr09[] = {
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
 };

static const float sr10[] = {
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
 };

static const float sr11[] = {
 -0.3172182974e-1F,
 0.2197332343F,
 0.8145158497F,
 0.4670475668F,
 };

static const struct RootApprox rootapprox[] = {
 {-7.00023F, -7.00017F,
 -14680480.0F / 16777216.0F * 8.0F,
 1085455.0F / 16777216.0F / 2097152.0F,
 sr00, ((sizeof (sr00) / sizeof ((sr00)[0])) - 1)},

 {-6.99988F, -6.99976F,
 -14679648.0F / 16777216.0F * 8.0F,
 8996753.0F / 16777216.0F / 4194304.0F,
 sr01, ((sizeof (sr01) / sizeof ((sr01)[0])) - 1)},

 {-6.00176F, -6.00137F,
 -12585818.0F / 16777216.0F * 8.0F,
 3468551.0F / 16777216.0F / 524288.0F,
 sr02, ((sizeof (sr02) / sizeof ((sr02)[0])) - 1)},

 {-5.9987F, -5.9985F,
 -12579992.0F / 16777216.0F * 8.0F,
 10936511.0F / 16777216.0F / 4194304.0F,
 sr03, ((sizeof (sr03) / sizeof ((sr03)[0])) - 1)},

 {-5.01F, -5.0064F,
 -10502995.0F / 16777216.0F * 8.0F,
 4225609.0F / 16777216.0F / 2097152.0F,
 sr04, ((sizeof (sr04) / sizeof ((sr04)[0])) - 1)},

 {-4.99266F, -4.991F,
 -10468028.0F / 16777216.0F * 8.0F,
 364821.0F / 16777216.0F / 262144.0F,
 sr05, ((sizeof (sr05) / sizeof ((sr05)[0])) - 1)},

 {-4.062F, -4.0255F,
 -8471156.0F / 16777216.0F * 8.0F,
 1002709.0F / 16777216.0F / 524288.0F,
 sr06, ((sizeof (sr06) / sizeof ((sr06)[0])) - 1)},

 {-3.965F, -3.953F,
 -16589707.0F / 16777216.0F * 4.0F,
 6037121.0F / 16777216.0F / 4194304.0F,
 sr07, ((sizeof (sr07) / sizeof ((sr07)[0])) - 1)},

 {-3.1962F, -3.121F,
 -13185134.0F / 16777216.0F * 4.0F,
 3545701.0F / 16777216.0F / 8388608.0F,
 sr08, ((sizeof (sr08) / sizeof ((sr08)[0])) - 1)},

 {-2.864F, -2.65F,
 -11524617.0F / 16777216.0F * 4.0F,
 1434663.0F / 16777216.0F / 524288.0F,
 sr09, ((sizeof (sr09) / sizeof ((sr09)[0])) - 1)},

 {-2.65F, -2.265F,
 -10305509.0F / 16777216.0F * 4.0F,
 10481785.0F / 16777216.0F / 8388608.0F,
 sr10, ((sizeof (sr10) / sizeof ((sr10)[0])) - 1)},

 {2.72F, 2.77F,
 11492392.0F / 16777216.0F * 4.0F,
 16106127.0F / 16777216.0F / 4194304.0F,
 sr11, ((sizeof (sr11) / sizeof ((sr11)[0])) - 1)},
 };
static const float half_ln2pim1 = 0.41893853320467274178032973640561765F;

static float lgamma_big(float x)
 {
 float xinv = 1.0F / x;
 float y = _FLog(x, 0) - 1.0F;
 float z = x - 0.5F;

 short xexp;

 _FDunscale(&xexp, &z);
 z *= y;
 if (_FDscale(&z, xexp) < 0)
  z += half_ln2pim1 + (c[0] * xinv);
 else
  _Feraise(0x08);
 return (z);
 }

static const float loge2hi = (float)(5814539.0 / 8388608.0);
static const float loge2lo = 1.1730463525082348212145817656807550e-7F;

static const float pi = 3.14159265358979323846264338327950288F;

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
   _Feraise(0x04);
   return (_FInf._Float);
   }

 default:
  if (x < -_FGamma_big)
   return (_FLog(pi / (x * _FSin(pi * (x - y), 0)), 0)
    - lgamma_big(-x));
  else if (_FGamma_big <= x)
   return (lgamma_big(x));
  else
   {
   int i;

   for (i = 0; i < (sizeof (rootapprox) / sizeof (rootapprox[0])); ++i)
    {
    if (rootapprox[i].from < x && x < rootapprox[i].to)
     break;
    }

   if (i < (sizeof (rootapprox) / sizeof (rootapprox[0])))
    {
    float y = x;

    y = ((y - rootapprox[i].zero1) - rootapprox[i].zero2);
    return (_FPoly(y, rootapprox[i].sr,
     rootapprox[i].srsize));
    }
   else if (1.2F < x && x < 1.5F)
    {
    float y = x;
    float ans;
    int i;

    for (i = 0; i < sizeof (approx) / sizeof (approx[0]); ++i)
     if (y < approx[i].to)
      break;

    y -= approx[i].mid;
    ans = _FPoly(y, approx[i].num, approx[i].nsize)
     / _FPoly(y, approx[i].den, approx[i].dsize);

    return (ans);
    }
   else
    {
    short xexp;
    float z;

    y = _FTgamma(&x, &xexp);
    x = -_FLog(x < 0.0F ? -x : x, 0);
    z = -xexp;
    x += loge2lo * z;
    x += loge2hi * z;
    x += log1pf(y);
    return (x);
    }
   }
  }
 }

