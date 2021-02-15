



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






float erff(float x);






struct Approx {
 float to, mid;
 const float *num;
 int nsize;
 const float *den;
 int dsize;
 };

static const float mid00 = 24.0F / 32.0F;
static const float mid0 = 32.0F / 32.0F;
static const float mid01 = 41.0F / 32.0F;
static const float s00[] = {
 0.2065228066e-1F,
 -0.1509142800F,
 0.4262061540F,
 -0.5573867797F,
 0.2855036029F,
 };

static const float c00[] = {
 0.52445813480e-1F,
 0.69036908669e-1F,
 0.42737154752F,
 0.27041611799F,
 0.98843403647F,
 };

static const float s01[] = {
 0.1710573619e-1F,
 -0.8396320332e-1F,
 0.1722734702F,
 -0.1720571890F,
 0.6967651920e-1F,
 };

static const float c01[] = {
 0.4467894790e-1F,
 0.1527638176F,
 0.5082267347F,
 0.6499285695F,
 0.9954897260F,
 };

static const float s10[] = {
 -0.2239868484e-1F,
 0.4179726125e-1F,
 -0.4815653097e-1F,
 0.2894837499e-1F,
 0.3064605465e-1F,
 -0.1470762643F,
 0.4069878610F,
 0.3017322315F,
 };

static const float c10[] = {
 0.6926808746e-1F,
 0.9999999985F,
 };

static const float s11[] = {
 -0.8446285903e-2F,
 0.3128812247e-1F,
 -0.7400038472e-1F,
 0.1409578962F,
 0.7004226993F,
 0.8193149023F,
 0.2627918549F,
 };

static const float c11[] = {
 0.9353449747F,
 1.5179787444F,
 0.9986785175F,
 };

static const float s12[] = {
 0.1998715188F,
 -0.2797274548F,
 0.1873345074F,
 0.8898860043e-1F,
 -0.5291699014F,
 1.0896799623F,
 1.0596253814F,
 0.2159547006F,
 };

static const float c12[] = {
 2.7743459979F,
 0.9999999888F,
 };

static const float s13[] = {
 0.1782164941F,
 -0.2101569589F,
 0.1055503047F,
 0.8748042990e-1F,
 -0.3116615231F,
 0.5180952345F,
 0.7383285367F,
 0.1731499256F,
 };

static const float c13[] = {
 1.4131173800F,
 0.9999999940F,
 };

static const float s14[] = {
 0.2517430525F,
 -0.2327740672F,
 0.6071824727e-1F,
 0.1254376919F,
 -0.2350343518F,
 0.2390976095F,
 0.6255417894F,
 0.1403571247F,
 };

static const float c14[] = {
 0.7802650683F,
 0.9999999786F,
 };

static const float s15[] = {
 0.4346081531F,
 -0.3009199102F,
 -0.2748935678e-1F,
 0.2224760355F,
 -0.1563740339F,
 -0.1265105205F,
 0.5353063469F,
 0.1140211403F,
 };

static const float c15[] = {
 0.3300570663e-1F,
 0.9999999994F,
 };

static const float s16[] = {
 -1.9548407228F,
 5.8308227961F,
 7.1402809645F,
 7.4736829048F,
 1.5645370870F,
 0.8596663043e-1F,
 };

static const float c16[] = {
 17.441400957F,
 13.239545392F,
 11.826775256F,
 0.9940050704F,
 };

static const float s17[] = {
 0.2185443099F,
 -0.2082407401F,
 -0.2869901063F,
 1.1248645477F,
 0.6549960255F,
 0.6194361398F,
 0.6272644470e-1F,
 };

static const float c17[] = {
 2.6221534281F,
 1.0463543863F,
 0.9997747361F,
 };

static const struct Approx approx[] = {
 {1.75F, 2515.0F
  / 4096.0F, s10, ((sizeof (s10) / sizeof ((s10)[0])) - 1), c10, ((sizeof (c10) / sizeof ((c10)[0])) - 1)},
 {2.15F, 67931.0F
  / 131072.0F, s11, ((sizeof (s11) / sizeof ((s11)[0])) - 1), c11, ((sizeof (c11) / sizeof ((c11)[0])) - 1)},
 {2.8F, 13795219.0F
  / 33554432.0F, s12, ((sizeof (s12) / sizeof ((s12)[0])) - 1), c12, ((sizeof (c12) / sizeof ((c12)[0])) - 1)},
 {3.5F, 10785353.0F
  / 33554432.0F, s13, ((sizeof (s13) / sizeof ((s13)[0])) - 1), c13, ((sizeof (c13) / sizeof ((c13)[0])) - 1)},
 {4.4F, 8606493.0F
  / 33554432.0F, s14, ((sizeof (s14) / sizeof ((s14)[0])) - 1), c14, ((sizeof (c14) / sizeof ((c14)[0])) - 1)},
 {5.4F, 6919895.0F
  / 33554432.0F, s15, ((sizeof (s15) / sizeof ((s15)[0])) - 1), c15, ((sizeof (c15) / sizeof ((c15)[0])) - 1)},
 {8.0F, 5204043.0F
  / 33554432.0F, s16, ((sizeof (s16) / sizeof ((s16)[0])) - 1), c16, ((sizeof (c16) / sizeof ((c16)[0])) - 1)},
 {10.123F, 469311.0F
  / 4194304.0F, s17, ((sizeof (s17) / sizeof ((s17)[0])) - 1), c17, ((sizeof (c17) / sizeof ((c17)[0])) - 1)},
 };
static const float erfc_half = 0.47693627620446987338141835364313056F;

float (erfcf)(float x)
 {
 switch (_FDtest(&x))
  {
 case 2:
  return (x);

 case 0:
  return (1.0F);

 case 1:
  return ((((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)) ? 2.0F : 0.0F);

 default:
  if (x < erfc_half)
   return (1.0F - erff(x));
  else if (x < 1.523F)
   if (x < mid0)
    {
    x -= mid00;
    return (_FPoly(x, s00, ((sizeof (s00) / sizeof ((s00)[0])) - 1)) / _FPoly(x, c00, ((sizeof (c00) / sizeof ((c00)[0])) - 1)));
    }
   else
    {
    x -= mid01;
    return (_FPoly(x, s01, ((sizeof (s01) / sizeof ((s01)[0])) - 1)) / _FPoly(x, c01, ((sizeof (c01) / sizeof ((c01)[0])) - 1)));
    }
  else
   {
   int i;

   for (i = 0; i < sizeof (approx) / sizeof (approx[0]); ++i)
    if (x < approx[i].to)
     {
     float w, y;

     y = 1.0F / x - approx[i].mid;
     w = _FPoly(y, approx[i].num, approx[i].nsize)
      / _FPoly(y, approx[i].den, approx[i].dsize);

     y = x;
     _FDint(&y, 24 / 3);
     w *= expf((y - x) * (y + x));
     y = - y * y;
     _FExp(&y, w, 0);
     return (y);
     }

   _Feraise(0x10);
   return (0.0F);
   }
  }
 }

