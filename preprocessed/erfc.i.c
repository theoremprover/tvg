# 1 "/arm-libs/library-src/dinkum/source/./erfc.c"
# 1 "<built-in>" 1
# 1 "<built-in>" 3
# 366 "<built-in>" 3
# 1 "<command line>" 1
# 1 "<built-in>" 2
# 1 "/arm-libs/library-src/dinkum/source/./erfc.c" 2

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
# 3 "/arm-libs/library-src/dinkum/source/./erfc.c" 2
# 1 "/arm-libs/library-src/dinkum/source/./xxerfc.h" 1

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
# 3 "/arm-libs/library-src/dinkum/source/./xxerfc.h" 2







static const double mid00 = 24.0 / 32.0;
static const double mid0 = 32.0 / 32.0;
static const double mid01 = 41.0 / 32.0;
# 211 "/arm-libs/library-src/dinkum/source/./xxerfc.h"
static const double s00[] = {
 0.20513042358524565923e-3,
 -0.17302909327917391697e-2,
 0.51668629864273204744e-2,
 -0.61018108811685117109e-2,
 0.17296128942169147008e-1,
 -0.12367859758895683874,
 0.37903680328736621018,
 -0.52712243098316381367,
 0.28518837675284931199,
 };

static const double c00[] = {
 -0.37788983802056158995e-5,
 0.31563412406623646172e-3,
 0.96597074108538761276e-3,
 0.89003991783184363171e-2,
 0.19304411411721719127e-1,
 0.97000814701276839169e-1,
 0.14001436609991328054,
 0.49370385730392383516,
 0.37276429728250795375,
 0.98734270070806922458,
 };

static const double s01[] = {
 0.77247320919989038633e-5,
 0.22525341796674351121e-4,
 -0.19345571017397467338e-3,
 -0.23110094671285694424e-2,
 0.22442956437592785401e-1,
 -0.82214574807852420520e-1,
 0.15845614523514938233,
 -0.16145785744410440848,
 0.69286736206390129211e-1,
 };

static const double c01[] = {
 0.13243369303587245858e-3,
 0.10961148685545737546e-2,
 0.57965688990380852173e-2,
 0.24145811406411854685e-1,
 0.72705248947080019466e-1,
 0.20197452162142900622,
 0.38821508541722939820,
 0.75162372253912655342,
 0.78397684142516694323,
 0.98992077724236469320,
 };

typedef struct Approx_53 {
 double to, mid;
 int nsize, dsize;
 double num[14];
 double den[3];
 } Approx_53;

static const Approx_53 approx0[] =
 {
  {
 2.150, 5051755693729277.0 / 9007199254740992.0,
 14, 2,
   {
 -0.86381945584175991307e-1,
 0.13483053545881869948,
 -0.13245818902108337834,
 0.83163693902502265776e-1,
 0.30111206414610808633e-3,
 -0.93853379462813035334e-1,
 0.16426802871764807892,
 -0.17233569546378735938,
 0.76415772074815067543e-1,
 0.16395297850473244500,
 -0.58471469916193587787,
 1.21448833383366656453,
 1.37173925081302581840,
 0.28071199775962147757,
   },
   {
 3.44420091579558932594,
 0.99999999999999991413,
   },
  },

  {
 3.000, 7191794753785443.0
  / 18014398509481984.0,
 14, 2,
   {
 -0.24436063911004128091,
 0.28546331899100017862,
 -0.19283511026461575004,
 0.57506411710317173766e-1,
 0.55467128136468893146e-1,
 -0.11130874266260128482,
 0.10410616412706168628,
 -0.48496478131438246512e-1,
 -0.30714623155242940949e-1,
 0.10765310029967631882,
 -0.16160396808256355249,
 0.18007923119004882982,
 0.63133425726592950391,
 0.21044604135810536956,
   },
   {
 0.79057133939163814885,
 0.99999999999999999013,
   },
  },

  {
 4.000, 2627099782632789.0 / 9007199254740992.0,
 14, 2,
   {
 6.94027370937458631920,
 -6.08544734624193419284,
 2.31879831734425252144,
 0.78118436567803517512,
 -1.91396435681184626797,
 1.37735777860201142744,
 -0.14960726890626714565,
 -0.82875496207928303445,
 1.00988585435214778575,
 -0.28824122606579654663,
 -1.12163651860242613786,
 2.85753060941119083705,
 1.45583092765501070369,
 0.15830072471294549373,
   },
   {
 6.01180083812201241500,
 0.99999999999999977751,
   },
  },

  {
 5.823, 3798631215838947.0 / 18014398509481984.0,
 13, 3,
   {
 0.19531849802619963394,
 0.61522573748464045868e-1,
 -0.36565091540205739740,
 0.34311325000620700576,
 -0.31194495597758597412e-1,
 -0.28322613880361215939,
 0.33594849905314274848,
 -0.35520022543526476069e-1,
 -0.53726815740653433290,
 1.21153032760480563268,
 1.31765305296850066116,
 0.77025064588641700967,
 0.11620354954329073278,
   },
   {
 3.13343966990119202182,
 2.07176571335611984074,
 0.99760066369871012713,
   },
  },

  {
 10.123, 2436607086707783.0 / 18014398509481984.0,
 14, 2,
   {
 -63.3112523999037671752,
 27.7781800267808681065,
 4.52231228065835338299,
 -10.0635405451701360167,
 3.26569997774942099066,
 2.02324182412782674568,
 -2.33038498026946369464,
 0.22776663101245270494,
 1.24447464496474482245,
 -0.91481036444256320588,
 -0.76409322595944346933,
 2.76838574655873197941,
 0.94493743012910894854,
 0.75631915367277104834e-1,
   },
   {
 5.23012528093218631775,
 0.99999999999999987376,
   },
  },

  {
 27.252, 1069919669648903.0 / 18014398509481984.0,
 13, 3,
   {
 -5.04520892433937595369,
 0.49840891513090937302,
 2.74899592191374698473,
 -0.93943527105192874028,
 -1.23689766628226479820,
 0.89630338637280367301,
 0.61024859928349147653,
 -0.99851426242736709386,
 -0.41379697483197006856,
 2.75303742755447474047,
 0.68214452414590938713,
 0.59154026904535669166,
 0.33307959299669689576e-1
   },
   {
 5.46618561475266897367,
 0.97728961527023062675,
 0.99575887705636488663,
   },
  },
 };
# 1419 "/arm-libs/library-src/dinkum/source/./xxerfc.h"
static const double erfc_half = 0.47693627620446987338141835364313056;

double (erfc)(double x)
 {
 switch (_Dtest(&x))
  {
 case 2:
  return (x);

 case 0:
  return (1.0);

 case 1:
  return (((*_Pmsw(&(x))) & ((unsigned short)0x8000)) ? 2.0 : 0.0);

 default:
  if (x < erfc_half)
   return (1.0 - erf(x));
  else if (x < 1.523)
   if (x < mid0)
    {
    x -= mid00;
    return (((_Poly(x, s00, ((sizeof (s00) / sizeof ((s00)[0])) - 1))) / (_Poly(x, c00, ((sizeof (c00) / sizeof ((c00)[0])) - 1)))));
    }
   else
    {
    x -= mid01;
    return (((_Poly(x, s01, ((sizeof (s01) / sizeof ((s01)[0])) - 1))) / (_Poly(x, c01, ((sizeof (c01) / sizeof ((c01)[0])) - 1)))));
    }
  else
   {
   int i;

   for (i = 0; i < sizeof (approx0) / sizeof (approx0[0]); ++i)
    if (x < approx0[i].to)
     {
     double w, y;

     y = (1.0 / (x)) - approx0[i].mid;
     w = ((_Poly(y, approx0[i].num, approx0[i].nsize - 1)) / (_Poly(y, approx0[i].den, approx0[i].dsize - 1)));




     y = x;
     _Dint(&y, 53 / 3);
     w *= exp((y - x) * (y + x));
     y = - y * y;
     _Exp(&y, w, 0);
     return (y);
     }

   _Feraise(0x08);
   return (0.0);
   }
  }
 }
# 4 "/arm-libs/library-src/dinkum/source/./erfc.c" 2

