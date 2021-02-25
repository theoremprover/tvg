



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






long double erfl(long double x);






struct Approx {
 long double to, mid;
 const long double *num;
 int nsize;
 const long double *den;
 int dsize;
 };

static const long double mid00 = 24.0L / 32.0L;
static const long double mid0 = 32.0L / 32.0L;
static const long double mid01 = 41.0L / 32.0L;
static const long double s00[] = {
 0.20513042358524565923e-3L,
 -0.17302909327917391697e-2L,
 0.51668629864273204744e-2L,
 -0.61018108811685117109e-2L,
 0.17296128942169147008e-1L,
 -0.12367859758895683874L,
 0.37903680328736621018L,
 -0.52712243098316381367L,
 0.28518837675284931199L,
 };

static const long double c00[] = {
 -0.37788983802056158995e-5L,
 0.31563412406623646172e-3L,
 0.96597074108538761276e-3L,
 0.89003991783184363171e-2L,
 0.19304411411721719127e-1L,
 0.97000814701276839169e-1L,
 0.14001436609991328054L,
 0.49370385730392383516L,
 0.37276429728250795375L,
 0.98734270070806922458L,
 };

static const long double s01[] = {
 0.77247320919989038633e-5L,
 0.22525341796674351121e-4L,
 -0.19345571017397467338e-3L,
 -0.23110094671285694424e-2L,
 0.22442956437592785401e-1L,
 -0.82214574807852420520e-1L,
 0.15845614523514938233L,
 -0.16145785744410440848L,
 0.69286736206390129211e-1L,
 };

static const long double c01[] = {
 0.13243369303587245858e-3L,
 0.10961148685545737546e-2L,
 0.57965688990380852173e-2L,
 0.24145811406411854685e-1L,
 0.72705248947080019466e-1L,
 0.20197452162142900622L,
 0.38821508541722939820L,
 0.75162372253912655342L,
 0.78397684142516694323L,
 0.98992077724236469320L,
 };

static const long double s10[] = {
 -0.86381945584175991307e-1L,
 0.13483053545881869948L,
 -0.13245818902108337834L,
 0.83163693902502265776e-1L,
 0.30111206414610808633e-3L,
 -0.93853379462813035334e-1L,
 0.16426802871764807892L,
 -0.17233569546378735938L,
 0.76415772074815067543e-1L,
 0.16395297850473244500L,
 -0.58471469916193587787L,
 1.21448833383366656453L,
 1.37173925081302581840L,
 0.28071199775962147757L,
 };

static const long double c10[] = {
 3.44420091579558932594L,
 0.99999999999999991413L,
 };

static const long double s11[] = {
 -0.24436063911004128091L,
 0.28546331899100017862L,
 -0.19283511026461575004L,
 0.57506411710317173766e-1L,
 0.55467128136468893146e-1L,
 -0.11130874266260128482L,
 0.10410616412706168628L,
 -0.48496478131438246512e-1L,
 -0.30714623155242940949e-1L,
 0.10765310029967631882L,
 -0.16160396808256355249L,
 0.18007923119004882982L,
 0.63133425726592950391L,
 0.21044604135810536956L,
 };

static const long double c11[] = {
 0.79057133939163814885L,
 0.99999999999999999013L,
 };

static const long double s12[] = {
 6.94027370937458631920L,
 -6.08544734624193419284L,
 2.31879831734425252144L,
 0.78118436567803517512L,
 -1.91396435681184626797L,
 1.37735777860201142744L,
 -0.14960726890626714565L,
 -0.82875496207928303445L,
 1.00988585435214778575L,
 -0.28824122606579654663L,
 -1.12163651860242613786L,
 2.85753060941119083705L,
 1.45583092765501070369L,
 0.15830072471294549373L,
 };

static const long double c12[] = {
 6.01180083812201241500L,
 0.99999999999999977751L,
 };

static const long double s13[] = {
 0.19531849802619963394L,
 0.61522573748464045868e-1L,
 -0.36565091540205739740L,
 0.34311325000620700576L,
 -0.31194495597758597412e-1L,
 -0.28322613880361215939L,
 0.33594849905314274848L,
 -0.35520022543526476069e-1L,
 -0.53726815740653433290L,
 1.21153032760480563268L,
 1.31765305296850066116L,
 0.77025064588641700967L,
 0.11620354954329073278L,
 };

static const long double c13[] = {
 3.13343966990119202182L,
 2.07176571335611984074L,
 0.99760066369871012713L,
 };

static const long double s14[] = {
 -63.3112523999037671752L,
 27.7781800267808681065L,
 4.52231228065835338299L,
 -10.0635405451701360167L,
 3.26569997774942099066L,
 2.02324182412782674568L,
 -2.33038498026946369464L,
 0.22776663101245270494L,
 1.24447464496474482245L,
 -0.91481036444256320588L,
 -0.76409322595944346933L,
 2.76838574655873197941L,
 0.94493743012910894854L,
 0.75631915367277104834e-1L,
 };

static const long double c14[] = {
 5.23012528093218631775L,
 0.99999999999999987376L,
 };

static const long double s15[] = {
  -5.04520892433937595369L,
  0.49840891513090937302L,
  2.74899592191374698473L,
  -0.93943527105192874028L,
  -1.23689766628226479820L,
  0.89630338637280367301L,
  0.61024859928349147653L,
  -0.99851426242736709386L,
  -0.41379697483197006856L,
  2.75303742755447474047L,
  0.68214452414590938713L,
  0.59154026904535669166L,
  0.33307959299669689576e-1L
 };

static const long double c15[] = {
  5.46618561475266897367L,
  0.97728961527023062675L,
  0.99575887705636488663L
 };

static const struct Approx approx[] = {
 {2.150L, 5051755693729277.0L
  / 9007199254740992.0L, s10, ((sizeof (s10) / sizeof ((s10)[0])) - 1), c10, ((sizeof (c10) / sizeof ((c10)[0])) - 1)},
 {3.000L, 7191794753785443.0L
  / 18014398509481984.0L, s11, ((sizeof (s11) / sizeof ((s11)[0])) - 1), c11, ((sizeof (c11) / sizeof ((c11)[0])) - 1)},
 {4.000L, 2627099782632789.0L
  / 9007199254740992.0L, s12, ((sizeof (s12) / sizeof ((s12)[0])) - 1), c12, ((sizeof (c12) / sizeof ((c12)[0])) - 1)},
 {5.823L, 3798631215838947.0L
  / 18014398509481984.0L, s13, ((sizeof (s13) / sizeof ((s13)[0])) - 1), c13, ((sizeof (c13) / sizeof ((c13)[0])) - 1)},
 {10.123L, 2436607086707783.0L
  / 18014398509481984.0L, s14, ((sizeof (s14) / sizeof ((s14)[0])) - 1), c14, ((sizeof (c14) / sizeof ((c14)[0])) - 1)},
 {27.252L, 1069919669648903.0L
  / 18014398509481984.0L, s15, ((sizeof (s15) / sizeof ((s15)[0])) - 1), c15, ((sizeof (c15) / sizeof ((c15)[0])) - 1)},
 };
static const long double erfc_half = 0.47693627620446987338141835364313056L;

long double (erfcl)(long double x)
 {
 switch (_LDtest(&x))
  {
 case 2:
  return (x);

 case 0:
  return (1.0L);

 case 1:
  return ((((_Lval *)(char *)&(x))->_Sh[3] & ((unsigned short)0x8000)) ? 2.0L : 0.0L);

 default:
  if (x < erfc_half)
   return (1.0L - erfl(x));
  else if (x < 1.523L)
   if (x < mid0)
    {
    x -= mid00;
    return (_LPoly(x, s00, ((sizeof (s00) / sizeof ((s00)[0])) - 1)) / _LPoly(x, c00, ((sizeof (c00) / sizeof ((c00)[0])) - 1)));
    }
   else
    {
    x -= mid01;
    return (_LPoly(x, s01, ((sizeof (s01) / sizeof ((s01)[0])) - 1)) / _LPoly(x, c01, ((sizeof (c01) / sizeof ((c01)[0])) - 1)));
    }
  else
   {
   int i;

   for (i = 0; i < sizeof (approx) / sizeof (approx[0]); ++i)
    if (x < approx[i].to)
     {
     long double w, y;

     y = 1.0L / x - approx[i].mid;
     w = _LPoly(y, approx[i].num, approx[i].nsize)
      / _LPoly(y, approx[i].den, approx[i].dsize);

     y = x;
     _LDint(&y, 53 / 3);
     w *= expl((y - x) * (y + x));
     y = - y * y;
     _LExp(&y, w, 0);
     return (y);
     }

   _Feraise(0x10);
   return (0.0L);
   }
  }
 }
