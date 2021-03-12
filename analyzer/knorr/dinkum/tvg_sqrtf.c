

/*

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
*/

typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } __attribute__ ((__may_alias__)) _Dconst;

typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


void (_Feraise)(int except)
 {

int __attribute__((fardata)) _Errno;

 int errh = (1 | 2);

 if ((errh & 2) != 0)
  {
  if ((except & (0x08 | 0x10)) != 0)
   except |= 0x20;
  feraiseexcept(except);
  }

 if ((errh & 1) == 0)
  ;
 else if ((except & 0x01) != 0)
  ( _Errno) = 0x0021;
 else if ((except & (0x04 | 0x10 | 0x08)) != 0)
  ( _Errno) = 0x0022;
}



short _FDnorm(_Fval *ps)
 {
 short xchar;
 unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

 xchar = 1;
 if ((ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0])
  {
  if (ps->_Sh[1] == 0)
   ps->_Sh[1] = ps->_Sh[0], ps->_Sh[0] = 0, xchar -= 16;
  for (; ps->_Sh[1] < 1 << 7; --xchar)
   {
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1
    | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
  for (; 1 << (7 + 1) <= ps->_Sh[1]; ++xchar)
   {
   ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> 1
    | ps->_Sh[1] << 15);
   ps->_Sh[1] >>= 1;
   }
  ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1));
  }
 ps->_Sh[1] |= sign;
 return (xchar);
 }


short _FDunscale(short *pex, float *px)
 {
 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7;

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  {
  *pex = 0;
  return ((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1);
  }
 else if (0 < xchar || (xchar = _FDnorm(ps)) <= 0)
  {
  ps->_Sh[1] = ps->_Sh[1] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1)))) | 0x7e << 7;
  *pex = xchar - 0x7e;
  return ((-1));
  }
 else
  {
  *pex = 0;
  return (0);
  }
 }



short _FDscale(float *px, long lexp)
 {
                   _Dconst _FInf = {{0, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};

 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (short)((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7);

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 else if (xchar == 0 && 0 < (xchar = _FDnorm(ps)))
  return (0);

 if (0 < lexp && ((unsigned short)((1 << (15 - 7)) - 1)) - xchar <= lexp)
  {
  *px = ps->_Sh[1] & ((unsigned short)0x8000) ? -_FInf._Float : _FInf._Float;
  return (1);
  }
 else if (-xchar < lexp)
  {
  ps->_Sh[1] = (unsigned short)(ps->_Sh[1] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))
   | (lexp + xchar) << 7);
  return ((-1));
  }
 else
  {
  unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

  ps->_Sh[1] = (unsigned short)(1 << 7 | ps->_Sh[1] & ((unsigned short)((1 << 7) - 1)));
  lexp += xchar - 1;
  if (lexp < -(16 + 1 + 7) || 0 <= lexp)
   {
   ps->_Sh[1] = sign;
   ps->_Sh[0] = 0;
   return (0);
   }
  else
   {
   short xexp = (short)lexp;
   unsigned short psx = 0;

   if (xexp <= -16)
    {
    psx = ps->_Sh[0] | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = ps->_Sh[1];
    ps->_Sh[1] = 0;
    xexp += 16;
    }
   if ((xexp = (short)-xexp) != 0)
    {
    psx = (ps->_Sh[0] << (16 - xexp)) | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> xexp
     | ps->_Sh[1] << (16 - xexp));
    ps->_Sh[1] >>= xexp;
    }

   ps->_Sh[1] |= sign;
   if ((0x8000 < psx
    || 0x8000 == psx && (ps->_Sh[0] & 0x0001) != 0)
    && (++ps->_Sh[0] & 0xffff) == 0)
    ++ps->_Sh[1];
   else if (ps->_Sh[1] == sign && ps->_Sh[0] == 0)
    return (0);
   return ((-1));
   }
  }
 }



float (sqrtf)(float x)
 {
   _Dconst _FNan = {{0, (((unsigned short)((1 << (15 - 7)) - 1)) << 7) | (1 << (7 - 1))}
      };

 short xexp;
 float y;

 switch (_FDunscale(&xexp, &x))
  {
 case 2:
 case 0:
  return (x);
 case 1:
  if (!(((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
   return (x);
 default:
  if ((((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
   {
   _Feraise(0x01);
   return (_FNan._Float);
   }
  if ((unsigned int)xexp & 1)
   x *= 2.0F, --xexp;
  y = (-0.09977F * x + 0.71035F) * x
   + 0.38660F;
  y += x / y;
  y = 0.25F * y + x / y;
  _FDscale(&y, xexp / 2);
  return (y);
  }
 }

