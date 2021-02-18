








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





long double _LXp_getw(long double *p, int n)
 {
 if (n == 0)
  return (0.0L);
 else if (n == 1 || p[0] == 0.0L || 1 < n && p[1] == 0.0L)
  return (p[0]);
 else if (n == 2)
  return (p[0] + p[1]);
 else
  return (p[0] + (p[1] + p[2]));
 }

long double *_LXp_setw(long double *p, int n, long double x)
 {
 long double x0 = x;
 short errx, xexp;

 if (n <= 0)
  ;
 else if (n == 1 || (errx = _LDunscale(&xexp, &x0)) == 0)
  p[0] = x0;
 else if (0 < errx)
  {
  p[0] = x0;
  p[1] = 0.0L;
  }
 else
  {
  _LDint(&x0, (53 / 2));
  _LDscale(&x0, xexp);

  p[0] = x0;
  p[1] = x - x0;
  if ((53 & 1) != 0 && 2 < n && p[1] != 0.0L)
   {
   x = p[1];
   _LDunscale(&xexp, &p[1]);
   _LDint(&p[1], (53 / 2));
   _LDscale(&p[1], xexp);
   p[2] = x - p[1];
   if (3 < n && p[2] != 0.0L)
    p[3] = 0.0L;
   }
  else if (2 < n)
   p[2] = 0.0L;
  }
 return (p);
 }

long double *_LXp_addh(long double *p, int n, long double x0)
 {
 long double xscaled = x0;
 short errx, xexp;

 if (n == 0)
  ;
 else if (0 < (errx = _LDunscale(&xexp, &xscaled)))
  if (errx == 2 || (errx = _LDtest(&p[0])) <= 0)
   p[0] = x0;
  else if (errx == 2 || (((_Lval *)(char *)&(x0))->_Sh[3] & ((unsigned short)0x8000)) == (((_Lval *)(char *)&(p[0]))->_Sh[3] & ((unsigned short)0x8000)))
   ;
  else
   {
   _Feraise(0x01);
   p[0] = _LNan._Long_double;
   if (1 < n)
    p[1] = 0.0L;
   }
 else if (errx < 0)
  {
  long prevexp = (2 * 1024);
  int k;

  for (k = 0; k < n; )
   {
   long double yscaled = p[k];
   int mybits = (53 / 2);
   short yexp;
   long diff;

   if (0 < (errx = _LDunscale(&yexp, &yscaled)))
    break;
   else if (errx == 0)
    {
    p[k] = x0;
    if (k + 1 < n)
     p[k + 1] = 0.0L;
    break;
    }
   else if ((diff = (long)yexp - xexp) <= -mybits
    && x0 != 0.0L)
    {
    int j;

    for (j = k; ++j < n && p[j] != 0.0L; )
     ;
    if (j < n - 1)
     ++j;
    else if (j == n)
     --j;
    for (; k < j; --j)
     p[j] = p[j - 1];
    p[k] = x0;
    x0 = 0.0L;
    }
   else if (mybits <= diff && x0 != 0.0L)
    {
    prevexp = yexp;
    ++k;
    }
   else
    {
    if ((p[k] += x0) == 0.0L)
     {
     {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0L; ) ; p[n - 1] = 0.0L;}
     if (p[k] == 0.0L)
      break;
     }
    x0 = p[k];
    _LDunscale(&xexp, &x0);
    if (prevexp - mybits < xexp)
     {
     _LDint(&x0, (short)(xexp - (prevexp - mybits)));
     _LDscale(&x0, xexp);
     if ((p[k] -= x0) == 0.0L)
      {
      {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0L; ) ; p[n - 1] = 0.0L;}
      }
     if (--k == 0)
      prevexp = (2 * 1024);
     else
      {
      xscaled = p[k - 1];
      _LDunscale(&yexp, &xscaled);
      prevexp = yexp;
      }
     }
    else if (k + 1 == n)
     break;
    else
     {
     x0 = p[k];
     _LDunscale(&yexp, &p[k]);
     _LDint(&p[k], (53 / 2));
     _LDscale(&p[k], yexp);
     x0 -= p[k];
     prevexp = yexp;

     xscaled = x0 != 0.0L ? x0 : p[k];
     _LDunscale(&xexp, &xscaled);
     ++k;
     }
    }
   }
  }
 return (p);
 }

long double *_LXp_mulh(long double *p, int n, long double x0)
 {
 short errx;
 int j, k;
 long double buf[4];

 if (0 < n)
  {
  buf[0] = p[0] * x0;
  if (0 <= (errx = _LDtest(&buf[0])))
   {
   if (errx == 2)
    _Feraise(0x01);
   p[0] = buf[0];
   if (0 < errx && 1 < n)
    p[1] = 0.0L;
   return (p);
   }
  p[0] = 0.0L;
  }

 for (j = 1, k = 0; k < n; ++k, --j)
  {
  for (; j < 4; ++j)
   if (k + j < n && p[k + j] != 0.0L)
    {
    buf[j] = p[k + j] * x0;
    p[k + j] = 0.0L;
    }
   else
    {
    buf[j] = 0.0L;
    j = 2 * 4;
    break;
    }

  if (buf[0] == 0.0L)
   break;
  else
   {
   int i = 0;
   long double y0 = buf[0];
   short xexp;

   _LDunscale(&xexp, &y0);
   _LDint(&y0, (53 / 2));
   _LDscale(&y0, xexp);
   _LXp_addh(p, n, y0);
   _LXp_addh(p, n, buf[0] - y0);

   for (; ++i < j; )
    if ((buf[i - 1] = buf[i]) == 0.0L)
     break;
   }
  }
 return (p);
 }

long double *_LXp_movx(long double *p, int n, long double *q)
 {
 memcpy(p, q, n * sizeof (long double));
 return (p);
 }

long double *_LXp_addx(long double *p, int n, long double *q, int m)
 {
 int k;

 for (k = 0; k < m && q[k] != 0.0L; ++k)
  _LXp_addh(p, n, q[k]);
 return (p);
 }

long double *_LXp_subx(long double *p, int n, long double *q, int m)
 {
 int k;

 for (k = 0; k < m && q[k] != 0.0L; ++k)
  _LXp_addh(p, n, -q[k]);
 return (p);
 }

long double *_LXp_ldexpx(long double *p, int n, int m)
 {
 int k;

 for (k = 0; k < n; ++k)
  {
  p[k] = ldexpl(p[k], m);
  if (p[k] == 0.0L)
   break;
  }
 return (p);
 }

long double *_LXp_mulx(long double *p, int n, long double *q, int m, long double *ptemp2)
 {
 if (n == 0 || m == 0)
  ;
 else if (q[0] == 0.0L || q[1] == 0.0L)
  _LXp_mulh(p, n, q[0]);
 else
  {
  long double *px = ptemp2;
  long double *pac = ptemp2 + n;
  int j;

  _LXp_movx(px, n, p);
  _LXp_mulh(p, n, q[0]);
  for (j = 1; j < m && q[j] != 0.0L; ++j)
   {
   _LXp_movx(pac, n, px);
   _LXp_mulh(pac, n, q[j]);
   _LXp_addx(p, n, pac, n);
   }
  }
 return (p);
 }

long double *_LXp_invx(long double *p, int n, long double *ptemp4)
 {
 short errx;

 if (n == 0)
  ;
 else if (0 <= (errx = _LDtest(&p[0])))
  {
  if (errx == 1)
   p[0] = 0.0L;
  else if (errx == 0)
   p[0] = _LInf._Long_double;

  }
 else
  {
  long double *pac = ptemp4;
  long double *py = ptemp4 + n;
  long double *ptemp2 = py + n;
  long double x0 = p[0];
  int k;

  _LXp_movx(py, n, p);
  _LXp_mulh(py, n, -1.0L);

  if (1 < n)
   x0 += p[1];
  _LXp_setw(p, n, 1.0L / x0);

  for (k = 1; k < n; k <<= 1)
   {
   _LXp_movx(pac, n, p);
   _LXp_mulx(pac, n, py, n, ptemp2);
   _LXp_addh(pac, n, 1.0L);
   _LXp_mulx(pac, n, p, n, ptemp2);
   _LXp_addx(p, n, pac, n);
   }
  }
 return (p);
 }

long double *_LXp_sqrtx(long double *p, int n, long double *ptemp4)
 {
 if (n == 0)
  ;
 else if (0 <= _LDtest(&p[0]) || p[0] < 0.0L)
  {
  if (p[0] < 0.0L)
   {
   _Feraise(0x01);
   p[0] = _LNan._Long_double;
   }
  }
 else
  {
  long double *pac = ptemp4;
  long double *py = ptemp4 + n;
  long double *ptemp2 = py + n;
  long double x0 = p[0];
  int k;

  if (1 < n)
   x0 += p[1];
  _LXp_setw(py, n, 1.0L / sqrtl(x0));

  for (k = 2; k < n; k <<= 1)
   {
   _LXp_movx(pac, n, py);
   _LXp_mulh(pac, n, -0.5L);
   _LXp_mulx(pac, n, p, n, ptemp2);
   _LXp_mulx(pac, n, py, n, ptemp2);
   _LXp_addh(pac, n, 1.5L);
   _LXp_mulx(py, n, pac, n, ptemp2);
   }
  _LXp_mulx(p, n, py, n, ptemp2);
  }
 return (p);
 }

