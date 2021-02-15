






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






extern inline int fesetround (int) __attribute__ ((always_inline,gnu_inline));
extern inline int fegetround (void) __attribute__ ((always_inline,gnu_inline));
extern inline __attribute__ ((always_inline,gnu_inline))
int fegetround (void)
{

  int res;
  __asm__ volatile ("mfcr %0, $psw " : "=d" (res) : : "memory");
  return (res & 0x03000000UL) >> 24uL;
}



extern inline __attribute__ ((always_inline,gnu_inline))
int fesetround (int round)
{





  __asm__ volatile ("updfl %0" :: "d" (0x300 | (round & 3)) : "memory");


  return 0;
}
typedef unsigned long fexcept_t;
typedef unsigned long fenv_t;
int feclearexcept(int);
int fegetexceptflag(fexcept_t *, int);
int feraiseexcept(int);
int fesetexceptflag(const fexcept_t *, int);
int fetestexcept(int);
int fegetround(void);
int fesetround(int);
int fegetenv(fenv_t *);
int feholdexcept(fenv_t *);
int fesetenv(const fenv_t *);
int feupdateenv(const fenv_t *);


fexcept_t fegettrapenable(void);
int fesettrapenable(fexcept_t);


double _Force_raise(int except);


extern fenv_t _Fenv0;



int (_Fltrounds)(void)
 {
 switch (fegetround())
  {
 case 0x3UL:
  return (0);

 case 0x0UL:
  return (1);

 case 0x1UL:
  return (2);

 case 0x2UL:
  return (3);

 default:
  return (-1);
  }
 }

