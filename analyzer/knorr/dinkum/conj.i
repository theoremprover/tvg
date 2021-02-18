



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













typedef double _Complex _Dcomplex;
typedef float _Complex _Fcomplex;
typedef long double _Complex _Lcomplex;

double cimag(_Dcomplex);
double creal(_Dcomplex);
float cimagf(_Fcomplex);
float crealf(_Fcomplex);
long double cimagl(_Lcomplex);
long double creall(_Lcomplex);


double cabs(_Dcomplex);
_Dcomplex cacos(_Dcomplex);
_Dcomplex cacosh(_Dcomplex);
double carg(_Dcomplex);
_Dcomplex casin(_Dcomplex);
_Dcomplex casinh(_Dcomplex);
_Dcomplex catan(_Dcomplex);
_Dcomplex catanh(_Dcomplex);
_Dcomplex ccos(_Dcomplex);
_Dcomplex ccosh(_Dcomplex);
_Dcomplex cexp(_Dcomplex);


_Dcomplex clog10(_Dcomplex);
_Dcomplex conj(_Dcomplex);
_Dcomplex cpow(_Dcomplex, _Dcomplex);
_Dcomplex cproj(_Dcomplex);

_Dcomplex csin(_Dcomplex);
_Dcomplex csinh(_Dcomplex);
_Dcomplex csqrt(_Dcomplex);
_Dcomplex ctan(_Dcomplex);
_Dcomplex ctanh(_Dcomplex);
double norm(_Dcomplex);

float cabsf(_Fcomplex);
_Fcomplex cacosf(_Fcomplex);
_Fcomplex cacoshf(_Fcomplex);
float cargf(_Fcomplex);
_Fcomplex casinf(_Fcomplex);
_Fcomplex casinhf(_Fcomplex);
_Fcomplex catanf(_Fcomplex);
_Fcomplex catanhf(_Fcomplex);
_Fcomplex ccosf(_Fcomplex);
_Fcomplex ccoshf(_Fcomplex);
_Fcomplex cexpf(_Fcomplex);

_Fcomplex clogf(_Fcomplex);
_Fcomplex clog10f(_Fcomplex);
_Fcomplex conjf(_Fcomplex);
_Fcomplex cpowf(_Fcomplex, _Fcomplex);
_Fcomplex cprojf(_Fcomplex);

_Fcomplex csinf(_Fcomplex);
_Fcomplex csinhf(_Fcomplex);
_Fcomplex csqrtf(_Fcomplex);
_Fcomplex ctanf(_Fcomplex);
_Fcomplex ctanhf(_Fcomplex);
float normf(_Fcomplex);

long double cabsl(_Lcomplex);
_Lcomplex cacosl(_Lcomplex);
_Lcomplex cacoshl(_Lcomplex);
long double cargl(_Lcomplex);
_Lcomplex casinl(_Lcomplex);
_Lcomplex casinhl(_Lcomplex);
_Lcomplex catanl(_Lcomplex);
_Lcomplex catanhl(_Lcomplex);
_Lcomplex ccosl(_Lcomplex);
_Lcomplex ccoshl(_Lcomplex);
_Lcomplex cexpl(_Lcomplex);

_Lcomplex clogl(_Lcomplex);
_Lcomplex clog10l(_Lcomplex);
_Lcomplex conjl(_Lcomplex);
_Lcomplex cpowl(_Lcomplex, _Lcomplex);
_Lcomplex cprojl(_Lcomplex);

_Lcomplex csinl(_Lcomplex);
_Lcomplex csinhl(_Lcomplex);
_Lcomplex csqrtl(_Lcomplex);
_Lcomplex ctanl(_Lcomplex);
_Lcomplex ctanhl(_Lcomplex);
long double norml(_Lcomplex);

_Dcomplex (_Cbuild)(double, double);
_Dcomplex (_Cmulcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Cmulcr)(_Dcomplex, double);
_Dcomplex (_Cdivcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Cdivcr)(_Dcomplex, double);
_Dcomplex (_Caddcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Caddcr)(_Dcomplex, double);
_Dcomplex (_Csubcc)(_Dcomplex, _Dcomplex);
_Dcomplex (_Csubcr)(_Dcomplex, double);

_Fcomplex (_FCbuild)(float, float);
_Fcomplex (_FCmulcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCmulcr)(_Fcomplex, float);
_Fcomplex (_FCdivcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCdivcr)(_Fcomplex, float);
_Fcomplex (_FCaddcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCaddcr)(_Fcomplex, float);
_Fcomplex (_FCsubcc)(_Fcomplex, _Fcomplex);
_Fcomplex (_FCsubcr)(_Fcomplex, float);

_Lcomplex (_LCbuild)(long double, long double);
_Lcomplex (_LCmulcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCmulcr)(_Lcomplex, long double);
_Lcomplex (_LCdivcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCdivcr)(_Lcomplex, long double);
_Lcomplex (_LCaddcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCaddcr)(_Lcomplex, long double);
_Lcomplex (_LCsubcc)(_Lcomplex, _Lcomplex);
_Lcomplex (_LCsubcr)(_Lcomplex, long double);




_Dcomplex clog(_Dcomplex);



_Dcomplex (conj)(_Dcomplex x)
 {
 double ans[2];

 ans[0] = creal(x);
 ans[1] = -cimag(x);
 return (*(_Dcomplex *)&ans[0]);
 }

