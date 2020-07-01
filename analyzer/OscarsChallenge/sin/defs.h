#ifndef _DEFS
#define _DEFS

#include <stddef.h>

typedef union
 {
 unsigned short _Sh[8];
 double _Val;
 } _Dval;
 
 typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } _Dconst;

extern const _Dconst _Denorm, _Hugeval, _Inf, _Nan, _Snan, _Rteps, _Eps;

//-----------------------------
//#ifndef __SIZE_TYPE__
//#define __SIZE_TYPE__
//#endif
//typedef unsigned int size_t;
//----------------------------

//OB: memcpy from HighTecARM was renamed to memcpy_HighTecARMImpl to avoid clashes with standard function from stdio.h
extern void *memcpy_HighTecARMImpl(void *, const void *, size_t);
extern double ldexp(double x, int xexp);
extern double sqrt(double x);

extern short _Dunscale(short *pex, double *px);
extern short _Dnorm(_Dval *ps);
extern short _Dscale(double *px, long lexp);
extern short _Dscalex(double *px, long lexp, int round_mode);
extern short _Dtest(double *px);
extern short _Dint(double *, short);
extern unsigned short *_Plsw(double *px);
extern unsigned short *_Pmsw(double *px);

extern double _Xp_getw(const double *p, int n);
extern double _Xp_getw_help(const double *p, int n,double sin_arg);

extern double *_Xp_setw(double *p, int n, double x);
extern double *_Xp_addh(double *p, int n, double x0);
extern double *_Xp_mulh(double *p, int n, double x0);
extern double *_Xp_setn(double *p, int n, long x);
extern double *_Xp_movx(double *p, int n, const double *q);
extern double *_Xp_addx(double *p, int n, const double *q, int m);
extern double *_Xp_subx(double *p, int n, const double *q, int m);
extern double *_Xp_ldexpx(double *p, int n, int m);
extern double *_Xp_mulx(double *p, int n, const double *q, int m, double *ptemp2);
extern double *_Xp_invx(double *p, int n, double *ptemp4);
extern double *_Xp_sqrtx(double *p, int n, double *ptemp4);

extern double fmod(double x, double y);

typedef unsigned long fexcept_t;
typedef unsigned long fenv_t;
extern void (_Feraise)(int except);
extern double _Force_raise(int except);
extern int (feraiseexcept)(int except);
extern int (fegetenv)(fenv_t *penv);
extern int (fesetenv)(const fenv_t *penv);
extern unsigned int _Quad(double *px, int retcode);

extern double _Sinx(double x, unsigned int qoff, int quads);

extern int _Errno;

extern double (sin)(double x);
extern double (cos)(double x);

// OS: for testing
#define __builtin_flt_rounds 1

#endif
