#ifndef _DEFS
#define _DEFS

#include <stddef.h>

typedef union
 {
 unsigned short _Sh[8];
 double _Val;
 } _Dval;

 		/* float declarations */
typedef union
{	/* pun floating type as integer array */
unsigned short _Sh[8];
float _Val;
} _Fval;
 
 typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } _Dconst;

extern const _Dconst _Denorm, _FEps, _Hugeval, _Inf, _FInf, _Nan, _FNan, _Snan, _Rteps, _Eps;
extern const double _Zero;
extern const float _FZero;

//-----------------------------
//#ifndef __SIZE_TYPE__
//#define __SIZE_TYPE__
//#endif
//typedef unsigned int size_t;
//----------------------------

//OB: memcpy from HighTecARM was renamed to memcpy_HighTecARMImpl to avoid clashes with standard function from stdio.h
extern void *memcpy_HighTecARMImpl(void *, const void *, size_t);
extern double ldexp(double x, int xexp);
extern float (ldexpf)(float x, int xexp);
extern double sqrt(double x);
extern float (sqrtf)(float x);
extern double (pow)(double x, double y);
extern  float (powf)(float x, float y);

extern short _Dunscale(short *pex, double *px);
extern short _Dnorm(_Dval *ps);
extern short _FDnorm(_Fval *ps);
extern short _FDscale(float *px, long lexp);
extern short _FDscalex(float *px, long lexp, int round_mode);
extern short _FDtest(float *px);
extern short _FDunscale(short *pex, float *px);
extern short _Dscale(double *px, long lexp);
extern short _Dscalex(double *px, long lexp, int round_mode);
extern short _Dtest(double *px);
extern short _Dint(double *, short);
extern unsigned short *_Plsw(double *px);
extern unsigned short *_Pmsw(double *px);
extern short _FDint(float *, short);
extern float _FLogpoly(float w);
extern unsigned short *_FPmsw(float *px);

extern float _FXp_getw(const float *, int);
extern float *_FXp_setn(float *, int, long);
extern float *_FXp_setw(float *, int, float);
extern float *_FXp_addh(float *, int, float);
extern float *_FXp_mulh(float *, int, float);
extern float *_FXp_movx(float *, int, const float *);
extern float *_FXp_addx(float *, int,
 const float *, int);
extern float *_FXp_subx(float *, int,
 const float *, int);
extern float *_FXp_ldexpx(float *, int, int);
extern float *_FXp_mulx(float *, int,
 const float *, int, float *);
extern float *_FXp_invx(float *, int, float *);
extern float *_FXp_sqrtx(float *, int, float *);

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

extern float (expf)(float x);
extern double (fma)(double x, double y, double z);
extern double (sin)(double x);
extern double (cos)(double x);
extern double (atanh)(double x);
extern double (cosh)(double x);
extern double (log1p)(double x);
extern double (log)(double x);
extern double (_Log)(double x, int baseflag);
extern double _Logpoly(double w);
extern short _Exp(double *px, double y, long eoff);
extern short _FExp(float *px, float y, long eoff);

extern unsigned long toRep(float x);


// OS: for testing
#define __builtin_flt_rounds 1

#endif
