# 1 "oscar.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "oscar.c"
# 1 "defs.h" 1



# 1 "C:/TDM-GCC-64/lib/gcc/x86_64-w64-mingw32/5.1.0/include/stddef.h" 1 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stddef.h" 1 3 4






# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 1 3 4
# 10 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 1 3 4
# 12 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_mac.h" 1 3 4
# 46 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_mac.h" 3 4
             
# 55 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_mac.h" 3 4
             
# 13 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_secapi.h" 1 3 4
# 14 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 275 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/vadefs.h" 1 3 4
# 9 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/vadefs.h" 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 1 3 4
# 565 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/sdks/_mingw_directx.h" 1 3 4
# 566 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/sdks/_mingw_ddk.h" 1 3 4
# 567 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 10 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/vadefs.h" 2 3 4




#pragma pack(push,_CRT_PACKING)
# 24 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/vadefs.h" 3 4
  
# 24 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/vadefs.h" 3 4
 typedef __builtin_va_list __gnuc_va_list;






  typedef __gnuc_va_list va_list;
# 103 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/vadefs.h" 3 4
#pragma pack(pop)
# 276 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 539 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw.h" 3 4
void __attribute__((__cdecl__)) __debugbreak(void);
extern __inline__ __attribute__((__always_inline__,__gnu_inline__)) void __attribute__((__cdecl__)) __debugbreak(void)
{
  __asm__ __volatile__("int {$}3":);
}




const char *__mingw_get_crt_info (void);
# 11 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 2 3 4




#pragma pack(push,_CRT_PACKING)
# 35 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
__extension__ typedef unsigned long long size_t;
# 45 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
__extension__ typedef long long ssize_t;






typedef size_t rsize_t;
# 62 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
__extension__ typedef long long intptr_t;
# 75 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
__extension__ typedef unsigned long long uintptr_t;
# 88 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
__extension__ typedef long long ptrdiff_t;
# 98 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef unsigned short wchar_t;







typedef unsigned short wint_t;
typedef unsigned short wctype_t;





typedef int errno_t;




typedef long __time32_t;




__extension__ typedef long long __time64_t;
# 138 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef __time64_t time_t;
# 422 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
struct threadlocaleinfostruct;
struct threadmbcinfostruct;
typedef struct threadlocaleinfostruct *pthreadlocinfo;
typedef struct threadmbcinfostruct *pthreadmbcinfo;
struct __lc_time_data;

typedef struct localeinfo_struct {
  pthreadlocinfo locinfo;
  pthreadmbcinfo mbcinfo;
} _locale_tstruct,*_locale_t;



typedef struct tagLC_ID {
  unsigned short wLanguage;
  unsigned short wCountry;
  unsigned short wCodePage;
} LC_ID,*LPLC_ID;




typedef struct threadlocaleinfostruct {
  int refcount;
  unsigned int lc_codepage;
  unsigned int lc_collate_cp;
  unsigned long lc_handle[6];
  LC_ID lc_id[6];
  struct {
    char *locale;
    wchar_t *wlocale;
    int *refcount;
    int *wrefcount;
  } lc_category[6];
  int lc_clike;
  int mb_cur_max;
  int *lconv_intl_refcount;
  int *lconv_num_refcount;
  int *lconv_mon_refcount;
  struct lconv *lconv;
  int *ctype1_refcount;
  unsigned short *ctype1;
  const unsigned short *pctype;
  const unsigned char *pclmap;
  const unsigned char *pcumap;
  struct __lc_time_data *lc_time_curr;
} threadlocinfo;







#pragma pack(pop)
# 8 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stddef.h" 2 3 4
# 18 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stddef.h" 3 4
  __attribute__ ((__dllimport__)) extern int *__attribute__((__cdecl__)) _errno(void);

  errno_t __attribute__((__cdecl__)) _set_errno(int _Value);
  errno_t __attribute__((__cdecl__)) _get_errno(int *_Value);


  __attribute__ ((__dllimport__)) extern unsigned long __attribute__((__cdecl__)) __threadid(void);

  __attribute__ ((__dllimport__)) extern uintptr_t __attribute__((__cdecl__)) __threadhandle(void);
# 423 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stddef.h" 3 4
typedef struct {
  long long __max_align_ll __attribute__((__aligned__(__alignof__(long long))));
  long double __max_align_ld __attribute__((__aligned__(__alignof__(long double))));
} max_align_t;
# 2 "C:/TDM-GCC-64/lib/gcc/x86_64-w64-mingw32/5.1.0/include/stddef.h" 2 3 4
# 5 "defs.h" 2


# 6 "defs.h"
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
# 30 "defs.h"
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
# 2 "oscar.c" 2
# 1 "xdtest.c" 1
# 11 "xdtest.c"
short _Dtest(double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;

 if ((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) == ((unsigned short)((1 << (15 - 4)) - 1)) << 4)
  return ((short)((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0));
  return (0);
 }

unsigned short *_Plsw(double *px)
 {
 return (&((_Dval *)(char *)px)->_Sh[0]);
 }

unsigned short *_Pmsw(double *px)
 {
 return (&((_Dval *)(char *)px)->_Sh[3]);
 }
# 3 "oscar.c" 2
# 1 "xferaise.c" 1

void (_Feraise)(int except)
 {

 int errh = (1 | 2);

 if ((errh & 2) != 0)
  {
  if ((except & (0x04 | 0x08)) != 0)
   except |= 0x10;
  feraiseexcept(except);
  }

 if ((errh & 1) == 0)
  ;
 else if ((except & 0x01) != 0)
  ( _Errno) = 0x0021;
 else if ((except & (0x02 | 0x08 | 0x04)) != 0)
  ( _Errno) = 0x0022;







 }
# 4 "oscar.c" 2
# 1 "xquad.c" 1


static const double c[] = {
 (double)(52707178.0 * 8),
 (double)(35788428.0 * 8 / 67108864.0L),
 (double)(9253169.0 * 8 / (67108864.0L * 67108864.0L)),
 (double)(40012672.0 * 8 / ((67108864.0L * 67108864.0L) * 67108864.0L)),
 (double)(57701188.0 * 8 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L))),
 (double)(43001056.0 * 8 / (((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) * 67108864.0L)),
 };
static const double piby2[(sizeof c / sizeof c[0])] = {
 (double)(52707178.0 / (67108864.0L * 4) * 8),
 (double)(35788428.0 / (67108864.0L * 4) * 8 / 67108864.0L),
 (double)(9253169.0 / (67108864.0L * 4) * 8 / (67108864.0L * 67108864.0L)),
 (double)(40012672.0 / (67108864.0L * 4) * 8 / ((67108864.0L * 67108864.0L) * 67108864.0L)),
 (double)(57701188.0 / (67108864.0L * 4) * 8 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L))),
 (double)(43001056.0 / (67108864.0L * 4) * 8 / (((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) * 67108864.0L)),
 };
static const double b[][(sizeof c / sizeof c[0])]={
 {
 (double)(-44650192.0 * 4 / 67108864.0L),
 (double)(24373128.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(52217969.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(32495948.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(52430789.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(10049912.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(52311224.0 * 4 / 67108864.0L),
 (double)(32151194.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(3521788.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(17658793.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(12653430.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(28545187.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(51596056.0 * 4 / 67108864.0L),
 (double)(34529797.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(57427415.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(47345698.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(19105487.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(33321368.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(45168630.0 * 2 / 67108864.0L),
 (double)(26184515.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(34707788.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(8089061.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(57195156.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(2619245.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(65114761.0 * 2 / 67108864.0L),
 (double)(20027062.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(47489540.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(14214764.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(59230352.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(31139624.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(52534019.0 * 4 / 67108864.0L),
 (double)(47861617.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(57586889.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(3460398.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(36832902.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(41682469.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-6849158.0 * 2 / 67108864.0L),
 (double)(17949024.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(45979062.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(9676238.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(39833771.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(34623173.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(45206937.0 * 4 / 67108864.0L),
 (double)(30064030.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(1277106.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(31481331.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(32310796.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(1285356.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-45881600.0 * 4 / 67108864.0L),
 (double)(65668541.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(35499664.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(66210623.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(58493317.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(65982999.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(1294181.0 * 2 / 67108864.0L),
 (double)(64808977.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(54574838.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(55770472.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(10000946.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(4603453.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-51696412.0 * 4 / 67108864.0L),
 (double)(16119248.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(25386904.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(11185286.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(56753234.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(29265903.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(52319465.0 * 2 / 67108864.0L),
 (double)(34308466.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(21037701.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(49304840.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(8546391.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(10934690.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(36475572.0 * 2 / 67108864.0L),
 (double)(15585858.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(7042195.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(63308505.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(34738113.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(43227007.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-57543755.0 * 2 / 67108864.0L),
 (double)(13151969.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(61014092.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(51172192.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(36610653.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(11303136.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(62328609.0 * 2 / 67108864.0L),
 (double)(17373473.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(20271818.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(2732297.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(44166545.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(13628803.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(48097582.0 * 2 / 67108864.0L),
 (double)(63049219.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(54727889.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(30569368.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(45941495.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(33618616.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-34138911.0 * 4 / 67108864.0L),
 (double)(472447.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(24191272.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(66788033.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(42935532.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(43807839.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(64560050.0 * 2 / 67108864.0L),
 (double)(12046811.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(9847201.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(48880213.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(61123341.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(34876678.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(39872682.0 * 4 / 67108864.0L),
 (double)(22873682.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(31384146.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(15814545.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(47398232.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(41988367.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(32224897.0 * 2 / 67108864.0L),
 (double)(52604533.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(21898454.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(52106995.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(4539170.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(13183912.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-13594073.0 * 2 / 67108864.0L),
 (double)(12725775.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(28801379.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(18128404.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(44774589.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(60130533.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-46124738.0 * 2 / 67108864.0L),
 (double)(21613885.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(34957299.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(53333399.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(36591729.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(62362664.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-6995674.0 * 2 / 67108864.0L),
 (double)(54175033.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(22625417.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(23179700.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(52525623.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(17345860.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-3462232.0 * 2 / 67108864.0L),
 (double)(46003782.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(45190672.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(51960393.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(2449619.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(8368730.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-34168926.0 * 2 / 67108864.0L),
 (double)(14513990.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(63122172.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(36035659.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(1080650.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(47601380.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-30221623.0 * 2 / 67108864.0L),
 (double)(39344933.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(48788887.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(35265370.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(2271449.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(43393681.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-44514277.0 * 2 / 67108864.0L),
 (double)(34044011.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(63424890.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(19238758.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(52815139.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(23109538.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-36385577.0 * 2 / 67108864.0L),
 (double)(57863133.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(20694677.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(52649708.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(48587469.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(7820136.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-36403504.0 * 4 / 67108864.0L),
 (double)(64031972.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(61650298.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(32999103.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(37147537.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(10436430.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(38413979.0 * 4 / 67108864.0L),
 (double)(22582941.0 * 4 / (67108864.0L * 67108864.0L)),
 (double)(1611179.0 * 4 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(21108706.0 * 4 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(27622625.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(37960282.0 * 4 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 {
 (double)(-4138705.0 * 2 / 67108864.0L),
 (double)(41933462.0 * 2 / (67108864.0L * 67108864.0L)),
 (double)(51111950.0 * 2 / (67108864.0L * 67108864.0L) / 67108864.0L),
 (double)(51269386.0 * 2 / (67108864.0L * 67108864.0L) / (67108864.0L * 67108864.0L)),
 (double)(59561043.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / 67108864.0L),
 (double)(6069291.0 * 2 / ((67108864.0L * 67108864.0L) * (67108864.0L * 67108864.0L)) / (67108864.0L * 67108864.0L))},
 };
static const double huge_rad = (double)(0.63661977236758134307553505349005744L
 * (67108864.0L * 67108864.0L * 2) / 67108864.0L);
static const double inv2pi = 0.15915494309189533576888376337251436;
static const double pi = 3.1415926535897932384626433832795029;
static const double piby4 = 0.78539816339744830961566084581987572;
static const double twobypi = 0.63661977236758134307553505349005745;
static const double inv_fracbits = 1.0 / (double)67108864.0L;
static double _Quad_multiply(double x, double y)
 {
 double ans = x * y;
 if (ans == 0.0)
  {
  short xexp, yexp;
  _Dunscale(&xexp, &x);
  _Dunscale(&yexp, &y);
  ans = x * y;
  _Dscale(&ans, xexp + yexp);
  }
 return (ans);
 }
unsigned int _Quad(double *px, int retcode)
 {
 double x = *px;
 double g;
 if (retcode & 1)
  {
  unsigned int qoff;
  _Dint(&x, -1);
  if (x == 0.0)
   x = *px;
  else
   {
   x = *px - x;
   *px = x;
   }
  qoff = (unsigned int)(int)(x + x);
  _Dint(px, 1);
  if (*px != 0.0)
   x -= *px;
  if (0.25 < x)
   {
   x -= 0.5;
   ++qoff;
   }
  else if (x < -0.25)
   {
   x += 0.5;
   --qoff;
   }
  *px = _Quad_multiply(x, pi);
  return (qoff);
  }
 if (-piby4 < x && x < piby4)
  {
  *px = x;
  return (0);
  }
 else if (-huge_rad < x && x < huge_rad)
  {
  g = x * twobypi;
  if (0.0 <= g)
   g += 0.5;
  else
   g -= 0.5;
  _Dint(&g, 0);
  if (g != 0.0)
   {
   double xpx[2], xpy[(sizeof c / sizeof c[0])];
   memcpy_HighTecARMImpl(xpy, piby2, (sizeof c / sizeof c[0]) * sizeof (double));
   _Xp_mulh(xpy, (sizeof c / sizeof c[0]), -g);
   _Xp_setw(xpx, 2, x);
   _Xp_addx(xpy, (sizeof c / sizeof c[0]), xpx, 2);


    x = _Xp_getw_help(xpy, (sizeof c / sizeof c[0]),x);
    }
  *px = x;
  }
 else
  {
  double xpx[2], xpy[(sizeof c / sizeof c[0])], xpz[(sizeof c / sizeof c[0])];
  short xexp;
  g = x;
  _Dunscale(&xexp, &g);
  if (xexp < 53 + 5 + (1 << 5))
   _Xp_setw(xpz, (sizeof c / sizeof c[0]), x);
  else
   {
   xexp = (xexp - (53 + 1)) >> 5;
   _Dscale(&x, -(xexp << 5));
   _Xp_setw(xpx, 2, x);
   memcpy_HighTecARMImpl(xpz, &b[xexp - 1][0], (sizeof c / sizeof c[0]) * sizeof (double));
   _Xp_mulh(xpz, (sizeof c / sizeof c[0]), xpx[0]);
   if (xpx[1] != 0.0)
    {
    memcpy_HighTecARMImpl(xpy, &b[xexp - 1][0], (sizeof c / sizeof c[0]) * sizeof (double));
    _Xp_mulh(xpy, (sizeof c / sizeof c[0]), xpx[1]);
    _Xp_addx(xpz, (sizeof c / sizeof c[0]), xpy, (sizeof c / sizeof c[0]));
    }
   }
  for (; xpz[0] < -huge_rad || huge_rad < xpz[0]; )
   {
   g = (xpz[0] + xpz[1]) * inv2pi;
   _Dint(&g, 0);
   _Xp_setw(xpx, 2, -g * inv_fracbits);
   memcpy_HighTecARMImpl(xpy, c, (sizeof c / sizeof c[0]) * sizeof (double));
   _Xp_mulh(xpy, (sizeof c / sizeof c[0]), xpx[0]);
   if (xpx[1] != 0.0)
    {
    double xpw[(sizeof c / sizeof c[0])];
    memcpy_HighTecARMImpl(xpw, c, (sizeof c / sizeof c[0]) * sizeof (double));
    _Xp_mulh(xpw, (sizeof c / sizeof c[0]), xpx[1]);
    _Xp_addx(xpy, (sizeof c / sizeof c[0]), xpw, (sizeof c / sizeof c[0]));
    }
   _Xp_addx(xpz, (sizeof c / sizeof c[0]), xpy, (sizeof c / sizeof c[0]));
   }
  g = (xpz[0] + xpz[1]) * twobypi;
  if (0.0 <= g)
   g += 0.5;
  else
   g -= 0.5;
  _Dint(&g, 0);
  if (g != 0.0)
   {
   double xpw[(sizeof c / sizeof c[0])];
   memcpy_HighTecARMImpl(xpw, c, (sizeof c / sizeof c[0]) * sizeof (double));
   _Xp_mulh(xpw, (sizeof c / sizeof c[0]), -g * 0.25 * inv_fracbits);
   _Xp_addx(xpz, (sizeof c / sizeof c[0]), xpw, (sizeof c / sizeof c[0]));
   }
  *px = _Xp_getw(xpz, (sizeof c / sizeof c[0]));
  }
 if (g < -(double)0x7fffffffL
  || (double)0x7fffffffL < g)
  g = fmod(g, (double)0x7fffffffL + 1.0);
 return ((unsigned int)(long)g & 0x3);
 }
unsigned int _Quadph(double *px, double phase)
 {
 unsigned int qoff = _Quad(px, 0);
 double ph0 = phase;
 double ans;
 _Dint(&ph0, 1);
 phase -= ph0;
 if (ph0 < -(double)(0x7fffffffL / 2)
  || (double)(0x7fffffffL / 2) < ph0)
  ph0 = fmod(ph0, (double)(0x7fffffffL / 2) + 1.0);
 qoff += ((unsigned int)(long)(ph0 * 2.0)) & 0x3;
 ans = *px + phase * pi;
 if (piby4 <= ans)
  {
  phase -= 0.5;
  ++qoff;
  *px += phase * pi;
  }
 else if (ans <= -piby4)
  {
  phase += 0.5;
  --qoff;
  *px += phase * pi;
  }
 else
  *px = ans;
 return (qoff);
 }
# 5 "oscar.c" 2
# 1 "xvalues.c" 1

const _Dconst _Denorm = {{1, 0, 0, 0}};
const _Dconst _Hugeval = {{0, 0, 0, ((unsigned short)((1 << (15 - 4)) - 1)) << 4}};
const _Dconst _Inf = {{0, 0, 0, ((unsigned short)((1 << (15 - 4)) - 1)) << 4}};
const _Dconst _Nan = {{0, 0, 0, (((unsigned short)((1 << (15 - 4)) - 1)) << 4) | (1 << (4 - 1))}};
const _Dconst _Snan = {{1, 0, 0, ((unsigned short)((1 << (15 - 4)) - 1)) << 4}};
const _Dconst _Eps = {{0, 0, 0, (0x3fe - (48 + 4) - 1) << 4}};
const _Dconst _Rteps = {{0, 0, 0, (0x3fe - (48 + 4) / 2) << 4}};
# 6 "oscar.c" 2
# 1 "xdunscal.c" 1


short _Dunscale(short *pex, double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;
 short xchar = (ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) >> 4;

 if (xchar == ((unsigned short)((1 << (15 - 4)) - 1)))
  {
  *pex = 0;
  return ((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0
   || ps->_Sh[1] != 0 || ps->_Sh[0] != 0 ? 2 : 1);
  }
 else if (0 < xchar || (xchar = _Dnorm(ps)) <= 0)
  {
  ps->_Sh[3] = (ps->_Sh[3] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) | 0x3fe << 4;
  *pex = xchar - 0x3fe;
  return ((-1));
  }
 else
  {
  *pex = 0;
  return (0);
  }
 }
# 7 "oscar.c" 2
# 1 "xdscale.c" 1


short _Dscale(double *px, long lexp)
 {
 return (_Dscalex(px, lexp, 4));
 }

short _Dscalex(double *px, long lexp, int round_mode)
 {
 _Dval *ps = (_Dval *)(char *)px;
 short xchar = (short)((ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) >> 4);

 if (xchar == ((unsigned short)((1 << (15 - 4)) - 1)))
  return ((short)((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2] != 0
   || ps->_Sh[1] != 0 || ps->_Sh[0] != 0 ? 2 : 1));
 else if (xchar == 0 && 0 < (xchar = _Dnorm(ps)))
  return (0);

 if (0 < lexp && ((unsigned short)((1 << (15 - 4)) - 1)) - xchar <= lexp)
  {
  *px = ps->_Sh[3] & ((unsigned short)0x8000) ? -_Inf._Double : _Inf._Double;
  return (1);
  }
 else if (-xchar < lexp)
  {
  ps->_Sh[3] = (unsigned short)((ps->_Sh[3] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1)))))
   | (lexp + xchar) << 4);
  return ((-1));
  }
 else
  {
  unsigned short sign = (unsigned short)(ps->_Sh[3] & ((unsigned short)0x8000));

  ps->_Sh[3] = (unsigned short)(1 << 4 | (ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))));
  lexp += xchar - 1;
  if (lexp < -(48 + 1 + 4) || 0 <= lexp)
   {
   ps->_Sh[3] = sign;
   ps->_Sh[2] = 0;
   ps->_Sh[1] = 0;
   ps->_Sh[0] = 0;
   return (0);
   }
  else
   {
   short xexp = (short)lexp;
   unsigned short psx = 0;

   for (; xexp <= -16; xexp += 16)
    {
    psx = ps->_Sh[0] | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = ps->_Sh[1];
    ps->_Sh[1] = ps->_Sh[2];
    ps->_Sh[2] = ps->_Sh[3];
    ps->_Sh[3] = 0;
    }
   if ((xexp = (short)-xexp) != 0)
    {
    psx = (ps->_Sh[0] << (16 - xexp)) | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> xexp
     | ps->_Sh[1] << (16 - xexp));
    ps->_Sh[1] = (unsigned short)(ps->_Sh[1] >> xexp
     | ps->_Sh[2] << (16 - xexp));
    ps->_Sh[2] = (unsigned short)(ps->_Sh[2] >> xexp
     | ps->_Sh[3] << (16 - xexp));
    ps->_Sh[3] >>= xexp;
    }

   ps->_Sh[3] |= sign;
   if (psx != 0)
    {
    int roundup = 0;

    if (round_mode == 4)
     round_mode = 1;

    switch (round_mode)
     {
    case 0:
     break;

    case 2:
     if (!sign)
      roundup = 1;
     break;

    case 3:
     if (sign)
      roundup = 1;
     break;

    default:
     if ((0x8000 < psx
      || (0x8000 == psx && (ps->_Sh[0] & 0x0001) != 0)))
      roundup = 1;
     break;
     }

    if (roundup
     && (++ps->_Sh[0] & 0xffff) == 0
     && (++ps->_Sh[1] & 0xffff) == 0
     && (++ps->_Sh[2] & 0xffff) == 0)
     ++ps->_Sh[3];
    }

   if (ps->_Sh[3] == sign
    && ps->_Sh[2] == 0
    && ps->_Sh[1] == 0
    && ps->_Sh[0] == 0)
    return (0);
   else
    return ((-1));
   }
  }
 }
# 8 "oscar.c" 2
# 1 "xdint.c" 1
# 60 "xdint.c"
short _Dint(double *px, short xexp)
 {
 _Dval *ps = (_Dval *)(char *)px;
 unsigned short frac;
 short xchar = (ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) >> 4;
 if (xchar == ((unsigned short)((1 << (15 - 4)) - 1)))
  return ((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) == 0 && ps->_Sh[2] == 0
   && ps->_Sh[1] == 0 && ps->_Sh[0] == 0
   ? 1 : 2);
 else if ((ps->_Sh[3] & ~((unsigned short)0x8000)) == 0 && ps->_Sh[2] == 0
  && ps->_Sh[1] == 0 && ps->_Sh[0] == 0)
  return (0);
 xchar = (0x3fe + 48 + 4 + 1) - xchar - xexp;
 if (xchar <= 0)
  return (0);
 else if ((48 + 4 + 1) <= xchar)
  {
  ps->_Sh[3] &= ((unsigned short)0x8000);
  ps->_Sh[2] = 0;
  ps->_Sh[1] = 0;
  ps->_Sh[0] = 0;
  return ((-1));
  }
 else
  {
  static const unsigned short mask[] = {
   0x0000, 0x0001, 0x0003, 0x0007,
   0x000f, 0x001f, 0x003f, 0x007f,
   0x00ff, 0x01ff, 0x03ff, 0x07ff,
   0x0fff, 0x1fff, 0x3fff, 0x7fff};
  static const size_t sub[] = {0, 1, 2, 3};
  frac = mask[xchar & 0xf];
  xchar >>= 4;
  frac &= ps->_Sh[sub[xchar]];
  ps->_Sh[sub[xchar]] ^= frac;
  switch (xchar)
   {
  case 3:
   frac |= ps->_Sh[2], ps->_Sh[2] = 0;
  case 2:
   frac |= ps->_Sh[1], ps->_Sh[1] = 0;
  case 1:
   frac |= ps->_Sh[0], ps->_Sh[0] = 0;
   }
  return (frac != 0 ? (-1) : 0);
  }
 }
# 9 "oscar.c" 2
# 1 "xprec.c" 1

# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 1 3
# 11 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_print_push.h" 1 3
# 12 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 2 3

#pragma pack(push,_CRT_PACKING)
# 26 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  
# 26 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
 struct _iobuf {
    char *_ptr;
    int _cnt;
    char *_base;
    int _flag;
    int _file;
    int _charbuf;
    int _bufsiz;
    char *_tmpfname;
  };
  typedef struct _iobuf FILE;
# 80 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_off_t.h" 1 3




  typedef long _off_t;

  typedef long off32_t;





  __extension__ typedef long long _off64_t;

  __extension__ typedef long long off64_t;
# 26 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_off_t.h" 3
typedef off32_t off_t;
# 81 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 2 3



  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) __iob_func(void);
# 103 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  __extension__ typedef long long fpos_t;
# 139 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
extern
  __attribute__((__format__ (gnu_scanf, 2, 3))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_sscanf(const char * __restrict__ _Src,const char * __restrict__ _Format,...);
extern
  __attribute__((__format__ (gnu_scanf, 2, 0))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vsscanf (const char * __restrict__ _Str,const char * __restrict__ Format,va_list argp);
extern
  __attribute__((__format__ (gnu_scanf, 1, 2))) __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_scanf(const char * __restrict__ _Format,...);
extern
  __attribute__((__format__ (gnu_scanf, 1, 0))) __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_vscanf(const char * __restrict__ Format, va_list argp);
extern
  __attribute__((__format__ (gnu_scanf, 2, 3))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_fscanf(FILE * __restrict__ _File,const char * __restrict__ _Format,...);
extern
  __attribute__((__format__ (gnu_scanf, 2, 0))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vfscanf (FILE * __restrict__ fp, const char * __restrict__ Format,va_list argp);

extern
  __attribute__((__format__ (gnu_printf, 3, 0))) __attribute__ ((__nonnull__ (3)))
  int __attribute__((__cdecl__)) __mingw_vsnprintf(char * __restrict__ _DstBuf,size_t _MaxCount,const char * __restrict__ _Format,
                               va_list _ArgList);
extern
  __attribute__((__format__ (gnu_printf, 3, 4))) __attribute__ ((__nonnull__ (3)))
  int __attribute__((__cdecl__)) __mingw_snprintf(char * __restrict__ s, size_t n, const char * __restrict__ format, ...);
extern
  __attribute__((__format__ (gnu_printf, 1, 2))) __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_printf(const char * __restrict__ , ... ) __attribute__ ((__nothrow__));
extern
  __attribute__((__format__ (gnu_printf, 1, 0))) __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_vprintf (const char * __restrict__ , va_list) __attribute__ ((__nothrow__));
extern
  __attribute__((__format__ (gnu_printf, 2, 3))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_fprintf (FILE * __restrict__ , const char * __restrict__ , ...) __attribute__ ((__nothrow__));
extern
  __attribute__((__format__ (gnu_printf, 2, 0))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vfprintf (FILE * __restrict__ , const char * __restrict__ , va_list) __attribute__ ((__nothrow__));
extern
  __attribute__((__format__ (gnu_printf, 2, 3))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_sprintf (char * __restrict__ , const char * __restrict__ , ...) __attribute__ ((__nothrow__));
extern
  __attribute__((__format__ (gnu_printf, 2, 0))) __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vsprintf (char * __restrict__ , const char * __restrict__ , va_list) __attribute__ ((__nothrow__));
extern
  __attribute__((__format__ (gnu_printf, 2, 3))) __attribute__((nonnull (1,2)))
  int __attribute__((__cdecl__)) __mingw_asprintf(char ** __restrict__ , const char * __restrict__ , ...) __attribute__ ((__nothrow__));
extern
  __attribute__((__format__ (gnu_printf, 2, 0))) __attribute__((nonnull (1,2)))
  int __attribute__((__cdecl__)) __mingw_vasprintf(char ** __restrict__ , const char * __restrict__ , va_list) __attribute__ ((__nothrow__));
# 377 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  int __attribute__((__cdecl__)) fprintf(FILE * __restrict__ _File,const char * __restrict__ _Format,...);
  int __attribute__((__cdecl__)) printf(const char * __restrict__ _Format,...);
  int __attribute__((__cdecl__)) sprintf(char * __restrict__ _Dest,const char * __restrict__ _Format,...) ;

  int __attribute__((__cdecl__)) vfprintf(FILE * __restrict__ _File,const char * __restrict__ _Format,va_list _ArgList);
  int __attribute__((__cdecl__)) vprintf(const char * __restrict__ _Format,va_list _ArgList);
  int __attribute__((__cdecl__)) vsprintf(char * __restrict__ _Dest,const char * __restrict__ _Format,va_list _Args) ;

  int __attribute__((__cdecl__)) fscanf(FILE * __restrict__ _File,const char * __restrict__ _Format,...) ;
  int __attribute__((__cdecl__)) scanf(const char * __restrict__ _Format,...) ;
  int __attribute__((__cdecl__)) sscanf(const char * __restrict__ _Src,const char * __restrict__ _Format,...) ;





  int __attribute__((__cdecl__)) __ms_vscanf(const char * __restrict__ Format, va_list argp);
  int __attribute__((__cdecl__)) __ms_vfscanf (FILE * __restrict__ fp, const char * __restrict__ Format,va_list argp);
  int __attribute__((__cdecl__)) __ms_vsscanf (const char * __restrict__ _Str,const char * __restrict__ Format,va_list argp);

  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  __attribute__ ((__nonnull__ (2)))
  int vfscanf (FILE *__stream, const char *__format, __builtin_va_list __local_argv)
  {
    return __ms_vfscanf (__stream, __format, __local_argv);
  }

  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  __attribute__ ((__nonnull__ (2)))
  int vsscanf (const char * __restrict__ __source, const char * __restrict__ __format, __builtin_va_list __local_argv)
  {
    return __ms_vsscanf( __source, __format, __local_argv );
  }
  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  __attribute__ ((__nonnull__ (1)))
  int vscanf(const char *__format, __builtin_va_list __local_argv)
  {
    return __ms_vscanf (__format, __local_argv);
  }




  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _filbuf(FILE *_File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _flsbuf(int _Ch,FILE *_File);



  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _fsopen(const char *_Filename,const char *_Mode,int _ShFlag);

  void __attribute__((__cdecl__)) clearerr(FILE *_File);
  int __attribute__((__cdecl__)) fclose(FILE *_File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fcloseall(void);



  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _fdopen(int _FileHandle,const char *_Mode);

  int __attribute__((__cdecl__)) feof(FILE *_File);
  int __attribute__((__cdecl__)) ferror(FILE *_File);
  int __attribute__((__cdecl__)) fflush(FILE *_File);
  int __attribute__((__cdecl__)) fgetc(FILE *_File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fgetchar(void);
  int __attribute__((__cdecl__)) fgetpos(FILE * __restrict__ _File ,fpos_t * __restrict__ _Pos);
  int __attribute__((__cdecl__)) fgetpos64(FILE * __restrict__ _File ,fpos_t * __restrict__ _Pos);
  char *__attribute__((__cdecl__)) fgets(char * __restrict__ _Buf,int _MaxCount,FILE * __restrict__ _File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fileno(FILE *_File);



  __attribute__ ((__dllimport__)) char *__attribute__((__cdecl__)) _tempnam(const char *_DirName,const char *_FilePrefix);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _flushall(void);
  FILE *__attribute__((__cdecl__)) fopen(const char * __restrict__ _Filename,const char * __restrict__ _Mode) ;
  FILE *fopen64(const char * __restrict__ filename,const char * __restrict__ mode);
  int __attribute__((__cdecl__)) fputc(int _Ch,FILE *_File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fputchar(int _Ch);
  int __attribute__((__cdecl__)) fputs(const char * __restrict__ _Str,FILE * __restrict__ _File);
  size_t __attribute__((__cdecl__)) fread(void * __restrict__ _DstBuf,size_t _ElementSize,size_t _Count,FILE * __restrict__ _File);
  FILE *__attribute__((__cdecl__)) freopen(const char * __restrict__ _Filename,const char * __restrict__ _Mode,FILE * __restrict__ _File) ;
  int __attribute__((__cdecl__)) fsetpos(FILE *_File,const fpos_t *_Pos);
  int __attribute__((__cdecl__)) fsetpos64(FILE *_File,const fpos_t *_Pos);
  int __attribute__((__cdecl__)) fseek(FILE *_File,long _Offset,int _Origin);



  int fseeko64(FILE* stream, _off64_t offset, int whence);
  int fseeko(FILE* stream, _off_t offset, int whence);
# 472 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  long __attribute__((__cdecl__)) ftell(FILE *_File);

  _off_t ftello(FILE * stream);
  _off64_t ftello64(FILE * stream);
# 484 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  __extension__ int __attribute__((__cdecl__)) _fseeki64(FILE *_File,long long _Offset,int _Origin);
  __extension__ long long __attribute__((__cdecl__)) _ftelli64(FILE *_File);
  size_t __attribute__((__cdecl__)) fwrite(const void * __restrict__ _Str,size_t _Size,size_t _Count,FILE * __restrict__ _File);
  int __attribute__((__cdecl__)) getc(FILE *_File);
  int __attribute__((__cdecl__)) getchar(void);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _getmaxstdio(void);
  char *__attribute__((__cdecl__)) gets(char *_Buffer) ;
  int __attribute__((__cdecl__)) _getw(FILE *_File);


  void __attribute__((__cdecl__)) perror(const char *_ErrMsg);

  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _pclose(FILE *_File);
  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _popen(const char *_Command,const char *_Mode);




  int __attribute__((__cdecl__)) putc(int _Ch,FILE *_File);
  int __attribute__((__cdecl__)) putchar(int _Ch);
  int __attribute__((__cdecl__)) puts(const char *_Str);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _putw(int _Word,FILE *_File);


  int __attribute__((__cdecl__)) remove(const char *_Filename);
  int __attribute__((__cdecl__)) rename(const char *_OldFilename,const char *_NewFilename);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _unlink(const char *_Filename);

  int __attribute__((__cdecl__)) unlink(const char *_Filename) ;


  void __attribute__((__cdecl__)) rewind(FILE *_File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _rmtmp(void);
  void __attribute__((__cdecl__)) setbuf(FILE * __restrict__ _File,char * __restrict__ _Buffer) ;
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _setmaxstdio(int _Max);
  __attribute__ ((__dllimport__)) unsigned int __attribute__((__cdecl__)) _set_output_format(unsigned int _Format);
  __attribute__ ((__dllimport__)) unsigned int __attribute__((__cdecl__)) _get_output_format(void);
  int __attribute__((__cdecl__)) setvbuf(FILE * __restrict__ _File,char * __restrict__ _Buf,int _Mode,size_t _Size);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _scprintf(const char * __restrict__ _Format,...);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _snscanf(const char * __restrict__ _Src,size_t _MaxCount,const char * __restrict__ _Format,...) ;
  FILE *__attribute__((__cdecl__)) tmpfile(void) ;
  char *__attribute__((__cdecl__)) tmpnam(char *_Buffer);
  int __attribute__((__cdecl__)) ungetc(int _Ch,FILE *_File);

  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _snprintf(char * __restrict__ _Dest,size_t _Count,const char * __restrict__ _Format,...) ;
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _vsnprintf(char * __restrict__ _Dest,size_t _Count,const char * __restrict__ _Format,va_list _Args) ;




       
       


  int __attribute__((__cdecl__)) __ms_vsnprintf(char * __restrict__ d,size_t n,const char * __restrict__ format,va_list arg)
    ;

  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  __attribute__ ((__nonnull__ (3)))
  int vsnprintf (char * __restrict__ __stream, size_t __n, const char * __restrict__ __format, va_list __local_argv)
  {
    return __ms_vsnprintf (__stream, __n, __format, __local_argv);
  }

  int __attribute__((__cdecl__)) __ms_snprintf(char * __restrict__ s, size_t n, const char * __restrict__ format, ...);


static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
__attribute__ ((__nonnull__ (3)))
int snprintf (char * __restrict__ __stream, size_t __n, const char * __restrict__ __format, ...)
{
  register int __retval;
  __builtin_va_list __local_argv; __builtin_va_start( __local_argv, __format );
  __retval = __ms_vsnprintf (__stream, __n, __format, __local_argv);
  __builtin_va_end( __local_argv );
  return __retval;
}


       
       


  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _vscprintf(const char * __restrict__ _Format,va_list _ArgList);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _set_printf_count_output(int _Value);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _get_printf_count_output(void);




                                                     __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_swscanf(const wchar_t * __restrict__ _Src,const wchar_t * __restrict__ _Format,...);
                                                     __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vswscanf (const wchar_t * __restrict__ _Str,const wchar_t * __restrict__ Format,va_list argp);
                                                     __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_wscanf(const wchar_t * __restrict__ _Format,...);
                                                     __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_vwscanf(const wchar_t * __restrict__ Format, va_list argp);
                                                     __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_fwscanf(FILE * __restrict__ _File,const wchar_t * __restrict__ _Format,...);
                                                     __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vfwscanf (FILE * __restrict__ fp, const wchar_t * __restrict__ Format,va_list argp);

                                                      __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_fwprintf(FILE * __restrict__ _File,const wchar_t * __restrict__ _Format,...);
                                                      __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_wprintf(const wchar_t * __restrict__ _Format,...);
                                                     __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vfwprintf(FILE * __restrict__ _File,const wchar_t * __restrict__ _Format,va_list _ArgList);
                                                     __attribute__ ((__nonnull__ (1)))
  int __attribute__((__cdecl__)) __mingw_vwprintf(const wchar_t * __restrict__ _Format,va_list _ArgList);
                                                      __attribute__ ((__nonnull__ (3)))
  int __attribute__((__cdecl__)) __mingw_snwprintf (wchar_t * __restrict__ s, size_t n, const wchar_t * __restrict__ format, ...);
                                                      __attribute__ ((__nonnull__ (3)))
  int __attribute__((__cdecl__)) __mingw_vsnwprintf (wchar_t * __restrict__ , size_t, const wchar_t * __restrict__ , va_list);
                                                      __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_swprintf(wchar_t * __restrict__ , const wchar_t * __restrict__ , ...);
                                                      __attribute__ ((__nonnull__ (2)))
  int __attribute__((__cdecl__)) __mingw_vswprintf(wchar_t * __restrict__ , const wchar_t * __restrict__ ,va_list);
# 725 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  int __attribute__((__cdecl__)) fwscanf(FILE * __restrict__ _File,const wchar_t * __restrict__ _Format,...) ;
  int __attribute__((__cdecl__)) swscanf(const wchar_t * __restrict__ _Src,const wchar_t * __restrict__ _Format,...) ;
  int __attribute__((__cdecl__)) wscanf(const wchar_t * __restrict__ _Format,...) ;

  int __attribute__((__cdecl__)) __ms_vwscanf (const wchar_t * __restrict__ , va_list);
  int __attribute__((__cdecl__)) __ms_vfwscanf (FILE * __restrict__ ,const wchar_t * __restrict__ ,va_list);
  int __attribute__((__cdecl__)) __ms_vswscanf (const wchar_t * __restrict__ ,const wchar_t * __restrict__ ,va_list);

  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  __attribute__ ((__nonnull__ (2)))
  int vfwscanf (FILE *__stream, const wchar_t *__format, __builtin_va_list __local_argv)
  {
    return __ms_vfwscanf (__stream, __format, __local_argv);
  }

  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  __attribute__ ((__nonnull__ (2)))
  int vswscanf (const wchar_t * __restrict__ __source, const wchar_t * __restrict__ __format, __builtin_va_list __local_argv)
  {
    return __ms_vswscanf( __source, __format, __local_argv );
  }
  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  __attribute__ ((__nonnull__ (1)))
  int vwscanf(const wchar_t *__format, __builtin_va_list __local_argv)
  {
    return __ms_vwscanf (__format, __local_argv);
  }



  int __attribute__((__cdecl__)) fwprintf(FILE * __restrict__ _File,const wchar_t * __restrict__ _Format,...);
  int __attribute__((__cdecl__)) wprintf(const wchar_t * __restrict__ _Format,...);
  int __attribute__((__cdecl__)) vfwprintf(FILE * __restrict__ _File,const wchar_t * __restrict__ _Format,va_list _ArgList);
  int __attribute__((__cdecl__)) vwprintf(const wchar_t * __restrict__ _Format,va_list _ArgList);
# 768 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _wfsopen(const wchar_t *_Filename,const wchar_t *_Mode,int _ShFlag);


  wint_t __attribute__((__cdecl__)) fgetwc(FILE *_File);
  __attribute__ ((__dllimport__)) wint_t __attribute__((__cdecl__)) _fgetwchar(void);
  wint_t __attribute__((__cdecl__)) fputwc(wchar_t _Ch,FILE *_File);
  __attribute__ ((__dllimport__)) wint_t __attribute__((__cdecl__)) _fputwchar(wchar_t _Ch);
  wint_t __attribute__((__cdecl__)) getwc(FILE *_File);
  wint_t __attribute__((__cdecl__)) getwchar(void);
  wint_t __attribute__((__cdecl__)) putwc(wchar_t _Ch,FILE *_File);
  wint_t __attribute__((__cdecl__)) putwchar(wchar_t _Ch);
  wint_t __attribute__((__cdecl__)) ungetwc(wint_t _Ch,FILE *_File);
  wchar_t *__attribute__((__cdecl__)) fgetws(wchar_t * __restrict__ _Dst,int _SizeInWords,FILE * __restrict__ _File);
  int __attribute__((__cdecl__)) fputws(const wchar_t * __restrict__ _Str,FILE * __restrict__ _File);
  __attribute__ ((__dllimport__)) wchar_t *__attribute__((__cdecl__)) _getws(wchar_t *_String) ;
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _putws(const wchar_t *_Str);

  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _scwprintf(const wchar_t * __restrict__ _Format,...);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _swprintf_c(wchar_t * __restrict__ _DstBuf,size_t _SizeInWords,const wchar_t * __restrict__ _Format,...);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _vswprintf_c(wchar_t * __restrict__ _DstBuf,size_t _SizeInWords,const wchar_t * __restrict__ _Format,va_list _ArgList);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _snwprintf(wchar_t * __restrict__ _Dest,size_t _Count,const wchar_t * __restrict__ _Format,...) ;
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _vsnwprintf(wchar_t * __restrict__ _Dest,size_t _Count,const wchar_t * __restrict__ _Format,va_list _Args) ;




       
       


  int __attribute__((__cdecl__)) __ms_snwprintf (wchar_t * __restrict__ s, size_t n, const wchar_t * __restrict__ format, ...);
  int __attribute__((__cdecl__)) __ms_vsnwprintf (wchar_t * __restrict__ , size_t, const wchar_t * __restrict__ , va_list);
  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  int snwprintf (wchar_t * __restrict__ s, size_t n, const wchar_t * __restrict__ format, ...)
  {
    int r;
    va_list argp;
    __builtin_va_start (argp, format);
    r = _vsnwprintf (s, n, format, argp);
    __builtin_va_end (argp);
    return r;
  }
  static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
  int __attribute__((__cdecl__)) vsnwprintf (wchar_t * __restrict__ s, size_t n, const wchar_t * __restrict__ format, va_list arg)
  {
    return _vsnwprintf(s,n,format,arg);
  }
       
       



  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _swprintf(wchar_t * __restrict__ _Dest,const wchar_t * __restrict__ _Format,...);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _vswprintf(wchar_t * __restrict__ _Dest,const wchar_t * __restrict__ _Format,va_list _Args);


# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/swprintf.inl" 1 3
# 21 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/swprintf.inl" 3
static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
                                                      __attribute__ ((__nonnull__ (3)))
int vswprintf (wchar_t *__stream, size_t __count, const wchar_t *__format, __builtin_va_list __local_argv)
{
  return vsnwprintf( __stream, __count, __format, __local_argv );
}

static __attribute__ ((__unused__)) __inline__ __attribute__((__cdecl__))
                                                      __attribute__ ((__nonnull__ (3)))
int swprintf (wchar_t *__stream, size_t __count, const wchar_t *__format, ...)
{
  register int __retval;
  __builtin_va_list __local_argv;

  __builtin_va_start( __local_argv, __format );
  __retval = vswprintf( __stream, __count, __format, __local_argv );
  __builtin_va_end( __local_argv );
  return __retval;
}
# 825 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 2 3
# 834 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  __attribute__ ((__dllimport__)) wchar_t *__attribute__((__cdecl__)) _wtempnam(const wchar_t *_Directory,const wchar_t *_FilePrefix);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _vscwprintf(const wchar_t * __restrict__ _Format,va_list _ArgList);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _snwscanf(const wchar_t * __restrict__ _Src,size_t _MaxCount,const wchar_t * __restrict__ _Format,...);
  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _wfdopen(int _FileHandle ,const wchar_t *_Mode);
  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _wfopen(const wchar_t * __restrict__ _Filename,const wchar_t *__restrict__ _Mode) ;
  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _wfreopen(const wchar_t * __restrict__ _Filename,const wchar_t * __restrict__ _Mode,FILE * __restrict__ _OldFile) ;



  __attribute__ ((__dllimport__)) void __attribute__((__cdecl__)) _wperror(const wchar_t *_ErrMsg);

  __attribute__ ((__dllimport__)) FILE *__attribute__((__cdecl__)) _wpopen(const wchar_t *_Command,const wchar_t *_Mode);




  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _wremove(const wchar_t *_Filename);
  __attribute__ ((__dllimport__)) wchar_t *__attribute__((__cdecl__)) _wtmpnam(wchar_t *_Buffer);
  __attribute__ ((__dllimport__)) wint_t __attribute__((__cdecl__)) _fgetwc_nolock(FILE *_File);
  __attribute__ ((__dllimport__)) wint_t __attribute__((__cdecl__)) _fputwc_nolock(wchar_t _Ch,FILE *_File);
  __attribute__ ((__dllimport__)) wint_t __attribute__((__cdecl__)) _ungetwc_nolock(wint_t _Ch,FILE *_File);
# 884 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  __attribute__ ((__dllimport__)) void __attribute__((__cdecl__)) _lock_file(FILE *_File);
  __attribute__ ((__dllimport__)) void __attribute__((__cdecl__)) _unlock_file(FILE *_File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fclose_nolock(FILE *_File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fflush_nolock(FILE *_File);
  __attribute__ ((__dllimport__)) size_t __attribute__((__cdecl__)) _fread_nolock(void * __restrict__ _DstBuf,size_t _ElementSize,size_t _Count,FILE * __restrict__ _File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fseek_nolock(FILE *_File,long _Offset,int _Origin);
  __attribute__ ((__dllimport__)) long __attribute__((__cdecl__)) _ftell_nolock(FILE *_File);
  __extension__ __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _fseeki64_nolock(FILE *_File,long long _Offset,int _Origin);
  __extension__ __attribute__ ((__dllimport__)) long long __attribute__((__cdecl__)) _ftelli64_nolock(FILE *_File);
  __attribute__ ((__dllimport__)) size_t __attribute__((__cdecl__)) _fwrite_nolock(const void * __restrict__ _DstBuf,size_t _Size,size_t _Count,FILE * __restrict__ _File);
  __attribute__ ((__dllimport__)) int __attribute__((__cdecl__)) _ungetc_nolock(int _Ch,FILE *_File);





  char *__attribute__((__cdecl__)) tempnam(const char *_Directory,const char *_FilePrefix) ;
  int __attribute__((__cdecl__)) fcloseall(void) ;
  FILE *__attribute__((__cdecl__)) fdopen(int _FileHandle,const char *_Format) ;
  int __attribute__((__cdecl__)) fgetchar(void) ;
  int __attribute__((__cdecl__)) fileno(FILE *_File) ;
  int __attribute__((__cdecl__)) flushall(void) ;
  int __attribute__((__cdecl__)) fputchar(int _Ch) ;
  int __attribute__((__cdecl__)) getw(FILE *_File) ;
  int __attribute__((__cdecl__)) putw(int _Ch,FILE *_File) ;
  int __attribute__((__cdecl__)) rmtmp(void) ;
# 926 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
int __attribute__((__cdecl__)) __mingw_str_wide_utf8 (const wchar_t * const wptr, char **mbptr, size_t * buflen);
# 940 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
int __attribute__((__cdecl__)) __mingw_str_utf8_wide (const char *const mbptr, wchar_t ** wptr, size_t * buflen);
# 949 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
void __attribute__((__cdecl__)) __mingw_str_free(void *ptr);





  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnl(int _Mode,const wchar_t *_Filename,const wchar_t *_ArgList,...);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnle(int _Mode,const wchar_t *_Filename,const wchar_t *_ArgList,...);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnlp(int _Mode,const wchar_t *_Filename,const wchar_t *_ArgList,...);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnlpe(int _Mode,const wchar_t *_Filename,const wchar_t *_ArgList,...);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnv(int _Mode,const wchar_t *_Filename,const wchar_t *const *_ArgList);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnve(int _Mode,const wchar_t *_Filename,const wchar_t *const *_ArgList,const wchar_t *const *_Env);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnvp(int _Mode,const wchar_t *_Filename,const wchar_t *const *_ArgList);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _wspawnvpe(int _Mode,const wchar_t *_Filename,const wchar_t *const *_ArgList,const wchar_t *const *_Env);
# 979 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 3
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _spawnv(int _Mode,const char *_Filename,const char *const *_ArgList);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _spawnve(int _Mode,const char *_Filename,const char *const *_ArgList,const char *const *_Env);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _spawnvp(int _Mode,const char *_Filename,const char *const *_ArgList);
  __attribute__ ((__dllimport__)) intptr_t __attribute__((__cdecl__)) _spawnvpe(int _Mode,const char *_Filename,const char *const *_ArgList,const char *const *_Env);






#pragma pack(pop)

# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/sec_api/stdio_s.h" 1 3
# 9 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/sec_api/stdio_s.h" 3
# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 1 3
# 10 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/sec_api/stdio_s.h" 2 3
# 992 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 2 3

# 1 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/_mingw_print_pop.h" 1 3
# 994 "C:/TDM-GCC-64/x86_64-w64-mingw32/include/stdio.h" 2 3
# 3 "xprec.c" 2


# 4 "xprec.c"
double _Xp_getw(const double *p, int n) {
 return _Xp_getw_help(p,n,0);
}
double _Xp_getw_help(const double *p, int n,double sin_arg)
 {
 printf("calling sin(%17.g) -> _Xp_getw(%.17g/%.17g,%d)\n",sin_arg,p[0],p[1],n);
 if (n == 0)
  return (0.0);
 else if (n == 1 || p[0] == 0.0 || p[1] == 0.0)
  return (p[0]);
 else if (n == 2 || p[2] == 0.0)
  return (p[0] + p[1]);
 else
  {
  double p01 = p[0] + p[1];
  double p2 = p[2];
  if (4 <= n)
   p2 += p[3];
  if (p01 - p[0] == p[1])
   return (p01 + p2);
  else
   return (p[0] + (p[1] + p2));
  }
 }
double *_Xp_setw(double *p, int n, double x)
 {
 double x0 = x;
 short errx, xexp;
 if (n <= 0)
  ;
 else if (n == 1 || (errx = _Dunscale(&xexp, &x0)) == 0)
  p[0] = x0;
 else if (0 < errx)
  {
  p[0] = x0;
  p[1] = 0.0;
  }
 else
  {
  _Dint(&x0, (53 / 2));
  _Dscale(&x0, xexp);
  p[0] = x0;
  p[1] = x - x0;
  if ((53 & 1) != 0 && 2 < n && p[1] != 0.0)
   {
   x = p[1];
   _Dunscale(&xexp, &p[1]);
   _Dint(&p[1], (53 / 2));
   _Dscale(&p[1], xexp);
   p[2] = x - p[1];
   if (3 < n && p[2] != 0.0)
    p[3] = 0.0;
   }
  else if (2 < n)
   p[2] = 0.0;
  }
 return (p);
 }
double *_Xp_addh(double *p, int n, double x0)
 {
 double xscaled = x0;
 short errx, xexp;
 if (n == 0)
  ;
 else if (0 < (errx = _Dunscale(&xexp, &xscaled)))
  if (errx == 2 || (errx = _Dtest(&p[0])) <= 0)
   p[0] = x0;
  else if (errx == 2 || ((*_Pmsw(&(x0))) & ((unsigned short)0x8000)) == ((*_Pmsw(&(p[0]))) & ((unsigned short)0x8000)))
   ;
  else
   {
   _Feraise(0x01);
   p[0] = _Nan._Double;
   if (1 < n)
    p[1] = 0.0;
   }
 else if (errx < 0)
  {
  long prevexp = (2 * 1024);
  int k;
  for (k = 0; k < n; )
   {
   double yscaled = p[k];
   int mybits = (53 / 2);
   short yexp;
   long diff;
   if (0 < (errx = _Dunscale(&yexp, &yscaled)))
    break;
   else if (errx == 0)
    {
    p[k] = x0;
    if (k + 1 < n)
     p[k + 1] = 0.0;
    break;
    }
   else if ((diff = (long)yexp - xexp) <= -mybits
    && x0 != 0.0)
    {
    int j;
    for (j = k; ++j < n && p[j] != 0.0; )
     ;
    if (j < n - 1)
     ++j;
    else if (j == n)
     --j;
    for (; k < j; --j)
     p[j] = p[j - 1];
    p[k] = x0;
    x0 = 0.0;
    }
   else if (mybits <= diff && x0 != 0.0)
    {
    prevexp = yexp;
    ++k;
    }
   else
    {
    if ((p[k] += x0) == 0.0)
     {
     {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0; ) ; p[n - 1] = 0.0;}
     if (p[k] == 0.0)
      break;
     }
    x0 = p[k];
    _Dunscale(&xexp, &x0);
    if (prevexp - mybits < xexp)
     {
     _Dint(&x0, (short)(xexp - (prevexp - mybits)));
     _Dscale(&x0, xexp);
     if ((p[k] -= x0) == 0.0)
      {
      {int m = k; for (; ++m < n && (p[m - 1] = p[m]) != 0.0; ) ; p[n - 1] = 0.0;}
      }
     if (--k == 0)
      prevexp = (2 * 1024);
     else
      {
      xscaled = p[k - 1];
      _Dunscale(&yexp, &xscaled);
      prevexp = yexp;
      }
     }
    else if (k + 1 == n)
     break;
    else
     {
     x0 = p[k];
     _Dunscale(&yexp, &p[k]);
     _Dint(&p[k], (53 / 2));
     _Dscale(&p[k], yexp);
     x0 -= p[k];
     prevexp = yexp;
     xscaled = x0 != 0.0 ? x0 : p[k];
     _Dunscale(&xexp, &xscaled);
     ++k;
     }
    }
   }
  }
 return (p);
 }
double *_Xp_mulh(double *p, int n, double x0)
 {
 short errx;
 int j, k;
 double buf[4];
 if (0 < n)
  {
  buf[0] = p[0] * x0;
  if (0 <= (errx = _Dtest(&buf[0])))
   {
   if (errx == 2)
    _Feraise(0x01);
   p[0] = buf[0];
   if (0 < errx && 1 < n)
    p[1] = 0.0;
   return (p);
   }
  p[0] = 0.0;
  }
 for (j = 1, k = 0; k < n; ++k, --j)
  {
  for (; j < 4; ++j)
   if (k + j < n && p[k + j] != 0.0)
    {
    buf[j] = p[k + j] * x0;
    p[k + j] = 0.0;
    }
   else
    {
    buf[j] = 0.0;
    j = 2 * 4;
    break;
    }
  if (buf[0] == 0.0)
   break;
  else
   {
   int i = 0;
   double y0 = buf[0];
   short xexp;
   _Dunscale(&xexp, &y0);
   _Dint(&y0, (53 / 2));
   _Dscale(&y0, xexp);
   _Xp_addh(p, n, y0);
   _Xp_addh(p, n, buf[0] - y0);
   for (; ++i < j; )
    if ((buf[i - 1] = buf[i]) == 0.0)
     break;
   }
  }
 return (p);
 }
double *_Xp_setn(double *p, int n, long x)
 {
 _Xp_setw(p, n, (double)x);
 return (p);
 }
double *_Xp_movx(double *p, int n, const double *q)
 {
 memcpy_HighTecARMImpl(p, q, n * sizeof (double));
 return (p);
 }
double *_Xp_addx(double *p, int n,
 const double *q, int m)
 {
 int k;
 for (k = 0; k < m && q[k] != 0.0; ++k)
  _Xp_addh(p, n, q[k]);
 return (p);
 }
double *_Xp_subx(double *p, int n,
 const double *q, int m)
 {
 int k;
 for (k = 0; k < m && q[k] != 0.0; ++k)
  _Xp_addh(p, n, -q[k]);
 return (p);
 }
double *_Xp_ldexpx(double *p, int n, int m)
 {
 int k;
 for (k = 0; k < n; ++k)
  {
  p[k] = ldexp(p[k], m);
  if (p[k] == 0.0)
   break;
  }
 return (p);
 }
double *_Xp_mulx(double *p, int n,
 const double *q, int m,
 double *ptemp2)
 {
 if (n == 0 || m == 0)
  ;
 else if (q[0] == 0.0 || q[1] == 0.0)
  _Xp_mulh(p, n, q[0]);
 else
  {
  double *px = ptemp2;
  double *pac = ptemp2 + n;
  int j;
  _Xp_movx(px, n, p);
  _Xp_mulh(p, n, q[0]);
  for (j = 1; j < m && q[j] != 0.0; ++j)
   {
   _Xp_movx(pac, n, px);
   _Xp_mulh(pac, n, q[j]);
   _Xp_addx(p, n, pac, n);
   }
  }
 return (p);
 }
double *_Xp_invx(double *p, int n, double *ptemp4)
 {
 short errx;
 if (n == 0)
  ;
 else if (0 <= (errx = _Dtest(&p[0])))
  {
  if (errx == 1)
   p[0] = 0.0;
  else if (errx == 0)
   p[0] = _Inf._Double;
  }
 else
  {
  double *pac = ptemp4;
  double *py = ptemp4 + n;
  double *ptemp2 = py + n;
  double x0 = p[0];
  int k;
  _Xp_movx(py, n, p);
  _Xp_mulh(py, n, -1.0);
  if (1 < n)
   x0 += p[1];
  _Xp_setw(p, n, (1.0 / (x0)));
  for (k = 1; k < n; k <<= 1)
   {
   _Xp_movx(pac, n, p);
   _Xp_mulx(pac, n, py, n, ptemp2);
   _Xp_addh(pac, n, 1.0);
   _Xp_mulx(pac, n, p, n, ptemp2);
   _Xp_addx(p, n, pac, n);
   }
  }
 return (p);
 }
double *_Xp_sqrtx(double *p, int n, double *ptemp4)
 {
 if (n == 0)
  ;
 else if (0 <= _Dtest(&p[0]) || p[0] < 0.0)
  {
  if (p[0] < 0.0)
   {
   _Feraise(0x01);
   p[0] = _Nan._Double;
   }
  }
 else
  {
  double *pac = ptemp4;
  double *py = ptemp4 + n;
  double *ptemp2 = py + n;
  double x0 = p[0];
  int k;
  if (1 < n)
   x0 += p[1];
  _Xp_setw(py, n, (1.0 / (sqrt(x0))));
  for (k = 2; k < n; k <<= 1)
   {
   _Xp_movx(pac, n, py);
   _Xp_mulh(pac, n, -0.5);
   _Xp_mulx(pac, n, p, n, ptemp2);
   _Xp_mulx(pac, n, py, n, ptemp2);
   _Xp_addh(pac, n, 1.5);
   _Xp_mulx(py, n, pac, n, ptemp2);
   }
  _Xp_mulx(p, n, py, n, ptemp2);
  }
 return (p);
 }
# 10 "oscar.c" 2
# 1 "xdnorm.c" 1


short _Dnorm(_Dval *ps)
 {
 short xchar;
 unsigned short sign = (unsigned short)(ps->_Sh[3] & ((unsigned short)0x8000));

 xchar = 1;
 if ((ps->_Sh[3] &= ((unsigned short)((1 << 4) - 1))) != 0 || ps->_Sh[2]
  || ps->_Sh[1] || ps->_Sh[0])
  {
  for (; ps->_Sh[3] == 0; xchar -= 16)
   {
   ps->_Sh[3] = ps->_Sh[2], ps->_Sh[2] = ps->_Sh[1];
   ps->_Sh[1] = ps->_Sh[0], ps->_Sh[0] = 0;
   }
  for (; ps->_Sh[3] < 1 << 4; --xchar)
   {
   ps->_Sh[3] = (unsigned short)(ps->_Sh[3] << 1
    | ps->_Sh[2] >> 15);
   ps->_Sh[2] = (unsigned short)(ps->_Sh[2] << 1
    | ps->_Sh[1] >> 15);
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1
    | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
  for (; 1 << (4 + 1) <= ps->_Sh[3]; ++xchar)
   {
   ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> 1
    | ps->_Sh[1] << 15);
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] >> 1
    | ps->_Sh[2] << 15);
   ps->_Sh[2] = (unsigned short)(ps->_Sh[2] >> 1
    | ps->_Sh[3] << 15);
   ps->_Sh[3] >>= 1;
   }
  ps->_Sh[3] &= ((unsigned short)((1 << 4) - 1));
  }
 ps->_Sh[3] |= sign;
 return (xchar);
 }
# 11 "oscar.c" 2
# 1 "errno.c" 1

 int _Errno = 0;
# 12 "oscar.c" 2
# 1 "memcpy.c" 1


void *(memcpy_HighTecARMImpl)(void * s1, const void * s2, size_t n)
 {
 char *su1 = (char *)s1;
 const char *su2 = (const char *)s2;
 for (; 0 < n; ++su1, ++su2, --n)
  *su1 = *su2;
 return (s1);
 }
# 13 "oscar.c" 2





static const double s[] = {
 0.00000000015893606014,
 -0.00000002505069049138,
 0.00000275573131527032,
 -0.00019841269827816117,
 0.00833333333331908278,
 -0.16666666666666612594,
 };

double _Sinx(double x, unsigned int qoff, int quads)
 {
 switch (_Dtest(&x))
  {
 case 2:
  return (x);
 case 0:
  if ((qoff & 0x1) != 0)
   x = 1.0;
  return ((qoff & 0x2) != 0 ? -x : x);
 case 1:
  _Feraise(0x01);
  return (_Nan._Double);
 default:
  qoff += _Quad(&x, quads);
  if (-_Rteps._Double < x && x < _Rteps._Double)
   {
   if ((qoff & 0x1) != 0)
    x = 1.0;
   }
  else
   {
   double w = x * x;
   if ((qoff & 0x1) != 0)
    x = 1.0 + w * (((((c[0] * w + c[1]) * w + c[2]) * w + c[3]) * w + c[4]) * w + c[5]);
   else
    x += x * w * (((((s[0] * w + s[1]) * w + s[2]) * w + s[3]) * w + s[4]) * w + s[5]);
   }
  if (qoff & 0x2)
   ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
  return (x);
  }
 }
