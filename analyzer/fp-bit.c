#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

# 1 "fp-bit.c"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "fp-bit.c"
# 44 "fp-bit.c"
# 1 "tconfig.h" 1
# 9 "tconfig.h"
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/lib/gcc/x86_64-w64-mingw32/8.1.0/include/stddef.h" 1 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/stddef.h" 1 3 4






# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 1 3 4
# 10 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 1 3 4
# 12 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw_mac.h" 1 3 4
# 13 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw_secapi.h" 1 3 4
# 14 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 282 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/vadefs.h" 1 3 4
# 9 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/vadefs.h" 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 1 3 4
# 578 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/sdks/_mingw_directx.h" 1 3 4
# 579 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 1 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/sdks/_mingw_ddk.h" 1 3 4
# 580 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 10 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/vadefs.h" 2 3 4




#pragma pack(push,_CRT_PACKING)
# 24 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/vadefs.h" 3 4
  
# 24 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/vadefs.h" 3 4
 typedef __builtin_va_list __gnuc_va_list;






  typedef __gnuc_va_list va_list;
# 103 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/vadefs.h" 3 4
#pragma pack(pop)
# 283 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 2 3 4
# 552 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/_mingw.h" 3 4
void __attribute__((__cdecl__)) __debugbreak(void);
extern __inline__ __attribute__((__always_inline__,__gnu_inline__)) void __attribute__((__cdecl__)) __debugbreak(void)
{
  __asm__ __volatile__("int {$}3":);
}




const char *__mingw_get_crt_info (void);
# 11 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 2 3 4




#pragma pack(push,_CRT_PACKING)
# 37 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef unsigned int size_t;
# 47 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef int ssize_t;




typedef size_t rsize_t;
# 64 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef int intptr_t;
# 77 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef unsigned int uintptr_t;
# 90 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef int ptrdiff_t;







typedef unsigned short wchar_t;







typedef unsigned short wint_t;
typedef unsigned short wctype_t;





typedef int errno_t;




typedef long __time32_t;




__extension__ typedef long long __time64_t;
# 136 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
typedef __time32_t time_t;
# 422 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/crtdefs.h" 3 4
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
# 8 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/stddef.h" 2 3 4
# 18 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/x86_64-w64-mingw32/include/stddef.h" 3 4
  __attribute__ ((__dllimport__)) extern int *__attribute__((__cdecl__)) _errno(void);

  errno_t __attribute__((__cdecl__)) _set_errno(int _Value);
  errno_t __attribute__((__cdecl__)) _get_errno(int *_Value);


  __attribute__ ((__dllimport__)) extern unsigned long __attribute__((__cdecl__)) __threadid(void);

  __attribute__ ((__dllimport__)) extern uintptr_t __attribute__((__cdecl__)) __threadhandle(void);
# 2 "C:/Program Files/mingw-w64/x86_64-8.1.0-posix-seh-rt_v6-rev0/mingw64/lib/gcc/x86_64-w64-mingw32/8.1.0/include/stddef.h" 2 3 4
# 9 "tconfig.h" 2
# 45 "fp-bit.c" 2
# 1 "coretypes.h" 1
# 46 "fp-bit.c" 2
# 1 "tm.h" 1
# 1 "falcon.h" 1
# 2 "tm.h" 2
# 1 "defaults.h" 1
# 2 "tm.h" 2
# 47 "fp-bit.c" 2
# 1 "fp-bit.h" 1
# 97 "fp-bit.h"

# 97 "fp-bit.h"
typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));




typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));







typedef int CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));


typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
# 196 "fp-bit.h"
 typedef USItype fractype;
 typedef UHItype halffractype;
 typedef SFtype FLO_type;
 typedef SItype intfrac;
# 347 "fp-bit.h"
typedef enum
{
  CLASS_SNAN,
  CLASS_QNAN,
  CLASS_ZERO,
  CLASS_NUMBER,
  CLASS_INFINITY
} fp_class_type;

typedef struct
{





  fp_class_type class;
  unsigned int sign;
  int normal_exp;


  union
    {
      fractype ll;
      halffractype l[2];
    } fraction;
} fp_number_type;

typedef union
{
  FLO_type value;
  fractype value_raw;
# 389 "fp-bit.h"
  struct
    {
      fractype fraction:23 __attribute__ ((packed));
      unsigned int exp:8 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
# 415 "fp-bit.h"
}
FLO_union_type;




extern FLO_type __pack_f (fp_number_type *);


extern void __unpack_f (FLO_union_type *, fp_number_type *);


extern FLO_type __addsf3 (FLO_type, FLO_type);
extern FLO_type __subsf3 (FLO_type, FLO_type);



extern FLO_type __mulsf3 (FLO_type, FLO_type);



extern FLO_type __divsf3 (FLO_type, FLO_type);


extern int __fpcmp_parts_f (fp_number_type *, fp_number_type *);


extern CMPtype __cmpsf2 (FLO_type, FLO_type);





extern CMPtype __eqsf2 (FLO_type, FLO_type);



extern CMPtype __nesf2 (FLO_type, FLO_type);



extern CMPtype __gtsf2 (FLO_type, FLO_type);



extern CMPtype __gesf2 (FLO_type, FLO_type);



extern CMPtype __ltsf2 (FLO_type, FLO_type);



extern CMPtype __lesf2 (FLO_type, FLO_type);



extern CMPtype __unordsf2 (FLO_type, FLO_type);





extern FLO_type __floatsisf (SItype);



extern SItype __fixsfsi (FLO_type);
# 492 "fp-bit.h"
extern FLO_type __floatunsisf (USItype);



extern FLO_type __negsf2 (FLO_type);




extern SFtype __make_fp (fp_class_type, unsigned int, int, USItype);


extern DFtype __make_dp (fp_class_type, unsigned int, int, UDItype);

extern DFtype __extendsfdf2 (SFtype);
# 48 "fp-bit.c" 2
# 130 "fp-bit.c"
const fp_number_type __thenan_sf = { CLASS_SNAN, 0, 0, {(fractype) 0} };
# 143 "fp-bit.c"
__inline__
static fp_number_type *
nan (void)
{




  return (fp_number_type *) (& __thenan_sf);



}

__inline__
static int
isnan ( fp_number_type * x)
{
  return __builtin_expect (x->class == CLASS_SNAN || x->class == CLASS_QNAN,
      0);
}

__inline__
static int
isinf ( fp_number_type * x)
{
  return __builtin_expect (x->class == CLASS_INFINITY, 0);
}



__inline__
static int
iszero ( fp_number_type * x)
{
  return x->class == CLASS_ZERO;
}

__inline__
static void
flip_sign ( fp_number_type * x)
{
  x->sign = !x->sign;
}


__inline__
static int
clzusi (USItype n)
{
  extern int __clzsi2 (USItype);
  if (sizeof (USItype) == sizeof (unsigned int))
    return __builtin_clz (n);
  else if (sizeof (USItype) == sizeof (unsigned long))
    return __builtin_clzl (n);
  else if (sizeof (USItype) == sizeof (unsigned long long))
    return __builtin_clzll (n);
  else
    return __clzsi2 (n);
}

extern FLO_type __pack_f ( fp_number_type * );


FLO_type
__pack_f ( fp_number_type * src)
{
  FLO_union_type dst;
  fractype fraction = src->fraction.ll;
  int sign = src->sign;
  int exp = 0;

  if (0 && (isnan (src) || isinf (src)))
    {



      exp = (0xff);
      fraction = ((fractype) 1 << 23) - 1;
    }
  else if (isnan (src))
    {
      exp = (0xff);
      if (src->class == CLASS_QNAN || 1)
 {



   fraction |= 0x100000L;

 }
    }
  else if (isinf (src))
    {
      exp = (0xff);
      fraction = 0;
    }
  else if (iszero (src))
    {
      exp = 0;
      fraction = 0;
    }
  else if (fraction == 0)
    {
      exp = 0;
    }
  else
    {
      if (__builtin_expect (src->normal_exp < (-(127)+1), 0))
 {
# 264 "fp-bit.c"
   int shift = (-(127)+1) - src->normal_exp;

   exp = 0;

   if (shift > 32 - 7L)
     {

       fraction = 0;
     }
   else
     {
       int lowbit = (fraction & (((fractype)1 << shift) - 1)) ? 1 : 0;
       fraction = (fraction >> shift) | lowbit;
     }
   if ((fraction & 0x7f) == 0x40)
     {
       if ((fraction & (1 << 7L)))
  fraction += 0x3f + 1;
     }
   else
     {

       fraction += 0x3f;
     }


   if (fraction >= ((fractype)1<<(23 +7L)))
     {
       exp += 1;
     }
   fraction >>= 7L;

 }
      else if (!0
        && __builtin_expect (src->normal_exp > 127, 0))
 {
   exp = (0xff);
   fraction = 0;
 }
      else
 {
   exp = src->normal_exp + 127;
   if (!0)
     {



       if ((fraction & 0x7f) == 0x40)
  {
    if (fraction & (1 << 7L))
      fraction += 0x3f + 1;
  }
       else
  {

    fraction += 0x3f;
  }
       if (fraction >= ((fractype)1<<(23 +1+7L)))
  {
    fraction >>= 1;
    exp += 1;
  }
     }
   fraction >>= 7L;

   if (0 && exp > (0xff))
     {

       exp = (0xff);
       fraction = ((fractype) 1 << 23) - 1;
     }
 }
    }





  dst.bits.fraction = fraction;
  dst.bits.exp = exp;
  dst.bits.sign = sign;
# 445 "fp-bit.c"
  return dst.value;
}



void
__unpack_f (FLO_union_type * src, fp_number_type * dst)
{



  fractype fraction;
  int exp;
  int sign;
# 476 "fp-bit.c"
  fraction = src->bits.fraction;
  exp = src->bits.exp;
  sign = src->bits.sign;
# 530 "fp-bit.c"
  dst->sign = sign;
  if (exp == 0)
    {

      if (fraction == 0



   )
 {

   dst->class = CLASS_ZERO;
 }
      else
 {



   dst->normal_exp = exp - 127 + 1;
   fraction <<= 7L;

   dst->class = CLASS_NUMBER;

   while (fraction < ((fractype)1<<(23 +7L)))
     {
       fraction <<= 1;
       dst->normal_exp--;
     }

   dst->fraction.ll = fraction;
 }
    }
  else if (!0
    && __builtin_expect (exp == (0xff), 0))
    {

      if (fraction == 0)
 {

   dst->class = CLASS_INFINITY;
 }
      else
 {




   if (fraction & 0x100000L)

     {
       dst->class = CLASS_QNAN;
     }
   else
     {
       dst->class = CLASS_SNAN;
     }

   dst->fraction.ll = fraction;
 }
    }
  else
    {

      dst->normal_exp = exp - 127;
      dst->class = CLASS_NUMBER;
      dst->fraction.ll = (fraction << 7L) | ((fractype)1<<(23 +7L));
    }
}



static fp_number_type *
_fpadd_parts (fp_number_type * a,
       fp_number_type * b,
       fp_number_type * tmp)
{
  intfrac tfraction;


  int a_normal_exp;
  int b_normal_exp;
  fractype a_fraction;
  fractype b_fraction;

  if (isnan (a))
    {
      return a;
    }
  if (isnan (b))
    {
      return b;
    }
  if (isinf (a))
    {

      if (isinf (b) && a->sign != b->sign)
 return nan ();
      return a;
    }
  if (isinf (b))
    {
      return b;
    }
  if (iszero (b))
    {
      if (iszero (a))
 {
   *tmp = *a;
   tmp->sign = a->sign & b->sign;
   return tmp;
 }
      return a;
    }
  if (iszero (a))
    {
      return b;
    }



  {
    int diff;
    int sdiff;

    a_normal_exp = a->normal_exp;
    b_normal_exp = b->normal_exp;
    a_fraction = a->fraction.ll;
    b_fraction = b->fraction.ll;

    diff = a_normal_exp - b_normal_exp;
    sdiff = diff;

    if (diff < 0)
      diff = -diff;
    if (diff < 32)
      {
 if (sdiff > 0)
   {
     b_normal_exp += diff;
     { b_fraction = (b_fraction >> diff) | !!(b_fraction & (((fractype) 1 << diff) - 1)); };
   }
 else if (sdiff < 0)
   {
     a_normal_exp += diff;
     { a_fraction = (a_fraction >> diff) | !!(a_fraction & (((fractype) 1 << diff) - 1)); };
   }
      }
    else
      {

 if (a_normal_exp > b_normal_exp)
   {
     b_normal_exp = a_normal_exp;
     b_fraction = 0;
   }
 else
   {
     a_normal_exp = b_normal_exp;
     a_fraction = 0;
   }
      }
  }

  if (a->sign != b->sign)
    {
      if (a->sign)
 {
   tfraction = -a_fraction + b_fraction;
 }
      else
 {
   tfraction = a_fraction - b_fraction;
 }
      if (tfraction >= 0)
 {
   tmp->sign = 0;
   tmp->normal_exp = a_normal_exp;
   tmp->fraction.ll = tfraction;
 }
      else
 {
   tmp->sign = 1;
   tmp->normal_exp = a_normal_exp;
   tmp->fraction.ll = -tfraction;
 }


      while (tmp->fraction.ll < ((fractype)1<<(23 +7L)) && tmp->fraction.ll)
 {
   tmp->fraction.ll <<= 1;
   tmp->normal_exp--;
 }
    }
  else
    {
      tmp->sign = a->sign;
      tmp->normal_exp = a_normal_exp;
      tmp->fraction.ll = a_fraction + b_fraction;
    }
  tmp->class = CLASS_NUMBER;



  if (tmp->fraction.ll >= ((fractype)1<<(23 +1+7L)))
    {
      { tmp->fraction.ll = (tmp->fraction.ll >> 1) | !!(tmp->fraction.ll & (((fractype) 1 << 1) - 1)); };
      tmp->normal_exp++;
    }
  return tmp;

}

FLO_type
__addsf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  res = _fpadd_parts (&a, &b, &tmp);

  return __pack_f (res);
}

FLO_type
__subsf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  b.sign ^= 1;

  res = _fpadd_parts (&a, &b, &tmp);

  return __pack_f (res);
}



static inline __attribute__ ((__always_inline__)) fp_number_type *
_fpmul_parts ( fp_number_type * a,
        fp_number_type * b,
        fp_number_type * tmp)
{
  fractype low = 0;
  fractype high = 0;

  if (isnan (a))
    {
      a->sign = a->sign != b->sign;
      return a;
    }
  if (isnan (b))
    {
      b->sign = a->sign != b->sign;
      return b;
    }
  if (isinf (a))
    {
      if (iszero (b))
 return nan ();
      a->sign = a->sign != b->sign;
      return a;
    }
  if (isinf (b))
    {
      if (iszero (a))
 {
   return nan ();
 }
      b->sign = a->sign != b->sign;
      return b;
    }
  if (iszero (a))
    {
      a->sign = a->sign != b->sign;
      return a;
    }
  if (iszero (b))
    {
      b->sign = a->sign != b->sign;
      return b;
    }



  {
# 862 "fp-bit.c"
    {
      UDItype answer = (UDItype)a->fraction.ll * (UDItype)b->fraction.ll;

      high = answer >> (4 * 8);
      low = answer;
    }
# 895 "fp-bit.c"
  }

  tmp->normal_exp = a->normal_exp + b->normal_exp
    + 32 - (23 + 7L);
  tmp->sign = a->sign != b->sign;
  while (high >= ((fractype)1<<(23 +1+7L)))
    {
      tmp->normal_exp++;
      if (high & 1)
 {
   low >>= 1;
   low |= 0x80000000L;
 }
      high >>= 1;
    }
  while (high < ((fractype)1<<(23 +7L)))
    {
      tmp->normal_exp--;

      high <<= 1;
      if (low & 0x80000000L)
 high |= 1;
      low <<= 1;
    }

  if (!0 && (high & 0x7f) == 0x40)
    {
      if (high & (1 << 7L))
 {






 }
      else if (low)
 {




   high += 0x3f + 1;


   high &= ~(fractype) 0x7f;
 }
    }
  tmp->fraction.ll = high;
  tmp->class = CLASS_NUMBER;
  return tmp;
}

FLO_type
__mulsf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  res = _fpmul_parts (&a, &b, &tmp);

  return __pack_f (res);
}



static inline __attribute__ ((__always_inline__)) fp_number_type *
_fpdiv_parts (fp_number_type * a,
       fp_number_type * b)
{
  fractype bit;
  fractype numerator;
  fractype denominator;
  fractype quotient;

  if (isnan (a))
    {
      return a;
    }
  if (isnan (b))
    {
      return b;
    }

  a->sign = a->sign ^ b->sign;

  if (isinf (a) || iszero (a))
    {
      if (a->class == b->class)
 return nan ();
      return a;
    }

  if (isinf (b))
    {
      a->fraction.ll = 0;
      a->normal_exp = 0;
      return a;
    }
  if (iszero (b))
    {
      a->class = CLASS_INFINITY;
      return a;
    }



  {




    a->normal_exp = a->normal_exp - b->normal_exp;
    numerator = a->fraction.ll;
    denominator = b->fraction.ll;

    if (numerator < denominator)
      {

 numerator *= 2;
 a->normal_exp--;
      }
    bit = ((fractype)1<<(23 +7L));
    quotient = 0;

    while (bit)
      {
 if (numerator >= denominator)
   {
     quotient |= bit;
     numerator -= denominator;
   }
 bit >>= 1;
 numerator *= 2;
      }

    if (!0 && (quotient & 0x7f) == 0x40)
      {
 if (quotient & (1 << 7L))
   {




   }
 else if (numerator)
   {


     quotient += 0x3f + 1;


     quotient &= ~(fractype) 0x7f;
   }
      }

    a->fraction.ll = quotient;
    return (a);
  }
}

FLO_type
__divsf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  res = _fpdiv_parts (&a, &b);

  return __pack_f (res);
}
# 1095 "fp-bit.c"
int
__fpcmp_parts_f (fp_number_type * a, fp_number_type * b)
{
# 1106 "fp-bit.c"
  if (isnan (a) || isnan (b))
    {
      return 1;
    }
  if (isinf (a) && isinf (b))
    {
# 1121 "fp-bit.c"
      return b->sign - a->sign;
    }

  if (isinf (a))
    {
      return a->sign ? -1 : 1;
    }
  if (isinf (b))
    {
      return b->sign ? 1 : -1;
    }
  if (iszero (a) && iszero (b))
    {
      return 0;
    }
  if (iszero (a))
    {
      return b->sign ? 1 : -1;
    }
  if (iszero (b))
    {
      return a->sign ? -1 : 1;
    }

  if (a->sign != b->sign)
    {

      return a->sign ? -1 : 1;
    }

  if (a->normal_exp > b->normal_exp)
    {
      return a->sign ? -1 : 1;
    }
  if (a->normal_exp < b->normal_exp)
    {
      return a->sign ? 1 : -1;
    }

  if (a->fraction.ll > b->fraction.ll)
    {
      return a->sign ? -1 : 1;
    }
  if (a->fraction.ll < b->fraction.ll)
    {
      return a->sign ? 1 : -1;
    }

  return 0;
}



CMPtype
__cmpsf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  return __fpcmp_parts_f (&a, &b);
}







CMPtype
__eqsf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;

  return __fpcmp_parts_f (&a, &b) ;
}



CMPtype
__nesf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;

  return __fpcmp_parts_f (&a, &b) ;
}



CMPtype
__gtsf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return -1;

  return __fpcmp_parts_f (&a, &b);
}



CMPtype
__gesf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return -1;
  return __fpcmp_parts_f (&a, &b) ;
}



CMPtype
__ltsf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;

  return __fpcmp_parts_f (&a, &b);
}



CMPtype
__lesf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;

  return __fpcmp_parts_f (&a, &b) ;
}





CMPtype
__unordsf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  return (isnan (&a) || isnan (&b));
}



FLO_type
__floatsisf (SItype arg_a)
{
  fp_number_type in;

  in.class = CLASS_NUMBER;
  in.sign = arg_a < 0;
  if (!arg_a)
    {
      in.class = CLASS_ZERO;
    }
  else
    {
      USItype uarg;
      int shift;
      in.normal_exp = 23 + 7L;
      if (in.sign)
 {


   if (arg_a == (- ((SItype) ((~(USItype)0) >> 1)) - 1))
     {
       return (FLO_type)(- ((SItype) ((~(USItype)0) >> 1)) - 1);
     }
   uarg = (-arg_a);
 }
      else
 uarg = arg_a;

      in.fraction.ll = uarg;
      shift = clzusi (uarg) - ((4 * 8) - 1 - 23 - 7L);
      if (shift > 0)
 {
   in.fraction.ll <<= shift;
   in.normal_exp -= shift;
 }
    }
  return __pack_f (&in);
}



FLO_type
__floatunsisf (USItype arg_a)
{
  fp_number_type in;

  in.sign = 0;
  if (!arg_a)
    {
      in.class = CLASS_ZERO;
    }
  else
    {
      int shift;
      in.class = CLASS_NUMBER;
      in.normal_exp = 23 + 7L;
      in.fraction.ll = arg_a;

      shift = clzusi (arg_a) - ((4 * 8) - 1 - 23 - 7L);
      if (shift < 0)
 {
   fractype guard = in.fraction.ll & (((fractype)1 << -shift) - 1);
   in.fraction.ll >>= -shift;
   in.fraction.ll |= (guard != 0);
   in.normal_exp -= shift;
 }
      else if (shift > 0)
 {
   in.fraction.ll <<= shift;
   in.normal_exp -= shift;
 }
    }
  return __pack_f (&in);
}



SItype
__fixsfsi (FLO_type arg_a)
{
  fp_number_type a;
  SItype tmp;
  FLO_union_type au;

  au.value = arg_a;
  __unpack_f (&au, &a);

  if (iszero (&a))
    return 0;
  if (isnan (&a))
    return 0;

  if (isinf (&a))
    return a.sign ? (-((SItype) ((~(USItype)0) >> 1)))-1 : ((SItype) ((~(USItype)0) >> 1));

  if (a.normal_exp < 0)
    return 0;
  if (a.normal_exp > (4 * 8) - 2)
    return a.sign ? (-((SItype) ((~(USItype)0) >> 1)))-1 : ((SItype) ((~(USItype)0) >> 1));
  tmp = a.fraction.ll >> ((23 + 7L) - a.normal_exp);
  return a.sign ? (-tmp) : (tmp);
}
# 1487 "fp-bit.c"
FLO_type
__negsf2 (FLO_type arg_a)
{
  fp_number_type a;
  FLO_union_type au;

  au.value = arg_a;
  __unpack_f (&au, &a);

  flip_sign (&a);
  return __pack_f (&a);
}





SFtype
__make_fp(fp_class_type class,
      unsigned int sign,
      int exp,
      USItype frac)
{
  fp_number_type in;

  in.class = class;
  in.sign = sign;
  in.normal_exp = exp;
  in.fraction.ll = frac;
  return __pack_f (&in);
}
# 1528 "fp-bit.c"
DFtype
__extendsfdf2 (SFtype arg_a)
{
  fp_number_type in;
  FLO_union_type au;

  au.value = arg_a;
  __unpack_f (&au, &in);

  return __make_dp (in.class, in.sign, in.normal_exp,
      ((UDItype) in.fraction.ll) << (52+8-(23+7)));
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int x = atoi(argv[1]);
    int y = atoi(argv[2]);
    printf("f(x=%i,z=%i) =\n%i\n",x,y,_fpdiv_parts(x,y));
    return 0;
}
#endif
