__attribute__((__cdecl__)) int printf(const char *, ...);
__attribute__((__cdecl__)) int sscanf(const char *, const char *, ...);

#ifdef MAN
int solver_pragma(int x, ...)
{
return 1;
}
void solver_find(int i)
{
printf("solver_find() encountered! %i\n",i);
}
#endif

typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));




typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));

typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned long long int UDItype;
 typedef UDItype fractype;
 typedef USItype fractype_s;
 typedef DFtype FLO_type;
 typedef SFtype FLO_type_s;
 typedef DItype intfrac;


 typedef SFtype FLO_type_s;

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
      fractype_s lla;
    } fraction;
} fp_number_type_s;

typedef union
{
  FLO_type_s value;
  fractype_s raw_value;
/*
  struct
    {
      fractype_s fraction:23 __attribute__ ((packed));
      unsigned int exp:8 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
*/
}
FLO_union_type_s;

typedef struct
{

  fp_class_type class;
  unsigned int sign;
  int normal_exp;
  union
    {
      fractype lla;
 //     halffractype l[2];
    } fraction;
} fp_number_type;

typedef union
{
  FLO_type value;
  fractype raw_value;
/*
  struct
    {
      fractype fraction:52 __attribute__ ((packed));
      unsigned int exp:11 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
  */
}
FLO_union_type;

fp_number_type*
__unpack_d_drill (FLO_union_type * src, fp_number_type * dst)
{
  fractype fraction;
  int exp;
  int sign;
  fraction = (src->raw_value) & (0x000fffffffffffffULL) ;
  exp = ((src->raw_value) >> 52) & (0x7ffULL);
  sign = (src->raw_value) >> 63 ;

  dst->sign = sign;
  if (exp == 0)
    {
         if (solver_pragma(2) && fraction == 0)
         {
           dst->class = CLASS_ZERO;
         }
      else
         {
           dst->normal_exp = exp - 1023 + 1;
           fraction <<= 8L;

           dst->class = CLASS_NUMBER;

           while (fraction < ((fractype)1<<(52 +8L)))
             {
               fraction <<= 1;
               dst->normal_exp--;
             }

           dst->fraction.lla = fraction;
         }
    }
  else if (solver_pragma(2) && exp == (0x7ff))
    {
	     if (fraction == 0)
	     {
	       dst->class = CLASS_INFINITY;
	     }
	      else
	     {
	       if (fraction & 0x8000000000000LL)

	         {
	           dst->class = CLASS_QNAN;
	         }
	       else
	         {
	           dst->class = CLASS_SNAN;
	         }


	       fraction &= ~0x8000000000000LL;
	       dst->fraction.lla = fraction << 8L;
	     }
    }
  else
    {

      dst->normal_exp = exp - 1023;
      dst->class = CLASS_NUMBER;
      dst->fraction.lla = (fraction << 8L) | ((fractype)1<<(52 +8L));
    }

    return(dst);
}

//extern const fp_number_type __thenan_df;


__inline__
static int
isnan (const fp_number_type *x)
{
  return __builtin_expect (x->class == CLASS_SNAN || x->class == CLASS_QNAN,
      0);
}

__inline__
static int
isinf (const fp_number_type * x)
{
  return __builtin_expect (x->class == CLASS_INFINITY, 0);
}



__inline__
static int
iszero (const fp_number_type * x)
{
  return x->class == CLASS_ZERO;
}

__inline__
static int
isnan_s (const fp_number_type_s *x)
{
  return __builtin_expect (x->class == CLASS_SNAN || x->class == CLASS_QNAN,
      0);
}

__inline__
static int
isinf_s (const fp_number_type_s * x)
{
  return __builtin_expect (x->class == CLASS_INFINITY, 0);
}

__inline__
static int
iszero_s (const fp_number_type_s * x)
{
  return x->class == CLASS_ZERO;
}

__inline__
static void
flip_sign ( fp_number_type * x)
{
  x->sign = !x->sign;
}


fractype_s
__pack_f (const fp_number_type_s *src)
{
  FLO_union_type_s dst;
  fractype_s fraction = src->fraction.lla;
  int sign = src->sign;
  int exp = 0;

  if (solver_pragma (1) && (0 && (isnan_s (src) || isinf_s (src))))
    {
      exp = (0xff);
      fraction = ((fractype) 1 << 23) - 1;
    }
  else if (solver_pragma(2) && isnan_s (src))
    {
      exp = (0xff);

      fraction >>= 7L;
      fraction &= 0x400000L - 1;
      if (src->class == CLASS_QNAN || 1)
 {
   fraction |= 0x400000L;

 }
    }
  else if (solver_pragma(2) && isinf_s (src))
    {
      exp = (0xff);
      fraction = 0;
    }
  else if (solver_pragma(2) && iszero_s (src))
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
      else if (__builtin_expect (src->normal_exp > 127, 0))
 {
   exp = (0xff);
   fraction = 0;
 }
      else
 {
   exp = src->normal_exp + 127;

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

   if (solver_pragma(2) && exp > (0xff))
     {
       exp = (0xff);
       fraction = ((fractype) 1 << 23) - 1;
     }
 }
    }

/*
  dst.bits.fraction = fraction;
  dst.bits.exp = exp;
  dst.bits.sign = sign;
  return dst.value;
*/
  dst.raw_value = fraction | (((fractype_s)exp)<<22) | ((fractype_s)sign<<31);
  return dst.raw_value;
}

fractype_s
__make_fp(fp_class_type class,
      unsigned int sign,
      int exp,
      USItype frac)
{
  fp_number_type_s in;

  in.class = class;
  in.sign = sign;
  in.normal_exp = exp;
  in.fraction.lla = frac;
  return __pack_f (&in);
}

fractype_s
__truncdfsf2 (DFtype arg_a)
{
  fp_number_type in;
  USItype sffrac;
  FLO_union_type au;

  au.value = arg_a;
  fp_number_type* fp1 = __unpack_d_drill (&au, &in);

  sffrac = fp1->fraction.lla >> (52+8-(23+7));

  if ((fp1->fraction.lla & (((USItype) 1 << (52+8-(23+7))) - 1)) != 0)
    sffrac |= 1;

  return __make_fp (fp1->class, fp1->sign, fp1->normal_exp, sffrac);
}
