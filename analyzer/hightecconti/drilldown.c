




#ifdef MAN
__attribute__((__cdecl__)) int printf(const char *, ...);
__attribute__((__cdecl__)) int sscanf(const char *, const char *, ...);
union { float float_val; unsigned long int uint_val; } float_conv;
union { double double_val; unsigned long long int ulong_val; } double_conv;
float u2f(unsigned long u)
{
float_conv.uint_val = u;
return float_conv.float_val;
}
double u2d(unsigned long long u)
{
double_conv.ulong_val = u;
return double_conv.double_val;
}
unsigned long f2u(float f)
{
float_conv.float_val = f;
return float_conv.uint_val;
}
unsigned long long d2u(double f)
{
double_conv.double_val = f;
return double_conv.ulong_val;
}
int solver_pragma(int x, ...)
{
return 1;
}
void solver_debug_Float(char *s, float x)
{
printf("DEBUG_VAL Float %s = %g = 0x%lx\n", s, x, f2u(x));
}
void solver_debug_Double(char *s, double x)
{
printf("DEBUG_VAL Double %s = %g = 0x%llx\n", s, x, d2u(x));
}
void solver_debug_UByte(char *s, unsigned char x)
{
printf("DEBUG_VAL UByte %s = %hhi = 0x%hhx\n", s, x, x);
}
void solver_debug_Short(char *s, short x)
{
printf("DEBUG_VAL Short %s = %hi = 0x%hx\n", s, x, x);
}
void solver_debug_UShort(char *s, unsigned short x)
{
printf("DEBUG_VAL UShort %s = %hu = 0x%hx\n", s, x, x);
}
void solver_debug_UInt(char *s, unsigned int x)
{
printf("DEBUG_VAL UInt %s = %u = 0x%x\n", s, x, x);
}
void solver_debug_Int(char *s, int x)
{
printf("DEBUG_VAL_Int %s = %i = 0x%lx\n", s, x, x);
}
void solver_debug_ULong(char *s, unsigned long x)
{
printf("DEBUG_VAL ULong %s = %lu = 0x%lx\n", s, x, x);
}
void solver_debug_Long(char *s, long x)
{
printf("DEBUG_VAL Long %s = %li = 0x%lx\n", s, x, x);
}
void solver_debug_ULongLong(char *s, unsigned long long x)
{
printf("DEBUG_VAL ULongLong %s = %llu = 0x%llx\n", s, x, x);
}
void solver_debug_LongLong(char *s, long long x)
{
printf("DEBUG_VAL LongLong %s = %lli = 0x%llx\n", s, x, x);
}

void solver_find(int i)
{
printf("solver_find() encountered! %i\n",i);
}
#endif





typedef float SFtype __attribute__ ((mode (SF)));
//typedef float DFtype __attribute__ ((mode (DF)));
typedef double DFtype;




typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));







typedef int CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));


typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
//typedef unsigned long int UDItype __attribute__ ((mode (DI)));
typedef unsigned long long int UDItype;
 typedef UDItype fractype;
 typedef USItype halffractype;
 typedef DFtype FLO_type;
 typedef DItype intfrac;
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
      fractype lla;
    } fraction;
} fp_number_type;

typedef union
{
  FLO_type value;
  unsigned long long int raw_value;

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


extern int __fpcmp_parts_d (fp_number_type *, fp_number_type *);
extern const fp_number_type __thenan_df;


__inline__
static const fp_number_type *
makenan (void)
{





  return & __thenan_df;

}

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

static __inline__ __attribute__ ((__always_inline__)) const fp_number_type *
_fpmul_parts ( fp_number_type * a,
        fp_number_type * b,
        fp_number_type * tmp)
{
  fractype low = 0;
  fractype high = 0;

  if (solver_pragma(2) && isnan (a))
    {
      a->sign = a->sign != b->sign;
      return a;
    }
  if (solver_pragma(2) && isnan (b))
    {
      b->sign = a->sign != b->sign;
      return b;
    }
  if (solver_pragma(2) && isinf (a))
    {
      if (iszero (b))
 return makenan ();
      a->sign = a->sign != b->sign;
      return a;
    }
  if (solver_pragma(2) && isinf (b))
    {
      if (iszero (a))
 {
   return makenan ();
 }
      b->sign = a->sign != b->sign;
      return b;
    }
  if (solver_pragma(2) && iszero (a))
    {
      a->sign = a->sign != b->sign;
      return a;
    }
  if (solver_pragma(2) && iszero (b))
    {
      b->sign = a->sign != b->sign;
      return b;
    }



  {
    {
      USItype nl = a->fraction.lla;
      USItype nh = a->fraction.lla >> (4 * (8));
      USItype ml = b->fraction.lla;
      USItype mh = b->fraction.lla >> (4 * (8));
      UDItype pp_ll = (UDItype) ml * nl;
      UDItype pp_hl = (UDItype) mh * nl;
      UDItype pp_lh = (UDItype) ml * nh;
      UDItype pp_hh = (UDItype) mh * nh;
      UDItype res2 = 0;
      UDItype res0 = 0;
      UDItype ps_hh__ = pp_hl + pp_lh;
      if (ps_hh__ < pp_hl)
         {
            res2 += (UDItype)1 << (4 * (8));
            solver_find(1);
         }
      pp_hl = (UDItype)(USItype)ps_hh__ << (4 * (8));
      res0 = pp_ll + pp_hl;
      if (res0 < pp_ll)
        {
         res2++;
        }
      res2 += (ps_hh__ >> (4 * (8))) + pp_hh;
      high = res2;
      low = res0;
    }

  }

  tmp->normal_exp = a->normal_exp + b->normal_exp
    + 64 - (52 + 8L);
  tmp->sign = a->sign != b->sign;
  while (high >= ((fractype)1<<(52 +1+8L)))
    {
      tmp->normal_exp++;
      if (high & 1)
         {
           solver_find(2);
           low >>= 1;
           low |= 0x8000000000000000LL;
         }
         else
         {
            solver_find(3);
		}
      high >>= 1;
    }
  while (high < ((fractype)1<<(52 +8L)))
    {
      tmp->normal_exp--;

      high <<= 1;
      if (low & 0x8000000000000000LL)
 high |= 1;
      low <<= 1;
    }

  if (!0 && (high & 0xff) == 0x80)
    {
      if((high & 0xff) == 0x80) solver_find(4);

      if (high & (1 << 8L))
 {






 }
      else if (low)
 {




   high += 0x7f + 1;


   high &= ~(fractype) 0xff;
 }
    }
  tmp->fraction.lla = high;
  tmp->class = CLASS_NUMBER;
  return tmp;
}

const fp_number_type __thenan_df = { CLASS_SNAN, 0, 0,
	{
		(fractype) 0
	}
};

/*
void
__unpack_d (FLO_union_type * src, fp_number_type * dst)
{

  fractype fraction;
  int exp;
  int sign;
  fraction = src->bits.fraction;
  exp = src->bits.exp;
  sign = src->bits.sign;

printf("__unpack_d: raw=%llx, fraction=%llx, exp=%llx, sign=%llx\n",src->raw_value,fraction,exp,sign);

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
  else if (!0
    && __builtin_expect (exp == (0x7ff), 0))
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
}
*/

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
  if (solver_pragma(2) && exp == 0)
    {
         if (fraction == 0)
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


/*
FLO_type
__pack_d (const fp_number_type *src)
{
  FLO_union_type dst;
  fractype fraction = src->fraction.lla;
  int sign = src->sign;
  int exp = 0;

  if (0 && (isnan (src) || isinf (src)))
    {



      exp = (0x7ff);
      fraction = ((fractype) 1 << 52) - 1;
    }
  else if (isnan (src))
    {
      exp = (0x7ff);

      fraction >>= 8L;
      fraction &= 0x8000000000000LL - 1;
      if (src->class == CLASS_QNAN || 1)
 {







   fraction |= 0x8000000000000LL;

 }
    }
  else if (isinf (src))
    {
      exp = (0x7ff);
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
      if (__builtin_expect (src->normal_exp < (-(1023)+1), 0))
 {
   int shift = (-(1023)+1) - src->normal_exp;

   exp = 0;

   if (shift > 64 - 8L)
     {

       fraction = 0;
     }
   else
     {
       int lowbit = (fraction & (((fractype)1 << shift) - 1)) ? 1 : 0;
       fraction = (fraction >> shift) | lowbit;
     }
   if ((fraction & 0xff) == 0x80)
     {
       if ((fraction & (1 << 8L)))
  fraction += 0x7f + 1;
     }
   else
     {

       fraction += 0x7f;
     }


   if (fraction >= ((fractype)1<<(52 +8L)))
     {
       exp += 1;
     }
   fraction >>= 8L;

 }
      else if (!0
        && __builtin_expect (src->normal_exp > 1023, 0))
 {
   exp = (0x7ff);
   fraction = 0;
 }
      else
 {
   exp = src->normal_exp + 1023;
   if (!0)
     {



       if ((fraction & 0xff) == 0x80)
  {
    if (fraction & (1 << 8L))
      fraction += 0x7f + 1;
  }
       else
  {

    fraction += 0x7f;
  }
       if (fraction >= ((fractype)1<<(52 +1+8L)))
  {
    fraction >>= 1;
    exp += 1;
  }
     }
   fraction >>= 8L;

   if (0 && exp > (0x7ff))
     {

       exp = (0x7ff);
       fraction = ((fractype) 1 << 52) - 1;
     }
 }
    }





  dst.bits.fraction = fraction;
  dst.bits.exp = exp;
  dst.bits.sign = sign;
  return dst.value;
}
*/

FLO_type
__pack_d_drill (const fp_number_type *src)
{
  FLO_union_type dst;
  fractype fraction = src->fraction.lla;
  int sign = src->sign;
  int exp = 0;

  if (0 && (isnan (src) || isinf (src)))
    {



      exp = (0x7ff);
      fraction = ((fractype) 1 << 52) - 1;
    }
  else if (isnan (src))
    {
      exp = (0x7ff);

      fraction >>= 8L;
      fraction &= 0x8000000000000LL - 1;
      if (src->class == CLASS_QNAN || 1)
 {







   fraction |= 0x8000000000000LL;

 }
    }
  else if (isinf (src))
    {
      exp = (0x7ff);
      fraction = 0;
    }
  else if (iszero (src))
    {
      exp = 0;
      fraction = 0;
    }
  else if (fraction == 0)
    {
        solver_find(6);
      exp = 0;
    }
  else
    {
      if (__builtin_expect (src->normal_exp < (-(1023)+1), 0))
 {
   int shift = (-(1023)+1) - src->normal_exp;

   exp = 0;

   if (shift > 64 - 8L)
     {

       fraction = 0;
     }
   else
     {
       int lowbit = (fraction & (((fractype)1 << shift) - 1)) ? 1 : 0;
       fraction = (fraction >> shift) | lowbit;
     }
   if ((fraction & 0xff) == 0x80)
     {
       if ((fraction & (1 << 8L)))
       {
            solver_find(7);
            fraction += 0x7f + 1;
        }
        else
        {
            solver_find(8);
        }
     }
   else
     {

       fraction += 0x7f;
     }


   if (fraction >= ((fractype)1<<(52 +8L)))
     {
        solver_find(9);
       exp += 1;
     }
   fraction >>= 8L;

 }
      else if (!0
        && __builtin_expect (src->normal_exp > 1023, 0))
 {
   exp = (0x7ff);
   fraction = 0;
 }
      else
 {
   exp = src->normal_exp + 1023;
   if (!0)
     {



       if ((fraction & 0xff) == 0x80)
      {
        if (fraction & (1 << 8L))
        {
            solver_find(10);
          fraction += 0x7f + 1;
        }
        else
        {
            solver_find(11);
        }
      }
       else
      {

        fraction += 0x7f;
      }
       if (fraction >= ((fractype)1<<(52 +1+8L)))
  {
    solver_find(12);
    fraction >>= 1;
    exp += 1;
  }
     }
   fraction >>= 8L;

   if (0 && exp > (0x7ff))
     {

       exp = (0x7ff);
       fraction = ((fractype) 1 << 52) - 1;
     }
 }
    }





//  dst.bits.fraction = fraction;
//  dst.bits.exp = exp;
//  dst.bits.sign = sign;
  dst.raw_value = fraction | (((fractype)exp)<<52) | ((fractype)sign<<63);
  return dst.value;
}

/*
FLO_type
__muldf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  const fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_d (&au, &a);
  __unpack_d (&bu, &b);

  res = _fpmul_parts (&a, &b, &tmp);

  return __pack_d (res);
}
*/

fp_number_type
__mymuldf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  const fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  fp_number_type* fp1 = __unpack_d_drill (&au, &a);
  fp_number_type* fp2 = __unpack_d_drill (&bu, &b);
  res = _fpmul_parts (fp1, fp2, &tmp);

//  return (__pack_d_drill(res));
	return (*res);
}

#ifdef MAN


/*
  fp_class_type class;
  unsigned int sign;
  int normal_exp;


  union
    {
      fractype lla;
    } fraction;
*/
void main(void)
{
	FLO_union_type f0a,f0b;
	f0a.raw_value = 4607182418815746050;
	f0b.raw_value = 4607182418800017408;
	__mymuldf3(f0a.value,f0b.value);

    fp_number_type fp_a = { 3,0,0,{  1152921508633379328ULL } };
    fp_number_type fp_b = { 3,0,0,{ 18446744073709551601ULL } };
    fp_number_type tmp;
	fp_number_type ea,eb;

	//_fpmul_parts(&fp_a, &fp_b, &tmp);

	double xa,xb;
	xa = __pack_d_drill(&fp_a);
	xb = __pack_d_drill(&fp_b);
	//printf("a=%g\n",a);

	FLO_union_type fua,fub;
	fua.value = xa;
	fub.value = xb;
    __unpack_d_drill (&fua, &ea);
    __unpack_d_drill (&fub, &eb);
	printf("ea.sign=%i,ea.exp=%i,ea.fraction=%llu,\nfua.raw_value=%llu\n",ea.sign,ea.normal_exp,ea.fraction.lla,fua.raw_value);
	printf("eb.sign=%i,eb.exp=%i,eb.fraction=%llu,\nfub.raw_value=%llu\n",eb.sign,eb.normal_exp,eb.fraction.lla,fub.raw_value);


/*

    fp_number_type tmp;

	double a;
	a = __pack_d_drill(&fp_a);
	FLO_union_type fua;
	fua.value = a;
	printf("a=%g, fua.raw_value=%llx\n",a,fua.raw_value);

	fp_number_type e1;
    __unpack_d_drill (&fua, &e1);
	printf("e1.sign=%i,e1.exp=%i,e1.fraction=%llu\n",e1.sign,e1.normal_exp,e1.fraction.lla);
*/
/*
	FLO_union_type a;
	a.value = ?;

	fp_number_type e1;
    __unpack_d_drill (&a, &e1);

	printf("e1.
*/
/*
FLO_type
__pack_d (const fp_number_type *src)

fractype
__unpack_d_drill (FLO_union_type * src, fp_number_type * dst)
*/
/*
    FLO_union_type d; d.value = 3.141592e-3;
    printf("d.value=%f\n",d.value);

    fp_number_type e1,e2;

    __unpack_d_drill (&d, &e1);
    printf("__unpack_d_drill(d.raw_value = %llx, dst->class = %u, dst->sign = %u, dst->normal_exp = %i,\n  dst->fraction.lla = %llx = %llu) = \n",d.raw_value, e1.class, e1.sign, e1.normal_exp, e1.fraction.lla,e1.fraction.lla);

    double ad = __pack_d_drill (&e1);
    printf("__pack_d_drill(e1) = %f\n",ad);
*/
}
#endif



