




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

int found_new = -1;

int founds[20] = { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

void solver_find(int i)
{
if(founds[i]==0)
{
	printf("solver_find(%i)\n",i);
	found_new = i;
	founds[i]++;
}
}
#endif




typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));




typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));







typedef int CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));


typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned long int UDItype __attribute__ ((mode (DI)));
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

  struct
    {
      fractype fraction:52 __attribute__ ((packed));
      unsigned int exp:11 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
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
 return makenan ();
      a->sign = a->sign != b->sign;
      return a;
    }
  if (isinf (b))
    {
      if (iszero (a))
 {
   return makenan ();
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
            solver_find(0);
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
      solver_find(1);
      tmp->normal_exp++;
      if (high & 1)
         {
           solver_find(2);
           low >>= 1;
           low |= 0x8000000000000000LL;
         }
         else solver_find(3);
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
static
const fp_number_type *
	_fpadd_parts(fp_number_type *a,
		fp_number_type *b,
		fp_number_type *tmp)
	{
		intfrac tfraction;

		int a_normal_exp;
		int b_normal_exp;
		fractype a_fraction;
		fractype b_fraction;

		if (isnan(a))
		{
			return a;
		}
		if (isnan(b))
		{
			return b;
		}
		if (isinf(a))
		{

			if (isinf(b) && a->sign != b->sign)
				return makenan();
			return a;
		}
		if (isinf(b))
		{
			return b;
		}
		if (iszero(b))
		{
			if (iszero(a))
			{ 	*tmp = *a;
				tmp->sign = a->sign &b->sign;
				return tmp;
			}
			return a;
		}
		if (iszero(a))
		{
			return b;
		}

		{

			int diff;
			int sdiff;

			a_normal_exp = a->normal_exp;
			b_normal_exp = b->normal_exp;
			a_fraction = a->fraction.lla;
			b_fraction = b->fraction.lla;

			diff = a_normal_exp - b_normal_exp;
			sdiff = diff;

			if (diff < 0)
				diff = -diff;
			if (diff < 64)
			{
				if (sdiff > 0)
				{
					b_normal_exp += diff;
					{
						b_fraction = (b_fraction >> diff) | !!(b_fraction &(((fractype) 1 << diff) - 1));
					};
				}
				else if (sdiff < 0)
				{
					a_normal_exp += diff;
					{
						a_fraction = (a_fraction >> diff) | !!(a_fraction &(((fractype) 1 << diff) - 1));
					};
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
				tmp->fraction.lla = tfraction;
			}
			else
			{
				tmp->sign = 1;
				tmp->normal_exp = a_normal_exp;
				tmp->fraction.lla = -tfraction;
			}

			while (tmp->fraction.lla < ((fractype) 1 << (52 + 8L)) && tmp->fraction.lla)
			{
				tmp->fraction.lla <<= 1;
				tmp->normal_exp--;
			}
		}
		else
		{
			tmp->sign = a->sign;
			tmp->normal_exp = a_normal_exp;
			tmp->fraction.lla = a_fraction + b_fraction;
		}
		tmp->class = CLASS_NUMBER;

		if (tmp->fraction.lla >= ((fractype) 1 << (52 + 1 + 8L)))
		{
		{ 	tmp->fraction.lla = (tmp->fraction.lla >> 1) | !!(tmp->fraction.lla &(((fractype) 1 << 1) - 1));
			};
			tmp->normal_exp++;
		}
		return tmp;
	}
*/

void
__unpack_d (FLO_union_type * src, fp_number_type * dst)
{

  fractype fraction;
  int exp;
  int sign;
  fraction = src->bits.fraction;
  exp = src->bits.exp;
  sign = src->bits.sign;

//printf("__unpack_d: raw=%llx, fraction=%llx, exp=%llx, sign=%llx\n",src->raw_value,fraction,exp,sign);

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

/*
fractype
__unpack_d_drill (FLO_union_type * src, fp_number_type * dst)
{

//    solver_debug_ULongLong("src->raw_value[0]",src->raw_value[0]);
  fractype fraction;
  int exp;
  int sign;
  fraction = (src->raw_value[0]) & (0x000fffffffffffffULL) ;
  exp = ((src->raw_value[0]) >> 52) & (0x7ffULL);
  sign = (src->raw_value[0]) >> 63 ;

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

    return(dst->fraction.lla);
}
*/

/*
  struct
    {
      fractype fraction:52 __attribute__ ((packed));
      unsigned int exp:11 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
*/

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
      solver_find(5);
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
       solver_find(6);
        fraction += 0x7f + 1;
        }
        else solver_find(7);
     }
   else
     {

       fraction += 0x7f;
     }


   if (fraction >= ((fractype)1<<(52 +8L)))
     {
     solver_find(8);
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
      { fraction += 0x7f + 1;
      solver_find(9);
      }
      else solver_find(10);
  }
       else
  {

    fraction += 0x7f;
  }
       if (fraction >= ((fractype)1<<(52 +1+8L)))
  {
  solver_find(11);
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

/*
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
        solver_find(4);
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
            solver_find(5);
            fraction += 0x7f + 1;
        }
     }
   else
     {

       fraction += 0x7f;
     }


   if (fraction >= ((fractype)1<<(52 +8L)))
     {
        solver_find(6);
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
            solver_find(7);
          fraction += 0x7f + 1;
        }
      }
       else
      {

        fraction += 0x7f;
      }
       if (fraction >= ((fractype)1<<(52 +1+8L)))
  {
    solver_find(8);
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
  dst.raw_value[0] = fraction | (((fractype)exp)<<52) | ((fractype)sign<<63);
  return dst.value;
}
*/

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

/*
FLO_type
__mymuldf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  const fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_d_drill (&au, &a);
  __unpack_d_drill (&bu, &b);

  res = _fpmul_parts (&a, &b, &tmp);

  return (__pack_d(res));
}
*/

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
/*
    fp_number_type a = { 3,0,0,{ 4006005331805730806ULL } };
    fp_number_type b = { 3,0,0,{ 17586381064309348604ULL } };
    fp_number_type tmp;
*/

    FLO_union_type a,b;

    unsigned long long int MAX = 1ULL<<52 - 1;
	unsigned long long int stepi = 50, stepj = 1000000;

    for(unsigned long long int i=0;i<(1<<11)-1;i+=stepi)
    {
        a.value = 3.14159265;
        a.bits.exp = i;
        for(unsigned long long int j=0;j<MAX;j+=stepj)
        {
            if(j%(10000000*stepj)==0) printf("%llu/%llu      \r",i,j);
            b.value = 3.14159265;
			b.bits.fraction = j;

            found_new = -1;
            __muldf3(a.value,b.value);
            if(found_new>=0)
            {
                FLO_union_type au,bu;
                printf("FOUND %i: a = %f = %16.16llx , b = %f = %16.16llx\n",found_new,a.value,a.raw_value,b.value,b.raw_value);
            }
        }
    }
}
#endif

