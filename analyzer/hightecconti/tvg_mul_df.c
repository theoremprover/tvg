










enum machine_mode
{
  VOIDmode,
  BLKmode,
  CCmode,
  BImode,
  QImode,
  HImode,
  SImode,
  DImode,
  TImode,
  PDImode,
  QQmode,
  HQmode,
  SQmode,
  DQmode,
  TQmode,
  UQQmode,
  UHQmode,
  USQmode,
  UDQmode,
  UTQmode,
  HAmode,
  SAmode,
  DAmode,
  TAmode,
  UHAmode,
  USAmode,
  UDAmode,
  UTAmode,
  HFmode,
  SFmode,
  DFmode,
  SDmode,
  DDmode,
  TDmode,
  CQImode,
  CHImode,
  CSImode,
  CDImode,
  CTImode,
  HCmode,
  SCmode,
  DCmode,
  MAX_MACHINE_MODE,

  MIN_MODE_RANDOM = VOIDmode,
  MAX_MODE_RANDOM = BLKmode,

  MIN_MODE_CC = CCmode,
  MAX_MODE_CC = CCmode,

  MIN_MODE_INT = QImode,
  MAX_MODE_INT = TImode,

  MIN_MODE_PARTIAL_INT = PDImode,
  MAX_MODE_PARTIAL_INT = PDImode,

  MIN_MODE_FRACT = QQmode,
  MAX_MODE_FRACT = TQmode,

  MIN_MODE_UFRACT = UQQmode,
  MAX_MODE_UFRACT = UTQmode,

  MIN_MODE_ACCUM = HAmode,
  MAX_MODE_ACCUM = TAmode,

  MIN_MODE_UACCUM = UHAmode,
  MAX_MODE_UACCUM = UTAmode,

  MIN_MODE_FLOAT = HFmode,
  MAX_MODE_FLOAT = DFmode,

  MIN_MODE_DECIMAL_FLOAT = SDmode,
  MAX_MODE_DECIMAL_FLOAT = TDmode,

  MIN_MODE_COMPLEX_INT = CQImode,
  MAX_MODE_COMPLEX_INT = CTImode,

  MIN_MODE_COMPLEX_FLOAT = HCmode,
  MAX_MODE_COMPLEX_FLOAT = DCmode,

  MIN_MODE_VECTOR_INT = VOIDmode,
  MAX_MODE_VECTOR_INT = VOIDmode,

  MIN_MODE_VECTOR_FRACT = VOIDmode,
  MAX_MODE_VECTOR_FRACT = VOIDmode,

  MIN_MODE_VECTOR_UFRACT = VOIDmode,
  MAX_MODE_VECTOR_UFRACT = VOIDmode,

  MIN_MODE_VECTOR_ACCUM = VOIDmode,
  MAX_MODE_VECTOR_ACCUM = VOIDmode,

  MIN_MODE_VECTOR_UACCUM = VOIDmode,
  MAX_MODE_VECTOR_UACCUM = VOIDmode,

  MIN_MODE_VECTOR_FLOAT = VOIDmode,
  MAX_MODE_VECTOR_FLOAT = VOIDmode,

  NUM_MACHINE_MODES = MAX_MACHINE_MODE
};

       
typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));




typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));







typedef int CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));


typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
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
      fractype ll;
      halffractype l[2];
    } fraction;
} fp_number_type;

typedef union
{
  FLO_type value;
  fractype value_raw;





  halffractype words[2];




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
      USItype nl = a->fraction.ll;
      USItype nh = a->fraction.ll >> (4 * (8));
      USItype ml = b->fraction.ll;
      USItype mh = b->fraction.ll >> (4 * (8));
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
            solver_find();
         }
      pp_hl = (UDItype)(USItype)ps_hh__ << (4 * (8));
      res0 = pp_ll + pp_hl;
      if (res0 < pp_ll)
 res2++;
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
           low >>= 1;
           low |= 0x8000000000000000LL;
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
      if (high & (1 << 8L))
 {






 }
      else if (low)
 {




   high += 0x7f + 1;


   high &= ~(fractype) 0xff;
 }
    }
  tmp->fraction.ll = high;
  tmp->class = CLASS_NUMBER;
  return tmp;
}

typedef float SFtype __attribute__((mode(SF)));
typedef float DFtype __attribute__((mode(DF)));

typedef int SItype __attribute__((mode(SI)));
typedef int DItype __attribute__((mode(DI)));

typedef unsigned int USItype __attribute__((mode(SI)));
typedef unsigned int UDItype __attribute__((mode(DI)));
typedef UDItype fractype;
typedef USItype halffractype;
typedef DFtype FLO_type;
typedef DItype intfrac;

const fp_number_type __thenan_df = { CLASS_SNAN, 0, 0,
	{
		(fractype) 0
	}
};

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
			a_fraction = a->fraction.ll;
			b_fraction = b->fraction.ll;

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
				tmp->fraction.ll = tfraction;
			}
			else
			{
				tmp->sign = 1;
				tmp->normal_exp = a_normal_exp;
				tmp->fraction.ll = -tfraction;
			}

			while (tmp->fraction.ll < ((fractype) 1 << (52 + 8L)) && tmp->fraction.ll)
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

		if (tmp->fraction.ll >= ((fractype) 1 << (52 + 1 + 8L)))
		{
		{ 	tmp->fraction.ll = (tmp->fraction.ll >> 1) | !!(tmp->fraction.ll &(((fractype) 1 << 1) - 1));
			};
			tmp->normal_exp++;
		}
		return tmp;
	}

void
__unpack_d (FLO_union_type * src, fp_number_type * dst)
{



  fractype fraction;
  int exp;
  int sign;
  fraction = src->bits.fraction;
  exp = src->bits.exp;
  sign = src->bits.sign;
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

   dst->fraction.ll = fraction;
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
   dst->fraction.ll = fraction << 8L;
 }
    }
  else
    {

      dst->normal_exp = exp - 1023;
      dst->class = CLASS_NUMBER;
      dst->fraction.ll = (fraction << 8L) | ((fractype)1<<(52 +8L));
    }
}

FLO_type
__pack_d (const fp_number_type *src)
{
  FLO_union_type dst;
  fractype fraction = src->fraction.ll;
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
