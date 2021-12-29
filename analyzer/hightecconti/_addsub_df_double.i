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

typedef enum
{
	CLASS_SNAN,
	CLASS_QNAN,
	CLASS_ZERO,
	CLASS_NUMBER,
	CLASS_INFINITY
}
fp_class_type;

typedef struct
{

	fp_class_type class;
	unsigned int sign;
	int normal_exp;

	struct
	{
		fractype ll;
	}
	fraction;
}
fp_number_type;

const fp_number_type __thenan_df = { CLASS_SNAN, 0, 0,
	{
		(fractype) 0
	}
};

typedef union
{
	FLO_type value;
	fractype bits;
}
FLO_union_type;

static
const fp_number_type *
	makenan(void)
	{

		return &__thenan_df;
	}

__inline__
static int
isnan(const fp_number_type *x)
{
	return __builtin_expect(x->class == CLASS_SNAN || x->class == CLASS_QNAN,
		0);
}

__inline__
static int
isinf(const fp_number_type *x)
{
	return __builtin_expect(x->class == CLASS_INFINITY, 0);
}

__inline__
static int
iszero(const fp_number_type *x)
{
	return x->class == CLASS_ZERO;
}

fractype
__unpack_d(FLO_union_type *src, fp_number_type *dst)
{
	fractype fraction;
	int exp;
	int sign;
	fraction = (fractype)(src->bits & ((1ULL << 52) - 1));
	exp = (int)((src->bits >> 52) & ((1ULL << 11) - 1));
	sign = (int)((src->bits >> 63) & 1ULL);
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

			while (fraction < ((fractype) 1 << (52 + 8L)))
			{
				fraction <<= 1;
				dst->normal_exp--;
			}

			dst->fraction.ll = fraction;
		}
	}
	else if (!0 &&
		__builtin_expect(exp == (0x7ff), 0))
	{

		if (fraction == 0)
		{

			dst->class = CLASS_INFINITY;
		}
		else
		{

			if (fraction &0x8000000000000LL)

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
		dst->fraction.ll = (fraction << 8L) | ((fractype) 1 << (52 + 8L));
	}

	return (dst->fraction.ll);
}

FLO_type
__pack_d(const fp_number_type *src)
{
	FLO_union_type dst;
	fractype fraction = src->fraction.ll;
	int sign = src->sign;
	int exp = 0;

	if (0 && (isnan(src) || isinf(src)))
	{

		exp = (0x7ff);
		fraction = ((fractype) 1 << 52) - 1;
	}
	else if (isnan(src))
	{
		exp = (0x7ff);

		fraction >>= 8L;
		fraction &= 0x8000000000000LL - 1;
		if (src->class == CLASS_QNAN || 1)
		{

			fraction |= 0x8000000000000LL;
		}
	}
	else if (isinf(src))
	{
		exp = (0x7ff);
		fraction = 0;
	}
	else if (iszero(src))
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
		if (__builtin_expect(src->normal_exp < (-(1023) + 1), 0))
		{
			int shift = (-(1023) + 1) - src->normal_exp;

			exp = 0;

			if (shift > 64 - 8L)
			{

				fraction = 0;
			}
			else
			{
				int lowbit = (fraction &(((fractype) 1 << shift) - 1)) ? 1 : 0;
				fraction = (fraction >> shift) | lowbit;
			}
			if ((fraction & 0xff) == 0x80)
			{
				if ((fraction &(1 << 8L)))
					fraction += 0x7f + 1;
			}
			else
			{

				fraction += 0x7f;
			}

			if (fraction >= ((fractype) 1 << (52 + 8L)))
			{
				exp += 1;
			}
			fraction >>= 8L;
		}
		else if (!0 &&
			__builtin_expect(src->normal_exp > 1023, 0))
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
					if (fraction &(1 << 8L))
						fraction += 0x7f + 1;
				}
				else
				{

					fraction += 0x7f;
				}
				if (fraction >= ((fractype) 1 << (52 + 1 + 8L)))
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

	dst.bits = fraction | ((fractype) exp << 52L) | ((fractype) sign << 63L);
	return dst.value;
}

typedef void(*gt_pointer_operator)(void *, void *);

extern FLO_type __pack_d(const fp_number_type *);
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

FLO_type
__adddf3(FLO_type arg_a, FLO_type arg_b)
{
	fp_number_type a;
	fp_number_type b;
	fp_number_type tmp;
	const fp_number_type * res;
	FLO_union_type au, bu;

	au.value = arg_a;
	bu.value = arg_b;

	__unpack_d(&au, &a);
	__unpack_d(&bu, &b);

	res = _fpadd_parts(&a, &b, &tmp);

	return __pack_d(res);
}

FLO_type
__subdf3(FLO_type arg_a, FLO_type arg_b)
{
	fp_number_type a;
	fp_number_type b;
	fp_number_type tmp;
	const fp_number_type * res;
	FLO_union_type au, bu;

	au.value = arg_a;
	bu.value = arg_b;

	__unpack_d(&au, &a);
	__unpack_d(&bu, &b);

	b.sign ^= 1;

	res = _fpadd_parts(&a, &b, &tmp);

	return __pack_d(res);
}

int __attribute__((__cdecl__)) printf(const char *,...);
void mainc(void)
{
  printf("sizeof(FLO_type)=%i\n",sizeof(FLO_type));
  printf("sizeof(fractype)=%i\n",sizeof(fractype));
}
