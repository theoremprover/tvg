#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif


static inline __attribute__ ((__always_inline__)) fp_number_type *
_fpdiv_parts (fp_number_type * a,
	      fp_number_type * b)
{
  fractype bit;
  fractype numerator;
  fractype denominator;
  fractype quotient;

  a->sign = a->sign ^ b->sign;

  {
    /* quotient =
       ( numerator / denominator) * 2^(numerator exponent -  denominator exponent)
     */

    a->normal_exp = a->normal_exp - b->normal_exp;
    numerator = a->fraction.ll;
    denominator = b->fraction.ll;

    if (numerator < denominator)
      {
	/* Fraction will be less than 1.0 */
	numerator *= 2;
	a->normal_exp--;
      }
    bit = IMPLICIT_1;
    quotient = 0;
    /* ??? Does divide one bit at a time.  Optimize.  */
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

    if (!ROUND_TOWARDS_ZERO && (quotient & GARDMASK) == GARDMSB)
      {
	if (quotient & (1 << NGARDS))
	  {
	    /* Because we're half way, we would round to even by adding
	       GARDROUND + 1, except that's also done in the packing
	       function, and rounding twice will lose precision and cause
	       the result to be too far off.  */
	  }
	else if (numerator)
	  {
#ifdef CALC
	    printf("GOTIT!\n");
#endif
	    /* We're a further than half way by the small amount
	       corresponding to the bits set in "numerator".  Knowing
	       that, we round here and not in pack_d, because there we
	       don't have "numerator" available anymore.  */
	    quotient += GARDROUND + 1;

	    /* Avoid further rounding in pack_d.  */
	    quotient &= ~(fractype) GARDMASK;
	  }
      }

    a->fraction.ll = quotient;
    return (a);
  }
}


#ifdef CALC
int main(int argc, char* argv[])
{
    //printf("IMPLICIT_1=%i\n",IMPLICIT_1);
    //printf("FRACBITS=%i, NGARDS=%i\n",FRACBITS,NGARDS);
    // FRACBITS=23, NGARDS=7

    int i = 1 ;
    int arga0 = atoi(argv[i++]); // a
    int arga1 = atoi(argv[i++]); // fp_class_type class;
    int arga2 = atoi(argv[i++]); // unsigned int sign;
    int arga3 = atoi(argv[i++]); // int normal_exp;
    int arga4 = atoi(argv[i++]); // fractype ll; }

    int argb0 = atoi(argv[i++]); // b
    int argb1 = atoi(argv[i++]); // fp_class_type class;
    int argb2 = atoi(argv[i++]); // unsigned int sign;
    int argb3 = atoi(argv[i++]); // int normal_exp;
    int argb4 = atoi(argv[i++]); // fractype ll; }

    fp_number_type a = { arga1, arga2, arga3, { arga4 } };
    fp_number_type b = { argb1, argb2, argb3, { argb4 } };

    fp_number_type* r = _fpdiv_parts(&a,&b);
    printf("f(a=%i, a={ %i,%i,%i, fraction={%i} },   b=%i, b={ %i,%i,%i, fraction={%i} }) =\n%i %i %i %i %i\n",
        arga0,arga1,arga2,arga3,arga4,
        argb0,argb1,argb2,argb3,argb4,
        r,r->class,r->sign,r->normal_exp,r->fraction.ll);
    return 0;
}
#endif
