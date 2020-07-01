/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_BIGFLOAT_ARITHMETIC_H_
#define _TARGET_UNITTEST_BIGFLOAT_ARITHMETIC_H_

#include <target/c/bigfloat/convenience.h>

/** Define addsub/multiply/divide for different floating point types */
UTT_LINKAGE utt_bigfloat_t utt_addsubm( utt_bigfloat_t left , utt_bigfloat_t right , bool subtraction )
{
	const int precision_shift = 5;
	
	/* Catch NaN */
	if( utt_isnanm(left) )
		return left;
	if( utt_isnanm(right) )
		return right;
	
	/* Flip the sign of 'right', in case of subtraction */
	if( subtraction )
		utt_negatem( &right );
	
	/* Catch Infinities */
	if( utt_isinfm(left) ) {
		if( utt_isinfm(right) ){
			if( utt_signbitm(left) != utt_signbitm(right) ) /* Inf + (-Inf) or vice versa */
				return UTT_NANM;
			else /* Inf+Inf or (-Inf)+(-Inf) */
				return left;
		}
	}
	else if( utt_isinfm(right) )
		return right;
	
	/* Make 'left' contain the greater absolute value */
	utt_int64_t exp_difference = (utt_int64_t)utt_exponentm(left) - (utt_int64_t)utt_exponentm(right);
	utt_int64_t difference = exp_difference;
	if( !difference ){
		utt_uint32_t	cursor = UTT_MP_FLOAT_EXPONENT_BYTES;
		while( cursor < UTT_MP_FLOAT_TOTAL_BYTES ){
			if( ( difference = ( left.data[cursor] - right.data[cursor] ) ) )
				break;
			cursor++;
		}
	}
	if( difference < 0 ){
		utt_bigfloat_t tmp = left;
		left = right;
		right = tmp;
		exp_difference = -exp_difference;
	}
	
	bool			new_signbit = utt_signbitm(left);
	utt_uint32_t	new_exponent = utt_exponentm(left);
	
	/* Compute the value of the non-stored '1' */
	utt_bigint_t	leading_one;
	utt_bi_set_32( &leading_one , 1 );
	utt_bi_shl( &leading_one , UTT_MP_FLOAT_MANTISSA_BYTES * 8 );
	
	/* Add 'left' to the accumulator */
	utt_bigint_t accumulator = utt_mantissam(left);
	
	/* Add the non-stored '1' at the beginning of the mantissa, if normal */
	if( new_exponent /*= exponent of 'left' */ )
		utt_bi_add( &accumulator , &leading_one );
	
	/* Shift accumulator left for more precision */
	utt_bi_shl( &accumulator , precision_shift );
	
	/* Add the non-stored '1' at the beginning of the mantissa of 'right', if normal */
	utt_bigint_t	mant_right = utt_mantissam(right);
	if( utt_exponentm(right) )
		utt_bi_add( &mant_right , &leading_one );
	
	/* Shift accumulator left for more precision, Shift right, in order to scale to 'left' */
	if( precision_shift > exp_difference )
		utt_bi_shl( &mant_right , precision_shift - exp_difference );
	else
		utt_bi_shr( &mant_right , exp_difference - precision_shift );
	
	int necessary_right_shift = precision_shift;
	
	/* Addition of Absolute Values? */
	if( new_signbit == utt_signbitm(right) )
	{
		/* Add 'right' to the accumulator */
		utt_bi_add( &accumulator , &mant_right );
		
		/* Normalize again? */
		if( utt_bi_log2( &accumulator ) > ( UTT_MP_FLOAT_MANTISSA_BYTES * 8 + precision_shift ) ){
			necessary_right_shift += 1; /* The right shift cannot be larger than 1, as 'right' can at maximum be >as large< as 'left' */
			new_exponent += 1;
		}
	}
	else /* Effectively Subtraction */
	{
		/* Subtract 'right' from the accumulator */
		utt_bi_sub( &accumulator , &mant_right );
		
		/* Is the result 0? */
		if( utt_bi_is_zero( &accumulator ) ){
			new_signbit = false; /* Prevent (-x) + x = -0 */
			new_exponent = 0;
		}
		else{
			/* Determine the number of bits to shift left in order to normalize */
			utt_int32_t	normalization_left_shift = (utt_int32_t)( UTT_MP_FLOAT_MANTISSA_BYTES * 8 + precision_shift ) - utt_bi_log2( &accumulator );
			
			/* Check, if we need to normalize */
			if( new_exponent > 0 && normalization_left_shift > 0 ){
				if( (utt_uint32_t)normalization_left_shift > new_exponent )
					normalization_left_shift = new_exponent;
				utt_bi_shl( &accumulator , normalization_left_shift );
				new_exponent -= normalization_left_shift;
			}
		}
	}
	
	/* Apply precision right shift with correct rounding */
	utt_bigint_t	threshold;
	utt_bi_set_32( & threshold , 1u << ( necessary_right_shift - 1 ) );
	utt_bi_add( &accumulator , &threshold );
	utt_bi_shr( &accumulator , necessary_right_shift );
	
	/* Compose the resulting big float */
	utt_bigfloat_t result;
	utt_set_mantissam( &result , accumulator );
	utt_set_exponentm( &result , new_exponent );
	utt_set_signbitm( &result , new_signbit );
	
	return result;
}
UTT_LINKAGE utt_bigfloat_t utt_multiplym( utt_bigfloat_t left , utt_bigfloat_t right ){ utt_assert_ex( false , "Multiplication of big floats not implemented." ); return left; }
UTT_LINKAGE utt_bigfloat_t utt_dividem( utt_bigfloat_t dividend , utt_bigfloat_t divisor ){ utt_assert_ex( false , "Division of big floats not implemented." ); return dividend; }

#endif
