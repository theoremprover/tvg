/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE_754_ARITHMETIC_H_
#define _TARGET_UNITTEST_IEEE_754_ARITHMETIC_H_

#include <target/c/ieee754/predefines.h>
#include <target/c/ieee754/convenience.h>

/** Define addsub/multiply/divide for different floating point types */
#if TARGET_NATIVE_DOUBLE == IEEE_754
	UTT_LINKAGE utt_ieee754_double utt_addsub( utt_ieee754_double left , utt_ieee754_double right , bool subtraction ){ return subtraction ? left - right : left + right; }
	UTT_LINKAGE utt_ieee754_double utt_multiply( utt_ieee754_double left , utt_ieee754_double right ){ return left * right; }
	UTT_LINKAGE utt_ieee754_double utt_divide( utt_ieee754_double dividend , utt_ieee754_double divisor ){ return dividend / divisor; }
#else
	UTT_LINKAGE utt_ieee754_double utt_addsub( utt_ieee754_double left , utt_ieee754_double right , bool subtraction )
	{
		const int precision_shift = 5;
		/* Catch NaN */
		if( utt_isnan(left) )
			return left;
		if( utt_isnan(right) )
			return right;
		
		/* Flip the sign of 'right', in case of subtraction */
		if( subtraction )
			right ^= UTT_SIGNBIT_MASK;
		
		/* Catch Infinities */
		if( utt_isinf(left) ) {
			if( utt_isinf(right) ){
				if( utt_signbit(left) != utt_signbit(right) ) /* Inf + (-Inf) or vice versa */
					return UTT_NAN;
				else /* Inf+Inf or (-Inf)+(-Inf) */
					return left;
			}
		}
		else if( utt_isinf(right) )
			return right;
		
		/* Make 'left' contain the greater absolute value */
		int exp_difference = utt_exponent(left) - utt_exponent(right);
		int difference = exp_difference;
		if( !difference )
			difference = utt_mantissa(left) - utt_mantissa(right);
		if( difference < 0 ){
			utt_ieee754_double tmp = left;
			left = right;
			right = tmp;
			exp_difference = -exp_difference;
		}
		
		bool new_signbit = utt_signbit(left);
		utt_uint16_t new_exponent = utt_exponent(left);
		
		/* Addition of Absolute Values? */
		if( new_signbit == utt_signbit(right) )
		{
			/* Add 'left' to the accumulator */
			utt_uint64_t accumulator = utt_mantissa(left);
			if( new_exponent )
				accumulator += UTT_MANTISSA_MASK + 1; /* Add the non-stored '1' at the beginning of the mantissa, if normal */
			/* Shift accumulator left for more precision */
			accumulator <<= precision_shift;
			
			/* Add 'right' to the accumulator */
			utt_uint64_t	mant_right = utt_mantissa(right);
			if( utt_exponent(right) )
				mant_right += UTT_MANTISSA_MASK + 1; /* Add the non-stored '1' at the beginning of the mantissa, if normal */
			accumulator += ( mant_right << precision_shift ) >> exp_difference;
			
			int right_shift = precision_shift;
			/* Normalize again? */
			const int normalization_threshold = ( ( UTT_MANTISSA_MASK + 1 ) << ( precision_shift + 1 ) ) - 1;
			if( accumulator > normalization_threshold  ){
				right_shift += 1;
				new_exponent += 1;
			}
			accumulator = ( accumulator + (1 << (right_shift-1)) ) >> right_shift;
		}
		else /* Effectively Subtraction */
		{
			/* Add 'left' to the accumulator */
			utt_uint64_t accumulator = utt_mantissa(left);
			if( new_exponent )
				accumulator += UTT_MANTISSA_MASK + 1; /* Add the non-stored '1' at the beginning of the mantissa, if normal */
			/* Shift accumulator left for more precision */
			accumulator <<= precision_shift;
			
			/* Subtract 'right' from the accumulator */
			utt_uint64_t	mant_right = utt_mantissa(right);
			if( utt_exponent(right) )
				mant_right += UTT_MANTISSA_MASK + 1; /* Add the non-stored '1' at the beginning of the mantissa, if normal */
			accumulator -= ( mant_right << precision_shift ) >> exp_difference;
			
			/* Normalize */
			const int normalization_threshold = ( ( UTT_MANTISSA_MASK + 1 ) << precision_shift ) - 1;
			while( accumulator <= normalization_threshold && new_exponent > 0 ){
				accumulator <<= 1;
				new_exponent -= 1;
			}
			accumulator = ( ( accumulator + (1 << (precision_shift-1)) ) >> precision_shift );
			
			/* Prevent (-x) + x = -0 */
			if( !accumulator )
				new_signbit = false;
		}
		
		return utt_fromraw(
			( (utt_uint64_t)new_signbit << UTT_SIGNBIT_SHIFT )
			| ( (utt_uint64_t)new_exponent << UTT_EXPONENT_SHIFT )
			| ( accumulator & UTT_MANTISSA_MASK ) /* Remove leading '1' */
		);
	}
	UTT_LINKAGE utt_ieee754_double utt_multiply( utt_ieee754_double left , utt_ieee754_double right ){ utt_assert_ex( false , "Non-native multiplication of IEEE 754 double not implemented." ); return left; }
	UTT_LINKAGE utt_ieee754_double utt_divide( utt_ieee754_double dividend , utt_ieee754_double divisor ){ utt_assert_ex( false , "Non-native division of IEEE 754 double not implemented." ); return dividend; }
#endif

#if TARGET_NATIVE_FLOAT == IEEE_754
	UTT_LINKAGE utt_ieee754_float utt_addsubf( utt_ieee754_float left , utt_ieee754_float right , bool subtraction ){ return subtraction ? left - right : left + right; }
#else
	UTT_LINKAGE utt_ieee754_float utt_addsubf( utt_ieee754_float left , utt_ieee754_float right , bool subtraction ){ utt_assert_ex( false , "Non-native addition/subtraction of IEEE 754 float not implemented." ); return left; }
	UTT_LINKAGE utt_ieee754_float utt_multiplyf( utt_ieee754_float left , utt_ieee754_float right ){ utt_assert_ex( false , "Non-native multiplication of IEEE 754 float not implemented." ); return left; }
	UTT_LINKAGE utt_ieee754_float utt_dividef( utt_ieee754_float dividend , utt_ieee754_float divisor ){ utt_assert_ex( false , "Non-native division of IEEE 754 float not implemented." ); return dividend; }
#endif

#if TARGET_NATIVE_HALF == IEEE_754
	UTT_LINKAGE utt_ieee754_half utt_addsubh( utt_ieee754_half left , utt_ieee754_half right , bool subtraction ){ return subtraction ? left - right : left + right; }
#else
	UTT_LINKAGE utt_ieee754_half utt_addsubh( utt_ieee754_half left , utt_ieee754_half right , bool subtraction ){ utt_assert_ex( false , "Non-native addition/subtraction of IEEE 754 half not implemented." ); return left; }
	UTT_LINKAGE utt_ieee754_half utt_multiplyh( utt_ieee754_half left , utt_ieee754_half right ){ utt_assert_ex( false , "Non-native multiplication of IEEE 754 half not implemented." ); return left; }
	UTT_LINKAGE utt_ieee754_half utt_divideh( utt_ieee754_half dividend , utt_ieee754_half divisor ){ utt_assert_ex( false , "Non-native division of IEEE 754 half not implemented." ); return dividend; }
#endif

#if TARGET_NATIVE_HALF == IEEE_754_ARM
	UTT_LINKAGE utt_ieee754_half_alt utt_addsubh2( utt_ieee754_half_alt left , utt_ieee754_half_alt right , bool subtraction ){ return subtraction ? left - right : left + right; }
#else
	UTT_LINKAGE utt_ieee754_half_alt utt_addsubh2( utt_ieee754_half_alt left , utt_ieee754_half_alt right , bool subtraction ){ utt_assert_ex( false , "Non-native addition/subtraction of IEEE 754 (alternative) half not implemented." ); return left; }
	UTT_LINKAGE utt_ieee754_half_alt utt_multiplyh2( utt_ieee754_half_alt left , utt_ieee754_half_alt right ){ utt_assert_ex( false , "Non-native multiplication of IEEE 754 (alternative) half not implemented." ); return left; }
	UTT_LINKAGE utt_ieee754_half_alt utt_divideh2( utt_ieee754_half_alt dividend , utt_ieee754_half_alt divisor ){ utt_assert_ex( false , "Non-native division of IEEE 754 (alternative) half not implemented." ); return dividend; }
#endif

#endif
