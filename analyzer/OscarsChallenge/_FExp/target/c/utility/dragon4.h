/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_DRAGON4_H_
#define _TARGET_UNITTEST_UTILITY_DRAGON4_H_

#include <target/c/c_upgrade.h>
#include <target/c/stdint.h>
#include <target/c/stdfloat.h>
#include <target/c/math.h>
#include <target/c/utility/bigint.h>

/**
 * The meat of this file is taken from http://www.ryanjuckett.com/programming/printing-floating-point-numbers/
 * It has been adapted to
 *	- only use C99 features,
 *	- ForeC++ data types,
 *	- ForeC++ source code formatting,
 *	- ForeC++ naming conventions and
 *	- ForeC++ function design.
 *
 * Corresponding license:
 *
 * Copyright (c) 2014 Ryan Juckett
 * http://www.ryanjuckett.com/
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.

 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *	  claim that you wrote the original software. If you use this software
 *	  in a product, an acknowledgment in the product documentation would be
 *	  appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not be
 *	  misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *	  distribution.
 */

/** Different modes for terminating digit output */
typedef enum{
	UTT_CUTOFFMODE_UNIQUE			/* as many digits as necessary to print a uniquely identifiable number */
	, UTT_CUTOFFMODE_TOTALLENGTH	/* up to cutoff_number significant digits */
	, UTT_CUTOFFMODE_FRACTIONLENGTH	/* up to cutoff_number significant digits past the decimal point */
} utt_cuttoffmode_t;

/**
 * This is an implementation the Dragon4 algorithm to convert a binary number
 * in floating point format to a decimal number in string format. The function
 * returns the number of digits written to the output buffer and the output is
 * not NUL terminated.
 *
 * The floating point input value is (mantissa * 2^exponent).
 *
 * See the following papers for more information on the algorithm:
 *	"How to Print Floating-Point Numbers Accurately"
 *	  Steele and White
 *	  http://kurtstephens.com/files/p372-steele.pdf
 *	"Printing Floating-Point Numbers Quickly and Accurately"
 *	  Burger and Dybvig
 *	  http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.72.4656&rep=rep1&type=pdf
 */
UTT_LINKAGE utt_uint32_t utt_flt_dragon4(
	const utt_bigint_t*		mantissa,				/* value significand */
	const utt_int32_t		exponent,				/* value exponent in base 2 */
	const utt_uint32_t		mantissa_high_bit_idx,	/* index of the highest set mantissa bit */
	const bool			 	has_unequal_margins,		/* is the high margin twice as large as the low margin */
	const utt_cuttoffmode_t	cutoff_mode,				/* how to determine output length */
	utt_uint32_t			cutoff_number,			/* parameter to the selected cutoff_mode */
	char*					out_buffer,				/* buffer to output into */
	utt_uint32_t			buffer_size,				/* maximum characters that can be printed to out_buffer */
	utt_int32_t*			out_exponent			/* the base 10 exponent of the first digit */
){
	char * cur_digit = out_buffer;

	utt_assert( buffer_size > 0 );

	/* if the mantissa is zero, the value is zero regardless of the exponent */
	if( utt_bi_is_zero(mantissa) ){
		*cur_digit = '0';
		*out_exponent = 0;
		return 1;
	}
	
	/*
	 * compute the initial state in integral form such that
	 *	value	  = scaled_value / scale
	 *	margin_low = scaled_margin_low / scale
	 */
	utt_bigint_t scale;				/* positive scale applied to value and margin such that they can be represented as whole numbers */
	utt_bigint_t scaled_value;		/* scale * mantissa */
	utt_bigint_t scaled_margin_low;	/* scale * 0.5 * (distance between this floating-point number and its immediate lower value) */

	/*
	 * For normalized IEEE floating point values, each time the exponent is incremented the margin also
	 * doubles. That creates a subset of transition numbers where the high margin is twice the size of
	 * the low margin.
	 */
	utt_bigint_t* scaled_margin_high_ptr;
	utt_bigint_t optional_margin_high;

	if(	 has_unequal_margins )
	{
		/* if we have no fractional component */
		if( exponent > 0 )
		{
			/*
			 * 1) Expand the input value by multiplying out the mantissa and exponent. This represents
			 *	  the input value in its whole number representation.
			 * 2) Apply an additional scale of 2 such that later comparisons against the margin values
			 *	  are simplified.
			 * 3) Set the margin value to the lowest mantissa bit's scale.
			 */
			
			/* scaled_value		= 2 * 2 * mantissa*2^exponent */
			utt_bi_assign( &scaled_value , mantissa );
			utt_bi_shl( &scaled_value, exponent + 2 );

			/* scale			= 2 * 2 * 1 */
			utt_bi_set_32( &scale , 4 );

			/* scaled_margin_low	= 2 * 2^(exponent-1) */
			utt_bi_pow2( &scaled_margin_low, exponent );

			/* scaled_margin_high = 2 * 2 * 2^(exponent-1) */
			utt_bi_pow2( &optional_margin_high, exponent + 1 );
		}
		/* else we have a fractional exponent */
		else
		{
			/* In order to track the mantissa data as an integer, we store it as is with a large scale */

			/* scaled_value		= 2 * 2 * mantissa */
			utt_bi_assign( &scaled_value , mantissa );
			utt_bi_shl( &scaled_value , 2 );

			/* scale			= 2 * 2 * 2^(-exponent) */
			utt_bi_pow2(&scale, -exponent + 2 );

			/* scaled_margin_low	= 2 * 2^(-1) */
			utt_bi_set_32( &scaled_margin_low , 1 );

			/* scaled_margin_high = 2 * 2 * 2^(-1) */
			utt_bi_set_32( &optional_margin_high , 2 );
		}

		/* the high and low margins are different */
		scaled_margin_high_ptr = &optional_margin_high;
	}
	else
	{
		/* if we have no fractional component */
		if( exponent > 0 )
		{
			/* 1) Expand the input value by multiplying out the mantissa and exponent. This represents */
			/*	  the input value in its whole number representation. */
			/* 2) Apply an additional scale of 2 such that later comparisons against the margin values */
			/*	  are simplified. */
			/* 3) Set the margin value to the lowest mantissa bit's scale. */

			/* scaled_value	   = 2 * mantissa*2^exponent */
			utt_bi_assign( &scaled_value , mantissa );
			utt_bi_shl( &scaled_value, exponent + 1 );

			/* scale		   = 2 * 1 */
			utt_bi_set_32( &scale , 2 );

			/* scaled_margin_low = 2 * 2^(exponent-1) */
			utt_bi_pow2( &scaled_margin_low, exponent );
		}
		/* else we have a fractional exponent */
		else
		{
			/* In order to track the mantissa data as an integer, we store it as is with a large scale */

			/* scaled_value	   = 2 * mantissa */
			utt_bi_assign( &scaled_value , mantissa );
			utt_bi_shl( &scaled_value , 1 );

			/* scale		   = 2 * 2^(-exponent) */
			utt_bi_pow2(&scale, -exponent + 1 );

			/* scaled_margin_low = 2 * 2^(-1) */
			utt_bi_set_32( &scaled_margin_low , 1 );
		}

		/* the high and low margins are equal */
		scaled_margin_high_ptr = &scaled_margin_low;
	}

	/*
	 * Compute an estimate for digit_exponent that will be correct or undershoot by one.
	 * This optimization is based on the paper "Printing Floating-Point Numbers Quickly and Accurately"
	 * by Burger and Dybvig http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.72.4656&rep=rep1&type=pdf
	 * We perform an additional subtraction of 0.69 to increase the frequency of a failed estimate
	 * because that lets us take a faster branch in the code. 0.69 is chosen because 0.69 + log10(2) is
	 * less than one by a reasonable epsilon that will account for any floating point error.
	 *
	 * We want to set digit_exponent to floor(log10(v)) + 1
	 *	v = mantissa*2^exponent
	 *	log2(v) = log2(mantissa) + exponent;
	 *	log10(v) = log2(v) * log10(2)
	 *	floor(log2(v)) = mantissa_high_bit_idx	 + exponent;
	 *	log10(v) - log10(2) < (mantissa_high_bit_idx	 + exponent) * log10(2) <= log10(v)
	 *	log10(v) < (mantissa_high_bit_idx	 + exponent) * log10(2) + log10(2) <= log10(v) + log10(2)
	 *	floor( log10(v) ) < ceil( (mantissa_high_bit_idx	 + exponent) * log10(2) ) <= floor( log10(v) ) + 1
	 */
	const utt_ieee754_double log10_2 = 0.30102999566398119521373889472449;
	utt_int32_t digit_exponent = (utt_int32_t)utt_ceil( (utt_ieee754_double)((utt_int32_t)mantissa_high_bit_idx	 + exponent) * log10_2 - 0.69);
	
	/*
	 * if the digit exponent is smaller than the smallest desired digit for fractional cutoff,
	 * pull the digit back into legal range at which point we will round to the appropriate value.
	 * Note that while our value for digit_exponent is still an estimate, this is safe because it
	 * only increases the number. This will either correct digit_exponent to an accurate value or it
	 * will clamp it above the accurate value.
	 */
	if( cutoff_mode == UTT_CUTOFFMODE_FRACTIONLENGTH && digit_exponent <= -(utt_int32_t)cutoff_number )
		digit_exponent = -(utt_int32_t)cutoff_number + 1;

	/* Divide value by 10^digit_exponent. */
	if( digit_exponent > 0 )
	{
		/* The exponent is positive creating a division so we multiply up the scale. */
		utt_bigint_t temp;
		utt_bi_mutliply_pow10( &temp, &scale, digit_exponent );
		scale = temp;
	}
	else if( digit_exponent < 0 )
	{
		/* The exponent is negative creating a multiplication so we multiply up the scaled_value, scaled_margin_low and scaled_margin_high. */
		utt_bigint_t pow10;
		utt_bi_pow10( &pow10, -digit_exponent );

		utt_bigint_t temp;
		utt_bi_mutliply( &temp, &scaled_value, &pow10 );
		scaled_value = temp;

		utt_bi_mutliply( &temp, &scaled_margin_low, &pow10 );
		scaled_margin_low = temp;

		if( scaled_margin_high_ptr != &scaled_margin_low )
			utt_bi_times2( scaled_margin_high_ptr, &scaled_margin_low );
	}

	/* If (value >= 1), our estimate for digit_exponent was too low */
	if( utt_bi_compare(&scaled_value,&scale) >= 0 )
	{
		/*
		 * The exponent estimate was incorrect.
		 * Increment the exponent and don't perform the premultiply needed
		 * for the first loop iteration.
		 */
		digit_exponent = digit_exponent + 1;
	}
	else
	{
		/*
		 * The exponent estimate was correct.
		 * Multiply larger by the output base to prepare for the first loop iteration.
		 */
		utt_bi_times10( &scaled_value );
		utt_bi_times10( &scaled_margin_low );
		if( scaled_margin_high_ptr != &scaled_margin_low )
			utt_bi_times2( scaled_margin_high_ptr, &scaled_margin_low );
	}

	/* Compute the cutoff exponent (the exponent of the final digit to print). Default to the maximum size of the output buffer. */
	utt_int32_t cutoff_exponent = digit_exponent - buffer_size;
	switch(cutoff_mode)
	{
	/* print digits until we pass the accuracy margin limits or buffer size */
	case UTT_CUTOFFMODE_UNIQUE:
		break;

	/* print cutoff_number of digits or until we reach the buffer size */
	case UTT_CUTOFFMODE_TOTALLENGTH:
		{
			utt_int32_t desired_cutoff_exponent = digit_exponent - (utt_int32_t)cutoff_number;
			if( desired_cutoff_exponent > cutoff_exponent)
				cutoff_exponent = desired_cutoff_exponent;
		}
		break;

	/* print cutoff_number digits past the decimal point or until we reach the buffer size */
	case UTT_CUTOFFMODE_FRACTIONLENGTH:
		{
			utt_int32_t desired_cutoff_exponent = -(utt_int32_t)cutoff_number;
			if( desired_cutoff_exponent > cutoff_exponent)
				cutoff_exponent = desired_cutoff_exponent;
		}
		break;
	}

	/* Output the exponent of the first digit we will print */
	*out_exponent = digit_exponent-1;

	/*
	 * In preparation for calling utt_bi_divmod_maxquot9(),
	 * we need to scale up our values such that the highest block of the denominator
	 * is greater than or equal to 8. We also need to guarantee that the numerator
	 * can never have a length greater than the denominator after each loop iteration.
	 * This requires the highest block of the denominator to be less than or equal to
	 * 429496729 which is the highest number that can be multiplied by 10 without
	 * overflowing to a new block.
	 */
	utt_assert( scale.length > 0 );
	utt_uint32_t high_block = scale.blocks[ scale.length - 1 ];
	if( high_block < 8 || high_block > 429496729 )
	{
		/*
		 * Perform a bit shift on all values to get the highest block of the denominator into
		 * the range [8,429496729]. We are more likely to make accurate quotient estimations
		 * in utt_bi_divmod_maxquot9() with higher denominator values so
		 * we shift the denominator to place the highest bit at index 27 of the highest block.
		 * This is safe because (2^28 - 1) = 268435455 which is less than 429496729. This means
		 * that all values with a highest bit at index 27 are within range.
		 */
		utt_uint32_t high_block_log2 = utt_log2_32(high_block);
		utt_uint32_t shift = (32 + 27 - high_block_log2) % 32;

		utt_bi_shl( &scale, shift );
		utt_bi_shl( &scaled_value, shift );
		utt_bi_shl( &scaled_margin_low, shift );
		if( scaled_margin_high_ptr != &scaled_margin_low )
			utt_bi_times2( scaled_margin_high_ptr, &scaled_margin_low );
	}

	/* These values are used to inspect why the print loop terminated so we can properly round the final digit. */
	bool			low;			/* did the value get within margin_low distance from zero */
	bool			high;			/* did the value get within margin_high distance from one */
	utt_uint32_t	output_digit;	/* current digit being output */

	if( cutoff_mode == UTT_CUTOFFMODE_UNIQUE )
	{
		/*
		 * For the unique cutoff mode, we will try to print until we have reached a level of
		 * precision that uniquely distinguishes this value from its neighbors. If we run
		 * out of space in the output buffer, we terminate early.
		 */
		while(true)
		{
			digit_exponent = digit_exponent-1;

			/* divide out the scale to extract the digit */
			output_digit = utt_bi_divmod_maxquot9(&scaled_value, &scale);
			utt_assert( output_digit < 10 );

			/* update the high end of the value */
			utt_bigint_t scaled_value_high;
			utt_bi_sum( &scaled_value_high, &scaled_value, scaled_margin_high_ptr );

			/* stop looping if we are far enough away from our neighboring values or if we have reached the cutoff digit */
			low = utt_bi_compare(&scaled_value, &scaled_margin_low) < 0;
			high = utt_bi_compare(&scaled_value_high, &scale) > 0;
			if( low | high | (digit_exponent == cutoff_exponent) )
				break;

			/* store the output digit */
			*cur_digit = (char)('0' + output_digit);
			++cur_digit;

			/* multiply larger by the output base */
			utt_bi_times10( &scaled_value );
			utt_bi_times10( &scaled_margin_low );
			if( scaled_margin_high_ptr != &scaled_margin_low)
				utt_bi_times2( scaled_margin_high_ptr, &scaled_margin_low );
		}
	}
	else
	{
		/*
		 * For length based cutoff modes, we will try to print until we
		 * have exhausted all precision (i.e. all remaining digits are zeros) or
		 * until we reach the desired cutoff digit.
		 */
		low = false;
		high = false;

		for( ;;)
		{
			digit_exponent = digit_exponent-1;

			/* divide out the scale to extract the digit */
			output_digit = utt_bi_divmod_maxquot9(&scaled_value, &scale);
			utt_assert( output_digit < 10 );

			if(	 utt_bi_is_zero( &scaled_value ) | (digit_exponent == cutoff_exponent) )
				break;

			/* store the output digit */
			*cur_digit = (char)('0' + output_digit);
			++cur_digit;

			/* multiply larger by the output base */
			utt_bi_times10(&scaled_value);
		}
	}

	/* Round off the final digit. Default to rounding down if value got too close to 0 */
	bool round_down = low;

	/* If it is legal to round up and down */
	if( low == high )
	{
		/*
		 * round to the closest digit by comparing value with 0.5. To do this we need to convert
		 * the inequality to large integer values.
		 *	compare( value, 0.5 )
		 *	compare( scale * value, scale * 0.5 )
		 *	compare( 2 * scale * value, scale )
		 */
		utt_bi_double(&scaled_value);
		utt_int32_t compare = utt_bi_compare(&scaled_value, &scale);
		round_down = compare < 0;

		/* if we are directly in the middle, round towards the even digit (i.e. IEEE rouding rules) */
		if( compare == 0)
			round_down = (output_digit & 1) == 0;
	}

	/* print the rounded digit */
	if( round_down ){
		*cur_digit = (char)('0' + output_digit);
		++cur_digit;
	}
	else
	{
		/* handle rounding up */
		if( output_digit == 9)
		{
			/* find the first non-nine prior digit */
			while(true)
			{
				/* if we are at the first digit */
				if( cur_digit == out_buffer ){
					/* output 1 at the next highest exponent */
					*cur_digit = '1';
					++cur_digit;
					*out_exponent += 1;
					break;
				}

				--cur_digit;
				if( *cur_digit != '9' ){
					/* increment the digit */
					*cur_digit += 1;
					++cur_digit;
					break;
				}
			}
		}
		else{
			/* values in the range [0,8] can perform a simple round up */
			*cur_digit = (char)('0' + output_digit + 1);
			++cur_digit;
		}
	}

	/* return the number of digits output */
	utt_uint32_t output_len = (utt_uint32_t)(cur_digit - out_buffer);
	utt_assert(output_len <= buffer_size);
	
	return output_len;
}

#endif
