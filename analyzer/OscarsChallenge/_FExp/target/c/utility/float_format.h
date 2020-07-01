/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_FLOAT_FORMAT_H_
#define _TARGET_UNITTEST_UTILITY_FLOAT_FORMAT_H_

#include <target/c/utility/dragon4.h>
#include <target/c/stdint.h>
#include <target/c/string.h>

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


/** Format in which to print the utt_ieee754_float */
typedef enum{
	UTT_FLOAT_FMT_POSITIONAL,	/* [-]ddddd.dddd */
	UTT_FLOAT_FMT_SCIENTIFIC,	/* [-]d.dddde[sign]ddd */
	UTT_FLOAT_FMT_MIXED,	 	/* mix between the above */
} utt_float_fmt_t;


/**
 * Outputs the positive number with positional notation: ddddd.dddd
 * The output is always NUL terminated and the output length (not including the
 * NUL) is returned.
 */
UTT_LINKAGE utt_uint32_t utt_flt_format_positional(
	char*				out_buffer,				/* buffer to output into */
	utt_uint32_t		buffer_size,			/* maximum characters that can be printed to out_buffer */
	const utt_bigint_t*	mantissa,				/* value significand */
	utt_int32_t			exponent,				/* value exponent in base 2 */
	utt_uint32_t		mantissa_high_bit_idx,	/* index of the highest set mantissa bit */
	bool				has_unequal_margins,	/* is the high margin twice as large as the low margin */
	utt_int32_t			precision,				/* Negative prints as many digits as are needed for a unique */
												/*	number. Positive specifies the maximum number of */
												/*	significant digits to print past the decimal point. */
	const char*			alphabet				/* Alphabet to use, expected order: ".0123456789abcdefghijklmnopqrstuvwxyz" */
){
	utt_assert(buffer_size > 0);

	utt_int32_t		print_exponent;
	utt_uint32_t	num_print_digits;
	utt_uint32_t	max_print_len = buffer_size - 1;

	/* Call the Dragon4 Implementation */
	num_print_digits = utt_flt_dragon4(
		mantissa
		, exponent
		, mantissa_high_bit_idx
		, has_unequal_margins
		, precision < 0 ? UTT_CUTOFFMODE_UNIQUE : UTT_CUTOFFMODE_FRACTIONLENGTH
		, precision < 0 ? 0 : precision
		, out_buffer
		, max_print_len
		, &print_exponent
	);

	utt_assert( num_print_digits > 0 );
	utt_assert( num_print_digits <= buffer_size );

	/* track the number of digits past the decimal point that have been printed */
	utt_uint32_t num_fraction_digits = 0;

	/* if output has a whole number */
	if( print_exponent >= 0)
	{
		/* leave the whole number at the start of the buffer */
		utt_uint32_t num_whole_digits = print_exponent+1;
		if( num_print_digits < num_whole_digits)
		{
			/* don't overflow the buffer */
			if( num_whole_digits > max_print_len)
				num_whole_digits = max_print_len;

			/* add trailing zeros up to the decimal point */
			for(  ; num_print_digits < num_whole_digits; ++num_print_digits )
				out_buffer[num_print_digits] = alphabet[ 1 ]; /* '0' in the Alphabet */
		}
		/* insert the decimal point prior to the fraction */
		else if( num_print_digits > (utt_uint32_t)num_whole_digits)
		{
			num_fraction_digits = num_print_digits - num_whole_digits;
			utt_uint32_t max_fraction_digits = max_print_len - num_whole_digits - 1;
			if( num_fraction_digits > max_fraction_digits)
				num_fraction_digits = max_fraction_digits;

			utt_memmove(out_buffer + num_whole_digits + 1, out_buffer + num_whole_digits, num_fraction_digits);
			out_buffer[num_whole_digits] = alphabet[ 0 ]; /* decimal point in the Alphabet */
			num_print_digits = num_whole_digits + 1 + num_fraction_digits;
		}
	}
	else
	{
		/* shift out the fraction to make room for the leading zeros */
		if( max_print_len > 2)
		{
			utt_uint32_t num_fraction_zeros = (utt_uint32_t)-print_exponent - 1;
			utt_uint32_t max_fraction_zeros = max_print_len - 2;
			if( num_fraction_zeros > max_fraction_zeros)
				num_fraction_zeros = max_fraction_zeros;

			utt_uint32_t digits_start_idx = 2 + num_fraction_zeros;

			/* shift the significant digits right such that there is room for leading zeros */
			num_fraction_digits = num_print_digits;
			utt_uint32_t max_fraction_digits = max_print_len - digits_start_idx;
			if( num_fraction_digits > max_fraction_digits)
				num_fraction_digits = max_fraction_digits;

			utt_memmove(out_buffer + digits_start_idx, out_buffer, num_fraction_digits);

			/* insert the leading zeros */
			for( utt_uint32_t i = 2; i < digits_start_idx; ++i)
				out_buffer[i] = alphabet[ 1 ]; /* '0' in the Alphabet */

			/* update the counts */
			num_fraction_digits += num_fraction_zeros;
			num_print_digits = num_fraction_digits;
		}

		/* add the decimal point */
		if( max_print_len > 1)
		{
			out_buffer[1] = alphabet[ 0 ]; /* decimal point in the Alphabet */
			num_print_digits += 1;
		}

		/* add the initial zero */
		if( max_print_len > 0)
		{
			out_buffer[0] = alphabet[ 1 ]; /* '0' in the Alphabet */
			num_print_digits += 1;
		}
	}

	/* add trailing zeros up to precision length */
	if( precision > (utt_int32_t)num_fraction_digits && num_print_digits < max_print_len)
	{
		/* add a decimal point if this is the first fractional digit we are printing */
		if( num_fraction_digits == 0 )
			out_buffer[num_print_digits++] = alphabet[ 0 ]; /* decimal point in the Alphabet */

		/* compute the number of trailing zeros needed */
		utt_uint32_t total_digits = num_print_digits + (precision - num_fraction_digits);
		if( total_digits > max_print_len)
			total_digits = max_print_len;

		for(  ; num_print_digits < total_digits; ++num_print_digits )
			out_buffer[num_print_digits] = alphabet[ 1 ]; /* '0' in the Alphabet */
	}

	/* terminate the buffer */
	utt_assert( num_print_digits <= max_print_len );
	out_buffer[num_print_digits] = '\0';

	return num_print_digits;
}

/**
 * Outputs the positive number with scientific notation: d.dddde[sign]ddd
 * The output is always NUL terminated and the output length (not including the
 * NUL) is returned.
 */
UTT_LINKAGE utt_uint32_t utt_flt_format_scientific(
	char*				out_buffer,				/* buffer to output into */
	utt_uint32_t		buffer_size,			/* maximum characters that can be printed to out_buffer */
	const utt_bigint_t*	mantissa,				/* value significand */
	utt_int32_t			exponent,				/* value exponent in base 2 */
	utt_uint32_t		mantissa_high_bit_idx,	/* index of the highest set mantissa bit */
	bool				has_unequal_margins,	/* is the high margin twice as large as the low margin */
	utt_int32_t			precision,				/* Negative prints as many digits as are needed for a unique */
												/*	number. Positive specifies the maximum number of */
												/*	significant digits to print past the decimal point. */
	const char*			alphabet				/* Alphabet to use, expected order: ".0123456789abcdefghijklmnopqrstuvwxyz" */
){
	utt_assert(buffer_size > 0);

	utt_int32_t print_exponent;
	utt_uint32_t num_print_digits;

	/* Call the Dragon4 Implementation */
	num_print_digits = utt_flt_dragon4(
		mantissa
		, exponent
		, mantissa_high_bit_idx
		, has_unequal_margins
		, precision < 0 ? UTT_CUTOFFMODE_UNIQUE : UTT_CUTOFFMODE_TOTALLENGTH
		, precision < 0 ? 0 : precision + 1
		, out_buffer
		, buffer_size
		, &print_exponent
	);

	utt_assert( num_print_digits > 0 );
	utt_assert( num_print_digits <= buffer_size );

	char* cur_out = out_buffer;

	/* keep the whole number as the first digit */
	if( buffer_size > 1){
		cur_out += 1;
		buffer_size -= 1;
	}

	/* insert the decimal point prior to the fractional number */
	utt_uint32_t num_fraction_digits = num_print_digits-1;
	if( num_fraction_digits > 0 && buffer_size > 1)
	{
		utt_uint32_t max_fraction_digits = buffer_size-2;
		if( num_fraction_digits > max_fraction_digits)
			num_fraction_digits =	 max_fraction_digits;

		utt_memmove(cur_out + 1 , cur_out , num_fraction_digits);
		cur_out[0] = alphabet[ 0 ]; /* decimal point in the Alphabet */
		cur_out += (1 + num_fraction_digits);
		buffer_size -= (1 + num_fraction_digits);
	}

	/* add trailing zeros up to precision length */
	if( precision > (utt_int32_t)num_fraction_digits && buffer_size > 1)
	{
		/* add a decimal point if this is the first fractional digit we are printing */
		if( num_fraction_digits == 0)
		{
			*cur_out = alphabet[ 0 ]; /* decimal point in the Alphabet */
			++cur_out;
			--buffer_size;
		}

		/* compute the number of trailing zeros needed */
		utt_uint32_t num_zeros = (precision - num_fraction_digits);
		if( num_zeros > buffer_size-1)
			num_zeros = buffer_size-1;

		for( char * end_ptr = cur_out + num_zeros; cur_out < end_ptr; ++cur_out )
			*cur_out = alphabet[ 1 ]; /* '0' in the Alphabet */
	}

	/* print the exponent into a local buffer and copy into output buffer */
	if( buffer_size > 1 )
	{
		char		exponent_buffer[6];
		utt_uint8_t exponent_buffer_size = 0;
		exponent_buffer[exponent_buffer_size++] = alphabet[ 15 ]; /* 'e' in the Alphabet */
		if( print_exponent >= 0 )
			exponent_buffer[exponent_buffer_size++] = '+';
		else{
			exponent_buffer[exponent_buffer_size++] = '-';
			print_exponent = -print_exponent;
		}

		utt_assert(print_exponent < 10000);
		utt_uint32_t thousands_digit	= print_exponent / 1000;
		utt_uint32_t hundreds_digit		= (print_exponent - thousands_digit*1000) / 100;
		utt_uint32_t tens_digit			= (print_exponent - thousands_digit*1000 - hundreds_digit*100) / 10;
		utt_uint32_t ones_digit			= (print_exponent - thousands_digit*1000 - hundreds_digit*100 - tens_digit*10);
		
		if( thousands_digit )
			exponent_buffer[exponent_buffer_size++] = alphabet[ 1 + thousands_digit ]; /* Convert number to digit */
		if( hundreds_digit )
			exponent_buffer[exponent_buffer_size++] = alphabet[ 1 + hundreds_digit ]; /* Convert number to digit */
		exponent_buffer[exponent_buffer_size++] = alphabet[ 1 + tens_digit ]; /* Convert number to digit */
		exponent_buffer[exponent_buffer_size++] = alphabet[ 1 + ones_digit ]; /* Convert number to digit */

		/* copy the exponent buffer into the output */
		utt_uint32_t max_exponent_size = buffer_size-1;
		utt_uint32_t exponent_size = (exponent_buffer_size < max_exponent_size) ? exponent_buffer_size : max_exponent_size;
		utt_memcpy( cur_out , exponent_buffer , exponent_size );

		cur_out += exponent_size;
		buffer_size -= exponent_size;
	}

	utt_assert( buffer_size > 0 );
	cur_out[0] = '\0';

	return cur_out - out_buffer;
}

/**
 * Print a hexadecimal value with a given width.
 * The output string is always NUL terminated and the string length (not
 * including the NUL) is returned.
 */
static void utt_flt_print_hex(char * out_buffer , const utt_bigint_t* value , utt_uint32_t num_hex_digits , const char* alphabet )
{	
	if( utt_bi_is_zero( value ) ){
		*out_buffer = '0';
		return;
	}
	
	/* Output digits from low to high */
	utt_bigint_t	copy;
	utt_bi_assign( &copy , value );
	
	do{
		out_buffer[--num_hex_digits] = alphabet[ ( copy.blocks[0] & 0xF ) + 1 ];
		utt_bi_shr( &copy , 4 );
	}while( !utt_bi_is_zero( &copy ) );
}

/**
 * Print special case values for infinities and NaNs.
 * The output string is always NUL terminated and the string length (not
 * including the NUL) is returned.
 */
static utt_uint32_t utt_flt_print_special(char * out_buffer , utt_uint32_t buffer_size , const utt_bigint_t* mantissa , const char* alphabet )
{
	utt_assert(buffer_size > 0);

	utt_uint32_t max_print_len = buffer_size-1;

	/* Check for infinity */
	if( utt_bi_is_zero(mantissa) ){
		/* copy and make sure the buffer is terminated */
		utt_uint32_t print_len = (8 < max_print_len) ? 8 : max_print_len;
		char infinity_str[8] = { alphabet[ 19 ] , alphabet[ 24 ] , alphabet[ 16 ] , alphabet[ 19 ] , alphabet[ 24 ] , alphabet[ 19 ] , alphabet[ 30 ] , alphabet[ 35 ] };
		utt_memcpy( out_buffer , infinity_str , print_len );
		out_buffer[print_len] = '\0';
		return print_len;
	}
	else{
		/* copy and make sure the buffer is terminated */
		utt_uint32_t print_len = (3 < max_print_len) ? 3 : max_print_len;
		char nan_str[3] = { alphabet[ 24 ] , alphabet[ 11 ] , alphabet[ 24 ] };
		utt_memcpy( out_buffer , nan_str , print_len );
		
		/* append opening bracket */
		if( max_print_len > 3 )
			out_buffer[print_len++] = '(';
		
		/* append HEX value */
		utt_uint32_t num_hex_digits = utt_bi_log2(mantissa) / 4 + 1;
		if( max_print_len > num_hex_digits ){
			utt_flt_print_hex( out_buffer+print_len , mantissa , num_hex_digits , alphabet );
			print_len += num_hex_digits;
		}
		
		/* append closing bracket */
		if( print_len < max_print_len )
			out_buffer[print_len++] = ')';
		
		out_buffer[print_len] = '\0';
		
		return print_len;
	}
}

#endif
