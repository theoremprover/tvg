/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGFLOAT_PRINT_H_
#define _TARGET_UNITTEST_UTILITY_BIGFLOAT_PRINT_H_

#include <target/c/utility/float_format.h>
#include <target/c/bigfloat/convenience.h>

/**
 * Print a 64-bit floating-point number as a decimal string.
 * The output string is always NUL terminated and the string length (not
 * including the NUL) is returned.
 */
UTT_LINKAGE utt_uint32_t utt_flt_printm
(
	char*				out_buffer,		/* buffer to output into */
	utt_uint32_t		buffer_size,	/* size of out_buffer */
	utt_bigfloat_t		value,			/* value to print */
	utt_float_fmt_t		format,			/* format to print with */
	utt_int32_t			precision,		/* If negative, the minimum number of digits to represent a */
										/* unique 64-bit floating point value is output. Otherwise, */
										/* this is the number of digits to print past the decimal point. */
	const char*			alphabet		/* Alphabet to use, expected order: ".0123456789abcdefghijklmnopqrstuvwxyz" */
){
	if( buffer_size == 0)
		return 0;

	if( buffer_size == 1){
		out_buffer[0] = '\0';
		return 0;
	}

	/* deconstruct the floating point value */
	utt_uint32_t	float_exponent = utt_exponentm( value );
	utt_bigint_t	float_mantissa = utt_mantissam( value );
	utt_uint32_t	prefix_length = 0;

	/* output the sign */
	if( utt_signbitm( value ) ){
		out_buffer[0] = '-';
		++out_buffer;
		--buffer_size;
		++prefix_length;
		utt_assert(buffer_size > 0);
	}

	/* if this is a special value */
	if( float_exponent == UTT_MP_FLOAT_EXPONENT_MAX )
		return utt_flt_print_special( out_buffer , buffer_size , &float_mantissa , alphabet ) + prefix_length;
	/* else this is a number */
	else
	{
		/* factor the value into its parts */
		utt_bigint_t	mantissa;
		utt_int32_t		exponent;
		utt_uint32_t	mantissa_high_bit_idx;
		bool has_unequal_margins;

		if( float_exponent != 0 )
		{
			/* Normal Values */
			/*
			 * The floating point equation is:
			 *  value = (1 + mantissa/2^52) * 2 ^ (exponent-1023)
			 * We convert the integer equation by factoring a 2^52 out of the exponent
			 *	value = (1 + mantissa/2^52) * 2^52 * 2 ^ (exponent-1023-52)
			 *	value = (2^52 + mantissa) * 2 ^ (exponent-1023-52)
			 * Because of the implied 1 in front of the mantissa we have 53 bits of precision.
			 *	 m = (2^52 + mantissa)
			 *	 e = (exponent-1023+1-53)
			 */
			
			/* Compute the value of the non-stored '1' */
			utt_bigint_t	leading_one;
			utt_bi_set_32( &leading_one , 1 );
			utt_bi_shl( &leading_one , UTT_MP_FLOAT_MANTISSA_BYTES * 8 );
			
			utt_bi_sum( &mantissa ,	&float_mantissa , &leading_one );
			exponent				= float_exponent - UTT_EXPONENT_BIASM - (UTT_MP_FLOAT_MANTISSA_BYTES * 8);
			mantissa_high_bit_idx		= (UTT_MP_FLOAT_MANTISSA_BYTES * 8);
			has_unequal_margins		= (float_exponent != 1) && utt_bi_is_zero(&float_mantissa);
		}
		else
		{
			/* Denormalized Values */
			/*
			 * The floating point equation is:
			 *	value = (mantissa/2^52) * 2 ^ (1-1023)
			 * We convert the integer equation by factoring a 2^52 out of the exponent
			 *	value = (mantissa/2^52) * 2^52 * 2 ^ (1-1023-52)
			 *	value = mantissa * 2 ^ (1-1023-52)
			 * We have up to 52 bits of precision.
			 *	 m = (mantissa)
			 *	 e = (1-1023-52)
			 */
			utt_bi_assign( &mantissa ,	&float_mantissa );
			exponent					= 1 - UTT_EXPONENT_BIASM - (UTT_MP_FLOAT_MANTISSA_BYTES * 8);
			mantissa_high_bit_idx			= utt_bi_log2(&mantissa);
			has_unequal_margins			= false;
		}
		
		/* Compute formatting? */
		if( format == UTT_FLOAT_FMT_MIXED )
		{
			utt_int32_t		print_exponent;
		
			/* Determine the printed exponent */
			utt_int32_t print_digits = utt_flt_dragon4(
				&mantissa
				, exponent
				, mantissa_high_bit_idx
				, has_unequal_margins
				, precision < 0 ? UTT_CUTOFFMODE_UNIQUE : UTT_CUTOFFMODE_TOTALLENGTH
				, precision < 0 ? 0 : 1 /* precision */
				, out_buffer
				, buffer_size - 1
				, &print_exponent
			);
			
			if( precision < 0 )
			{
				/* Catch number x that needs fewer digits than log10(x) to represent exactly */
				if( print_exponent > 0 && print_exponent >= print_digits )
					print_digits = print_exponent + 1;
				
				if( precision < -print_digits ) /* abs(precision) > print_digits */
					precision = print_digits; /* Don't use up all the precision */
				else
					precision = -precision;
			}
			if( precision > print_exponent && print_exponent >= -4 )
				format = UTT_FLOAT_FMT_POSITIONAL, precision = precision - 1 - print_exponent;
			else
				format = UTT_FLOAT_FMT_SCIENTIFIC, precision -= 1;
		}
		
		/* format the value */
		switch( format )
		{
		case UTT_FLOAT_FMT_POSITIONAL:
			return
				utt_flt_format_positional(
					out_buffer
					, buffer_size
					, &mantissa
					, exponent
					, mantissa_high_bit_idx
					, has_unequal_margins
					, precision
					, alphabet
				)
				+ prefix_length
			;
		case UTT_FLOAT_FMT_SCIENTIFIC:
			return
				utt_flt_format_scientific(
					out_buffer
					, buffer_size
					, &mantissa
					, exponent
					, mantissa_high_bit_idx
					, has_unequal_margins
					, precision
					, alphabet
				)
				+ prefix_length
			;
		default:
			out_buffer[0] = '\0';
			return 0;
		}
	}
}
/** Function to print a utt_bigfloat_t */
UTT_LINKAGE void utt_flt_putm( utt_bigfloat_t val ){
	char	memory[100];
	utt_flt_printm( memory , 100 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
	utt_putstr( memory );
}
UTT_LINKAGE utt_size_t utt_flt_putm_buf( utt_bigfloat_t val , char* buffer ){
	return utt_flt_printm( buffer , 100 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
}


#endif
