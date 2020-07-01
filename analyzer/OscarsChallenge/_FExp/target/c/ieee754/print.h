/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE754_PRINT_H_
#define _TARGET_UNITTEST_IEEE754_PRINT_H_

#include <target/c/utility/float_format.h>
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

/**
 * Print a 64-bit floating-point number as a decimal string.
 * The output string is always NUL terminated and the string length (not
 * including the NUL) is returned.
 */
UTT_LINKAGE utt_uint32_t utt_flt_print(
	char*				out_buffer,		/* buffer to output into */
	utt_uint32_t		buffer_size,	/* size of out_buffer */
	utt_ieee754_double	value,			/* value to print */
	utt_float_fmt_t		format,			/* format to print with */
	utt_int32_t			precision,		/* This is the |number| of digits to print past the decimal point. */
										/* If negative, the output will not contain more digits than absolutely */
										/* necessary to uniquely represent the floating point value */
	const char*			alphabet		/* Alphabet to use, expected order: ".0123456789abcdefghijklmnopqrstuvwxyz" */
){
	if( buffer_size == 0)
		return 0;

	if( buffer_size == 1){
		out_buffer[0] = '\0';
		return 0;
	}

	/* deconstruct the floating point value */
	utt_uint32_t	float_exponent = utt_exponent( value );
	utt_uint64_t	float_mantissa = utt_mantissa( value );
	utt_uint32_t	prefix_length = 0;

	/* output the sign */
	if( utt_signbit( value ) ){
		out_buffer[0] = '-';
		++out_buffer;
		--buffer_size;
		++prefix_length;
		utt_assert(buffer_size > 0);
	}

	/* if this is a special value */
	if( float_exponent == ( UTT_EXPONENT_MASK >> UTT_EXPONENT_SHIFT ) ){
		utt_bigint_t mantissa;
		utt_bi_set_64( &mantissa , float_mantissa );
		return utt_flt_print_special( out_buffer , buffer_size , &mantissa , alphabet ) + prefix_length;
	}
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
			utt_bi_set_64( &mantissa , 	(1ull << 52) | float_mantissa );
			exponent					= float_exponent - 1023 - 52;
			mantissa_high_bit_idx		= 52;
			has_unequal_margins			= (float_exponent != 1) && (float_mantissa == 0);
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
			utt_bi_set_64( &mantissa , 	float_mantissa );
			exponent					= 1 - 1023 - 52;
			mantissa_high_bit_idx		= utt_log2_64(float_mantissa);
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



/**
 * Print a 32-bit floating-point number as a decimal string.
 * The output string is always NUL terminated and the string length (not
 * including the NUL) is returned.
 */
UTT_LINKAGE utt_uint32_t utt_flt_printf(
	char*				out_buffer,		/* buffer to output into */
	utt_uint32_t		buffer_size,	/* size of out_buffer */
	utt_ieee754_float	value,			/* value to print */
	utt_float_fmt_t		format,			/* format to print with */
	utt_int32_t			precision,		/* This is the |number| of digits to print past the decimal point. */
										/* If negative, the output will not contain more digits than absolutely */
										/* necessary to uniquely represent the floating point value */
	const char*			alphabet		/* Alphabet to use, expected order: ".0123456789abcdefghijklmnopqrstuvwxyz" */
){
	if( buffer_size == 0)
		return 0;

	if( buffer_size == 1){
		out_buffer[0] = '\0';
		return 0;
	}

	/* deconstruct the floating point value */
	utt_uint32_t float_exponent = utt_exponentf( value );
	utt_uint32_t float_mantissa = utt_mantissaf( value );
	utt_uint32_t prefix_length = 0;

	/* output the sign */
	if( utt_signbitf( value ) )
	{
		out_buffer[0] = '-';
		++out_buffer;
		--buffer_size;
		++prefix_length;
		utt_assert(buffer_size > 0);
	}

	/* if this is a special value */
	if( float_exponent == ( UTT_EXPONENT_MASKF >> UTT_EXPONENT_SHIFTF ) ){
		utt_bigint_t	mantissa;
		utt_bi_set_32( &mantissa , float_mantissa );
		return utt_flt_print_special( out_buffer , buffer_size , &mantissa , alphabet ) + prefix_length;
	}
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
			 *	value = (1 + mantissa/2^23) * 2 ^ (exponent-127)
			 * We convert the integer equation by factoring a 2^23 out of the exponent
			 *	value = (1 + mantissa/2^23) * 2^23 * 2 ^ (exponent-127-23)
			 *	value = (2^23 + mantissa) * 2 ^ (exponent-127-23)
			 * Because of the implied 1 in front of the mantissa we have 24 bits of precision.
			 *	 m = (2^23 + mantissa)
			 *	 e = (exponent-127-23)
			 */
			utt_bi_set_32( &mantissa , 	(1UL << 23) | float_mantissa );
			exponent					= float_exponent - 127 - 23;
			mantissa_high_bit_idx		= 23;
			has_unequal_margins			= (float_exponent != 1) && (float_mantissa == 0);
		}
		else
		{
			/* Denormalized Values */
			/*
			 * The floating point equation is:
			 *	value = (mantissa/2^23) * 2 ^ (1-127)
			 * We convert the integer equation by factoring a 2^23 out of the exponent
			 *	value = (mantissa/2^23) * 2^23 * 2 ^ (1-127-23)
			 *	value = mantissa * 2 ^ (1-127-23)
			 * We have up to 23 bits of precision.
			 *	 m = (mantissa)
			 *	 e = (1-127-23)
			 */
			utt_bi_set_32( &mantissa , 	float_mantissa );
			exponent					= 1 - 127 - 23;
			mantissa_high_bit_idx		= utt_log2_32(float_mantissa);
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



/**
 * Print a 16-bit floating-point number as a decimal string.
 * The output string is always NUL terminated and the string length (not
 * including the NUL) is returned.
 */
UTT_LINKAGE utt_uint32_t utt_flt_printh(
	char*				out_buffer,		/* buffer to output into */
	utt_uint32_t		buffer_size,	/* size of out_buffer */
	utt_ieee754_half	value,			/* value to print */
	utt_float_fmt_t		format,			/* format to print with */
	utt_int32_t			precision,		/* This is the |number| of digits to print past the decimal point. */
										/* If negative, the output will not contain more digits than absolutely */
										/* necessary to uniquely represent the floating point value */
	const char*			alphabet		/* Alphabet to use, expected order: ".0123456789abcdefghijklmnopqrstuvwxyz" */
){
	if( buffer_size == 0)
		return 0;

	if( buffer_size == 1){
		out_buffer[0] = '\0';
		return 0;
	}

	/* deconstruct the floating point value */
	utt_uint16_t float_exponent = utt_exponenth( value );
	utt_uint16_t float_mantissa = utt_mantissah( value );
	utt_uint32_t prefix_length = 0;

	/* output the sign */
	if( utt_signbith( value ) )
	{
		out_buffer[0] = '-';
		++out_buffer;
		--buffer_size;
		++prefix_length;
		utt_assert(buffer_size > 0);
	}

	/* if this is a special value */
	if( float_exponent == ( UTT_EXPONENT_MASKH >> UTT_EXPONENT_SHIFTH ) ){
		utt_bigint_t	mantissa;
		utt_bi_set_32( &mantissa , float_mantissa );
		return utt_flt_print_special( out_buffer , buffer_size , &mantissa , alphabet ) + prefix_length;
	}
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
			 *	value = (1 + mantissa/2^10) * 2 ^ (exponent-15)
			 * We convert the integer equation by factoring a 2^10 out of the exponent
			 *	value = (1 + mantissa/2^10) * 2^10 * 2 ^ (exponent-15-10)
			 *	value = (2^10 + mantissa) * 2 ^ (exponent-15-10)
			 * Because of the implied 1 in front of the mantissa we have 24 bits of precision.
			 *	 m = (2^10 + mantissa)
			 *	 e = (exponent-15-10)
			 */
			utt_bi_set_32( &mantissa , 	(1UL << 10) | float_mantissa );
			exponent					= float_exponent - 15 - 10;
			mantissa_high_bit_idx		= 10;
			has_unequal_margins			= (float_exponent != 1) && (float_mantissa == 0);
		}
		else
		{
			/* Denormalized Values */
			/*
			 * The floating point equation is:
			 *	value = (mantissa/2^10) * 2 ^ (1-15)
			 * We convert the integer equation by factoring a 2^10 out of the exponent
			 *	value = (mantissa/2^10) * 2^10 * 2 ^ (1-15-10)
			 *	value = mantissa * 2 ^ (1-15-10)
			 * We have up to 10 bits of precision.
			 *	 m = (mantissa)
			 *	 e = (1-15-10)
			 */
			utt_bi_set_32( &mantissa , 	float_mantissa );
			exponent					= 1 - 15 - 10;
			mantissa_high_bit_idx		= utt_log2_16(float_mantissa);
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



/**
 * Print a 16-bit floating-point number as a decimal string.
 * The output string is always NUL terminated and the string length (not
 * including the NUL) is returned.
 */
UTT_LINKAGE utt_uint32_t utt_flt_printh2(
	char*					out_buffer,		/* buffer to output into */
	utt_uint32_t			buffer_size,	/* size of out_buffer */
	utt_ieee754_half_alt	value,			/* value to print */
	utt_float_fmt_t			format,			/* format to print with */
	utt_int32_t				precision,		/* This is the |number| of digits to print past the decimal point. */
											/* If negative, the output will not contain more digits than absolutely */
											/* necessary to uniquely represent the floating point value */
	const char*				alphabet		/* Alphabet to use, expected order: ".0123456789abcdefghijklmnopqrstuvwxyz" */
){
	if( buffer_size == 0)
		return 0;

	if( buffer_size == 1){
		out_buffer[0] = '\0';
		return 0;
	}

	/* deconstruct the floating point value */
	utt_uint16_t float_exponent = utt_exponenth2( value );
	utt_uint16_t float_mantissa = utt_mantissah2( value );
	utt_uint32_t prefix_length = 0;

	/* output the sign */
	if( utt_signbith2( value ) )
	{
		out_buffer[0] = '-';
		++out_buffer;
		--buffer_size;
		++prefix_length;
		utt_assert(buffer_size > 0);
	}

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
		 *	value = (1 + mantissa/2^10) * 2 ^ (exponent-15)
		 * We convert the integer equation by factoring a 2^10 out of the exponent
		 *	value = (1 + mantissa/2^10) * 2^10 * 2 ^ (exponent-15-10)
		 *	value = (2^10 + mantissa) * 2 ^ (exponent-15-10)
		 * Because of the implied 1 in front of the mantissa we have 24 bits of precision.
		 *	 m = (2^10 + mantissa)
		 *	 e = (exponent-15-10)
		 */
		utt_bi_set_32( &mantissa , 	(1UL << 10) | float_mantissa );
		exponent					= float_exponent - 15 - 10;
		mantissa_high_bit_idx		= 10;
		has_unequal_margins			= (float_exponent != 1) && (float_mantissa == 0);
	}
	else
	{
		/* Denormalized Values */
		/*
		 * The floating point equation is:
		 *	value = (mantissa/2^10) * 2 ^ (1-15)
		 * We convert the integer equation by factoring a 2^10 out of the exponent
		 *	value = (mantissa/2^10) * 2^10 * 2 ^ (1-15-10)
		 *	value = mantissa * 2 ^ (1-15-10)
		 * We have up to 10 bits of precision.
		 *	 m = (mantissa)
		 *	 e = (1-15-10)
		 */
		utt_bi_set_32( &mantissa , 	float_mantissa );
		exponent					= 1 - 15 - 10;
		mantissa_high_bit_idx		= utt_log2_16(float_mantissa);
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


/** Function to print a utt_ieee754_double */
UTT_LINKAGE void utt_flt_put( utt_ieee754_double val ){
	char	memory[50];
	utt_flt_print( memory , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
	utt_putstr( memory );
}
UTT_LINKAGE utt_size_t utt_flt_put_buf( utt_ieee754_float val , char* buffer ){
	return utt_flt_print( buffer , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
}


/** Function to print a utt_ieee754_float */
UTT_LINKAGE void utt_flt_putf( utt_ieee754_float val ){
	char	memory[50];
	utt_flt_printf( memory , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
	utt_putstr( memory );
}
UTT_LINKAGE utt_size_t utt_flt_putf_buf( utt_ieee754_float val , char* buffer ){
	return utt_flt_printf( buffer , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
}


/** Function to print a utt_ieee754_half */
UTT_LINKAGE void utt_flt_puth( utt_ieee754_half val ){
	char	memory[50];
	utt_flt_printh( memory , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
	utt_putstr( memory );
}
UTT_LINKAGE utt_size_t utt_flt_puth_buf( utt_ieee754_float val , char* buffer ){
	return utt_flt_printh( buffer , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
}

/** Function to print a utt_ieee754_half_alt */
UTT_LINKAGE void utt_flt_puth2( utt_ieee754_half_alt val ){
	char	memory[50];
	utt_flt_printh2( memory , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
	utt_putstr( memory );
}
UTT_LINKAGE utt_size_t utt_flt_puth2_buf( utt_ieee754_float val , char* buffer ){
	return utt_flt_printh2( buffer , 50 , val , UTT_FLOAT_FMT_SCIENTIFIC , -1 , ".0123456789abcdefghijklmnopqrstuvwxyz" );
}

#endif
