/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGFLOAT_FROMHEX_H_
#define _TARGET_UNITTEST_UTILITY_BIGFLOAT_FROMHEX_H_

#include <target/c/bigfloat/predefines.h>

/** Define a conversion function from a normalized (non-inf,non-nan) hexfloat string (e.g. "-0x1.cdfffffff3p-4") to utt_bigfloat_t */
#define utt_fromhexm( val ) utt_fromhexm_impl( #val )
UTT_LINKAGE utt_bigfloat_t utt_fromhexm_impl( const char* str )
{
	#define getxalpha( c ) ((unsigned char)(c & 0xDF) - 'A')
	#define getxnum( c ) ((unsigned char)(c - '0'))
	
	utt_bigfloat_t	result = { UTT_MP_FLOAT_0 };
	utt_int32_t		exponent_bias = 0;
	unsigned char	val;
	unsigned int	result_cursor;
	
	/* Parse Prefix */
	bool sign = false;
	if( str[0] == '-' ){
		str++;
		sign = true;
	}
	if( str[0] == '0' && (str[1] & 0xDF) == 'X' )
		str += 2;
	
	/* Parse First Digit of Mantissa and store it in 'val'*/
	if( (val = getxnum(*str)) < 10u )
		{}
	else if( (val = getxalpha( *str )) < 6u )
		val += 10;
	else
		goto return_nan;
	
	/* Skip the number we just parsed as well as the '.' */
	++str;
	if( str[0] == '.' )
		++str;
	
	/* Parse Rest of Mantissa */
	switch( val )
	{
		case 0:
			return sign ? (utt_bigfloat_t){UTT_MP_FLOAT_N0} : (utt_bigfloat_t){UTT_MP_FLOAT_0};
		case 1:
			for( result_cursor = UTT_MP_FLOAT_EXPONENT_BYTES ; result_cursor < UTT_MP_FLOAT_TOTAL_BYTES ; result_cursor++ , str += 2 )
			{
				if( (val = getxnum(str[0])) < 10u )
					result.data[result_cursor] |= val << 4;
				else if( (val = getxalpha(str[0])) < 6u )
					result.data[result_cursor] |= ( val += 10 ) << 4;
				else
					break;
				if( (val = getxnum(str[1])) < 10u )
					result.data[result_cursor] |= val;
				else if( (val = getxalpha(str[1])) < 6u )
					result.data[result_cursor] |= ( val += 10 );
				else
					break;
			}
			break;
		case 2:		case 3:
			exponent_bias -= 1;
			for( result_cursor = UTT_MP_FLOAT_EXPONENT_BYTES ; result_cursor < UTT_MP_FLOAT_TOTAL_BYTES ; result_cursor++ , str += 2 )
			{
				result.data[result_cursor] |= val << 7;
				if( (val = getxnum(str[0])) < 10u )
					result.data[result_cursor] |= val << 3;
				else if( (val = getxalpha(str[0])) < 6u )
					result.data[result_cursor] |= ( val += 10 ) << 3;
				else
					break;
				if( (val = getxnum(str[1])) < 10u )
					result.data[result_cursor] |= val >> 1;
				else if( (val = getxalpha(str[1])) < 6u )
					result.data[result_cursor] |= ( val += 10 ) >> 1;
				else
					break;
			}
			break;
		case 4:		case 5:
		case 6:		case 7:
			exponent_bias -= 2;
			for( result_cursor = UTT_MP_FLOAT_EXPONENT_BYTES ; result_cursor < UTT_MP_FLOAT_TOTAL_BYTES ; result_cursor++ , str += 2 )
			{
				result.data[result_cursor] |= val << 6;
				if( (val = getxnum(str[0])) < 10u )
					result.data[result_cursor] |= val << 2;
				else if( (val = getxalpha(str[0])) < 6u )
					result.data[result_cursor] |= ( val += 10 ) << 2;
				else
					break;
				if( (val = getxnum(str[1])) < 10u )
					result.data[result_cursor] |= val >> 2;
				else if( (val = getxalpha(str[1])) < 6u )
					result.data[result_cursor] |= ( val += 10 ) >> 2;
				else
					break;
			}
			break;
		case 8:		case 9:
		case 10:	case 11:
		case 12:	case 13:
		case 14:	case 15:
			exponent_bias -= 3;
			for( result_cursor = UTT_MP_FLOAT_EXPONENT_BYTES ; result_cursor < UTT_MP_FLOAT_TOTAL_BYTES ; result_cursor++ , str += 2 )
			{
				result.data[result_cursor] |= val << 5;
				if( (val = getxnum(str[0])) < 10u )
					result.data[result_cursor] |= val << 1;
				else if( (val = getxalpha(str[0])) < 6u )
					result.data[result_cursor] |= ( val += 10 ) << 1;
				else
					break;
				if( (val = getxnum(str[1])) < 10u )
					result.data[result_cursor] |= val >> 3;
				else if( (val = getxalpha( str[1] )) < 6u )
					result.data[result_cursor] |= ( val += 10 ) >> 3;
				else
					break;
			}
			break;
	}
	
	
	/* Parse Rest of Mantissa (if any), while ignoring everything */
	while( getxnum(*str) < 10u || getxalpha(*str) < 6u )
		str++;
	
	/* Parse Exponent prefix 'p' */
	if( str[0] != 'p' )
		goto return_nan;
	str++;
	
	/* Parse Exponent Sign */
	bool			exp_sign = false;
	utt_int32_t		exponent = 0;
	if( *str == '-' ){
		exp_sign = true;
		str++;
	}
	else if( *str == '+' )
		str++;
	
	/* Parse Exponent */
	while( *str >= '0' && *str <= '9' ){
		exponent = exponent * 10 + ( *str - '0' );
		++str;
	}
	
	/* Determine overall exponent to store in bits */
	exponent = ( exp_sign ? -exponent : exponent ) + UTT_EXPONENT_BIASM - exponent_bias;
	
	/* Assert, we can store the exponent and its /not/ below zero (=> would result in denormalized number) */
	utt_assert( exponent > 0 && exponent < (utt_int32_t)UTT_MP_FLOAT_EXPONENT_MAX );
	
	/* Store the exponent */
	utt_uint32_t exp = exponent;
	for( result_cursor = UTT_MP_FLOAT_EXPONENT_BYTES - 1 ; result_cursor > 0 ; result_cursor-- ){
		result.data[result_cursor] = exp & 0xFF;
		exp >>= 8;
	}
	
	/* Store first byte */
	result.data[0] = ( exp & 0x7Fu ) | ( sign << 7u );
	
	return result;
	
return_nan:
	return (utt_bigfloat_t){UTT_MP_FLOAT_NAN};
	#undef getxalpha
	#undef getxnum
}

#endif