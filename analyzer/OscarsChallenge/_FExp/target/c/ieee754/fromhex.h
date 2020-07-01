/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE_754_FROMHEX_H_
#define _TARGET_UNITTEST_IEEE_754_FROMHEX_H_

#include <target/c/ieee754/predefines.h>

#if TARGET_NATIVE_DOUBLE == IEEE_754
	#define utt_fromhex( val ) val
#else
	#define utt_fromhex( val ) utt_fromhex_impl( #val )
#endif

#if TARGET_NATIVE_FLOAT == IEEE_754
	#define utt_fromhexf( val ) val ## f
#else
	#define utt_fromhexf( val ) utt_fromhexf_impl( #val )
#endif

#if TARGET_NATIVE_HALF == IEEE_754
	#define utt_fromhexh( val ) ((TARGET_IEEE_754_HALF_TYPE)val)
#else
	#define utt_fromhexh( val ) utt_fromhexh_impl( #val )
#endif

#if TARGET_NATIVE_HALF == IEEE_754_ARM
	#define utt_fromhexh2( val ) ((TARGET_IEEE_754_HALF_ALT_TYPE)val)
#else
	#define utt_fromhexh2( val ) utt_fromhexh_impl( #val ) /* utt_fromhexh_impl does the same as  */
#endif

/** Define a conversion function from a normalized (non-inf,non-nan) hexfloat string (e.g. "-0x1.cdfffffff3p-4") to utt_ieee754_half */
UTT_LINKAGE utt_ieee754_half utt_fromhexh_impl( const char* str )
{
	#define getxalpha( c ) ((unsigned char)(c & 0xDF) - 'A')
	#define getxnum( c ) ((unsigned char)(c - '0'))
	
	unsigned char	val;
	
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
	
	/* Is the number we are about to parse '0.something' ? */
	if( val == 0 )
		return sign ? utt_fromrawh(UTT_SIGNBIT_MASKH) : utt_fromrawh(0);
	
	/* Skip the number we just parsed as well as the potential '.' */
	++str;
	if( str[0] == '.' )
		++str;
	
	/* Parse the Mantissa */
	unsigned char	bits_parsed = utt_log2_8( val );
	signed char		shift = UTT_EXPONENT_SHIFTH - bits_parsed;
	utt_uint16_t	result = 0u;
	while( shift > -4 ){
		result |= ( shift >= 0 ? val << shift : val >> -shift );
		if( (val = getxnum(str[0])) < 10u )
			{}
		else if( (val = getxalpha(str[0])) < 6u )
			val += 10;
		else
			break;
		++str;
		shift -= 4;
	}
	
	/* Parse Rest of Mantissa letters (if any), while ignoring everything */
	while( getxnum(*str) < 10u || getxalpha(*str) < 6u )
		++str;
	
	/* Parse Exponent prefix 'p' */
	if( str[0] != 'p' )
		goto return_nan;
	++str;
	
	/* Parse Exponent Sign */
	bool			exp_sign = false;
	utt_int16_t		exponent = 0;
	if( *str == '-' ){
		exp_sign = true;
		str++;
	}
	else if( *str == '+' )
		str++;
	
	/* Parse Exponent */
	while( *str >= '0' && *str <= '9' ){
		exponent = exponent * 10 + ( *str - '0' );
		str++;
	}
	
	/* Determine overall exponent to store in bits */
	exponent = ( exp_sign ? -exponent : exponent ) + UTT_EXPONENT_BIASH + bits_parsed;
	
	/* Assert, we can store the exponent */
	utt_assert( exponent <= (utt_int16_t)( UTT_EXPONENT_MASKH >> UTT_EXPONENT_SHIFTH ) ); /* The <= instead of '<' is to support the alternative half */
	
	/* Take care of denormalized values and the leading '1'-bit */
	if( exponent <= 0 ){
		result >>= 1; /* The leading '1'-bit is now part of the mantissa */
		result >>= -exponent;
		exponent = 0;
	}
	else
		result &= UTT_MANTISSA_MASKH;
	
	/* Store exponent and return */
	return utt_fromrawh(
		result
		| ( (utt_uint16_t)exponent << UTT_EXPONENT_SHIFTH )
		| ( sign ? UTT_SIGNBIT_MASKH : 0u )
	);
	
return_nan:
	return UTT_NANH;
	#undef getxalpha
	#undef getxnum
}

#endif