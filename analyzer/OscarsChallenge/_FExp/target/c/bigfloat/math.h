/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGFLOAT_MATH_H_
#define _TARGET_UNITTEST_UTILITY_BIGFLOAT_MATH_H_

#include <target/c/bigfloat/convenience.h>
#include <target/c/bigfloat/fromhex.h>

/** Define nextafter for multiprecision floating point type */
UTT_LINKAGE utt_bigfloat_t utt_nextafterm( const utt_bigfloat_t from , const utt_bigfloat_t to )
{
	unsigned int i = 0;
	utt_uint32_t	fexp = utt_exponentm( from );
	if( fexp == UTT_MP_FLOAT_EXPONENT_MAX ) /* 'from' is NaN? */
		for( i = UTT_MP_FLOAT_EXPONENT_BYTES ; i < UTT_MP_FLOAT_TOTAL_BYTES ; i++ )
			if( from.data[i] )
				return from;
	
	utt_uint32_t	texp = utt_exponentm( to );
	if( texp == UTT_MP_FLOAT_EXPONENT_MAX ) /* 'to' is NaN? */
		for( i = UTT_MP_FLOAT_EXPONENT_BYTES ; i < UTT_MP_FLOAT_TOTAL_BYTES ; i++ )
			if( to.data[i] )
				return to;
	
	/* Compute first byte difference, excluding the signbit */
	int difference = ( from.data[0] & 0x7F ) - ( to.data[0] & 0x7F );
	
	/* Difference in first byte? */
	if( !difference ){
		difference = utt_memcmp( (char*)&from + 1 , (char*)&to + 1 , sizeof(utt_bigfloat_t) - 1 );
		if( !difference ) /* both identical? */
			return to;
	}
	
	/* both +-zero? */
	bool to_zero = !texp;
	if( !fexp )
	{
		bool from_zero = true;
		for( i = UTT_MP_FLOAT_EXPONENT_BYTES ; i < UTT_MP_FLOAT_TOTAL_BYTES ; i++ ){
			from_zero |= from.data[i];
			to_zero |= to.data[i];
		}
		
		/* 'from' is +-zero? */
		if( from_zero ){
			if( to_zero )
				/* Both are +/- zero */
				return to;
			
			/* => Do the first step */
			else if( utt_signbitm( to ) )
				return utt_fromrawm( UTT_MP_FLOAT_N1 );
			else
				return utt_fromrawm( UTT_MP_FLOAT_1 );
		}
	}
	
	utt_bigfloat_t result = from;
	
	/* Decrease or increase absolute value? */
	if( difference < 0 )
	{
		/* Find byte to increase at */
		for( i = UTT_MP_FLOAT_TOTAL_BYTES - 1 ; i >= UTT_MP_FLOAT_EXPONENT_BYTES ; i-- )
			if( result.data[i] < 255u ){
				result.data[i]++;
				break;
			}
		/* Reset less significant bytes to 0 */
		while( ++i < UTT_MP_FLOAT_TOTAL_BYTES )
			result.data[i] = 0;
	}
	else{
		/* Find byte to decrease at */
		for( i = UTT_MP_FLOAT_TOTAL_BYTES - 1 ; i >= UTT_MP_FLOAT_EXPONENT_BYTES ; i-- )
			if( result.data[i] > 0u ){
				result.data[i]--;
				break;
			}
		/* Reset less significant bytes to 255 */
		while( ++i < UTT_MP_FLOAT_TOTAL_BYTES )
			result.data[i] = 255u;
	}
	
	return result;
}

#endif
