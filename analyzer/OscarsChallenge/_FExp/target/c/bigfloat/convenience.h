/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGFLOAT_CONVENIENCE_H_
#define _TARGET_UNITTEST_UTILITY_BIGFLOAT_CONVENIENCE_H_

#include <target/c/bigfloat/predefines.h>
#include <target/c/utility/bigint.h>

/** Define a getter for the signbit */
UTT_LINKAGE bool utt_signbitm( const utt_bigfloat_t val ){ return val.data[0] >> 7u ; }

/** Define a setter for the signbit */
UTT_LINKAGE void utt_set_signbitm( utt_bigfloat_t* val , bool sign ){
	if( sign )
		val->data[0] |= 0x80u;
	else
		val->data[0] &= 0x7Fu;
}

/** Define a negation function for big floats */
UTT_LINKAGE void utt_negatem( utt_bigfloat_t* val ){
	val->data[0] ^= 0x80u;
}

/** Define a getter for the exponent */
UTT_LINKAGE utt_uint32_t utt_exponentm( const utt_bigfloat_t val ){
	utt_uint32_t result = val.data[0] & 0x7F; /* First Byte */
	unsigned int i = 1;
	while( i < UTT_MP_FLOAT_EXPONENT_BYTES ){
		result <<= 8;
		result |= val.data[i];
		i++;
	}
	return result;
}

/** Define a setter for the exponent */
UTT_LINKAGE void utt_set_exponentm( utt_bigfloat_t* val , utt_uint32_t exponent ){
	utt_uint32_t	cursor = UTT_MP_FLOAT_EXPONENT_BYTES - 1;
	while( cursor > 0 ){
		val->data[cursor--] = exponent & 0xFF;
		exponent >>= 8;
	}
	val->data[0] = ( exponent & 0x7Fu ) | ( utt_signbitm(*val) << 7u ); /* Store the first byte */
}

/** Define a getter for the mantissa */
UTT_LINKAGE utt_bigint_t utt_mantissam( const utt_bigfloat_t val ){
	utt_bigint_t result;
	unsigned int i = UTT_MP_FLOAT_EXPONENT_BYTES;
	while( i < UTT_MP_FLOAT_TOTAL_BYTES && !val.data[i] )
		i++;
	if( i < UTT_MP_FLOAT_TOTAL_BYTES ){
		utt_bi_set_nonempty_zero( &result );
		do{
			utt_bi_shl( &result , 8 );
			result.blocks[0] |= val.data[i++];
		}while( i < UTT_MP_FLOAT_TOTAL_BYTES );
	}
	else
		utt_bi_set_zero( &result );
	return result;
}

/** Define a setter for the mantissa */
UTT_LINKAGE void utt_set_mantissam( utt_bigfloat_t* val , utt_bigint_t mantissa ){
	unsigned int i = UTT_MP_FLOAT_TOTAL_BYTES - 1;
	while( i >= UTT_MP_FLOAT_EXPONENT_BYTES ){
		if( !mantissa.length )
			break;
		val->data[i--] = mantissa.blocks[0] & 0xFFu;
		utt_bi_shr( &mantissa , 8 );
	}
	while( i >= UTT_MP_FLOAT_EXPONENT_BYTES )
		val->data[i--] = 0u;
}

/** Define fpclassify for multiprecision floating point types */
UTT_LINKAGE utt_ieee754_class_t utt_fp_classifym( const utt_bigfloat_t val ){
	unsigned int i;
	if( utt_exponentm( val ) == UTT_MP_FLOAT_EXPONENT_MAX ){
		for( i = UTT_MP_FLOAT_EXPONENT_BYTES ; i < UTT_MP_FLOAT_TOTAL_BYTES ; i++ )
			if( val.data[i] )
				return UTT_FP_NAN;
		return UTT_FP_INFINITE;
	}
	else if( !utt_exponentm( val ) ){
		for( i = UTT_MP_FLOAT_EXPONENT_BYTES ; i < UTT_MP_FLOAT_TOTAL_BYTES ; i++ )
			if( val.data[i] )
				return UTT_FP_SUBNORMAL;
		return UTT_FP_ZERO;
	}
	return UTT_FP_NORMAL;
}

/** Define isnan for multiprecision floating point types */
UTT_LINKAGE bool utt_isnanm( const utt_bigfloat_t val ){ return utt_fp_classifym( val ) == UTT_FP_NAN; }

/** Define isinf for multiprecision floating point types */
UTT_LINKAGE bool utt_isinfm( const utt_bigfloat_t val ){ return utt_fp_classifym( val ) == UTT_FP_INFINITE; }

/** Define abs for different floating point types */
UTT_LINKAGE utt_bigfloat_t utt_fabsm( utt_bigfloat_t val ){ val.data[0] &= 0x7F; return val; }

#endif