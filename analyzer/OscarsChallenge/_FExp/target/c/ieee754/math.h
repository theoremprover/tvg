/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE_754_MATH_H_
#define _TARGET_UNITTEST_IEEE_754_MATH_H_

#include <target/c/ieee754/predefines.h>
#include <target/c/ieee754/convenience.h>


/** Define nextafter for different floating point types */
#define UTT_DEFINE_NEXTAFTER( TYPENAME , suffix , SUFFIX , INT_BITS , HAS_SPECIAL ) \
UTT_LINKAGE utt_ieee754_##TYPENAME utt_nextafter##suffix( utt_ieee754_##TYPENAME from , utt_ieee754_##TYPENAME to ) \
{ \
	utt_uint##INT_BITS##_t fraw = utt_toraw##suffix(from), traw = utt_toraw##suffix(to); \
	utt_uint##INT_BITS##_t fabs = utt_toraw##suffix( utt_fabs##suffix(from) ), tabs = utt_toraw##suffix( utt_fabs(to) ); \
	if( HAS_SPECIAL ){ \
		if( utt_isnan##suffix(from) ) \
			return from; \
		if( utt_isnan##suffix(to) || fraw == traw || !( fabs | tabs ) ) /* 'to' is NaN, both identical or both +-zero? */ \
			return to; \
	} \
	if( !fabs ) /* 'from' is +-zero? => Do the first step */ \
		return utt_fromraw( ( traw & UTT_SIGNBIT_MASK##SUFFIX ) + 1 ); \
	char f_lt_t = \
		(	fabs == fraw ? (utt_int##INT_BITS##_t)fabs : -(utt_int##INT_BITS##_t)fabs ) \
		< ( tabs == traw ? (utt_int##INT_BITS##_t)tabs : -(utt_int##INT_BITS##_t)tabs ) \
		? 1 : 0 \
	; \
	/* Multiply the sign of 'to' with whether its lower than 'from' */ \
	return utt_fromraw##suffix( fraw + ( ( ( fraw >> (INT_BITS-1) ) ^ f_lt_t ) << 1 ) - 1 ); \
}
UTT_DEFINE_NEXTAFTER( double	,		,		, 64 , true		)
UTT_DEFINE_NEXTAFTER( float		, f		, F		, 32 , true		)
UTT_DEFINE_NEXTAFTER( half		, h		, H		, 16 , true		)
UTT_DEFINE_NEXTAFTER( half_alt	, h2	, H2	, 16 , false	)



/** ceil Implementation for differentfloating point types */
#define UTT_DEFINE_CEIL( TYPENAME , suffix , SUFFIX , INT_BITS , HAS_SPECIAL ) \
UTT_LINKAGE utt_ieee754_##TYPENAME utt_ceil##suffix( utt_ieee754_##TYPENAME value ) \
{ \
    int exponent = utt_exponent##suffix( value ); \
	\
	/* Shortcut for +/-infinity and NaNs */ \
	if( HAS_SPECIAL && exponent == (UTT_EXPONENT_MASK >> UTT_EXPONENT_SHIFT) ) \
		return value; \
	\
	/* Shortcut for +0 */ \
	utt_uint##INT_BITS##_t result = utt_toraw##suffix(value); \
	if( !result ) \
		return result; \
	\
	/* Small numbers get rounded to -0 or +1, depending on the sign */ \
    if( exponent < UTT_EXPONENT_BIAS##SUFFIX ) \
		return utt_fromraw##suffix( result >= UTT_SIGNBIT_MASK##SUFFIX ? UTT_SIGNBIT_MASK##SUFFIX : ( (utt_uint##INT_BITS##_t)UTT_EXPONENT_BIAS##SUFFIX << UTT_EXPONENT_SHIFT##SUFFIX ) ); \
	\
	/* Numbers without fractional bits are mapped to themselves */ \
	int fractional_bits = UTT_EXPONENT_SHIFT##SUFFIX + UTT_EXPONENT_BIAS##SUFFIX - exponent; \
    if( fractional_bits <= 0 ) \
		return value; \
	\
	/* Mask out fractional bits */ \
	result &= ( (utt_uint##INT_BITS##_t)(UTT_SIGNBIT_MASK##SUFFIX | UTT_EXPONENT_MASK##SUFFIX | UTT_MANTISSA_MASK##SUFFIX) << fractional_bits ); \
	\
	/* Increase by '1', if positive (but only if we don't yet have a whole number already!) */ \
	if( result != utt_toraw##suffix(value) && !utt_signbit##suffix( value ) ) \
		result += 1uLL << fractional_bits; \
	\
    return utt_fromraw##suffix( result ); \
}
UTT_DEFINE_CEIL( double		,		,		, 64 , true		)
UTT_DEFINE_CEIL( float		, f		, F		, 32 , true		)
UTT_DEFINE_CEIL( half		, h		, H		, 16 , true		)
UTT_DEFINE_CEIL( half_alt	, h2	, H2	, 16 , false	)

#endif
