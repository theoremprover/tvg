/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGFLOAT_CONVERSION_H_
#define _TARGET_UNITTEST_UTILITY_BIGFLOAT_CONVERSION_H_

#include <target/c/ieee754/convenience.h>
#include <target/c/bigfloat/predefines.h>
#include <target/c/predefines.h>

/** Conversion from IEEE 754 types to big float */
#define UTT_MP_FLOAT_DEFINE_TO_BIGFLOAT( TYPENAME , suffix , SUFFIX , INT_BITS ) \
UTT_LINKAGE utt_bigfloat_t utt_##TYPENAME##2bigfloat( utt_ieee754_##TYPENAME val ){ \
	utt_bigfloat_t result = utt_fromrawm(UTT_MP_FLOAT_0); \
	switch( utt_fp_classify##suffix(val) ){ \
		case UTT_FP_INFINITE: \
			if( utt_signbit##suffix(val) ) \
				result = UTT_NINFINITYM; \
			else \
				result = UTT_INFINITYM; \
			break; \
		case UTT_FP_NAN: \
			result = UTT_NANM; \
			break; \
		case UTT_FP_ZERO: \
			if( utt_signbit##suffix(val) ) \
				result = utt_fromrawm(UTT_MP_FLOAT_N0); \
			break; \
		case UTT_FP_NORMAL:{ \
			utt_size_t				result_cursor; \
			utt_uint##INT_BITS##_t	mantissa = utt_mantissa##suffix(val) << (INT_BITS - UTT_EXPONENT_SHIFT##SUFFIX); \
			utt_uint32_t			new_exponent = (utt_int32_t)utt_exponent##suffix(val) - UTT_EXPONENT_BIAS##SUFFIX + UTT_EXPONENT_BIASM; \
			utt_set_exponentm( &result , new_exponent ); \
			utt_set_signbitm( &result , utt_signbit##suffix(val) ); \
			for( result_cursor = 0 ; result_cursor < UTT_MP_FLOAT_MANTISSA_BYTES && result_cursor < INT_BITS/8 ; result_cursor++ ) /* Store the mantissa */ \
				result.data[result_cursor + UTT_MP_FLOAT_EXPONENT_BYTES] = ( mantissa >> ( INT_BITS - 8 - result_cursor * 8 ) ) & 0xFFu; \
			break; \
		} \
		case UTT_FP_SUBNORMAL:{ \
			utt_size_t				result_cursor; \
			utt_uint8_t				num_mant_bits = utt_log2_##INT_BITS( utt_mantissa##suffix(val) ); \
			utt_uint##INT_BITS##_t	mantissa = utt_mantissa##suffix(val) << (INT_BITS - num_mant_bits); \
			utt_uint32_t			new_exponent = UTT_EXPONENT_BIASM - ( UTT_EXPONENT_BIAS##SUFFIX - 1 ) - ( UTT_EXPONENT_SHIFT##SUFFIX - num_mant_bits ); \
			utt_set_exponentm( &result , new_exponent ); \
			utt_set_signbitm( &result , utt_signbit##suffix(val) ); \
			for( result_cursor = 0 ; result_cursor < UTT_MP_FLOAT_MANTISSA_BYTES && result_cursor < INT_BITS/8 ; result_cursor++ ) /* Store the mantissa */ \
				result.data[result_cursor + UTT_MP_FLOAT_EXPONENT_BYTES] = ( mantissa >> ( INT_BITS - 8 - result_cursor * 8 ) ) & 0xFFu; \
			break; \
		} \
		default: \
			break; \
	} \
	return result;  \
}
UTT_MP_FLOAT_DEFINE_TO_BIGFLOAT( double	,		,		, 64 )
UTT_MP_FLOAT_DEFINE_TO_BIGFLOAT( float		, f		, F		, 32 )
UTT_MP_FLOAT_DEFINE_TO_BIGFLOAT( half		, h		, H		, 16 )
UTT_MP_FLOAT_DEFINE_TO_BIGFLOAT( half_alt	, h2	, H2	, 16 )

/** Conversion from big float to IEEE 754 types */
#define UTT_MP_FLOAT_DEFINE_FROM_BIGFLOAT( TYPENAME , suffix , SUFFIX , INT_BITS ) \
UTT_LINKAGE utt_ieee754_##TYPENAME utt_bigfloat2##TYPENAME##_ex( utt_bigfloat_t val , utt_round_t round_mode ) \
{ \
	switch( utt_fp_classifym(val) ) \
	{ \
	case UTT_FP_INFINITE:	return utt_signbitm(val) ? UTT_NINFINITY##SUFFIX : UTT_INFINITY##SUFFIX; \
	case UTT_FP_NAN:		return UTT_NAN##SUFFIX; \
	case UTT_FP_ZERO:		return utt_fromraw##suffix( (utt_uint##INT_BITS##_t)utt_signbitm(val) << UTT_SIGNBIT_SHIFT##SUFFIX ); \
	case UTT_FP_SUBNORMAL: \
		switch( round_mode ){ \
			case UTT_ROUND_AWAY_FROM_ZERO:		return utt_fromraw##suffix( UTT_SIGNBIT_MASK##SUFFIX * utt_signbitm(val) | 1u ); \
			case UTT_ROUND_TOWARD_POS_INFINITY:	return utt_fromraw##suffix( (utt_uint##INT_BITS##_t)1u << ( UTT_SIGNBIT_SHIFT##SUFFIX * utt_signbitm(val) ) ); /* -0.0 or 1ulp */ \
			case UTT_ROUND_TOWARD_NEG_INFINITY:	return utt_fromraw##suffix( ( UTT_SIGNBIT_MASK##SUFFIX | 1 ) * utt_signbitm(val) ); \
			default:							return utt_fromraw##suffix( (utt_uint##INT_BITS##_t)utt_signbitm(val) << UTT_SIGNBIT_SHIFT##SUFFIX ); \
		} \
		break; \
	case UTT_FP_NORMAL: \
		break; \
	} \
	\
	int exp = utt_exponentm( val ) - UTT_EXPONENT_BIASM + UTT_EXPONENT_BIAS##SUFFIX; /* Compute resulting exponent */ \
	\
	bool cut_half; /* Indicates, we are chopping off something >=.5 */ \
	bool cut_low; /* Indicates, we are chopping off something >0 and <.5 */ \
	utt_uint##INT_BITS##_t result; \
	\
	/* Results in normal value? */ \
	if( exp > 0 ){ \
		utt_bigint_t mantissa = utt_mantissam( val ); \
		cut_low		= utt_bi_shr( &mantissa , UTT_EXPONENT_SHIFTM - UTT_EXPONENT_SHIFT##SUFFIX - 1 ); /* Shift away everything between >=0. and <0.5 ULP */ \
		cut_half	= utt_bi_shr( &mantissa , 1 ); /* Shift away 0.5 ULP */ \
		result		= utt_bi_get_##INT_BITS( &mantissa ); \
	} \
	/* Results in subnormal value? */ \
	else if( exp >= -UTT_EXPONENT_SHIFT##SUFFIX ){ \
		/* Note: Initially, the '>=' was a '>', but this allows us to say 'cut_half=false' a few lines down the road. */ \
		utt_bigint_t mantissa = utt_mantissam( val ); \
		cut_low		= utt_bi_shr( &mantissa , UTT_EXPONENT_SHIFTM - UTT_EXPONENT_SHIFT##SUFFIX - exp ); /* Shift away everything between >=0. and <0.5 ULP */ \
		cut_half	= utt_bi_shr( &mantissa , 1 ); /* Shift away 0.5 ULP */ \
		result		= utt_bi_get_##INT_BITS( &mantissa ) | ( (utt_uint##INT_BITS##_t)1u << (UTT_EXPONENT_SHIFT##SUFFIX + exp - 1) ); \
		exp			= 0; \
	} \
	else{ \
		result = 0; \
		cut_half = false; \
		cut_low = true; /* We must be cutting something away, since we didn't start with '0' in the first place */ \
	} \
	\
	switch( round_mode ){ \
		case UTT_ROUND_TOWARD_POS_INFINITY:		result += cut_half || cut_low ? 1 - utt_signbitm( val ) : 0; break; /* Round up, if we cut >0 when x>0 */ \
		case UTT_ROUND_TOWARD_NEG_INFINITY:		result += cut_half || cut_low ? utt_signbitm( val ) : 0; break; /* Round up, if we cut >0 when x<0 */ \
		case UTT_ROUND_AWAY_FROM_ZERO:			result += cut_half || cut_low ? 1 : 0; break; /* Round up, if we cut >0 */ \
		case UTT_ROUND_TO_NEAREST:				result += cut_half ? 1 : 0; break; /* Round up, if we cut >=0.5 */ \
		case UTT_ROUND_TO_NEAREST_TIES_ZERO:	result += cut_half && cut_low ? 1 : 0; break; /* Round up, if we cut >.5 */ \
		case UTT_ROUND_TO_NEAREST_TIES_EVEN: \
		case UTT_ROUND_TO_NEAREST_IEEE753: \
			if( cut_low ) /* Round up, if we cut >0.5 */ \
				result += cut_half ? 1 : 0; \
			else if( cut_half ) \
				result += result % 2u; /* Round up only, if the mantissa is currently uneven */ \
			break; \
		default:								break; \
	} \
	/* Need to normalize again? */ \
	if( result > UTT_MANTISSA_MASK##SUFFIX && exp != 0 ){ \
		/*
		 * The reason, we don't manually normalize again previously denormalized numbers (see "&& exp != 0 " above )
		 * is that 0.1111...b rounded up results in the correct value already (the minimum non-denormalized number).
		 * Since 'exp' is still 0, the result is correctly computed down below in the call to 'utt_fromrawh'.
		 */ \
		result >>= 1; /* The result can only be larger than 1.11111...b due to rounding, in which case a left shift by one is sufficient */ \
		++exp; /* Adjust the exponent */ \
	} \
	\
	/* Greater maximum possible exponent? */ \
	if( exp > (int)(UTT_EXPONENT_MASK##SUFFIX >> UTT_EXPONENT_SHIFT##SUFFIX) ) \
		switch( round_mode ){ \
			case UTT_ROUND_TOWARD_POS_INFINITY:	return utt_signbitm( val ) ? UTT_SIGNBIT_MASK##SUFFIX | ( UTT_EXPONENT_MASK##SUFFIX - 1 ) : UTT_EXPONENT_MASK##SUFFIX; \
			case UTT_ROUND_TOWARD_NEG_INFINITY:	return utt_signbitm( val ) ? UTT_SIGNBIT_MASK##SUFFIX | UTT_EXPONENT_MASK##SUFFIX : UTT_EXPONENT_MASK##SUFFIX - 1; \
			case UTT_ROUND_TO_NEAREST_IEEE753: \
			case UTT_ROUND_AWAY_FROM_ZERO:		return ( (utt_uint##INT_BITS##_t)utt_signbitm( val ) << UTT_SIGNBIT_SHIFT##SUFFIX ) | UTT_EXPONENT_MASK##SUFFIX; \
			default:							return ( (utt_uint##INT_BITS##_t)utt_signbitm( val ) << UTT_SIGNBIT_SHIFT##SUFFIX ) | ( UTT_EXPONENT_MASK##SUFFIX - 1 ); \
		} \
	\
	return utt_fromraw##suffix( result | ( (utt_uint##INT_BITS##_t)exp << UTT_EXPONENT_SHIFT##SUFFIX ) | ( (utt_uint##INT_BITS##_t)utt_signbitm( val ) << UTT_SIGNBIT_SHIFT##SUFFIX ) ); \
} \
UTT_LINKAGE utt_ieee754_##TYPENAME utt_bigfloat2##TYPENAME( utt_bigfloat_t value ){ \
	return utt_bigfloat2##TYPENAME##_ex( value , UTT_ROUND_TO_NEAREST_IEEE753 ); \
}
UTT_MP_FLOAT_DEFINE_FROM_BIGFLOAT( double	,		,		, 64 )
UTT_MP_FLOAT_DEFINE_FROM_BIGFLOAT( float	, f		, F		, 32 )
UTT_MP_FLOAT_DEFINE_FROM_BIGFLOAT( half		, h		, H		, 16 )
UTT_MP_FLOAT_DEFINE_FROM_BIGFLOAT( half_alt	, h2	, H2	, 16 )

#endif