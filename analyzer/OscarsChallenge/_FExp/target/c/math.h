/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_MATH_H_
#define _TARGET_UNITTEST_MATH_H_

#include <target/c/c_upgrade.h>
#include <target/c/stdint.h>

/** Logarithm base 2 */
UTT_LINKAGE utt_uint8_t utt_log2_8( utt_uint8_t val ){
	static const utt_uint8_t lut[256] = {
		0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
		4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
		5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
		5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
		7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
	};
	return lut[ val ];
}
UTT_LINKAGE utt_uint8_t utt_log2_16( utt_uint16_t val ){
	return val >> 8 ? 8 + utt_log2_8( (utt_uint8_t) ( val >> 8 ) ) : utt_log2_8( (utt_uint8_t)val );
}
UTT_LINKAGE utt_uint8_t utt_log2_32( utt_uint32_t val ){
	return val >> 16 ? 16 + utt_log2_16( (utt_uint16_t) ( val >> 16 ) ) : utt_log2_16( (utt_uint16_t)val );
}
UTT_LINKAGE utt_uint8_t utt_log2_64( utt_uint64_t val ){
	return val >> 32 ? 32 + utt_log2_32( (utt_uint32_t) ( val >> 32 ) ) : utt_log2_32( (utt_uint32_t)val );
}

/** Enumeration of different rounding modes */
typedef enum{
	UTT_ROUND_INDETERMINATE				= -1
	, UTT_ROUND_TOWARD_ZERO				= 0
	, UTT_ROUND_TO_NEAREST				= 1
	, UTT_ROUND_TOWARD_POS_INFINITY		= 2
	, UTT_ROUND_TOWARD_NEG_INFINITY		= 3
	, UTT_ROUND_AWAY_FROM_ZERO			= 4
	, UTT_ROUND_TO_NEAREST_TIES_ZERO	= 5
	, UTT_ROUND_TO_NEAREST_TIES_EVEN	= 6
	, UTT_ROUND_TO_NEAREST_IEEE753		= 7
} utt_round_t;

/** Implementations of min and max */
#define utt_min( lhs , rhs ) (lhs < rhs ? lhs : rhs)
#define utt_max( lhs , rhs ) (lhs > rhs ? lhs : rhs)

#endif
