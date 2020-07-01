/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGFLOAT_PREDEFINES_H_
#define _TARGET_UNITTEST_UTILITY_BIGFLOAT_PREDEFINES_H_

#include <target/c/predefines.h>
#include <target/c/stdfloat.h>

/** Specification of the Multiprecision floating point type */
#ifndef UTT_MP_FLOAT_MANTISSA_BYTES
	#define UTT_MP_FLOAT_MANTISSA_BYTES		16u /* 128 bits mantissa */
#endif
#ifndef UTT_MP_FLOAT_TOTAL_BYTES
	#define UTT_MP_FLOAT_TOTAL_BYTES		20u /* Exponent of up to 4 bytes is possible */
#endif
#define UTT_MP_FLOAT_EXPONENT_BYTES			( UTT_MP_FLOAT_TOTAL_BYTES - UTT_MP_FLOAT_MANTISSA_BYTES )
#define UTT_MP_FLOAT_EXPONENT_MAX			( ( 1uL << ( UTT_MP_FLOAT_EXPONENT_BYTES * 8uL - 1uL ) ) - 1uL )
#define UTT_MP_FLOAT_NAN					"\177\377\377\377\377\177\377\377\377\377\377\377\377\377\377\377\377\377\377\377"
#define UTT_MP_FLOAT_INFINITY				"\177\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTT_MP_FLOAT_NINFINITY				"\377\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTT_MP_FLOAT_0						"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTT_MP_FLOAT_N0						"\200\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTT_MP_FLOAT_1						"\077\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
#define UTT_MP_FLOAT_N1						"\277\377\377\377\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"


/** Datatype Declaration */
typedef struct utt_bigfloat_t{
	/** Byte 1: Signbit+Exponent1 ; Byte 2: Exponent2 ; ... ; Byte N: Mantissa1 ; Byte N+1: Mantissa2 ; ... */
	utt_uint8_t data[ UTT_MP_FLOAT_TOTAL_BYTES ]; /* Byte Ordering: Big Endian */
} utt_bigfloat_t;


/** Returns the supplied data converted to a utt_bigfloat_t */
#define utt_fromrawm( value ) ((utt_bigfloat_t){ .data = value })


/** Define IEEE 754 floating point constants */
#define UTT_EXPONENT_BIASM	( ( 1uL << ( ( UTT_MP_FLOAT_TOTAL_BYTES - UTT_MP_FLOAT_MANTISSA_BYTES ) * 8uL - 2uL ) ) - 1uL )
#define UTT_EXPONENT_SHIFTM	UTT_MP_FLOAT_MANTISSA_BYTES * 8
#define UTT_INFINITYM		utt_fromrawm( UTT_MP_FLOAT_INFINITY )
#define UTT_NINFINITYM		utt_fromrawm( UTT_MP_FLOAT_NINFINITY )
#define UTT_NANM			utt_fromrawm( UTT_MP_FLOAT_NAN )

#endif
