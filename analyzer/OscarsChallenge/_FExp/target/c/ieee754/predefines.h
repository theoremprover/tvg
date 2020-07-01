/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE_754_PREDEFINES_H_
#define _TARGET_UNITTEST_IEEE_754_PREDEFINES_H_

#include <target/c/stdint.h>
#include <target/c/math.h>

#ifndef TARGET_IEEE_754_HALF_TYPE
	#if TARGET_NATIVE_HALF == IEEE_754
		#if defined(__GNUC__) || defined(__clang__) /** Use GCC builtins */
			#define TARGET_IEEE_754_HALF_TYPE __fp16
		#else
			#define TARGET_IEEE_754_HALF_TYPE _Float16
		#endif
	#else
		#define TARGET_IEEE_754_HALF_TYPE utt_uint16_t
	#endif
#endif

#ifndef TARGET_IEEE_754_HALF_ALT_TYPE
	#if TARGET_NATIVE_HALF == IEEE_754_ARM
		#if defined(__GNUC__) || defined(__clang__) /** Use GCC builtins */
			#define TARGET_IEEE_754_HALF_ALT_TYPE __fp16
		#else
			#define TARGET_IEEE_754_HALF_ALT_TYPE _Float16
		#endif
	#else
		#define TARGET_IEEE_754_HALF_ALT_TYPE utt_uint16_t
	#endif
#endif

#ifndef TARGET_IEEE_754_FLOAT_TYPE
	#if TARGET_NATIVE_FLOAT == IEEE_754
		#define TARGET_IEEE_754_FLOAT_TYPE float
	#else
		#define TARGET_IEEE_754_FLOAT_TYPE utt_uint32_t
	#endif
#endif

#ifndef TARGET_IEEE_754_DOUBLE_TYPE
	#if TARGET_NATIVE_DOUBLE == IEEE_754
		#define TARGET_IEEE_754_DOUBLE_TYPE double
	#else
		#define TARGET_IEEE_754_DOUBLE_TYPE utt_uint64_t
	#endif
#endif



/** Define, what the native floating oint types will be promoted to and how to get the right type then */
#if TARGET_NATIVE_DOUBLE == IEEE_754
	#define TARGET_IEEE_754_DOUBLE_PRM_TYPE		double
#else
	#define TARGET_IEEE_754_DOUBLE_PRM_TYPE		utt_uint64_t
#endif
#define TARGET_IEEE_754_DOUBLE_PRM_CAST			utt_ieee754_double

#if TARGET_NATIVE_FLOAT == IEEE_754
	#define TARGET_IEEE_754_FLOAT_PRM_TYPE		double
	#define TARGET_IEEE_754_FLOAT_PRM_CAST		utt_double2float
#else
	#define TARGET_IEEE_754_FLOAT_PRM_TYPE		int
	#define TARGET_IEEE_754_FLOAT_PRM_CAST		utt_ieee754_float
#endif

#if TARGET_NATIVE_HALF == IEEE_754
	#define TARGET_IEEE_754_HALF_PRM_TYPE		double
	#define TARGET_IEEE_754_HALF_PRM_CAST		utt_double2half
#else
	#define TARGET_IEEE_754_HALF_PRM_TYPE		int
	#define TARGET_IEEE_754_HALF_PRM_CAST		utt_ieee754_half
#endif

#if TARGET_NATIVE_HALF == IEEE_754_ARM
	#define TARGET_IEEE_754_HALF_ALT_PRM_TYPE	double
	#define TARGET_IEEE_754_HALF_ALT_PRM_CAST	utt_double2half_alt
#else
	#define TARGET_IEEE_754_HALF_ALT_PRM_TYPE	int
	#define TARGET_IEEE_754_HALF_ALT_PRM_CAST	utt_ieee754_half_alt
#endif



/** Define different IEEE 753 floating point types */
typedef TARGET_IEEE_754_DOUBLE_TYPE		utt_ieee754_double;
typedef TARGET_IEEE_754_FLOAT_TYPE		utt_ieee754_float;
typedef TARGET_IEEE_754_HALF_TYPE		utt_ieee754_half;
typedef TARGET_IEEE_754_HALF_ALT_TYPE	utt_ieee754_half_alt;



/** Define unions for IEEE 754 floating point computations */
typedef union {
	unsigned char		data[sizeof(utt_ieee754_double)];
	utt_ieee754_double	value;
	utt_uint64_t		raw;
} utt_ieee754_double_t;

typedef union {
	unsigned char		data[sizeof(utt_ieee754_float)];
	utt_ieee754_float	value;
	utt_uint32_t		raw;
} utt_ieee754_float_t;

typedef union {
	unsigned char		data[sizeof(utt_ieee754_half)];
	utt_ieee754_half	value;
	utt_uint16_t		raw;
} utt_ieee754_half_t;

typedef union {
	unsigned char			data[sizeof(utt_ieee754_half)];
	utt_ieee754_half_alt	value;
	utt_uint16_t			raw;
} utt_ieee754_half_alt_t;



/** Returns the supplied utt_ieee754_float as raw integer and vice versa */
UTT_LINKAGE utt_uint64_t utt_toraw( utt_ieee754_double val ){ utt_ieee754_double_t tmp = { .value = val }; return tmp.raw; }
UTT_LINKAGE utt_uint32_t utt_torawf( utt_ieee754_float val ){ utt_ieee754_float_t tmp = { .value = val }; return tmp.raw; }
UTT_LINKAGE utt_uint16_t utt_torawh( utt_ieee754_half val ){ utt_ieee754_half_t tmp = { .value = val }; return tmp.raw; }
UTT_LINKAGE utt_uint16_t utt_torawh2( utt_ieee754_half_alt val ){ utt_ieee754_half_alt_t tmp = { .value = val }; return tmp.raw; }
#define utt_fromraw( val ) ((utt_ieee754_double_t){ .raw = val }).value
#define utt_fromrawf( val ) ((utt_ieee754_float_t){ .raw = val }).value
#define utt_fromrawh( val ) ((utt_ieee754_half_t){ .raw = val }).value
#define utt_fromrawh2( val ) ((utt_ieee754_half_alt_t){ .raw = val }).value



/** Define IEEE 754 floating point constants */
#define UTT_EXPONENT_BIAS		1023
#define UTT_EXPONENT_BIASF		127
#define UTT_EXPONENT_BIASH		15
#define UTT_EXPONENT_BIASH2		UTT_EXPONENT_BIASH
#define UTT_EXPONENT_SHIFT		52u
#define UTT_EXPONENT_SHIFTF		23u
#define UTT_EXPONENT_SHIFTH		10u
#define UTT_EXPONENT_SHIFTH2	UTT_EXPONENT_SHIFTH
#define UTT_SIGNBIT_SHIFT		63u
#define UTT_SIGNBIT_SHIFTF		31u
#define UTT_SIGNBIT_SHIFTH		15u
#define UTT_SIGNBIT_SHIFTH2		UTT_SIGNBIT_SHIFTH
#define UTT_SIGNBIT_MASK		( 1uLL << UTT_SIGNBIT_SHIFT )
#define UTT_SIGNBIT_MASKF		( 1uLL << UTT_SIGNBIT_SHIFTF )
#define UTT_SIGNBIT_MASKH		( 1uLL << UTT_SIGNBIT_SHIFTH )
#define UTT_SIGNBIT_MASKH2		UTT_SIGNBIT_MASKH
#define UTT_EXPONENT_MASK		0x7FF0000000000000uLL
#define UTT_EXPONENT_MASKF		0x7F800000uL
#define UTT_EXPONENT_MASKH		0x7C00u
#define UTT_EXPONENT_MASKH2		UTT_EXPONENT_MASKH
#define UTT_MANTISSA_MASK		0xFFFFFFFFFFFFFuLL
#define UTT_MANTISSA_MASKF		0x7FFFFFuL
#define UTT_MANTISSA_MASKH		0x3FFu
#define UTT_MANTISSA_MASKH2		UTT_MANTISSA_MASKH
#if TARGET_NATIVE_DOUBLE == IEEE_754
	#define UTT_INFINITY		(1./0.)
	#define UTT_NINFINITY		(-1./0.)
	#define UTT_NAN				(0./0.)
#else
	#define UTT_INFINITY		utt_fromraw(UTT_EXPONENT_MASK)
	#define UTT_NINFINITY		utt_fromraw(UTT_EXPONENT_MASK|UTT_SIGNBIT_MASK)
	#define UTT_NAN				utt_fromraw(UTT_EXPONENT_MASK|(UTT_MANTISSA_MASK>>1))
#endif
#if TARGET_NATIVE_FLOAT == IEEE_754
	#define UTT_INFINITYF		(1.f/0.f)
	#define UTT_NINFINITYF		(-1.f/0.f)
	#define UTT_NANF			(0.f/0.f)
#else
	#define UTT_INFINITYF		utt_fromrawf(UTT_EXPONENT_MASKF)
	#define UTT_NINFINITYF		utt_fromrawf(UTT_EXPONENT_MASKF|UTT_SIGNBIT_MASKF)
	#define UTT_NANF			utt_fromrawf(UTT_EXPONENT_MASKF|(UTT_MANTISSA_MASKF>>1))
#endif
#if TARGET_NATIVE_HALF == IEEE_754 && TARGET_NATIVE_FLOAT == IEEE_754
	#define UTT_INFINITYH		((TARGET_IEEE_754_HALF_TYPE)(1.f/0.f))
	#define UTT_NINFINITYH		((TARGET_IEEE_754_HALF_TYPE)(-1.f/0.f))
	#define UTT_NANH			((TARGET_IEEE_754_HALF_TYPE)(0.f/0.f))
#elif TARGET_NATIVE_HALF == IEEE_754 && TARGET_NATIVE_DOUBLE == IEEE_754
	#define UTT_INFINITYH		((TARGET_IEEE_754_HALF_TYPE)(1./0.))
	#define UTT_NINFINITYH		((TARGET_IEEE_754_HALF_TYPE)(-1./0.))
	#define UTT_NANH			((TARGET_IEEE_754_HALF_TYPE)(0./0.))
#else
	#define UTT_INFINITYH		utt_fromrawh(UTT_EXPONENT_MASKH)
	#define UTT_NINFINITYH		utt_fromrawh(UTT_EXPONENT_MASKH|UTT_SIGNBIT_MASKH)
	#define UTT_NANH			utt_fromrawh(UTT_EXPONENT_MASKH|(UTT_MANTISSA_MASKH>>1))
#endif
#define UTT_INFINITYH2			utt_fromrawh2(0x7FFFu) /* Not defined, only for sake of completeness */
#define UTT_NINFINITYH2			utt_fromrawh2(0xFFFFu) /* Not defined, only for sake of completeness */
#define UTT_NANH2				utt_fromrawh2(0u) /* Not defined, only for sake of completeness */

#endif