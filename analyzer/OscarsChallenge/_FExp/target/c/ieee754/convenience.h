/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE_754_CONVENIENCE_H_
#define _TARGET_UNITTEST_IEEE_754_CONVENIENCE_H_

#include <target/c/ieee754/predefines.h>


/** Define a getter for the signbit */
UTT_LINKAGE bool utt_signbit( utt_ieee754_double val ){ return utt_toraw( val ) >> UTT_SIGNBIT_SHIFT; }
UTT_LINKAGE bool utt_signbitf( utt_ieee754_float val ){ return utt_torawf( val ) >> UTT_SIGNBIT_SHIFTF; }
UTT_LINKAGE bool utt_signbith( utt_ieee754_half val ){ return utt_torawh( val ) >> UTT_SIGNBIT_SHIFTH; }
UTT_LINKAGE bool utt_signbith2( utt_ieee754_half_alt val ){ return utt_torawh2( val ) >> UTT_SIGNBIT_SHIFTH; }

/** Define a getter for the mantissa */
UTT_LINKAGE utt_uint64_t utt_mantissa( utt_ieee754_double val ){ return utt_toraw( val ) & UTT_MANTISSA_MASK; }
UTT_LINKAGE utt_uint32_t utt_mantissaf( utt_ieee754_float val ){ return utt_torawf( val ) & UTT_MANTISSA_MASKF; }
UTT_LINKAGE utt_uint16_t utt_mantissah( utt_ieee754_half val ){ return utt_torawh( val ) & UTT_MANTISSA_MASKH; }
UTT_LINKAGE utt_uint16_t utt_mantissah2( utt_ieee754_half_alt val ){ return utt_torawh2( val ) & UTT_MANTISSA_MASKH; }

/** Define a getter for the exponent */
UTT_LINKAGE utt_uint16_t utt_exponent( utt_ieee754_double val ){ return ( utt_toraw( val ) & UTT_EXPONENT_MASK ) >> UTT_EXPONENT_SHIFT; }
UTT_LINKAGE utt_uint8_t utt_exponentf( utt_ieee754_float val ){ return ( utt_torawf( val ) & UTT_EXPONENT_MASKF ) >> UTT_EXPONENT_SHIFTF; }
UTT_LINKAGE utt_uint8_t utt_exponenth( utt_ieee754_half val ){ return ( utt_torawh( val ) & UTT_EXPONENT_MASKH ) >> UTT_EXPONENT_SHIFTH; }
UTT_LINKAGE utt_uint8_t utt_exponenth2( utt_ieee754_half_alt val ){ return ( utt_torawh2( val ) & UTT_EXPONENT_MASKH ) >> UTT_EXPONENT_SHIFTH; }



/** Define IEC 559 floating point classes */
typedef enum {
	UTT_FP_ZERO = 1
	, UTT_FP_SUBNORMAL = 2
	, UTT_FP_NORMAL = 4
	, UTT_FP_INFINITE = 8
	, UTT_FP_NAN = 16
} utt_ieee754_class_t;



/** Define fpclassify for different floating point types */
UTT_LINKAGE utt_ieee754_class_t utt_fp_classify( utt_ieee754_double val ){
	if( ( utt_toraw(val) & UTT_EXPONENT_MASK ) == UTT_EXPONENT_MASK )
		return utt_mantissa(val) ? UTT_FP_NAN : UTT_FP_INFINITE;
	else if( !(utt_toraw(val) & UTT_EXPONENT_MASK) )
		return utt_mantissa(val) ? UTT_FP_SUBNORMAL : UTT_FP_ZERO;
	return UTT_FP_NORMAL;
}

UTT_LINKAGE utt_ieee754_class_t utt_fp_classifyf( utt_ieee754_float val ){
	if( ( utt_torawf(val) & UTT_EXPONENT_MASKF ) == UTT_EXPONENT_MASKF )
		return utt_mantissaf(val) ? UTT_FP_NAN : UTT_FP_INFINITE;
	else if( !(utt_torawf(val) & UTT_EXPONENT_MASKF) )
		return utt_mantissaf(val) ? UTT_FP_SUBNORMAL : UTT_FP_ZERO;
	return UTT_FP_NORMAL;
}

UTT_LINKAGE utt_ieee754_class_t utt_fp_classifyh( utt_ieee754_half val ){
	if( ( utt_torawh(val) & UTT_EXPONENT_MASKH ) == UTT_EXPONENT_MASKH )
		return utt_mantissah(val) ? UTT_FP_NAN : UTT_FP_INFINITE;
	else if( !(utt_torawh(val) & UTT_EXPONENT_MASKH) )
		return utt_mantissah(val) ? UTT_FP_SUBNORMAL : UTT_FP_ZERO;
	return UTT_FP_NORMAL;
}

UTT_LINKAGE utt_ieee754_class_t utt_fp_classifyh2( utt_ieee754_half_alt val ){
	if( !(utt_torawh2(val) & UTT_EXPONENT_MASKH) )
		return utt_mantissah(val) ? UTT_FP_SUBNORMAL : UTT_FP_ZERO;
	return UTT_FP_NORMAL;
}



/** Define isnan for different floating point types */
UTT_LINKAGE bool utt_isnan( utt_ieee754_double val ){ return utt_fp_classify( val ) == UTT_FP_NAN; }
UTT_LINKAGE bool utt_isnanf( utt_ieee754_float val ){ return utt_fp_classifyf( val ) == UTT_FP_NAN; }
UTT_LINKAGE bool utt_isnanh( utt_ieee754_half val ){ return utt_fp_classifyh( val ) == UTT_FP_NAN; }
UTT_LINKAGE bool utt_isnanh2( utt_ieee754_half_alt val ){ (void)val; return false; }

/** Define isinf for different floating point types */
UTT_LINKAGE bool utt_isinf( utt_ieee754_double val ){ return utt_fp_classify( val ) == UTT_FP_INFINITE; }
UTT_LINKAGE bool utt_isinff( utt_ieee754_float val ){ return utt_fp_classifyf( val ) == UTT_FP_INFINITE; }
UTT_LINKAGE bool utt_isinfh( utt_ieee754_half val ){ return utt_fp_classifyh( val ) == UTT_FP_INFINITE; }
UTT_LINKAGE bool utt_isinfh2( utt_ieee754_half_alt val ){ (void)val; return false; }

/** Define abs for different floating point types */
UTT_LINKAGE utt_ieee754_double utt_fabs( utt_ieee754_double val ){ return utt_fromraw( utt_toraw(val) & ~UTT_SIGNBIT_MASK ); }
UTT_LINKAGE utt_ieee754_float utt_fabsf( utt_ieee754_float val ){ return utt_fromrawf( utt_torawf(val) & ~UTT_SIGNBIT_MASKF ); }
UTT_LINKAGE utt_ieee754_half utt_fabsh( utt_ieee754_half val ){ return utt_fromrawh( utt_torawh(val) & ~UTT_SIGNBIT_MASKH ); }
UTT_LINKAGE utt_ieee754_half_alt utt_fabsh2( utt_ieee754_half_alt val ){ return utt_fromrawh2( utt_torawh2(val) & ~UTT_SIGNBIT_MASKH ); }

#endif