/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE_754_CONVERSION_H_
#define _TARGET_UNITTEST_IEEE_754_CONVERSION_H_

#include <target/c/ieee754/predefines.h>
#include <target/c/ieee754/convenience.h>


/** Function to cast from 'utt_ieee754_half' to 'utt_ieee754_float' and vice versa */
#if ( TARGET_NATIVE_FLOAT == IEEE_754 ) && ( TARGET_NATIVE_HALF == IEEE_754 )
	UTT_LINKAGE utt_ieee754_float utt_half2float( utt_ieee754_half val ){ return (utt_ieee754_float)val; }
	UTT_LINKAGE utt_ieee754_half utt_float2half_ex( utt_ieee754_float val , utt_round_t round_mode ){ (void)round_mode; return (utt_ieee754_half)val; }
#else
	UTT_LINKAGE utt_ieee754_float utt_half2float( utt_ieee754_half val ){
		switch( utt_fp_classifyh(val) )
		{
		case UTT_FP_INFINITE:
			return utt_signbith(val) ? UTT_NINFINITYF : UTT_INFINITYF;
		case UTT_FP_NAN:
			return utt_fromrawf(
				( (utt_uint32_t)utt_signbith(val) << UTT_SIGNBIT_SHIFTF )
				| UTT_EXPONENT_MASKF
				| utt_mantissah(val)
			);
		case UTT_FP_ZERO:
			return utt_fromrawf( (utt_uint32_t)utt_signbith(val) << UTT_SIGNBIT_SHIFTF );
		case UTT_FP_NORMAL:
			return utt_fromrawf(
				( (utt_uint32_t)utt_signbith(val) << UTT_SIGNBIT_SHIFTF )
				| (utt_uint32_t)( (int)utt_exponenth(val) - UTT_EXPONENT_BIASH + UTT_EXPONENT_BIASF ) << UTT_EXPONENT_SHIFTF
				| ( (utt_uint32_t)utt_mantissah(val) << ( UTT_EXPONENT_SHIFTF - UTT_EXPONENT_SHIFTH ) )
			);
		case UTT_FP_SUBNORMAL:
			{
				utt_uint16_t num_mant_bits = utt_log2_16( utt_mantissah(val) );
				return utt_fromrawf(
					( (utt_uint32_t)utt_signbith(val) << UTT_SIGNBIT_SHIFTF )
					| ( (utt_uint32_t)( UTT_EXPONENT_BIASF - ( UTT_EXPONENT_BIASH - 1 ) - ( UTT_EXPONENT_SHIFTH - num_mant_bits ) ) << UTT_EXPONENT_SHIFTF )
					| ( ( (utt_uint32_t)utt_mantissah(val) << ( UTT_EXPONENT_SHIFTF - num_mant_bits ) ) & /*Remove trailing '1'*/ UTT_MANTISSA_MASKF )
				);
			}
		}
	}
	UTT_LINKAGE utt_ieee754_half utt_float2half_ex( utt_ieee754_float val , utt_round_t round_mode )
	{
		switch( utt_fp_classifyf(val) )
		{
		case UTT_FP_INFINITE:	return utt_signbitf(val) ? UTT_NINFINITYH : UTT_INFINITYH;
		case UTT_FP_NAN:		return UTT_NANH;
		case UTT_FP_ZERO:		return utt_fromrawh( (utt_uint16_t)utt_signbitf(val) << UTT_SIGNBIT_SHIFTH );
		case UTT_FP_SUBNORMAL:
			switch( round_mode ){
				case UTT_ROUND_AWAY_FROM_ZERO:		return utt_fromrawh( UTT_SIGNBIT_MASKH * utt_signbitf(val) | 1 );
				case UTT_ROUND_TOWARD_POS_INFINITY:	return utt_fromrawh( (utt_uint16_t)1u << ( UTT_SIGNBIT_SHIFTH * utt_signbitf(val) ) ); /* -0.0 or 1ulp */
				case UTT_ROUND_TOWARD_NEG_INFINITY:	return utt_fromrawh( ( UTT_SIGNBIT_MASKH | 1 ) * utt_signbitf(val) );
				default:							return utt_fromrawh( (utt_uint16_t)utt_signbitf(val) << UTT_SIGNBIT_SHIFTH );
			}
			break;
		case UTT_FP_NORMAL:
			break;
		}
		
		int exp = utt_exponentf( val ) - UTT_EXPONENT_BIASF + UTT_EXPONENT_BIASH; /* Compute resulting exponent */
		
		bool cut_half; /* Indicates, we are chopping off something >=.5 */
		bool cut_low; /* Indicates, we are chopping off something >0 and <.5 */
		utt_uint16_t result;
		
		/* Results in normal value? */
		if( exp > 0 ){
			result		= ( utt_mantissaf( val ) >> (UTT_EXPONENT_SHIFTF - UTT_EXPONENT_SHIFTH) );
			cut_half	= utt_mantissaf( val ) & ( 1 << (UTT_EXPONENT_SHIFTF - UTT_EXPONENT_SHIFTH - 1) );
			cut_low		= utt_mantissaf( val ) & ( ( 1 << (UTT_EXPONENT_SHIFTF - UTT_EXPONENT_SHIFTH - 1) ) - 1 );
		}
		/* Results in subnormal value? */
		else if( exp >= -UTT_EXPONENT_SHIFTH ){
			/* Note: Initially, the '>=' was a '>', but this allows us to say 'cut_half=false' a few lines down the road. */
			int shift	= UTT_EXPONENT_SHIFTF - UTT_EXPONENT_SHIFTH + 1 - exp;
			result		= ( utt_mantissaf( val ) + UTT_MANTISSA_MASKF + 1 ) >> shift;
			cut_half	= 1L << (shift - 1);
			cut_low		= utt_mantissaf( val ) & ( cut_half - 1 );
			cut_half	= utt_mantissaf( val ) & cut_half;
			exp			= 0;
		}
		else{
			result		= 0;
			exp			= 0;
			cut_half	= false;
			cut_low		= true; /* We must be cutting something away, since we didn't start with '0' in the first place */
		}
		
		switch( round_mode ){
			case UTT_ROUND_TOWARD_POS_INFINITY:		result += cut_half || cut_low ? 1 - utt_signbitf( val ) : 0; break; /* Round up, if we cut >0 when x>0 */
			case UTT_ROUND_TOWARD_NEG_INFINITY:		result += cut_half || cut_low ? utt_signbitf( val ) : 0; break; /* Round up, if we cut >0 when x<0 */
			case UTT_ROUND_AWAY_FROM_ZERO:			result += cut_half || cut_low ? 1 : 0; break; /* Round up, if we cut >0 */
			case UTT_ROUND_TO_NEAREST:				result += cut_half ? 1 : 0; break; /* Round up, if we cut >=0.5 */
			case UTT_ROUND_TO_NEAREST_TIES_ZERO:	result += cut_half && cut_low ? 1 : 0; break; /* Round up, if we cut >.5 */
			case UTT_ROUND_TO_NEAREST_TIES_EVEN:
			case UTT_ROUND_TO_NEAREST_IEEE753:
				if( cut_low ) /* Round up, if we cut >0.5 */
					result += cut_half ? 1 : 0;
				else if( cut_half )
					result += result % 2u; /* Round up only, if the mantissa is currently uneven */
				break;
			default:								break;
		}
		
		/* Need to normalize again? */
		if( result > UTT_MANTISSA_MASKH && exp != 0 ){
			/*
			 * The reason, we don't manually normalize again previously denormalized numbers (see "&& exp" above )
			 * is that 0.1111...b rounded up results in the correct value already (the minimum non-denormalized number).
			 * Since 'exp' is still 0, the result is correctly computed down below in the call to 'utt_fromrawh'.
			 */
			result >>= 1; /* The result can only be larger than 1.11111...b due to rounding, in which case a left shift by one is sufficient */
			++exp; /* Adjust the exponent */
		}
		
		/* Greater maximum possible exponent? */
		if( exp > (int)(UTT_EXPONENT_MASKH >> UTT_EXPONENT_SHIFTH) )
			switch( round_mode ){
				case UTT_ROUND_TOWARD_POS_INFINITY:	return utt_signbitf( val ) ? UTT_SIGNBIT_MASKH | ( UTT_EXPONENT_MASKH - 1 ) : UTT_EXPONENT_MASKH;
				case UTT_ROUND_TOWARD_NEG_INFINITY:	return utt_signbitf( val ) ? UTT_SIGNBIT_MASKH | UTT_EXPONENT_MASKH : UTT_EXPONENT_MASKH - 1;
				case UTT_ROUND_TO_NEAREST_IEEE753:
				case UTT_ROUND_AWAY_FROM_ZERO:		return ( utt_signbitf( val ) << UTT_SIGNBIT_SHIFTH ) | UTT_EXPONENT_MASKH;
				default:							return ( utt_signbitf( val ) << UTT_SIGNBIT_SHIFTH ) | ( UTT_EXPONENT_MASKH - 1 );
			}
		
		return utt_fromrawh( result | ( (utt_uint8_t)exp << UTT_EXPONENT_SHIFTH ) | ( utt_signbitf( val ) << UTT_SIGNBIT_SHIFTH ) );
	}
#endif
UTT_LINKAGE utt_ieee754_half utt_float2half( utt_ieee754_float val ){ return utt_float2half_ex( val , UTT_ROUND_TO_NEAREST_IEEE753 ); }



/** Function to cast from 'utt_ieee754_half_alt' to 'utt_ieee754_float' and vice versa */
#if ( TARGET_NATIVE_FLOAT == IEEE_754 ) && ( TARGET_NATIVE_HALF == IEEE_754 )
	UTT_LINKAGE utt_ieee754_float utt_half_alt2float( utt_ieee754_half_alt val ){ return (utt_ieee754_float)val; }
	UTT_LINKAGE utt_ieee754_half_alt utt_float2half_alt_ex( utt_ieee754_float val , utt_round_t round_mode ){ (void)round_mode; return (utt_ieee754_half_alt)val; }
#else
	UTT_LINKAGE utt_ieee754_float utt_half_alt2float( utt_ieee754_half_alt val ){
		switch( utt_fp_classifyh2(val) )
		{
		case UTT_FP_ZERO:
			return utt_fromrawf( (utt_uint32_t)utt_signbith2(val) << UTT_SIGNBIT_SHIFTF );
		case UTT_FP_NORMAL:
			return utt_fromrawf(
				( (utt_uint32_t)utt_signbith2(val) << UTT_SIGNBIT_SHIFTF )
				| (utt_uint32_t)( (int)utt_exponenth2(val) - UTT_EXPONENT_BIASH + UTT_EXPONENT_BIASF ) << UTT_EXPONENT_SHIFTF
				| ( (utt_uint32_t)utt_mantissah2(val) << ( UTT_EXPONENT_SHIFTF - UTT_EXPONENT_SHIFTH ) )
			);
		case UTT_FP_SUBNORMAL:
		default:
			{
				utt_uint16_t num_mant_bits = utt_log2_16( utt_mantissah2(val) );
				return utt_fromrawf(
					( (utt_uint32_t)utt_signbith2(val) << UTT_SIGNBIT_SHIFTF )
					| ( (utt_uint32_t)( UTT_EXPONENT_BIASF - ( UTT_EXPONENT_BIASH - 1 ) - ( UTT_EXPONENT_SHIFTH - num_mant_bits ) ) << UTT_EXPONENT_SHIFTF )
					| ( ( (utt_uint32_t)utt_mantissah2(val) << ( UTT_EXPONENT_SHIFTF - num_mant_bits ) ) & /*Remove trailing '1'*/ UTT_MANTISSA_MASKF )
				);
			}
		}
	}
	UTT_LINKAGE utt_ieee754_half_alt utt_float2half_alt_ex( utt_ieee754_float val , utt_round_t round_mode ){ utt_assert_ex( false , "Non-native conversion between IEEE 754 float and half not implemented." ); utt_ieee754_half_alt tmp = 0; return tmp; }
#endif
UTT_LINKAGE utt_ieee754_half_alt utt_float2half_alt( utt_ieee754_float val ){ return utt_float2half_alt_ex( val , UTT_ROUND_TO_NEAREST_IEEE753 ); }



/** Function to cast from 'utt_ieee754_float' to 'utt_ieee754_double' and vice versa */
#if ( TARGET_NATIVE_FLOAT == IEEE_754 ) && ( TARGET_NATIVE_DOUBLE == IEEE_754 )
	UTT_LINKAGE utt_ieee754_double utt_float2double( utt_ieee754_float val ){ return (utt_ieee754_double)val; }
	UTT_LINKAGE utt_ieee754_float utt_double2float_ex( utt_ieee754_double val , utt_round_t round_mode ){ (void)round_mode; return (utt_ieee754_float)val; }
#else
	UTT_LINKAGE utt_ieee754_double utt_float2double( utt_ieee754_float val ){
		switch( utt_fp_classifyf(val) )
		{
		case UTT_FP_INFINITE:
			return utt_signbitf(val) ? UTT_NINFINITY : UTT_INFINITY;
		case UTT_FP_NAN:
			return utt_fromraw(
				( (utt_uint64_t)utt_signbitf(val) << UTT_SIGNBIT_SHIFT )
				| UTT_EXPONENT_MASK
				| utt_mantissaf(val)
			);
		case UTT_FP_ZERO:
			return utt_fromraw( (utt_uint64_t)utt_signbitf(val) << UTT_SIGNBIT_SHIFT );
		case UTT_FP_NORMAL:
			return utt_fromraw(
				( (utt_uint64_t)utt_signbitf(val) << UTT_SIGNBIT_SHIFT )
				| (utt_uint64_t)( (int)utt_exponentf(val) - UTT_EXPONENT_BIASF + UTT_EXPONENT_BIAS ) << UTT_EXPONENT_SHIFT
				| ( (utt_uint64_t)utt_mantissaf(val) << ( UTT_EXPONENT_SHIFT - UTT_EXPONENT_SHIFTF ) )
			);
		case UTT_FP_SUBNORMAL:
			{
				utt_uint16_t num_mant_bits = utt_log2_32( utt_mantissaf(val) );
				return utt_fromraw(
					( (utt_uint64_t)utt_signbitf(val) << UTT_SIGNBIT_SHIFT )
					| ( (utt_uint64_t)( UTT_EXPONENT_BIAS - ( UTT_EXPONENT_BIASF - 1 ) - ( UTT_EXPONENT_SHIFTF - num_mant_bits ) ) << UTT_EXPONENT_SHIFT )
					| ( ( (utt_uint64_t)utt_mantissaf(val) << ( UTT_EXPONENT_SHIFT - num_mant_bits + 1 ) ) & /*Remove trailing '1'*/ UTT_MANTISSA_MASK )
				);
			}
		}
	}
	UTT_LINKAGE utt_ieee754_float utt_double2float_ex( utt_ieee754_double val ){ utt_assert_ex( false , "Non-native conversion between IEEE 754 float and double not implemented." ); utt_ieee754_double tmp = 0; return tmp; }
#endif
UTT_LINKAGE utt_ieee754_float utt_double2float( utt_ieee754_double val ){ return utt_double2float_ex( val , UTT_ROUND_TO_NEAREST_IEEE753 ); }

/** Function to cast from 'utt_ieee754_double' to 'utt_ieee754_half' and vice versa */
UTT_LINKAGE utt_ieee754_double utt_half2double( utt_ieee754_half val ){ return utt_float2double( utt_half2float(val) ); }
UTT_LINKAGE utt_ieee754_half utt_double2half_ex( utt_ieee754_double val , utt_round_t round_mode ){
	return utt_float2half_ex( utt_double2float_ex( val , round_mode ) , round_mode );
}
UTT_LINKAGE utt_ieee754_half utt_double2half( utt_ieee754_double val ){ return utt_double2half_ex( val , UTT_ROUND_TO_NEAREST_IEEE753 ); }

/** Function to cast from 'utt_ieee754_double' to 'utt_ieee754_half_alt' and vice versa */
UTT_LINKAGE utt_ieee754_double utt_half_alt2double( utt_ieee754_half_alt val ){ return utt_float2double( utt_half_alt2float(val) ); }
UTT_LINKAGE utt_ieee754_half_alt utt_double2half_alt_ex( utt_ieee754_double val , utt_round_t round_mode ){
	return utt_float2half_alt_ex( utt_double2float_ex( val , round_mode ) , round_mode );
}
UTT_LINKAGE utt_ieee754_half_alt utt_double2half_alt( utt_ieee754_double val ){ return utt_double2half_alt_ex( val , UTT_ROUND_TO_NEAREST_IEEE753 ); }

#endif