/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_IEEE_754_COMPARISON_H_
#define _TARGET_UNITTEST_IEEE_754_COMPARISON_H_

#include <target/c/ieee754/predefines.h>
#include <target/c/ieee754/convenience.h>

/** Define isless for different floating point types */
#if TARGET_NATIVE_DOUBLE == IEEE_754
	UTT_LINKAGE bool utt_isless( utt_ieee754_double left , utt_ieee754_double right ){ return left < right; }
#else
	UTT_LINKAGE bool utt_isless( utt_ieee754_double left , utt_ieee754_double right )
	{
		utt_uint64_t labs = utt_toraw( utt_fabs(left) ), rabs = utt_toraw( utt_fabs(right) );
		return
			!utt_isnan(left)
			&& !utt_isnan(right)
			&& ( utt_signbit(left) ? -(utt_int64_t)labs : (utt_int64_t)labs )
			< (	utt_signbit(right) ? -(utt_int64_t)rabs : (utt_int64_t)rabs )
		;
	}
#endif

#if TARGET_NATIVE_FLOAT == IEEE_754
	UTT_LINKAGE bool utt_islessf( utt_ieee754_float left , utt_ieee754_float right ){ return left < right; }
#else
	UTT_LINKAGE bool utt_islessf( utt_ieee754_float left , utt_ieee754_float right ){
		utt_uint32_t labs = utt_torawf( utt_fabsf(left) ), rabs = utt_torawf( utt_fabsf(right) );
		return
			!utt_isnanf(left)
			&& !utt_isnanf(right)
			&& ( utt_signbitf(left) ? -(utt_int32_t)labs : (utt_int32_t)labs )
			< (	utt_signbitf(right) ? -(utt_int32_t)rabs : (utt_int32_t)rabs )
		;
	}
#endif
#if TARGET_NATIVE_HALF == IEEE_754
	UTT_LINKAGE bool utt_islessh( utt_ieee754_half left , utt_ieee754_half right ){ return left < right; }
#else
	UTT_LINKAGE bool utt_islessh( utt_ieee754_half left , utt_ieee754_half right ){
		utt_uint16_t labs = utt_torawh( utt_fabsh(left) ), rabs = utt_torawh( utt_fabsh(right) );
		return
			!utt_isnanh(left)
			&& !utt_isnanh(right)
			&& ( utt_signbith(left) ? -(utt_int16_t)labs : (utt_int16_t)labs )
			< (	utt_signbith(right) ? -(utt_int16_t)rabs : (utt_int16_t)rabs )
		;
	}
#endif
#if TARGET_NATIVE_HALF == IEEE_754_ARM
	UTT_LINKAGE bool utt_islessh2( utt_ieee754_half_alt left , utt_ieee754_half_alt right ){ return left < right; }
#else
	UTT_LINKAGE bool utt_islessh2( utt_ieee754_half_alt left , utt_ieee754_half_alt right ){
		utt_uint16_t labs = utt_torawh( utt_fabsh(left) ), rabs = utt_torawh( utt_fabsh(right) );
		return
			( utt_signbith(left) ? -(utt_int16_t)labs : (utt_int16_t)labs )
			< (	utt_signbith(right) ? -(utt_int16_t)rabs : (utt_int16_t)rabs )
		;
	}
#endif

#endif
