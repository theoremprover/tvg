/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_ORACLE_COMPARISON_H_
#define _TARGET_UNITTEST_ORACLE_COMPARISON_H_

#include <target/c/math.h>
#include <target/c/stdfloat.h>
#include <target/c/bigfloat/bigfloat.h>
#include <target/c/stdio.h>

/* Customize this for shortcutting */
#ifndef TARGET_ULP_TOO_LARGE
#define TARGET_ULP_TOO_LARGE 999
#endif

/** Struct to specify error in various ways */
typedef struct utt_error_t{
	utt_ieee754_double	abs_error;
	utt_ieee754_double	ulp_error;
} utt_error_t;


/** Enumeration of the types of thresholds */
typedef unsigned short int utt_threshold;
/* Range */
#define UTT_THLD_NONE		((utt_threshold)0x0)
#define UTT_THLD_BELOW		((utt_threshold)0x1 << 0)
#define UTT_THLD_ABOVE		((utt_threshold)0x1 << 1)
#define UTT_THLD_SYMMETRIC	(UTT_THLD_BELOW|UTT_THLD_ABOVE)
	
/* Inclusiveness */
#define UTT_THLD_BLW_INCL	((utt_threshold)0x1 << 2)
#define UTT_THLD_ABV_INCL	((utt_threshold)0x1 << 3)
#define UTT_THLD_INCL		(UTT_THLD_BLW_INCL|UTT_THLD_ABV_INCL)
#define UTT_THLD_EXCL		((utt_threshold)0x0)
	
/* Type */
#define UTT_THLD_ULP		((utt_threshold)0x1 << 4)
#define UTT_THLD_ABS		((utt_threshold)0x1 << 5)



/**
 * Set of functions the check, whether the distance of two utt_ieee754_float values can reasonably be computed
 */
UTT_LINKAGE bool utt_check_classifyable_impl( utt_ieee754_class_t obs_class , utt_ieee754_class_t ref_class , bool obs_signbit , bool ref_signbit , utt_error_t* dest )
{
	/* Handle Infinities */
	if( obs_class == UTT_FP_INFINITE ){
		if( obs_signbit != ref_signbit || ref_class != UTT_FP_INFINITE )
			dest->abs_error = UTT_INFINITY , dest->ulp_error = UTT_INFINITY;
		return false;
	}
	else if( ref_class == UTT_FP_INFINITE ){ /* Still infinitely large error */
		dest->abs_error = UTT_INFINITY , dest->ulp_error = UTT_INFINITY;
		return false;
	}
	
	/* Handle NaNs */
	if( obs_class == UTT_FP_NAN && ref_class == UTT_FP_NAN ) /* Both are NaN: Perfect! */
		return false;
	else if( obs_class == UTT_FP_NAN || ref_class == UTT_FP_NAN ){ /* One or the other is NaN, but not both */
		dest->abs_error = UTT_INFINITY , dest->ulp_error = UTT_INFINITY;
		return false;
	}
	
	/* Handle Zeros */
	if( obs_class == UTT_FP_ZERO && ref_class == UTT_FP_ZERO ){
		dest->abs_error = 0 , dest->ulp_error = ( obs_signbit == ref_signbit ? 0 : 1 );
		return false;
	}
	
	/* Things are comparable, so we can proceed to determine the error between both */
	return true;
}
#define UTT_DEFINE_CHECK_CLASSIFYABLE( TYPENAME , suffix ) \
UTT_LINKAGE bool utt_check_classifyable##suffix( utt_ieee754_##TYPENAME observed , utt_bigfloat_t reference , utt_error_t* dest ){ \
	return utt_check_classifyable_impl( utt_fp_classify##suffix(observed) , utt_fp_classifym(reference) , utt_signbit##suffix(observed) , utt_signbitm(reference) , dest ); \
}
UTT_DEFINE_CHECK_CLASSIFYABLE( double	,		)
UTT_DEFINE_CHECK_CLASSIFYABLE( float	, f		)
UTT_DEFINE_CHECK_CLASSIFYABLE( half		, h		)
UTT_DEFINE_CHECK_CLASSIFYABLE( half_alt	, h2	)


/** Functions to determine the error between an observed result and a reference value */
#define UTT_DEFINE_DETERMINE_ERROR( TYPENAME , suffix , SUFFIX , HAS_INFINITY ) \
UTT_LINKAGE utt_error_t utt_determine_error##suffix( utt_ieee754_##TYPENAME observed , utt_bigfloat_t reference ) \
{ \
	utt_error_t result = { 0 , 0 }; \
	\
	/* Check, if the two numbers are even comparable */ \
	if( !utt_check_classifyable##suffix( observed , reference , &result ) ) \
		return result; /* apparently not... */ \
	\
	/* Perform initial step */ \
	utt_bigfloat_t			observed_precise = utt_##TYPENAME##2bigfloat( observed ); \
	utt_ieee754_##TYPENAME	next = utt_nextafter##suffix( \
		observed \
		, utt_islessm( observed_precise , reference ) ? UTT_INFINITY##SUFFIX : UTT_NINFINITY##SUFFIX \
	); \
	utt_bigfloat_t			next_precise = utt_##TYPENAME##2bigfloat( next ); \
	\
	/* Set the absolute error between both */ \
	result.abs_error = utt_bigfloat2double( utt_addsubm( observed_precise , reference , true ) ); \
	\
	/* Give an estimate of the error in ULP */ \
	utt_ieee754_double initial_guess = utt_bigfloat2double( utt_addsubm( reference , observed_precise , true ) ) / utt_bigfloat2double( utt_addsubm( next_precise , observed_precise , true ) ); \
	if( utt_fabs(initial_guess) > TARGET_ULP_TOO_LARGE ){ \
		result.ulp_error = initial_guess; \
		return result; \
	}\
	\
	/* we will approach the reference values while counting epsilons */ \
	if( utt_islessm( observed_precise , reference ) ) \
		while( true ) \
		{ \
			if( utt_islessm( reference , next_precise ) ){ \
				utt_ieee754_double estimated_ulp = utt_bigfloat2double( utt_addsubm( reference , observed_precise , true ) ) / utt_bigfloat2double( utt_addsubm( next_precise , observed_precise , true ) ); \
				result.ulp_error -= estimated_ulp; \
				break; \
			} \
			\
			/* Did we reach the maximum or minimum representable value? => Stop here */ \
			if( !HAS_INFINITY && observed == next ){ \
				result.ulp_error = -TARGET_ULP_TOO_LARGE; \
				break; \
			} \
			\
			/* Perform iteration cycle, i.e. go one epsilon closer to reference */ \
			result.ulp_error -= 1.; \
			observed = next; \
			observed_precise = next_precise; \
			next = utt_nextafter##suffix( observed , UTT_INFINITY##SUFFIX ); \
			next_precise = utt_##TYPENAME##2bigfloat( next ); \
		} \
	else if( utt_islessm( reference , observed_precise ) ) \
		while( true ) \
		{ \
			if( utt_islessm( next_precise , reference ) ){ \
				utt_ieee754_double estimated_ulp = utt_bigfloat2double( utt_addsubm( observed_precise , reference , true ) ) / utt_bigfloat2double( utt_addsubm( observed_precise , next_precise , true ) ); \
				result.ulp_error += estimated_ulp; \
				break; \
			} \
			\
			/* Did we reach the maximum or minimum representable value? => Stop here */ \
			if( !HAS_INFINITY && observed == next ){ \
				result.ulp_error = TARGET_ULP_TOO_LARGE; \
				break; \
			} \
			\
			/* Perform iteration cycle, i.e. go one epsilon closer to reference */ \
			result.ulp_error += 1.; \
			observed = next; \
			observed_precise = next_precise; \
			next = utt_nextafter##suffix( observed , UTT_NINFINITY##SUFFIX ); \
			next_precise = utt_##TYPENAME##2bigfloat( next ); \
		} \
	\
	return result; \
}
UTT_DEFINE_DETERMINE_ERROR( double		,		, 		, true	)
UTT_DEFINE_DETERMINE_ERROR( float		,	f	, F		, true	)
UTT_DEFINE_DETERMINE_ERROR( half		,	h	, H		, true	)
UTT_DEFINE_DETERMINE_ERROR( half_alt	,	h2	, H2	, false	)


/** Checks whether the supplied error meets (is below) the supplied threshold */
UTT_LINKAGE bool utt_check_threshold( utt_error_t error_struct , utt_threshold thld_type , utt_ieee754_double threshold )
{
	bool				result = false;
	bool				estimate = false;
	utt_ieee754_double	error;
	bool				inclusive;
	const char*			unit;
	
	if( thld_type & UTT_THLD_ABS ){
		error = error_struct.abs_error;
		unit = "ABS";
	}
	else if( thld_type & UTT_THLD_ULP ){
		error = error_struct.ulp_error;
		unit = "ULP";
		estimate = error > TARGET_ULP_TOO_LARGE || error < -TARGET_ULP_TOO_LARGE;
	}
	else{
		utt_print("No threshold type set!");
		return false;
	}
	
	bool below = utt_signbit(error);
	if( below ){
		error = -error;
		inclusive = thld_type & UTT_THLD_BLW_INCL;
		if( !(thld_type & UTT_THLD_BELOW) )
			threshold = 0;
	}
	else{
		inclusive = thld_type & UTT_THLD_ABV_INCL;
		if( !(thld_type & UTT_THLD_ABOVE) )
			threshold = 0;
	}
	
	/* Compute result */
	result = inclusive ? error <= threshold : error < threshold;
	
	/* Output */
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		if( estimate && below )
			utt_print( "ca.(-)%11.-5g" , error );
		else if( estimate )
			utt_print( "ca. %13.-7g" , error );
		else if( below )
			utt_print( "(-)%14.-8g" , error );
		else
			utt_print( "%17.-11g" , error );
		utt_print(
			/*   1         2               3   4  */
			" = [%s DEV] = %s THRESHOLD OF %.g %s\n"
			, unit /* 1 */
			, result ? "MEETS" : "VIOLATES" /* 2 */
			, threshold /* 3 */
			, inclusive ? "(inclusive)" : "(exclusive)" /* 4 */
		);
	}
	
	return result;
}



/**
 * Function that can check whether the supplied 'observed' value is
 * close enough to the reference with regards to the supplied threshold.
 * If that is not the case, the function prints the deviation and the compared values.
 */
UTT_LINKAGE bool utt_is_approximately_impl(
	utt_ieee754_double observed , utt_bigfloat_t reference , utt_threshold thld_type
	, utt_ieee754_double threshold , const char* observed_repr , const char* reference_repr , int reference_precision
){
	bool result = utt_check_threshold( utt_determine_error( observed , reference ) , thld_type , threshold );
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = %.-17g\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = %.*jg\n" , reference_repr , reference_precision , reference );
	}
	return result;
}

UTT_LINKAGE bool utt_is_approximatelyf_impl(
	utt_ieee754_float observed , utt_bigfloat_t reference , utt_threshold thld_type
	, utt_ieee754_double threshold , const char* observed_repr , const char* reference_repr , int reference_precision
){
	bool result = utt_check_threshold( utt_determine_errorf( observed , reference ) , thld_type , threshold );
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = %.-8hg\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = %.*jg\n" , reference_repr , reference_precision , reference );
	}
	return result;
}

UTT_LINKAGE bool utt_is_approximatelyh_impl(
	utt_ieee754_half observed , utt_bigfloat_t reference , utt_threshold thld_type
	, utt_ieee754_double threshold , const char* observed_repr , const char* reference_repr , int reference_precision
){
	bool result = utt_check_threshold( utt_determine_errorh( observed , reference ) , thld_type , threshold );
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = %.-5hhg\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = %.*jg\n" , reference_repr , reference_precision , reference );
	}
	return result;
}

UTT_LINKAGE bool utt_is_approximatelyh2_impl(
	utt_ieee754_half_alt observed , utt_bigfloat_t reference , utt_threshold thld_type
	, utt_ieee754_double threshold , const char* observed_repr , const char* reference_repr , int reference_precision
){
	bool result = utt_check_threshold( utt_determine_errorh2( observed , reference ) , thld_type , threshold );
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = %#.-5hhg\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = %.*jg\n" , reference_repr , reference_precision , reference );
	}
	return result;
}

/* Comparators with utt_bigfloat_t as reference */
#define utt_is_approximatelym( act , ref , thld_type , thld )		utt_is_approximately_impl( act , ref , thld_type , thld , #act , #ref , UTT_INT_MIN )
#define utt_is_approximatelyfm( act , ref , thld_type , thld )		utt_is_approximatelyf_impl( act , ref , thld_type , thld , #act , #ref , UTT_INT_MIN )
#define utt_is_approximatelyhm( act , ref , thld_type , thld )		utt_is_approximatelyh_impl( act , ref , thld_type , thld , #act , #ref , UTT_INT_MIN )
#define utt_is_approximatelyh2m( act , ref , thld_type , thld )		utt_is_approximatelyh2_impl( act , ref , thld_type , thld , #act , #ref , UTT_INT_MIN )

/* Comparators with utt_ieee754_double as reference */
#define utt_is_approximately( act , ref , thld_type , thld )		utt_is_approximately_impl( act , utt_double2bigfloat(ref) , thld_type , thld , #act , #ref , -17 )
#define utt_is_approximatelyf( act , ref , thld_type , thld )		utt_is_approximatelyf_impl( act , utt_double2bigfloat(ref) , thld_type , thld , #act , #ref , -17 )
#define utt_is_approximatelyh( act , ref , thld_type , thld )		utt_is_approximatelyh_impl( act , utt_double2bigfloat(ref) , thld_type , thld , #act , #ref , -17 )
#define utt_is_approximatelyh2( act , ref , thld_type , thld )		utt_is_approximatelyh2_impl( act , utt_double2bigfloat(ref) , thld_type , thld , #act , #ref , -17 )

/* Comparators with utt_ieee754_float as reference */
#define utt_is_approximatelyff( act , ref , thld_type , thld )		utt_is_approximatelyf_impl( act , utt_float2bigfloat(ref) , thld_type , thld , #act , #ref , -8 )
#define utt_is_approximatelyhf( act , ref , thld_type , thld )		utt_is_approximatelyh_impl( act , utt_float2bigfloat(ref) , thld_type , thld , #act , #ref , -8 )
#define utt_is_approximatelyh2f( act , ref , thld_type , thld )		utt_is_approximatelyh2_impl( act , utt_float2bigfloat(ref) , thld_type , thld , #act , #ref , -8 )

/* Comparators with utt_ieee754_half as reference */
#define utt_is_approximatelyhh( act , ref , thld_type , thld )		utt_is_approximatelyh_impl( act , utt_half2bigfloat(ref) , thld_type , thld , #act , #ref , -5 )

/* Comparators with utt_ieee754_half_alt as reference */
#define utt_is_approximatelyh2h2( act , ref , thld_type , thld )	utt_is_approximatelyh2_impl( act , utt_half_alt2bigfloat(ref) , thld_type , thld , #act , #ref , -5 )


/**
 * Function that can check whether the supplied 'observed' value equals 'reference'.
 * If that is not the case, the function prints the compared values.
 */
UTT_LINKAGE bool utt_is_exactly_impl( utt_intmax_t observed , utt_intmax_t reference , const char* observed_repr , const char* reference_repr ){
	bool result = ( observed == reference ) ? true : false;
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = %lld\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = %lld\n" , reference_repr , reference );
		utt_print( "                        [CMP] = %s\n" , result ? "EQUAL" : "NOT EQUAL" );
	}
	return result;
}
UTT_LINKAGE bool utt_is_exactlyu_impl( utt_uintmax_t observed , utt_uintmax_t reference , const char* observed_repr , const char* reference_repr ){
	bool result = ( observed == reference ) ? true : false;
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = %llu\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = %llu\n" , reference_repr , reference );
		utt_print( "                        [CMP] = %s\n" , result ? "EQUAL" : "NOT EQUAL" );
	}
	return result;
}
UTT_LINKAGE bool utt_is_exactlyb_impl( bool observed , bool reference , const char* observed_repr , const char* reference_repr ){
	bool result = observed ? ( reference ? true : false ) : ( reference ? false : true );
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = %b\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = %b\n" , reference_repr , reference );
		utt_print( "                        [CMP] = %s\n" , result ? "EQUAL" : "NOT EQUAL" );
	}
	return result;
}
UTT_LINKAGE bool utt_is_exactlyp_impl( const void* observed , const void* reference , const char* observed_repr , const char* reference_repr ){
	bool result = ( observed == reference ) ? true : false;
	#ifndef TARGET_VERBOSE
		if( !result )
	#endif
	{
		utt_print( "%#21.21s = [OBS] = 0x%p\n" , observed_repr , observed );
		utt_print( "%#21.21s = [REF] = 0x%p\n" , reference_repr , reference );
		utt_print( "                        [CMP] = %s\n" , result ? "EQUAL" : "NOT EQUAL" );
	}
	return result;
}

#define utt_is_exactly( act , ref )		utt_is_exactly_impl( act , ref , #act , #ref )
#define utt_is_exactlyu( act , ref )	utt_is_exactlyu_impl( act , ref , #act , #ref )
#define utt_is_exactlyp( act , ref )	utt_is_exactlyp_impl( act , ref , #act , #ref )
#define utt_is_exactlyb( act , ref )	utt_is_exactlyb_impl( act , ref , #act , #ref )


/**
 * Function that compares two buffers on byte-wise congruency
 */
UTT_LINKAGE bool utt_is_congruent_impl( const void* observed , const void* reference , utt_size_t num_bytes , const char* observed_repr , const char* reference_repr )
{
	const unsigned char* observed_iter	= (const unsigned char*)observed;
	const unsigned char* reference_iter	= (const unsigned char*)reference;
	
	/* Same Pointers? */
	if( observed == reference )
	{
		#ifdef TARGET_VERBOSE
		utt_print( "%#21.21s = [&OBS] = 0x%p\n" , observed_repr , observed );
		utt_print( "%#21.21s = [&REF] = 0x%p\n" , reference_repr , reference );
		if( observed ){
			utt_print( "                [REF] = [OBS] = \"");
			for( utt_size_t i = 0 ; i < num_bytes ; ++i )
				if( i > 16 ){
					utt_print("...");
					break;
				}
				else
					utt_print("%#c" , observed_iter[i] );
		}
		utt_print( "\"\n                        [CMP] = SAME ADDRESS -> CONGRUENT\n" );
		#endif
		return true;
	}
	/* Null pointers involved? */
	else if( !observed || !reference ){
		utt_print( "%#21.21s = [&OBS] = 0x%p\n" , observed_repr , observed );
		utt_print( "%#21.21s = [&REF] = 0x%p\n" , reference_repr , reference );
		utt_print( "\"\n                        [CMP] = NULL POINTER -> NOT CONGRUENT\n" );
	}
	
	utt_size_t	iter = 0;
	for( ; iter != num_bytes ; iter++ )
	{
		unsigned char obs = observed_iter[iter];
		unsigned char ref = reference_iter[iter];
		if( obs != ref )
		{
			utt_print( "%#21.21s = [&OBS] = 0x%p\n" , observed_repr , observed );
			utt_print( "%#21.21s = [&REF] = 0x%p\n" , reference_repr , reference );
			utt_print( "                        [OBS] = \"");
			
			/* Determine start position */
			utt_size_t start = iter > 10 ? iter - 10 : 0;
			
			/* Print reference string */
			if( start > 0 )
				utt_print("...");
			for( utt_size_t i = start ; i < num_bytes ; ++i )
				if( i - start > 16 ){
					utt_print("...");
					break;
				}
				else
					utt_print("%#c" , observed_iter[i] );
			
			/* Print Observed string */
			utt_print( "\"\n                        [REF] = \"");
			if( start > 0 )
				utt_print("...");
			for( utt_size_t i = start ; i < num_bytes ; ++i )
				if( i - start > 16 ){
					utt_print("...");
					break;
				}
				else
					utt_print("%#c" , reference_iter[i] );
			
			utt_print( "\"\n                        [OBS:%zu] = 0x%x ('%#c')\n" , iter+1 , obs , obs );
			utt_print( "                        [REF:%zu] = 0x%x ('%#c')\n" , iter+1 , ref , ref );
			utt_print( "                        [CMP] = NOT BYTE-WISE CONGRUENT\n"  );
			return false;
		}
	}
	#ifdef TARGET_VERBOSE
		utt_print( "%#21.21s = [&OBS] = 0x%p\n" , observed_repr , observed );
		utt_print( "%#21.21s = [&REF] = 0x%p\n" , reference_repr , reference );
		utt_print( "                        [OBS] = \"");
		for( utt_size_t i = 0 ; i < num_bytes ; ++i )
			if( i > 16 ){
				utt_print("...");
				break;
			}
			else
				utt_print("%#c" , observed_iter[i] );
		utt_print( "\"\n                        [REF] = \"");
		for( utt_size_t i = 0 ; i < num_bytes ; ++i )
			if( i > 16 ){
				utt_print("...");
				break;
			}
			else
				utt_print("%#c" , reference_iter[i] );
		utt_print( "\"\n                        [CMP] = BYTE-WISE CONGRUENT (%zu BYTES)\n" );
	#endif
	return true;
}

#define utt_is_congruent( act , ref , num_bytes )	utt_is_congruent_impl( act , ref , num_bytes , #act , #ref )

#endif
