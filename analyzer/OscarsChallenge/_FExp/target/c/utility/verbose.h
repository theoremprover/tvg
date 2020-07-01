/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_VERBOSE_H_
#define _TARGET_UNITTEST_UTILITY_VERBOSE_H_

#include <target/c/stdio.h>
#include <target/c/stdfloat.h>

#define DEFINE_VERBOSE_FUNCTION( TYPE , TYPE_NAME , FORMAT ) \
	UTT_LINKAGE TYPE utt_verbose ## TYPE_NAME( TYPE value ){ \
		utt_print( FORMAT "\n" , value ); \
		return value; \
	} \
	UTT_LINKAGE TYPE* utt_verbose ## TYPE_NAME ## _ptr ( TYPE* value ){ \
		utt_print( "0x%p\n" , (void*)value ); \
		return value; \
	}

UTT_LINKAGE void* utt_verbose_void_ptr( void* value ){
	utt_print( "0x%p\n" , value );
	return value;
}

DEFINE_VERBOSE_FUNCTION( short					, short						, "%hd" )
DEFINE_VERBOSE_FUNCTION( short int				, short_int					, "%hd" )
DEFINE_VERBOSE_FUNCTION( int					, int						, "%d" )
DEFINE_VERBOSE_FUNCTION( long					, long						, "%ld" )
DEFINE_VERBOSE_FUNCTION( long int				, long_int					, "%ld" )
DEFINE_VERBOSE_FUNCTION( long long				, long_long					, "%lld" )
DEFINE_VERBOSE_FUNCTION( long long int			, long_long_int				, "%lld" )

DEFINE_VERBOSE_FUNCTION( unsigned short			, unsigned_short			, "%hu" )
DEFINE_VERBOSE_FUNCTION( unsigned short int		, unsigned_short_int		, "%hu" )
DEFINE_VERBOSE_FUNCTION( unsigned int			, unsigned_int				, "%u" )
DEFINE_VERBOSE_FUNCTION( unsigned long			, unsigned_long				, "%lu" )
DEFINE_VERBOSE_FUNCTION( unsigned long int		, unsigned_long_int			, "%lu" )
DEFINE_VERBOSE_FUNCTION( unsigned long long		, unsigned_long_long		, "%llu" )
DEFINE_VERBOSE_FUNCTION( unsigned long long int	, unsigned_long_long_int	, "%llu" )

DEFINE_VERBOSE_FUNCTION( utt_intmax_t			, utt_intmax_t				, "%jd" )
DEFINE_VERBOSE_FUNCTION( utt_uintmax_t			, utt_uintmax_t				, "%ju" )
DEFINE_VERBOSE_FUNCTION( utt_size_t				, utt_size_t				, "%zu" )
DEFINE_VERBOSE_FUNCTION( utt_ptrdiff_t			, utt_ptrdiff_t				, "%td" )

DEFINE_VERBOSE_FUNCTION( utt_ieee754_double_t	, utt_ieee754_double_t		, "%.g" )
DEFINE_VERBOSE_FUNCTION( utt_ieee754_float_t	, utt_ieee754_float_t		, "%.hg" )
DEFINE_VERBOSE_FUNCTION( utt_ieee754_half_t		, utt_ieee754_half_t		, "%.hhg" )
DEFINE_VERBOSE_FUNCTION( utt_ieee754_half_alt_t	, utt_ieee754_half_alt_t	, "%#.hhg" )
DEFINE_VERBOSE_FUNCTION( utt_bigfloat_t			, utt_bigfloat_t			, "%.jg" )

#endif
