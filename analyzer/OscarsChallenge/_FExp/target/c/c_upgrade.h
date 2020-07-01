/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_C_UPGRADE_H_
#define _TARGET_UNITTEST_C_UPGRADE_H_

#include <target/c/predefines.h>

/** Stub __has_builtin for non-clang compilers */
#ifndef __has_builtin
	#ifdef __GNUC__
		#define __has_builtin(x) 1
	#else
		#define __has_builtin(x) 0
	#endif
#endif


/** Define a nullptr constant */
#if !defined(__cplusplus) || __cplusplus < 2011u
	#define nullptr ((void*)0)
#endif


/** Define exit() */
#ifndef TARGET_NO_EXIT
	#if __has_builtin(__builtin_exit) /** Can use GCC builtins */
		UTT_LINKAGE void utt_exit( int exit_code ){ __builtin_exit( exit_code ); }
	#else
		extern
		#ifdef __cplusplus
		"C"
		#endif
		void exit( int );
		UTT_LINKAGE void utt_exit( int exit_code ){ exit( exit_code ); }
	#endif
#else
	UTT_LINKAGE void utt_exit( int exit_code ){ (void)exit_code; }
#endif


/** Define assert */
UTT_LINKAGE void utt_assert_impl( const char *msg , const char *file , int line ){
	utt_print( "%s:%d: Assertion Failed: %s\n" , file , line , msg );
	utt_exit( -1 );
}
#define utt_assert( EX ) (void)( (EX) || ( utt_assert_impl( #EX , __FILE__ , __LINE__ ) , 0 ) )
#define utt_assert_ex( EX , MSG ) (void)( (EX) || ( utt_assert_impl( MSG , __FILE__ , __LINE__ ) , 0 ) )


/** Define Endianness Enumeration */
typedef enum utt_endian_t
{
#ifdef _WIN32
	UTT_ENDIAN_LITTLE	= 4321
	, UTT_ENDIAN_BIG	= 1234
	, UTT_ENDIAN_NATIVE	= UTT_ENDIAN_LITTLE
#else
	UTT_ENDIAN_LITTLE	= __ORDER_LITTLE_ENDIAN__
	, UTT_ENDIAN_BIG	= __ORDER_BIG_ENDIAN__
	, UTT_ENDIAN_NATIVE	= __BYTE_ORDER__
#endif
}utt_endian_t;


#endif
