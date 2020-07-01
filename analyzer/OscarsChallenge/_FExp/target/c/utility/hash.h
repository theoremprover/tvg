/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_HASH_H_
#define _TARGET_UNITTEST_UTILITY_HASH_H_

#include <target/c/stdint.h>

/** Definition of a type that represents a hash */
typedef utt_uint32_t	utt_hash_t;

/** Hash function to hash a 4 byte wide integer */
UTT_LINKAGE utt_hash_t utt_hashu32( utt_uint32_t x ){
	x++;
	x ^= x >> 17;
	x *= 0xed5ad4bb;
	x ^= x >> 11;
	x *= 0xac4c1b51;
	x ^= x >> 15;
	x *= 0x31848bab;
	x ^= x >> 14;
	return x;
}

/** Function to combine two hashes asymmetrically (so that the order does play a role) */
UTT_LINKAGE utt_hash_t utt_hash_combine_asymmetric( utt_hash_t lhs , utt_hash_t rhs ){
	return lhs ^ ( rhs + 0x9e3779b9 + (lhs << 6) + (lhs >> 2) );
}

/** Function to combine two hashes asymmetrically (so that the order does not play role) */
UTT_LINKAGE utt_hash_t utt_hash_combine_symmetric( utt_hash_t lhs , utt_hash_t rhs ){
	return lhs ^ rhs;
}

/** Function to hash an unsigned long long */
UTT_LINKAGE utt_hash_t utt_hash_buf( const void* buf , utt_size_t len )
{
	utt_hash_t		result = 0;
	
	if( len )
	{
		const utt_uint8_t*	iter = (const utt_uint8_t*)buf;
		utt_size_t			len_quarters = ( len + 3 ) / 4;
		utt_uint32_t		val = 0;
		
		/* Duff's Device */
		switch( len % 4 ){
			case 0:	do{ val = (val << 8) + *iter++;
			case 3:		val = (val << 8) + *iter++;
			case 2:		val = (val << 8) + *iter++;
			case 1:		val = (val << 8) + *iter++;
						result = utt_hash_combine_asymmetric( result , utt_hashu32( val ) );
						val = 0;
			}while( --len_quarters );
		}
	}
	
	return result;
}

/** Function to hash any type (except buffers) */
#define utt_hash( val ) utt_hash_buf( &(val) , sizeof(val) )

#endif
