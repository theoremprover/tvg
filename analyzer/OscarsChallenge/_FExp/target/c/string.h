/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_STRING_H_
#define _TARGET_UNITTEST_STRING_H_

#include <target/c/stdint.h>

/** memset replacement */
UTT_LINKAGE void utt_memset( void* buffer , utt_uint8_t value , utt_size_t len )
{
	if( !len )
		return;
	
	utt_uint8_t*	iter = (utt_uint8_t*)buffer;
	utt_size_t		len_eights = ( len + 7 ) / 8;
	
	/* Duff's Device */
	switch( len % 8 ){
		case 0:	do{	*iter++ = value;
		case 7:		*iter++ = value;
		case 6:		*iter++ = value;
		case 5:		*iter++ = value;
		case 4:		*iter++ = value;
		case 3:		*iter++ = value;
		case 2:		*iter++ = value;
		case 1:		*iter++ = value;
		}while( --len_eights );
	}
}


/** memcpy replacement */
UTT_LINKAGE void utt_memcpy( void* to , const void* from , utt_size_t len )
{
	if( !len )
		return;
	
	const utt_uint8_t*	iter_from = (const utt_uint8_t*)from;
	utt_uint8_t*		iter_to = (utt_uint8_t*)to;
	utt_size_t			len_eights = ( len + 7 ) / 8;
	
	/* Duff's Device */
	switch( len % 8 ){
		case 0:	do{	*iter_to++ = *iter_from++;
		case 7:		*iter_to++ = *iter_from++;
		case 6:		*iter_to++ = *iter_from++;
		case 5:		*iter_to++ = *iter_from++;
		case 4:		*iter_to++ = *iter_from++;
		case 3:		*iter_to++ = *iter_from++;
		case 2:		*iter_to++ = *iter_from++;
		case 1:		*iter_to++ = *iter_from++;
		}while( --len_eights );
	}
}


/** memmove replacement */
UTT_LINKAGE void utt_memmove( void* to , const void* from , utt_size_t len )
{
	if( !len || to == from )
		return;
	
	const utt_uint8_t*	iter_from = (const utt_uint8_t*)from;
	utt_uint8_t*		iter_to = (utt_uint8_t*)to;
	utt_size_t			len_eights = ( len + 7 ) / 8;
	
	if( (utt_intptr_t)to < (utt_intptr_t)from )
		/* Duff's Device */
		switch( len % 8 ){
			case 0:	do{	*iter_to++ = *iter_from++;
			case 7:		*iter_to++ = *iter_from++;
			case 6:		*iter_to++ = *iter_from++;
			case 5:		*iter_to++ = *iter_from++;
			case 4:		*iter_to++ = *iter_from++;
			case 3:		*iter_to++ = *iter_from++;
			case 2:		*iter_to++ = *iter_from++;
			case 1:		*iter_to++ = *iter_from++;
			}while( --len_eights );
		}
	else{ /* Copy Backwards */
		iter_from += len;
		iter_to += len;
		
		/* Duff's Device */
		switch( len % 8 ){
			case 0:	do{	*--iter_to = *--iter_from;
			case 7:		*--iter_to = *--iter_from;
			case 6:		*--iter_to = *--iter_from;
			case 5:		*--iter_to = *--iter_from;
			case 4:		*--iter_to = *--iter_from;
			case 3:		*--iter_to = *--iter_from;
			case 2:		*--iter_to = *--iter_from;
			case 1:		*--iter_to = *--iter_from;
			}while( --len_eights );
		}
	}
}

/** memcmp replacement */
UTT_LINKAGE int utt_memcmp( const void* left , const void* right , utt_size_t n )
{
	const utt_uint8_t* pleft = (const utt_uint8_t*)left;
	const utt_uint8_t* pright = (const utt_uint8_t*)right;
	while( n-- )
		if( *pleft != *pright )
			return (int)*pleft - (int)*pright;
		else
			pleft++ , pright++;
	return 0;
}

/** strlen replacement */
UTT_LINKAGE utt_size_t utt_strlen( const char* str ){
	const char* tmp = str;
	while( *tmp )
		++tmp;
	return tmp - str;
}

/** strcmp replacement */
UTT_LINKAGE utt_size_t utt_strcmp( const char* lhs , const char* rhs ){
	while( *lhs && *lhs == *rhs )
		lhs++, rhs++;
	return *lhs < *rhs ? -1 : ( *lhs > *rhs ? 1 : 0 );
}

/** strcpy replacement */
UTT_LINKAGE void utt_strcpy( char* dest , const char* src ){
	while( *src )
		*dest++ = *src++;
	*dest = '\0';
}

/** strcat replacement */
UTT_LINKAGE char* utt_strcat( char* dest , const char* src ){
	char *dest_bkp = dest;
	while( *dest )
		dest++;
	while( ( *dest++ = *src++ ) );
	return dest_bkp;
}

/** islower replacement */
UTT_LINKAGE int utt_islower( int c ){
	return c >= 'a' && c <= 'z';
}

/** isupper replacement */
UTT_LINKAGE int utt_isupper( int c ){
	return c >= 'A' && c <= 'Z';
}

/** tolower replacement */
UTT_LINKAGE utt_size_t utt_tolower( unsigned char c ){
	return utt_isupper(c) ? c + ( 'a' - 'A' ) : c;
}

/** toupper replacement */
UTT_LINKAGE utt_size_t utt_toupper( unsigned char c ){
	return utt_islower(c) ? c - ( 'a' - 'A' ) : c;
}

#endif
