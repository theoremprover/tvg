/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGINT_H_
#define _TARGET_UNITTEST_UTILITY_BIGINT_H_

#include <target/c/c_upgrade.h>
#include <target/c/stdint.h>

/**
 * The meat of this file is taken from http://www.ryanjuckett.com/programming/printing-floating-point-numbers/
 * It has been adapted to
 *	- only use C99 features,
 *	- ForeC++ data types,
 *	- ForeC++ source code formatting,
 *	- ForeC++ naming conventions and
 *	- ForeC++ function design.
 *
 * Corresponding license:
 *
 * Copyright (c) 2014 Ryan Juckett
 * http://www.ryanjuckett.com/
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.

 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *	  claim that you wrote the original software. If you use this software
 *	  in a product, an acknowledgment in the product documentation would be
 *	  appreciated but is not required.
 *
 * 2. Altered source versions must be plainly marked as such, and must not be
 *	  misrepresented as being the original software.
 *
 * 3. This notice may not be removed or altered from any source
 *	  distribution.
 */



/**
 * Maximum number of 32 bit blocks needed in high precision
 * arithmetic to print out 64 bit IEEE floating point values.
 */
#define BIG_INT_MAX_BLOCKS 38

/**
 * This structure stores a high precision unsigned integer. It uses a buffer
 * of 32 bit integer blocks along with a length. The lowest bits of the integer
 * are stored at the start of the buffer and the length is set to the minimum
 * value that contains the integer. Thus, there are never any zero blocks at the
 * end of the buffer.
 */
typedef struct utt_bigint_t{
	utt_uint32_t	length;
	utt_uint32_t	blocks[BIG_INT_MAX_BLOCKS];
} utt_bigint_t;


/** Set the supplied bigint to the supplied int value */
UTT_LINKAGE void utt_bi_set_32( utt_bigint_t* num , utt_uint64_t val ){
	num->blocks[0] = val;
	num->length	   = (val != 0);
}
UTT_LINKAGE void utt_bi_set_64( utt_bigint_t* num , utt_uint64_t val ){
	if( val > 0xFFFFFFFFuL ){
		num->blocks[0] = val & 0xFFFFFFFFuL;
		num->blocks[1] = (val >> 32) & 0xFFFFFFFFuL;
		num->length	   = 2;
	}
	else{
		num->blocks[0] = (utt_uint32_t)val;
		num->length	   = (val != 0);
	}
}
UTT_LINKAGE void utt_bi_set_imax( utt_bigint_t* num , utt_uintmax_t val ){
	num->length = 0;
	while( val ){
		num->blocks[num->length++] = val & 0xFFFFFFFFuL;
		val >>= 32;
	}
}

/** Read the supplied bigint as int */
UTT_LINKAGE utt_uint64_t utt_bi_get_64( utt_bigint_t* num ){
	switch( num->length ){
		case 1: return num->blocks[0];
		case 2: return (utt_uint64_t)num->blocks[0] | ( (utt_uint64_t)num->blocks[1] << 32 );
		default: utt_assert( num->length <= 2 ); /* Make sure, we end up with a uint64 */
	}
	return 0;
}
UTT_LINKAGE utt_uint32_t utt_bi_get_32( utt_bigint_t* num ){
	switch( num->length ){
		case 1: return num->blocks[0];
		default: utt_assert( num->length <= 1 ); /* Make sure, we end up with a uint32 */
	}
	return 0;
}
UTT_LINKAGE utt_uint16_t utt_bi_get_16( utt_bigint_t* num ){
	return utt_bi_get_32( num );
}


/** Check, whether the supplied big int is zero, or set it to zero */
UTT_LINKAGE void utt_bi_set_zero( utt_bigint_t* num ){ num->length = 0; }
UTT_LINKAGE void utt_bi_set_nonempty_zero( utt_bigint_t* num ){ num->length = 1; num->blocks[0] = 0; }
UTT_LINKAGE bool utt_bi_is_zero( const utt_bigint_t* num ){ return !num->length; }


/** Assign a big integer to another (faster than plain assignment) */
UTT_LINKAGE void utt_bi_assign( utt_bigint_t* dest , const utt_bigint_t* src ){
	for( utt_int32_t i = 0 ; i < src->length ; i++ )
		dest->blocks[i] = src->blocks[i];
	dest->length = src->length;
}



/** Returns 0 if lhs = rhs, negative if lhs < rhs and positive if lhs > rhs */
UTT_LINKAGE utt_int32_t utt_bi_compare(const utt_bigint_t* lhs, const utt_bigint_t* rhs)
{
	/* A bigger length implies a bigger number. */
	utt_int32_t lengthDiff = lhs->length - rhs->length;
	if( lengthDiff != 0)
		return lengthDiff;

	/* Compare blocks one by one from high to low. */
	for( utt_int32_t i = lhs->length - 1; i >= 0; --i)
	{
		if( lhs->blocks[i] == rhs->blocks[i])
			continue;
		else if( lhs->blocks[i] > rhs->blocks[i])
			return 1;
		else
			return -1;
	}

	/* no blocks differed */
	return 0;
}



/** result = lhs + rhs */
UTT_LINKAGE void utt_bi_sum(utt_bigint_t* pResult, const utt_bigint_t* lhs, const utt_bigint_t* rhs)
{
	/* determine which operand has the smaller length */
	const utt_bigint_t * pLarge;
	const utt_bigint_t * pSmall;
	if( lhs->length < rhs->length){
		pSmall = lhs;
		pLarge = rhs;
	}
	else{
		pSmall = rhs;
		pLarge = lhs;
	}

	const utt_uint32_t largeLen = pLarge->length;
	const utt_uint32_t smallLen = pSmall->length;

	/* The output will be at least as long as the largest input */
	pResult->length = largeLen;

	/* Add each block and add carry the overflow to the next block */
	utt_uint64_t carry = 0;
	const utt_uint32_t * pLargeCur	= pLarge->blocks;
	const utt_uint32_t * pLargeEnd	= pLargeCur + largeLen;
	const utt_uint32_t * pSmallCur	= pSmall->blocks;
	const utt_uint32_t * pSmallEnd	= pSmallCur + smallLen;
	utt_uint32_t *		 pResultCur = pResult->blocks;
	while( pSmallCur != pSmallEnd)
	{
		utt_uint64_t sum = carry + (utt_uint64_t)(*pLargeCur) + (utt_uint64_t)(*pSmallCur);
		carry = sum >> 32;
		(*pResultCur) = sum & 0xFFFFFFFF;
		++pLargeCur;
		++pSmallCur;
		++pResultCur;
	}

	/* Add the carry to any blocks that only exist in the large operand */
	while( pLargeCur != pLargeEnd)
	{
		utt_uint64_t sum = carry + (utt_uint64_t)(*pLargeCur);
		carry = sum >> 32;
		(*pResultCur) = sum & 0xFFFFFFFF;
		++pLargeCur;
		++pResultCur;
	}

	/* If there's still a carry, append a new block */
	if( carry != 0)
	{
		utt_assert(carry == 1);
		utt_assert((utt_uint32_t)(pResultCur - pResult->blocks) == largeLen && (largeLen < BIG_INT_MAX_BLOCKS));
		*pResultCur = 1;
		pResult->length = largeLen + 1;
	}
	else
		pResult->length = largeLen;
}


/** result = result + value */
UTT_LINKAGE void utt_bi_add( utt_bigint_t* pResult , const utt_bigint_t* value ){
	utt_bigint_t	copy;
	utt_bi_assign( &copy , pResult );
	utt_bi_sum( pResult , &copy , value );
}


/**
 * result = |lhs - rhs|
 * the function returns 'true', if the result is actually negative, 'false' otheriwse.
 */
UTT_LINKAGE bool utt_bi_dif( utt_bigint_t* pResult , const utt_bigint_t* lhs , const utt_bigint_t* rhs )
{
	/* Check, if lhs is bigger than rhs */
	bool		negative = false;
	int			comparison_result = utt_bi_compare( lhs , rhs );
	if( comparison_result < 0 )
		negative = true;
	else if( !comparison_result ){
		utt_bi_set_zero( pResult );
		return false;
	}
	
	/* determine which operand has the smaller length */
	const utt_bigint_t * pLarge = negative ? rhs : lhs;
	const utt_bigint_t * pSmall = negative ? lhs : rhs;
	const utt_uint32_t largeLen = pLarge->length;
	const utt_uint32_t smallLen = pSmall->length;
	
	/* Subtracting nothing? */
	if( utt_bi_is_zero( pSmall ) )
		return negative;
		
	/* The output will be at least as long as the largest input */
	pResult->length = largeLen;

	/* Subtract each block and add carry the overflow to the next block */
	utt_uint64_t carry = 0;
	const utt_uint32_t*	pLargeCur	= pLarge->blocks;
	const utt_uint32_t*	pLargeEnd	= pLargeCur + largeLen;
	const utt_uint32_t*	pSmallCur	= pSmall->blocks;
	const utt_uint32_t*	pSmallEnd	= pSmallCur + smallLen;
	utt_uint32_t*		pResultCur = pResult->blocks;
	int					trailing_empty_blocks = 0;
	while( pSmallCur != pSmallEnd )
	{
		utt_int64_t sum = (utt_int64_t)(*pLargeCur) - (utt_int64_t)(*pSmallCur) - (utt_int64_t)carry;
		carry = 0;
		while( sum < 0 ){
			sum += (1uLL << 32);
			++carry;
		}
		trailing_empty_blocks = (utt_uint32_t)sum ? 0 : trailing_empty_blocks + 1;
		*pResultCur = (utt_uint32_t)sum;
		++pLargeCur;
		++pSmallCur;
		++pResultCur;
	}

	/* Add the carry to any blocks that only exist in the large operand */
	while( pLargeCur != pLargeEnd )
	{
		utt_int64_t sum = (utt_int64_t)(*pLargeCur) - carry;
		carry = 0;
		while( sum < 0 ){
			sum += (1uLL << 32);
			++carry;
		}
		trailing_empty_blocks = (utt_uint32_t)sum ? 0 : trailing_empty_blocks + 1;
		(*pResultCur) = (utt_uint32_t)sum;
		++pLargeCur;
		++pResultCur;
		
		if( !carry )
			break;
	}
	
	/* Copy remaining blocks */
	while( pLargeCur != pLargeEnd ){
		*pResultCur = *pLargeCur;
		trailing_empty_blocks = *pResultCur ? 0 : trailing_empty_blocks + 1;
		++pLargeCur;
		++pResultCur;
	}

	/* Remove 0 blocks */
	pResult->length -= trailing_empty_blocks;
	
	return negative;
}

/** result = result - value */
UTT_LINKAGE bool utt_bi_sub(utt_bigint_t* pResult, const utt_bigint_t* value){
	utt_bigint_t	copy;
	utt_bi_assign( &copy , pResult );
	return utt_bi_dif( pResult , &copy , value );
}

/** result = lhs * rhs */
UTT_LINKAGE void utt_bi_mutliply(utt_bigint_t * pResult, const utt_bigint_t* lhs, const utt_bigint_t* rhs)
{
	utt_assert( pResult != lhs && pResult != rhs );

	/* determine which operand has the smaller length */
	const utt_bigint_t * pLarge;
	const utt_bigint_t * pSmall;
	if( lhs->length < rhs->length){
		pSmall = lhs;
		pLarge = rhs;
	}
	else{
		pSmall = rhs;
		pLarge = lhs;
	}

	/* set the maximum possible result length */
	utt_uint32_t maxResultLen = pLarge->length + pSmall->length;
	utt_assert( maxResultLen <= BIG_INT_MAX_BLOCKS );

	/* clear the result data */
	for(utt_uint32_t * pCur = pResult->blocks, *pEnd = pCur + maxResultLen; pCur != pEnd; ++pCur)
		*pCur = 0;

	/* perform standard long multiplication */
	const utt_uint32_t *pLargeBeg = pLarge->blocks;
	const utt_uint32_t *pLargeEnd = pLargeBeg + pLarge->length;

	/* for each small block */
	utt_uint32_t *pResultStart = pResult->blocks;
	for(const utt_uint32_t *pSmallCur = pSmall->blocks, *pSmallEnd = pSmallCur + pSmall->length;
		pSmallCur != pSmallEnd;
		++pSmallCur, ++pResultStart)
	{
		/* if non-zero, multiply against all the large blocks and add into the result */
		const utt_uint32_t multiplier = *pSmallCur;
		if( multiplier != 0)
		{
			const utt_uint32_t *pLargeCur = pLargeBeg;
			utt_uint32_t *pResultCur = pResultStart;
			utt_uint64_t carry = 0;
			do
			{
				utt_uint64_t product = (*pResultCur) + (*pLargeCur)*(utt_uint64_t)multiplier + carry;
				carry = product >> 32;
				*pResultCur = product & 0xFFFFFFFF;
				++pLargeCur;
				++pResultCur;
			} while(pLargeCur != pLargeEnd);

			utt_assert(pResultCur < pResult->blocks + maxResultLen);
			*pResultCur = (utt_uint32_t)(carry & 0xFFFFFFFF);
		}
	}

	/* check if the terminating block has no set bits */
	if( maxResultLen > 0 && pResult->blocks[maxResultLen - 1] == 0)
		pResult->length = maxResultLen-1;
	else
		pResult->length = maxResultLen;
}



/** result = lhs * rhs */
UTT_LINKAGE void utt_bi_cmultiply(utt_bigint_t * pResult, const utt_bigint_t* lhs, utt_uint32_t rhs)
{
	/* perform long multiplication */
	utt_uint32_t carry = 0;
	utt_uint32_t *pResultCur = pResult->blocks;
	const utt_uint32_t *pLhsCur = lhs->blocks;
	const utt_uint32_t *pLhsEnd = lhs->blocks + lhs->length;

	for(  ; pLhsCur != pLhsEnd; ++pLhsCur, ++pResultCur ){
		utt_uint64_t product = (utt_uint64_t)(*pLhsCur) * rhs + carry;
		*pResultCur = (utt_uint32_t)(product & 0xFFFFFFFF);
		carry = product >> 32;
	}

	/* if there is a remaining carry, grow the array */
	if( carry != 0){
		/* grow the array */
		utt_assert(lhs->length + 1 <= BIG_INT_MAX_BLOCKS);
		*pResultCur = (utt_uint32_t)carry;
		pResult->length = lhs->length + 1;
	}
	else
		pResult->length = lhs->length;
}

/** result = result * value */
UTT_LINKAGE void utt_bi_times(utt_bigint_t * pResult, utt_uint32_t value ){
	utt_bigint_t	copy;
	utt_bi_assign( &copy , pResult );
	utt_bi_cmultiply( pResult , &copy , value );
}

/** result = in * 2 */
UTT_LINKAGE void utt_bi_times2(utt_bigint_t * pResult, const utt_bigint_t* in)
{
	/* shift all the blocks by one */
	utt_uint32_t carry = 0;

	utt_uint32_t *pResultCur = pResult->blocks;
	const utt_uint32_t *pLhsCur = in->blocks;
	const utt_uint32_t *pLhsEnd = in->blocks + in->length;
	for(  ; pLhsCur != pLhsEnd; ++pLhsCur, ++pResultCur )
	{
		utt_uint32_t cur = *pLhsCur;
		*pResultCur = (cur << 1) | carry;
		carry = cur >> 31;
	}

	if( carry != 0)
	{
		/* grow the array */
		utt_assert(in->length + 1 <= BIG_INT_MAX_BLOCKS);
		*pResultCur = carry;
		pResult->length = in->length + 1;
	}
	else
		pResult->length = in->length;
}



/** result = result * 2 */
UTT_LINKAGE void utt_bi_double(utt_bigint_t * pResult)
{
	/* shift all the blocks by one */
	utt_uint32_t carry = 0;

	utt_uint32_t *pCur = pResult->blocks;
	utt_uint32_t *pEnd = pResult->blocks + pResult->length;
	
	for( ; pCur != pEnd; ++pCur ){
		utt_uint32_t cur = *pCur;
		*pCur = (cur << 1) | carry;
		carry = cur >> 31;
	}

	if( carry != 0 ){
		/* grow the array */
		utt_assert(pResult->length + 1 <= BIG_INT_MAX_BLOCKS);
		*pCur = carry;
		++pResult->length;
	}
}



/** result = result * 10 */
UTT_LINKAGE void utt_bi_times10(utt_bigint_t * pResult)
{
	/* multiply all the blocks */
	utt_uint64_t carry = 0;

	utt_uint32_t *pCur = pResult->blocks;
	utt_uint32_t *pEnd = pResult->blocks + pResult->length;
	
	for(  ; pCur != pEnd; ++pCur ){
		utt_uint64_t product = (utt_uint64_t)(*pCur) * 10ull + carry;
		(*pCur) = (utt_uint32_t)(product & 0xFFFFFFFF);
		carry = product >> 32;
	}

	if( carry != 0){
		/* grow the array */
		utt_assert(pResult->length + 1 <= BIG_INT_MAX_BLOCKS);
		*pCur = (utt_uint32_t)carry;
		++pResult->length;
	}
}



/** Returns the nth power of 10	 as unsigned int */
UTT_LINKAGE utt_uint32_t utt_bi_power10_32( int index ){
	utt_uint32_t lut[] =
	{
		1,			/* 10 ^ 0 */
		10,			/* 10 ^ 1 */
		100,		/* 10 ^ 2 */
		1000,		/* 10 ^ 3 */
		10000,		/* 10 ^ 4 */
		100000,		/* 10 ^ 5 */
		1000000,	/* 10 ^ 6 */
		10000000,	/* 10 ^ 7 */
	};
	return lut[ index ];
}



/**
 * Note: This has a lot of wasted space in the big integer structures of the
 *		 early table entries. It wouldn't be terribly hard to make the multiply
 *		 function work on integer pointers with an array length instead of
 *		 the utt_bigint_t struct which would allow us to store a minimal amount of
 *		 data here.
 */
UTT_LINKAGE utt_bigint_t* utt_bi_power10_big( int index ){
	static utt_bigint_t lut[] =
	{
		/* 10 ^ 8 */
		{ 1, { 100000000 } },
		/* 10 ^ 16 */
		{ 2, { 0x6fc10000, 0x002386f2 } },
		/* 10 ^ 32 */
		{ 4, { 0x00000000, 0x85acef81, 0x2d6d415b, 0x000004ee, } },
		/* 10 ^ 64 */
		{ 7, { 0x00000000, 0x00000000, 0xbf6a1f01, 0x6e38ed64, 0xdaa797ed, 0xe93ff9f4, 0x00184f03, } },
		/* 10 ^ 128 */
		{ 14, { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x2e953e01, 0x03df9909, 0x0f1538fd,
				0x2374e42f, 0xd3cff5ec, 0xc404dc08, 0xbccdb0da, 0xa6337f19, 0xe91f2603, 0x0000024e, } },
		/* 10 ^ 256 */
		{ 27, { 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000, 0x00000000,
				0x00000000, 0x982e7c01, 0xbed3875b, 0xd8d99f72, 0x12152f87, 0x6bde50c6, 0xcf4a6e70,
				0xd595d80f, 0x26b2716e, 0xadc666b0, 0x1d153624, 0x3c42d35a, 0x63ff540e, 0xcc5573c0,
				0x65f9ef17, 0x55bc28f2, 0x80dcc7f7, 0xf46eeddc, 0x5fdcefce, 0x000553f7, } }
	};
	return &lut[ index ];
}



/** result = 10 ^ exponent */
UTT_LINKAGE void utt_bi_pow10(utt_bigint_t * pResult, utt_uint32_t exponent)
{
	/* make sure the exponent is within the bounds of the lookup table data */
	utt_assert(exponent < 512);

	/* create two temporary values to reduce large integer copy operations */
	utt_bigint_t temp1;
	utt_bigint_t temp2;
	utt_bigint_t *pCurTemp = &temp1;
	utt_bigint_t *pNextTemp = &temp2;

	/* initialize the result by looking up a 32-bit power of 10 corresponding to the first 3 bits */
	utt_uint32_t smallExponent = exponent & 0x7;
	utt_bi_set_32( pCurTemp , utt_bi_power10_32(smallExponent) );

	/* remove the low bits that we used for the 32-bit lookup table */
	exponent >>= 3;
	utt_uint32_t tableIdx = 0;

	/* while there are remaining bits in the exponent to be processed */
	while( exponent != 0)
	{
		/* if the current bit is set, multiply it with the corresponding power of 10 */
		if(exponent & 1)
		{
			/* multiply into the next temporary */
			utt_bi_mutliply( pNextTemp, pCurTemp, utt_bi_power10_big(tableIdx) );

			/* swap to the next temporary */
			utt_bigint_t * pSwap = pCurTemp;
			pCurTemp = pNextTemp;
			pNextTemp = pSwap;
		}

		/* advance to the next bit */
		++tableIdx;
		exponent >>= 1;
	}

	/* output the result */
	utt_bi_assign( pResult , pCurTemp );
}



/** result = in * 10 ^ exponent */
UTT_LINKAGE void utt_bi_mutliply_pow10(utt_bigint_t * pResult, const utt_bigint_t* in, utt_uint32_t exponent)
{
	/* make sure the exponent is within the bounds of the lookup table data */
	utt_assert(exponent < 512);

	/* create two temporary values to reduce large integer copy operations */
	utt_bigint_t temp1;
	utt_bigint_t temp2;
	utt_bigint_t *pCurTemp = &temp1;
	utt_bigint_t *pNextTemp = &temp2;

	/* initialize the result by looking up a 32-bit power of 10 corresponding to the first 3 bits */
	utt_uint32_t smallExponent = exponent & 0x7;
	if( smallExponent != 0)
		utt_bi_cmultiply( pCurTemp, in, utt_bi_power10_32(smallExponent) );
	else
		utt_bi_assign( pCurTemp , in );

	/* remove the low bits that we used for the 32-bit lookup table */
	exponent >>= 3;
	utt_uint32_t tableIdx = 0;

	/* while there are remaining bits in the exponent to be processed */
	while( exponent != 0)
	{
		/* if the current bit is set, multiply it with the corresponding power of 10 */
		if(exponent & 1)
		{
			/* multiply into the next temporary */
			utt_bi_mutliply( pNextTemp, pCurTemp, utt_bi_power10_big(tableIdx) );

			/* swap to the next temporary */
			utt_bigint_t * pSwap = pCurTemp;
			pCurTemp = pNextTemp;
			pNextTemp = pSwap;
		}

		/* advance to the next bit */
		++tableIdx;
		exponent >>= 1;
	}

	/* output the result */
	utt_bi_assign( pResult , pCurTemp );
}



/** result = 2 ^ exponent */
UTT_LINKAGE void utt_bi_pow2(utt_bigint_t * pResult, utt_uint32_t exponent)
{
	utt_uint32_t blockIdx = exponent / 32;
	utt_assert( blockIdx < BIG_INT_MAX_BLOCKS );

	for(  utt_uint32_t i = 0; i <= blockIdx; ++i)
		pResult->blocks[i] = 0;

	pResult->length = blockIdx + 1;

	utt_uint32_t bitIdx = (exponent % 32);
	pResult->blocks[blockIdx] |= (1 << bitIdx);
}



/**
 * This function will divide two large numbers under the assumption that the
 * result is within the range [0,10) and the input numbers have been shifted
 * to satisfy:
 * - The highest block of the divisor is greater than or equal to 8 such that
 *	 there is enough precision to make an accurate first guess at the quotient.
 * - The highest block of the divisor is less than the maximum value on an
 *	 unsigned 32-bit integer such that we can safely increment without overflow.
 * - The dividend does not contain more blocks than the divisor such that we
 *	 can estimate the quotient by dividing the equivalently placed high blocks.
 *
 * quotient	 = floor(dividend / divisor)
 * remainder = dividend - quotient*divisor
 *
 * pDividend is updated to be the remainder and the quotient is returned.
 */
UTT_LINKAGE utt_uint32_t utt_bi_divmod_maxquot9(utt_bigint_t * pDividend, const utt_bigint_t* divisor)
{
	/* Check that the divisor has been correctly shifted into range and that it is not */
	/* smaller than the dividend in length. */
	utt_assert( !utt_bi_is_zero( divisor ) &&
				divisor->blocks[divisor->length-1] >= 8 &&
				divisor->blocks[divisor->length-1] < 0xFFFFFFFF &&
				pDividend->length <= divisor->length );

	/* If the dividend is smaller than the divisor, the quotient is zero and the divisor is already */
	/* the remainder. */
	utt_uint32_t length = divisor->length;
	if( pDividend->length < divisor->length)
		return 0;

	const utt_uint32_t * pFinalDivisorBlock	 = divisor->blocks + length - 1;
	utt_uint32_t *		 pFinalDividendBlock = pDividend->blocks + length - 1;

	/* Compute an estimated quotient based on the high block value. This will either match the actual quotient or */
	/* undershoot by one. */
	utt_uint32_t  quotient = *pFinalDividendBlock / (*pFinalDivisorBlock + 1);
	utt_assert(quotient <= 9);

	/* Divide out the estimated quotient */
	if( quotient != 0)
	{
		/* dividend = dividend - divisor*quotient */
		const utt_uint32_t *pDivisorCur = divisor->blocks;
		utt_uint32_t *pDividendCur		= pDividend->blocks;

		utt_uint64_t borrow = 0;
		utt_uint64_t carry = 0;
		do
		{
			utt_uint64_t product = (utt_uint64_t)*pDivisorCur * (utt_uint64_t)quotient + carry;
			carry = product >> 32;

			utt_uint64_t difference = (utt_uint64_t)*pDividendCur - (product & 0xFFFFFFFF) - borrow;
			borrow = (difference >> 32) & 1;

			*pDividendCur = difference & 0xFFFFFFFF;

			++pDivisorCur;
			++pDividendCur;
		} while(pDivisorCur <= pFinalDivisorBlock);

		/* remove all leading zero blocks from dividend */
		while( length > 0 && pDividend->blocks[length - 1] == 0)
			--length;

		pDividend->length = length;
	}

	/* If the dividend is still larger than the divisor, we overshot our estimate quotient. To correct, */
	/* we increment the quotient and subtract one more divisor from the dividend. */
	if(	 utt_bi_compare( pDividend, divisor) >= 0 )
	{
		++quotient;

		/* dividend = dividend - divisor */
		const utt_uint32_t *pDivisorCur = divisor->blocks;
		utt_uint32_t *pDividendCur		= pDividend->blocks;

		utt_uint64_t borrow = 0;
		do
		{
			utt_uint64_t difference = (utt_uint64_t)*pDividendCur - (utt_uint64_t)*pDivisorCur - borrow;
			borrow = (difference >> 32) & 1;

			*pDividendCur = difference & 0xFFFFFFFF;

			++pDivisorCur;
			++pDividendCur;
		} while(pDivisorCur <= pFinalDivisorBlock);

		/* remove all leading zero blocks from dividend */
		while( length > 0 && pDividend->blocks[length - 1] == 0)
			--length;

		pDividend->length = length;
	}

	return quotient;
}

/** result = result << shift */
UTT_LINKAGE void utt_bi_shl(utt_bigint_t * pResult, utt_uint32_t shift)
{
	/* take care of '0' being shifted */
	if( !pResult->length || !shift )
		return;

	utt_uint32_t shiftBlocks = shift / 32;
	utt_uint32_t shiftBits = shift % 32;

	/* process blocks high to low so that we can safely process in place */
	const utt_uint32_t *	pInBlocks	= pResult->blocks;
	utt_int32_t			   inLength	   = pResult->length;
	utt_assert( inLength + shiftBlocks < BIG_INT_MAX_BLOCKS );

	/* check if the shift is block aligned */
	if( shiftBits == 0)
	{
		/* copy blcoks from high to low */
		for( utt_uint32_t * pInCur = pResult->blocks + inLength, *pOutCur = pInCur + shiftBlocks;
			 pInCur >= pInBlocks;
			 --pInCur, --pOutCur)
		{
			*pOutCur = *pInCur;
		}

		/* zero the remaining low blocks */
		for(  utt_uint32_t i = 0; i < shiftBlocks; ++i)
			pResult->blocks[i] = 0;

		pResult->length += shiftBlocks;
	}
	/* else we need to shift partial blocks */
	else
	{
		utt_int32_t inBlockIdx	= inLength - 1;
		utt_uint32_t outBlockIdx = inLength + shiftBlocks;
		
		/* set the length to hold the shifted blocks */
		utt_assert( outBlockIdx < BIG_INT_MAX_BLOCKS );
		pResult->length = outBlockIdx + 1;

		/* output the initial blocks */
		const utt_uint32_t lowBitsShift = (32 - shiftBits);
		utt_uint32_t highBits = 0;
		utt_uint32_t block = pResult->blocks[inBlockIdx];
		utt_uint32_t lowBits = block >> lowBitsShift;
		while(  inBlockIdx > 0 )
		{
			pResult->blocks[outBlockIdx] = highBits | lowBits;
			highBits = block << shiftBits;

			--inBlockIdx;
			--outBlockIdx;

			block = pResult->blocks[inBlockIdx];
			lowBits = block >> lowBitsShift;
		}

		/* output the final blocks */
		utt_assert( outBlockIdx == shiftBlocks + 1 );
		pResult->blocks[outBlockIdx] = highBits | lowBits;
		pResult->blocks[outBlockIdx-1] = block << shiftBits;

		/* zero the remaining low blocks */
		for(  utt_uint32_t i = 0; i < shiftBlocks; ++i)
			pResult->blocks[i] = 0;

		/* check if the terminating block has no set bits */
		if( pResult->blocks[pResult->length - 1] == 0)
			--pResult->length;
	}
}

/**
 * result = result >> shift
 * 
 * Return value is 'true', if '1' bits have been shifted away
 */
UTT_LINKAGE bool utt_bi_shr(utt_bigint_t * pResult, utt_uint32_t shift)
{
	/* take care of '0' being shifted */
	if( !pResult->length || !shift )
		return false;

	utt_uint32_t shiftBlocks = shift / 32;
	utt_uint32_t shiftBits = shift % 32;

	/* process blocks low to high so that we can safely process in place */
	utt_uint32_t *	pInBlocks	= pResult->blocks;
	utt_int32_t		inLength	= pResult->length;
	
	/* Will the result be zero? */
	if( shiftBlocks >= inLength ){
		if( utt_bi_is_zero( pResult ) )
			return false;
		utt_bi_set_zero( pResult );
		return true;
	}
	
	/* Do we shift away '1' bits? */
	bool one_bits = false;
	for( utt_uint32_t* pInCur = pInBlocks, *pEnd = pInBlocks + shiftBlocks; pInCur < pEnd; ++pInCur )
		if( *pInCur ){
			one_bits = true;
			break;
		}
	
	/* check if the shift is block aligned */
	if( shiftBits == 0 )
	{
		/* copy blocks from low to high */
		for( utt_uint32_t * pOutCur = pInBlocks, *pInCur = pOutCur + shiftBlocks, *pEnd = pInBlocks + inLength;
			 pInCur < pEnd;
			 ++pInCur, ++pOutCur)
		{
			*pOutCur = *pInCur;
		}
		
		pResult->length -= shiftBlocks;
	}
	/* else we need to shift partial blocks */
	else
	{
		const utt_uint32_t highBitsShift = (32 - shiftBits);
		
		if( !one_bits )
			one_bits = pResult->blocks[shiftBlocks] & ( 0xFFFFFFFF >> highBitsShift ) ? true : false;
		
		utt_int32_t inBlockIdx	= shiftBlocks;
		utt_uint32_t outBlockIdx = 0;
		
		/* set the length to hold all except the shifted blocks */
		pResult->length -= shiftBlocks;
		
		/* compute the initial blocks */
		utt_uint32_t highBits = inBlockIdx + 1 < inLength ? pResult->blocks[inBlockIdx+1] << highBitsShift: 0;
		utt_uint32_t lowBits = pResult->blocks[inBlockIdx] >> shiftBits;
		while( inBlockIdx < inLength )
		{
			pResult->blocks[outBlockIdx] = highBits | lowBits;

			++inBlockIdx;
			++outBlockIdx;
			
			lowBits = pResult->blocks[inBlockIdx] >> shiftBits;
			highBits = inBlockIdx + 1 < inLength ? pResult->blocks[inBlockIdx+1] << highBitsShift: 0;
		}

		/* check if the terminating block has no set bits */
		if( pResult->blocks[pResult->length - 1] == 0)
			--pResult->length;
	}
	
	return one_bits;
}

/**
 * Returns the log2(value)
 */
UTT_LINKAGE utt_uint32_t utt_bi_log2( const utt_bigint_t * pValue )
{
	utt_uint32_t highest_set_block = pValue->length;
	while( highest_set_block > 0 && pValue->blocks[highest_set_block-1] == 0)
		--highest_set_block;
	if( !highest_set_block )
		return 0;
	highest_set_block -= 1;
	return (utt_uint32_t)utt_log2_32( pValue->blocks[highest_set_block] ) + highest_set_block * 32;
}

#endif
