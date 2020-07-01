/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_UTILITY_BIGFLOAT_COMPARISON_H_
#define _TARGET_UNITTEST_UTILITY_BIGFLOAT_COMPARISON_H_

#include <target/c/bigfloat/predefines.h>


/** Define isless for multiprecision floating point type */
UTT_LINKAGE bool utt_islessm( const utt_bigfloat_t left , const utt_bigfloat_t right )
{
	if( utt_isnanm(left) || utt_isnanm(right) )
		return false;
	
	bool sbl = utt_signbitm(left);
	if( sbl != utt_signbitm(right) )
		return sbl;
	
	/* Compute first byte difference, excluding the signbit */
	int difference = ( left.data[0] & 0x7F ) - ( right.data[0] & 0x7F );
	
	/* Difference in first byte? */
	if( !difference )
		difference = utt_memcmp( (char*)&left + 1 , (char*)&right + 1 , sizeof(utt_bigfloat_t) - 1 );
	
	return sbl ? difference > 0 : difference < 0;
}

#endif