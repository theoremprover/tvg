/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_TIME_H_
#define _TARGET_UNITTEST_TIME_H_

#include <target/c/predefines.h>

/* 1.0 = 1 second */
typedef utt_ieee754_float	utt_duration_t;


#ifndef TARGET_NO_TIME
	
	/* Include standard header */
	#include <time.h> /* If <time.h> is not resolvable, pass -DTARGET_NO_TIME */
	
	/* Define the type of a timepoint */
	typedef clock_t utt_timepoint_t;
	
	UTT_LINKAGE utt_timepoint_t utt_get_time(){
		return clock();
	}
	
	UTT_LINKAGE utt_duration_t	utt_get_duration( utt_timepoint_t start , utt_timepoint_t end ){
		return ((utt_duration_t)( end - start )) / CLOCKS_PER_SEC;
	}
	
#else
	
	/* Define the type of a timepoint */
	typedef utt_uint32_t utt_timepoint_t;
	
	UTT_LINKAGE utt_timepoint_t utt_get_time(){
		static utt_timepoint_t cur = 0;
		return cur++;
	}
	
	UTT_LINKAGE utt_duration_t	utt_get_duration( utt_timepoint_t start , utt_timepoint_t end ){
		return start - end;
	}
	
#endif

#endif
