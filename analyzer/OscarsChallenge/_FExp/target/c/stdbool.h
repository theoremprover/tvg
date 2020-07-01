/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_STDBOOL_H_
#define _TARGET_UNITTEST_STDBOOL_H_

/** Define bool, true and false */
#ifndef __cplusplus
	#if !defined(_BOOL) && !defined(bool)
		typedef unsigned char bool;
	#else
		typedef _Bool bool;
	#endif
	#ifndef true
		#define true ((bool)1)
	#endif
	#ifndef false
		#define false ((bool)0)
	#endif
#endif

#endif