/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_PREDEFINES_H_
#define _TARGET_UNITTEST_PREDEFINES_H_

/** Define the meaning of floating point types */
#define IEEE_754		7541
#define IEEE_754_ARM	7542

#ifndef TARGET_NATIVE_HALF
	#define TARGET_NATIVE_HALF 0
#endif
#ifndef TARGET_NATIVE_FLOAT
	#define TARGET_NATIVE_FLOAT IEEE_754
#endif
#ifndef TARGET_NATIVE_DOUBLE
	#define TARGET_NATIVE_DOUBLE IEEE_754
#endif


/** Define a shorthand for "__attribute__((packed))" */
#define UTT_PACKED __attribute__((packed))


/** Define what ForeC++ Target Functions should be linked as */
#define UTT_LINKAGE static inline


/** Forward Declarations... */
UTT_LINKAGE void utt_print( const char* fmt , ... );
UTT_LINKAGE void utt_putstr( const char* str );


#endif
