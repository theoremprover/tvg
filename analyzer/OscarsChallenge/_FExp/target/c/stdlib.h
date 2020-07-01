/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_STDLIB_H_
#define _TARGET_UNITTEST_STDLIB_H_

#include <target/c/ieee754/predefines.h>

/** Replacement Functions for abs, labs, llabs and intmaxabs */
UTT_LINKAGE int utt_abs( int val ){ return val < 0 && val != UTT_INT_MIN ? -val : val; }
UTT_LINKAGE long int utt_labs( long int val ){ return val < 0L && val != UTT_LONG_MIN ? -val : val; }
UTT_LINKAGE long long int utt_llabs( long long int val ){ return val < 0LL && val != UTT_LONGLONG_MIN ? -val : val; }
UTT_LINKAGE utt_intmax_t utt_imaxabs( utt_intmax_t val ){ return val < 0LL && val != UTT_INTMAX_MIN ? -val : val; }

#endif