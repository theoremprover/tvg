/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_CTYPE_H_
#define _TARGET_UNITTEST_CTYPE_H_

#include <target/c/stdbool.h>
#include <target/c/predefines.h>

UTT_LINKAGE bool utt_isprint( unsigned char c ){ return c >= 0x20 && c <= 0x7e; }

#endif
