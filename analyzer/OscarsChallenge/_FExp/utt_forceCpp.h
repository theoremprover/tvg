#ifndef utt_forceCpp
#define utt_forceCpp

#include "target/c/unittest.h"

#ifndef PRINTF
#define PRINTF utt_print
#endif
#ifndef NEXTAFTERF
#define NEXTAFTERF utt_nextafterf
#endif
#ifndef INFINITY
#define INFINITY UTT_INFINITY
#endif
#ifndef NAN
#define NAN UTT_NAN
#endif
#ifndef isnan
#define isnan utt_isnan
#endif
#ifndef isinf
#define isinf utt_isinf
#endif

#endif