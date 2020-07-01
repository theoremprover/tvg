/**
 * common specifications of the compare routines
*/
#ifndef COMPARE_H
#define COMPARE_H

/* basic definitions */
#define TEST_TRUE 1
#define TEST_FALSE 0
#define TEST_UNTESTED -1
#define TEST_ERROR -2
#define TEST_BOOL int
#ifdef FORECPP_PRINT
#include "utt_forceCpp.h"
#endif
#if ( defined(DEBUG) || defined(SUMMARY) ) && !defined(PRINTF)
#include <stdio.h>
#endif
#ifndef PRINTF
/* is used for printing test results */
#define PRINTF printf
#endif

/** 
  * different comparison types
  */
#define COMPARE_TYPE_ABS_TOLERANCE 1
#define COMPARE_TYPE_REL_TOLERANCE 2
#define COMPARE_TYPE_ULP_TOLERANCE 4
#define COMPARE_TYPE_ABS_ULP_TOLERANCE 8
#define COMPARE_TYPE_EXAKT_TOLERANCE 16
#define COMPARE_TYPE_EQUAL_TOLERANCE 32
#define COMPARE_TYPE_BINHEX_TOLERANCE 64
#define COMPARE_TYPE_ABS_REL_TOLERANCE 128

#endif /* COMPARE_H */
