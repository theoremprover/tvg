/**
 * specifications of the compare routines for float
*/
#include "float_analysis.h"
#include "float_compares.h"

/* extern functions used to compare values (the only imports from math.h) */
double fabs(double);
double sqrt(double);

/**
 * main comparison function for float values
 * - found: the tested values
 * - expected: the expected value
 * - comparetype: indicates which comparison method shall be used (ABS/REL/ULP/ABS_ULP/EXAKT/EQUAL/BINHEX)
 * - limit: the limit in case of combined comparisons
 * - maxtol_below: tolerances (below the limit if limit is specified)
 * - maxtol_above: tolerances (above the limit if limit is specified)
 * - maxdeviation_below: maximal deviation (below the limit if limit is specified)
 * - maxdeviation_above: maximal deviation (above the limit if limit is specified)
*/
TEST_BOOL test_compare_float(float found, float expected,int comparetype, float maxtol_below,float maxtol_above,float limit,float* maxdeviation_below, float* maxdeviation_above) {
	
		TEST_BOOL result = TEST_UNTESTED;
        /** first test special cases */
        if (isnan(found)!=isnan(expected)) {
#if defined(DEBUG)
                PRINTF("NAN-DEVIATION: %.9g <> %.9g: ",found,expected);
#endif
                return TEST_FALSE;
        }
        if (isnan(found) && isnan(expected)) {
                return TEST_TRUE;
        }
        if (isinf(found)!=isinf(expected)) {
#if defined(DEBUG)
                PRINTF("INF-DEVIATION: %.9g <> %.9g: ",found,expected);
#endif
                return TEST_FALSE;
        }
        if (isinf(found) && isinf(expected) && found!=expected) {
#if defined(DEBUG)
                PRINTF("INF-DEVIATION: %.9g <> %.9g: ",found,expected);
#endif
                return TEST_FALSE;
        }
        if (isinf(found) && isinf(expected) && found==expected) {
                return TEST_TRUE;
        }
	if (comparetype&COMPARE_TYPE_ABS_TOLERANCE) {
		/* check absolute tolerance */
		if (fabs(found-expected)>maxtol_below) {
			result = TEST_FALSE;
#if defined(DEBUG)
			PRINTF("ABS: %.9g (%llx) - %.9g (%llx) = %.9g > %.9g :",found,fb_float2int(found),expected,fb_float2int(expected),fabs(found-expected),maxtol_below);
#endif
			if (fabs(found-expected)>*maxdeviation_below) {
				*maxdeviation_below=fabs(found-expected);
			}
		} else {
			if (result==TEST_UNTESTED) {
				result=TEST_TRUE;
			}
		}
	}
	if (comparetype&COMPARE_TYPE_REL_TOLERANCE) {
		/* check relative tolerance */
		if (expected!=0.0 && fabs((found-expected)/expected)>maxtol_below) {
			result = TEST_FALSE;
#if defined(DEBUG)
			PRINTF("REL: %.9g (%llx) - %.9g (%llx) = %.9g > %.9g :",found,fb_float2int(found),expected,fb_float2int(expected),fabs((found-expected)/expected),maxtol_below);
#endif
			if (fabs((found-expected)/expected)>*maxdeviation_below) {
				*maxdeviation_below=fabs((found-expected)/expected);
			}
		} else {
			if (result==TEST_UNTESTED) {
				result=TEST_TRUE;
			}
		}
	}
	if (comparetype&COMPARE_TYPE_ABS_REL_TOLERANCE) {
		/* check absolute & relative tolerance */
		if (fabs(expected)<=limit) {
			// check absolute tolerance
			if (fabs(found-expected)>maxtol_below) {
				result = TEST_FALSE;
#if defined(DEBUG)
				PRINTF("ABS_REL<10: %.9g (%llx) - %.9g (%llx) = %.9g > %.9g :",found,fb_float2int(found),expected,fb_float2int(expected),fabs(found-expected),maxtol_below);
#endif
				if (fabs(found-expected)>*maxdeviation_below) {
					*maxdeviation_below=fabs(found-expected);
				}
			} else {
				if (result==TEST_UNTESTED) {
					result=TEST_TRUE;
				}
			}
		} else {
			// check relative tolerance
			if (fabs((found-expected)/expected)>maxtol_above) {
				result = TEST_FALSE;
#if defined(DEBUG)
				PRINTF("ABS_REL>10: %.9g (%llx) - %.9g (%llx) = %.9g > %.9g :",found,fb_float2int(found),expected,fb_float2int(expected),fabs((found-expected)/expected),maxtol_above);
#endif
				if (fabs(found-expected)>*maxdeviation_above) {
					*maxdeviation_above=fabs(found-expected);
				}
			} else {
				if (result==TEST_UNTESTED) {
					result=TEST_TRUE;
				}
			}
		}
	}
	if (comparetype&COMPARE_TYPE_ULP_TOLERANCE) {
		/* check ULP tolerance */
		if (!float_equal_ulp(found,expected,(uint32) maxtol_below)) {
			result = TEST_FALSE;
			uint32 dif = getDeltaULP(found, expected);
#if defined(DEBUG)
			float_bits fb_found = float2fb(found);
			float_bits fb_expected = float2fb(expected);
			PRINTF("ULP (%llu) of: %.9g (%llx,%llx,%llx) and %.9g (%llx,%llx,%llx) is greater than %.0f :",
				dif,
				found,fb_found.s,fb_found.m,fb_found.e,
				expected,fb_expected.s,fb_expected.m,fb_expected.e,
				maxtol_below);
#endif
			if (dif>*maxdeviation_below) {
				*maxdeviation_below=dif;
			} 
		} else {
			if (result==TEST_UNTESTED) {
				result=TEST_TRUE;
			}
		}
	}
	if (comparetype&COMPARE_TYPE_ABS_ULP_TOLERANCE) {
		/* check ABS tolerance if expected value below the limit, otherwise ULP */
		if (fabs(expected) <= limit) {
			if (fabs(found-expected)>maxtol_below) {
				result = TEST_FALSE;
#if defined(DEBUG)
				PRINTF("ABS-ULP: %.9g-%.9g=%.9g > %.9g :",found,expected,fabs(found-expected),maxtol_below);
#endif
				if (fabs(found-expected)>*maxdeviation_below) {
					*maxdeviation_below=fabs(found-expected);
				}
			} else {
				if (result==TEST_UNTESTED) {
					result=TEST_TRUE;
				}
			}
		} else {
			if (!float_equal_ulp(found,expected,(uint32)maxtol_above)) {
				result = TEST_FALSE;
				uint32 dif = getDeltaULP(found, expected);
#if defined(DEBUG)
				float_bits fb_found = float2fb(found);
				float_bits fb_expected = float2fb(expected);
				PRINTF("ABS-ULP (%llu) of: %.9g (%llx,%llx,%llx) and %.9g (%llx,%llx,%llx) is greater than %llu :",
					dif,
					found,fb_found.s,fb_found.m,fb_found.e,
					expected,fb_expected.s,fb_expected.m,fb_expected.e,
					(uint32)maxtol_above);
#endif
				if (dif>*maxdeviation_above) {
					*maxdeviation_above=dif;
				} 
			} else {
				if (result==TEST_UNTESTED) {
					result=TEST_TRUE;
				}
			}
		}
	}
	if (comparetype&COMPARE_TYPE_EXAKT_TOLERANCE) {
		/* check EXAKT tolerance */
		float exakt=fabs(found-expected);
#define MAX(a,b) ((a)>(b)?(a):(b))
		if (expected!=0.0) { 
			exakt=exakt/MAX(sqrt(fabs(found*expected)),1);
		}
		if (exakt>maxtol_below) {
			result = TEST_FALSE;
#if defined(DEBUG)
			PRINTF("EX-TOL: %.9g > %.9g :",exakt,maxtol_below);
#endif
			if (exakt>*maxdeviation_below) {
				*maxdeviation_below=exakt;
			}
		} else {
			if (result==TEST_UNTESTED) {
				result=TEST_TRUE;
			}
		}
	}
	if (comparetype&COMPARE_TYPE_EQUAL_TOLERANCE) {
		/* compare floating values */
		if (found!=expected) {
			result = TEST_FALSE;
#if defined(DEBUG)
			PRINTF("==: %.9g != %.9g :",found,expected);
#endif
			if (fabs(found-expected)>*maxdeviation_below) {
				*maxdeviation_below=fabs(found-expected);
			}
		} else {
			if (result==TEST_UNTESTED) {
				result=TEST_TRUE;
			}
		}
	}
	if (comparetype&COMPARE_TYPE_BINHEX_TOLERANCE) {
		/* binary compare hex values */
		hexfloat hfFound;
		hfFound.f_val=found;
		hexfloat hfExpected;
		hfExpected.f_val=expected;
		if (hfFound.x_val!=hfExpected.x_val) {
			result = TEST_FALSE;
#if defined(DEBUG)
			PRINTF("0x==: %.9g (0x%llx)!= %.9g (0x%llx) :",hfFound.f_val,hfFound.x_val,hfExpected.f_val,hfExpected.x_val);
#endif
			if (fabs(found-expected)>*maxdeviation_below) {
				*maxdeviation_below=fabs(found-expected);
			}
		} else {
			if (result==TEST_UNTESTED) {
				result=TEST_TRUE;
			}
		}
	}

	return result;
}


