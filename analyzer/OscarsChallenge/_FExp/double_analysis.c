#include "double_analysis.h"

uint64 double_getDeltaULP(double x, double y){
	uint64 res_u64, delta_exp;
	uint64 delta_mantisse;
	double_bits x_bits;
	double_bits y_bits;
	if (x != x || y != y){
		res_u64 = 0xFFFFFFFFFFFFFFFFU;
	} else if (x == INFINITY || y == INFINITY || x == -INFINITY || y == -INFINITY){
		res_u64 = 0xFFFFFFFFFFFFFFFFU;
	} else {
		res_u64 = 0;
		x_bits = double2db(x);
		y_bits = double2db(y);
		if (x_bits.s == y_bits.s){
			if (x_bits.e > y_bits.e){
				delta_exp = x_bits.e - y_bits.e;
				if (x_bits.m >= y_bits.m){
					delta_mantisse = x_bits.m - y_bits.m;
				} else {
					delta_mantisse = (0x10000000000000U - y_bits.m) + x_bits.m;
					--delta_exp;
				}
			} else if (x_bits.e < y_bits.e){
				delta_exp = y_bits.e - x_bits.e;
				if (x_bits.m <= y_bits.m){
					delta_mantisse = y_bits.m - x_bits.m;
				} else {
					delta_mantisse = (0x10000000000000U - x_bits.m) + y_bits.m;
					--delta_exp;
				}
			} else {
				delta_exp = 0;
				if (x_bits.m <= y_bits.m){
					delta_mantisse = y_bits.m - x_bits.m;
				} else {
					delta_mantisse = x_bits.m - y_bits.m;
				}
			}

		} else {
				delta_exp = x_bits.e + y_bits.e;
				delta_mantisse = x_bits.m + y_bits.m;
		}

		res_u64 = res_u64 + (uint64)((0x10000000000000U * delta_exp) + delta_mantisse) ;
	}
	return res_u64;
}

void double_computeAndPrint(double x, double y){
	//printdouble(x);
	//printdouble(y);
	PRINTF("BIT ULP Distance of above two floats is: %llu [%lu = not comparable]\n", double_getDeltaULP(x, y), ULONG_MAX);
}

#ifdef WITH_MAIN
/* Tests for getDeltaULP_BIT*/
int main(int argc, char *argv[]){
	PRINTF("Start tests\n------------------------\n");
	///computeAndPrint(-0.592597902f, -0.592598021f);
	///computeAndPrint(0.819272459f, 0.81927228f);
	///computeAndPrint(0.81927228f, 0.819272459f);
	///computeAndPrint(FLT_NAN_MAXONES_NEG, FLT_NAN_MINONES_NEG);
	///computeAndPrint(0.0f, -0.0f);
	///computeAndPrint(1.0f, -0.5f);
	///computeAndPrint(-0.648228407f, -0.648228347f);
	///computeAndPrint(FLT_MAX-FLT_MAX/1.5f, FLT_MAX);
	///computeAndPrint(FLT_MIN, 0.0f);
	///computeAndPrint(-10.0f, 10.0f);
	///computeAndPrint(0.0f, INFINITY);
	///computeAndPrint(FLT_MIN_DENORM_POS, FLT_MAX_DENORM_POS);
	///computeAndPrint(FLT_MIN, FLT_MAX);
	///computeAndPrint(-FLT_MIN, FLT_MAX);
	///computeAndPrint(-FLT_MAX, FLT_MAX);
	return 0;
}
#endif
