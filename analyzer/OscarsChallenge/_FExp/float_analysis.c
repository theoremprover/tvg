#include "float_analysis.h"

void printfloat(float v) {
    float_int tmp;
    float_bits tmp2;
    tmp.fval = v;
    tmp2 = float2fb(v);
    PRINTF("%+.9g [hex: %08X, s:%u,e:%u,m:%u]\n",v,tmp.ival,tmp2.s,tmp2.e,tmp2.m);
}

uint32 getDeltaULP(float x, float y){
	uint32 res_u32;
	float z, direction;
	if (x != x || y != y){
		res_u32 = UINT_MAX;
	} else if (x == INFINITY || y == INFINITY || x == -INFINITY || y == -INFINITY){
		res_u32 = UINT_MAX;
	} else {
		direction = (y > x) ? FLT_MAX: -FLT_MAX;
		z = x;
		res_u32 = 0;
		while (y != z){
			z = NEXTAFTERF(z, direction);
			++res_u32;
			if (res_u32 >= UINT_MAX){
				/* shouldn't be reached (on 64-bit??)*/
				break;
			}
		}
	}
	return res_u32;
}

uint32 getDeltaULP_BIT(float x, float y){
	uint32 res_u32, delta_exp;
	uint32 delta_mantisse;
	float_bits x_bits;
	float_bits y_bits;
	if (x != x || y != y){
		res_u32 = 0xFFFFFFFFU;
	} else if (x == INFINITY || y == INFINITY || x == -INFINITY || y == -INFINITY){
		res_u32 = 0xFFFFFFFFU;
	} else {
		res_u32 = 0;
		x_bits = float2fb(x);
		y_bits = float2fb(y);
		if (x_bits.s == y_bits.s){
			if (x_bits.e > y_bits.e){
				delta_exp = x_bits.e - y_bits.e;
				if (x_bits.m >= y_bits.m){
					delta_mantisse = x_bits.m - y_bits.m;
				} else {
					delta_mantisse = (0x800000U - y_bits.m) + x_bits.m;
					--delta_exp;
				}
			} else if (x_bits.e < y_bits.e){
				delta_exp = y_bits.e - x_bits.e;
				if (x_bits.m <= y_bits.m){
					delta_mantisse = y_bits.m - x_bits.m;
				} else {
					delta_mantisse = (0x800000U - x_bits.m) + y_bits.m;
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
		
		res_u32 = res_u32 + (uint32)((0x800000U * delta_exp) + delta_mantisse) ;
	}
	return res_u32;
}

void computeAndPrint(float x, float y){
	printfloat(x);
	printfloat(y);
	PRINTF("    ULP Distance of above two floats is: %u [%u = not comparable]\n", getDeltaULP(x, y), UINT_MAX);
	PRINTF("BIT ULP Distance of above two floats is: %u [%u = not comparable]\n", getDeltaULP_BIT(x, y), UINT_MAX);
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
	computeAndPrint(FLT_MIN, 0.0f);
	computeAndPrint(-10.0f, 10.0f);
	computeAndPrint(0.0f, INFINITY);
	computeAndPrint(FLT_MIN_DENORM_POS, FLT_MAX_DENORM_POS);
	computeAndPrint(FLT_MIN, FLT_MAX);
	computeAndPrint(-FLT_MIN, FLT_MAX);
	computeAndPrint(-FLT_MAX, FLT_MAX);
	return 0;
}
#endif
