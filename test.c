#include <stdio.h>

#define MAX_COMBINATIONS 16
typedef struct {
	UBYTE numCombinationsTrue = 0;
	ULONG combinationsTrue[MAX_COMBINATIONS];
	UBYTE numCombinationsFalse = 0;
	ULONG combinationsFalse[MAX_COMBINATIONS];
	ULONG independentConditions = 0;
	} DECISION;

void decisionOutcome(ULONG cur_combination,DECISION* decision,BOOL decision_outcome)
{
	if(decision_outcome) addCombination(cur_combination,&(decision->numCombinationsTrue),combinationsTrue);
	else addCombination(cur_combination,&(decision->numCombinationsFalse),combinationsFalse);
}

void addCombination(ULONG cur_combination,UBYTE* p_num_combinations,ULONG* combinations)
{
	for(UBYTE i=0;i<*p_num_combinations;i++)
	{
		if(cur_combination==combinations[i]) return;
	}
	combinations[(*p_num_combinations)++] = cur_combination;
}


DECISION decision1234 = {
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L},
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L},
	0L };
DECISION all_decisions[1] = { &decision1234 };

int combs[] = { 

int main(int argc,char *argv[])
{
	if(argc<4) return(1);

	for(int i=0;i<10;i++)
	{
		int a,b,c;
		a = combs[i*3];
		if(a)   // Lazy "||" operator, evaluate b and c only if a==True
		{
			add
			b = combs[i*3+1];
			// Strict "&" operator, hence evaluating c unconditionally
			c = combs[i*3+2];
		}
		else
		{
			// b and c not evaluated, could be any 
		}

		ULONG cur_comb = (a&1)<<0 | (b&1)<<1 | (c&1)<<2;
		if(a || (b & c))
		{
			decisionOutcome(cur_comb,&decision1234,True);
			printf("True\n");
		}
		else
		{
			decisionOutcome(cur_comb,&decision1234,True);
			printf("False\n");
		}
	}

	return(0);
}
