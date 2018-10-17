#include <stdio.h>
#include <stdlib.h>

typedef unsigned long ULONG;
typedef unsigned char UBYTE;

#define MAX_COMBINATIONS 16
typedef struct {
	UBYTE numCombinationsTrue = 0;
	ULONG combinationsTrue[MAX_COMBINATIONS];
	UBYTE numCombinationsFalse = 0;
	ULONG combinationsFalse[MAX_COMBINATIONS];
	ULONG independentConditions = 0L;
	} DECISION;

void addCombination(ULONG cur_combination,UBYTE* p_num_combinations,ULONG* combinations)
{
	for(UBYTE i=0;i<*p_num_combinations;i++)
	{
		if(cur_combination==combinations[i]) return;
	}
	if(*p_num_combinations>=MAX_COMBINATIONS)
	{
		printf("MAX_COMBINATIONS exceeded!\n");
		exit(1);
	}
	combinations[(*p_num_combinations)++] = cur_combination;
}

void decisionOutcome(ULONG cur_combination,DECISION* decision,BOOL decision_outcome)
{
	if(decision_outcome)
		addCombination(cur_combination,&(decision->numCombinationsTrue),decision->combinationsTrue);
	else
		addCombination(cur_combination,&(decision->numCombinationsFalse),decision->combinationsFalse);
}


DECISION decision1234 = {
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L},
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L},
	0L };
DECISION all_decisions[1] = { &decision1234 };

int combs[3][] = { {0,0,0}, {1,0,0}, {0,1,0}, {0,0,1} };

void showDecision(DECISION* decision)
{
/*
typedef struct {
	UBYTE numCombinationsTrue = 0;
	ULONG combinationsTrue[MAX_COMBINATIONS];
	UBYTE numCombinationsFalse = 0;
	ULONG combinationsFalse[MAX_COMBINATIONS];
	ULONG independentConditions = 0L;
	} DECISION;
*/
	printf("combinationsTrue:\n");
	for(int i=0;i<decision->numCombinationsTrue;i++) printf("%3b, ",decision->combinationsTrue[i]);
	printf("\ncombinationsFalse:\n");
	for(int i=0;i<decision->numCombinationsFalse;i++) printf("%3b, ",decision->combinationsFalse[i]);
	printf("\nindependentConditions = %3b\n",decision->independentConditions);
}


int main(int argc,char *argv[])
{
	if(argc<4) return(1);

	for(int i=0;i<sizeof(combs);i++)
	{
		int a,b,c;
		a = combs[0][i];
		if(a)   // Lazy "||" operator, evaluate b and c only if a==True
		{
			add
			b = combs[1][i];
			// Strict "&" operator, hence evaluating c unconditionally
			c = combs[2][i];
		}
		else
		{
			// b and c not evaluated, could be any 
		}

		ULONG cur_comb = ((ULONG)a&1)<<0 | ((ULONG)b&1)<<1 | ((ULONG)c&1)<<2;
		if(a || (b & c))
		{
			decisionOutcome(cur_comb,&decision1234,0);
			printf("True\n");
		}
		else
		{
			decisionOutcome(cur_comb,&decision1234,1);
			printf("False\n");
		}
	}

	showDecision(&decision1234);

	return(0);
}
