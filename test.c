#include <stdio.h>
#include <stdlib.h>

typedef unsigned long ULONG;
typedef unsigned char UBYTE;

#define MAX_COMBINATIONS 16
typedef struct {
	UBYTE numConditions;
	UBYTE numCombinationsTrue;
	ULONG combinationsTrue[MAX_COMBINATIONS];
// 010,000
	UBYTE numCombinationsFalse;
	ULONG combinationsFalse[MAX_COMBINATIONS];
// 001
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

void decisionOutcome(ULONG cur_combination,DECISION* decision,int decision_outcome)
{
	if(decision_outcome)
		addCombination(cur_combination,&(decision->numCombinationsTrue),decision->combinationsTrue);
	else
		addCombination(cur_combination,&(decision->numCombinationsFalse),decision->combinationsFalse);
}


DECISION decision1234 = { 3,
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L},
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L} };
DECISION* all_decisions[1] = { &decision1234 };

int combs[][3] = { {0,0,0}, {1,0,0}, {0,1,0}, {0,0,1} };

void showComb(UBYTE numCond,ULONG comb)
{
	ULONG mask = comb;
	for(int i=0;i<numCond;i++)
	{
		printf("%c",'0'+(mask&1));
	}
}

void showDecision(DECISION* decision)
{
	printf("combinationsTrue:\n");
	for(int i=0;i<decision->numCombinationsTrue;i++) printf("%s, ",showComb(decision->numConditions,decision->combinationsTrue[i]));
	printf("\ncombinationsFalse:\n");
	for(int i=0;i<decision->numCombinationsFalse;i++) printf("%s, ",showComb(decision->numConditions,decision->combinationsFalse[i]));
}

void evalCoverage(DECISION* decision)
{
	for(int i=0;i<decision->numConditions;i++)
	{
		ULONG b = 1<<i;
		for(int j=0;j<decision->numCombinationsTrue;j++)
		{
			for(int k=0;k<decision->numCombinationsFalse;k++)
			{
				if((decision->combinationsTrue[j])&(~b)==(decision->combinationsFalse[k])&(~b))
				{
					printf("Condition %i independent: %i,%i",i,j,k);
				}
			}
		}
	}
}

int main(int argc,char *argv[])
{
	for(int i=0;i<sizeof(combs)/sizeof(combs[0]);i++)
	{
		int a,b,c;
		a = combs[i][0];
		if(a)   // Lazy "||" operator, evaluate b and c only if a==True
		{
			b = combs[i][1];
			// Strict "&" operator, hence evaluating c unconditionally
			c = combs[i][2];
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
	evalCoverage(&decision1234);

	return(0);
}
