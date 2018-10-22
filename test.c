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
		if(cur_combination==combinations[i])
		{
			printf("Found %lx at %i\n",cur_combination,i);
			return;
		}
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


DECISION all_decisions[] = {
	{ 3,
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L},
	0, {0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L,0L} }
	};

#define TT 0
#define FF 1

int combs[][3] = { {TT,TT,TT}, {FF,TT,TT}, {TT,FF,TT}, {TT,TT,FF} };

void showComb(UBYTE numCond,ULONG comb)
{
	for(int i=(int)(numCond-1);i>=0;i--)
	{
		printf("%c",'0'+(int)((comb>>i)&1));
	}
}

void showDecision(DECISION* decision)
{
	printf("combinationsTrue :  ");
	for(int i=0;i<decision->numCombinationsTrue;i++)
	{
		showComb(decision->numConditions,decision->combinationsTrue[i]);
		printf(", ");
	}
	printf("\ncombinationsFalse:  ");
	for(int i=0;i<decision->numCombinationsFalse;i++)
	{
		showComb(decision->numConditions,decision->combinationsFalse[i]);
		printf(", ");
	}
	printf("\n");
}

void evalCoverage()
{
	int mcdc = TT;

	for(int d=0;d<sizeof(all_decisions)/sizeof(DECISION);d++)
	{
		DECISION* decision = &(all_decisions[d]);

		if((decision->numCombinationsTrue<1) || (decision->numCombinationsFalse<1))
		{
			printf("MISSING OUTCOME: d=%i\n",d);
			mcdc = FF;
		}

		ULONG b = 1<<(decision->numConditions - 1);
		for(int i=0;i<decision->numConditions;i++,b=b>>1)
		{
			int indep = FF;
			for(int j=0;j<decision->numCombinationsTrue;j++)
			{
				for(int k=0;k<decision->numCombinationsFalse;k++)
				{
					if((decision->combinationsTrue[j] & ~b) == (decision->combinationsFalse[k] & ~b))
					{
						indep = TT;
					}
				}
			}
			mcdc = indep;
		}
	}

	printf("mcdc=%i\n",mcdc);
}

int main(int argc,char *argv[])
{
	DECISION* decision1234 = &(all_decisions[0]);

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
			b = c = 0;
		}

		ULONG cur_comb = ((ULONG)a&1)<<2 | ((ULONG)b&1)<<1 | ((ULONG)c&1)<<0;
		printf("Cur comb: "); showComb(decision1234->numConditions,cur_comb); printf("\n");
		if(a || (b & c))
		{
			decisionOutcome(cur_comb,decision1234,TT);
			printf("True\n");
		}
		else
		{
			decisionOutcome(cur_comb,decision1234,FF);
			printf("False\n");
		}
	}

	showDecision(decision1234);
	evalCoverage();

	return(0);
}
