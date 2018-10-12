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

int main(int argc,char *argv[])
{
	if(argc<4) return(1);

	decision1234_cond[0] = (int)strcmp(argv[1],"1")==0;
	if(a)   // Lazy "||" operator
	{
		decision1234_cond[1] = (int)strcmp(argv[2],"1");
		// Strict "&" operator, so no "if"
		decision1234_cond[2] = (int)strcmp(argv[3],"1");
	}
	if(decision1234_cond[0]) decision1234_cond_True[0]++; else decision1234_cond_False[0]++;
	if(decision1234_cond[1]) decision1234_cond_True[1]++; else decision1234_cond_False[1]++;
	if(decision1234_cond[2]) decision1234_cond_True[2]++; else decision1234_cond_False[2]++;
	if((strcmp(argv[1],"1")==0) || (strcmp(argv[2],"1") & strcmp(argv[3],"1")))
	{
		decision1234_True++;
		printf("True\n");
	}
	else
	{
		decision1234_False++;
		printf("False\n");
	}

	return(0);
}
