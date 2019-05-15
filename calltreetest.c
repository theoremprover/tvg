#include <stdio.h>
#include "calltreetest2.h"

int g(int x)
{
	return x+1;
}

int f(int i)
{
	int j = h(i) + g(i);
	return j;
}

int main(void)
{
	int a[3];
	{
		{ a[0] = f(1); }
	}
	printf("OK!\n");
	return f(h(1));
}
