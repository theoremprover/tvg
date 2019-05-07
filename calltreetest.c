#include <stdio.h>

int g(int x)
{
	return x+1;
}

int h(int x)
{
	return x-1;
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
	return f(h(1));
}
