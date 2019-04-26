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
#ifdef X
	int y = f(g(h(1)));
#endif
	return f(h(1));
}
