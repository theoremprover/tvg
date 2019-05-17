#include "calltreetest.h"

int i(int x)
{
	return j(3)*f(x);
}

int j(int x)
{
	return x-10;
}
