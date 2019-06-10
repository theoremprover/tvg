int g(int x)
{
	if(x>5)
		return(2*x);

	else
	{
		x = x + 1;
	}

	return(x+10);
}

int f(int x)
{
	int z = 2*g(x);

	int y = 3*z;

	if(y>10)
	{
		return(g(y));
	}
	else
	{
		if(z>5)
		{
			y = g(x+2);
		}
	}

	return(y+1);
}

int main()
{
	int x;
	x= 6;
	x=-2;
	x=-9;
}
