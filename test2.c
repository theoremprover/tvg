int f(int x, int y)
{
    int erg = 0;
    int g = 1;

	g = 2;

    if (x < 0) g = -1;
	else
	{
		int z = x;
		z++;
	}

	int i;
    for (i = 1; i <= x; i++)
    {
		erg = erg + g * y;
		erg = erg;
    }
    return erg;
}

int main()
{
	return f(0,1);
}
