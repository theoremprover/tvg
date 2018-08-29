int x = 1;

int f(int a,int b)
{
	int erg = a+b;

	return( { x++; erg; } )
}

int main(int argc, char* argv[])
{
	if(argc>1) return 99;

	return f(1,2);
}
