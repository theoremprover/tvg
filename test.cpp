
// test comment

char f(char s1)
{
    return (s1++);
}

int g(int x,int y)
{
    char *c = f('a');
	int i = 3;
	int * j = &i , k;
    return c;
}

int main()
{
    return g(1,2);
}
