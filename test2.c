void mytrace(char *);
void opentrace(char *);
void closetrace();
typedef int INSTRUMENTED_ALREADY;
int abs(int x)
{
    if (x > 0)
    {
        return x;
    }
    else
    {
        return 0 - x;
    }
}
int g_rev(int x, int y)
{
    int e = y;
    if (x > 1)
    {
        {
            mytrace("TRACE: (\"test2.c\": line 19)\n");
            e = e * 2;
        }
    }
    return e - 1;
}
int f(int x, int y)
{
    int erg = 0;
    int f = 1;
    if (x < 0)
    {
        {
            mytrace("TRACE: (\"test2.c\": line 29)\n");
            f = -1;
        }
    }
    for (int i = 1; i <= abs(x); i++)
    {
        {
            mytrace("TRACE: (\"test2.c\": line 33)\n");
            erg = erg + f * y;
        }
        {
            mytrace("TRACE: (\"test2.c\": line 34)\n");
            erg = erg;
        }
    }
    return erg;
}
int main()
{
    opentrace("/tvg/tvg/incs/trace.txt");
    {
        return 0;
    }
}
int h(int x)
{
    return x - 1;
}
int h2(int x, int y)
{
    return x - 2 * y;
}