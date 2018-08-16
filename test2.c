void mytrace(char *);
void opentrace(char *);
void closetrace();
typedef int INSTRUMENTED_ALREADY;
int f(int x, int y)
{
    int erg = 0;
    int g = 1;
    {
        mytrace("TRACE: (\"test2.c\": line 8)\n");
        g = 2;
    }
    if (x < 0)
    {
        mytrace("TRACE: (\"test2.c\": line 10)\n");
        g = -1;
    }
    else
    {
        int z = x;
        z++;
    }
    for (int i = 1; i <= x; i++)
    {
        {
            mytrace("TRACE: (\"test2.c\": line 19)\n");
            erg = erg + g * y;
        }
        {
            mytrace("TRACE: (\"test2.c\": line 20)\n");
            erg = erg;
        }
    }
    return erg;
}
int main()
{
    opentrace("/tvg/tvg/incs/trace.txt");
    {
        {
            closetrace();
            return 0;
        }
    }
    closetrace();
}