void opentrace();
void closetrace();
typedef struct {
            long line, column; long cnt;
        } COUNTER;
typedef struct {
            char sourcefilename[250]; COUNTER counters[];
        } SRCFILE;
int f(int x, int y)
{
    src_test2_c.counters[11].cnt++;
    {
        int erg = 0;
        int g = 1;
        {
            src_test2_c.counters[1].cnt++;
            g = 2;
        }
        {
            src_test2_c.counters[5].cnt++;
            if (x < 0)
            {
                src_test2_c.counters[2].cnt++;
                g = -1;
            }
            else
            {
                src_test2_c.counters[4].cnt++;
                {
                    int z = x;
                    {
                        src_test2_c.counters[3].cnt++;
                        z++;
                    }
                }
            }
        }
        int i;
        {
            src_test2_c.counters[9].cnt++;
            for (i = 1; i <= x; i++)
            {
                src_test2_c.counters[8].cnt++;
                {
                    {
                        src_test2_c.counters[6].cnt++;
                        erg = erg + g * y;
                    }
                    {
                        src_test2_c.counters[7].cnt++;
                        erg = erg;
                    }
                }
            }
        }
        {
            src_test2_c.counters[10].cnt++;
            return erg;
        }
    }
}
int main()
{
    opentrace();
    {
        src_test2_c.counters[13].cnt++;
        {
            {
                src_test2_c.counters[12].cnt++;
                {
                    int my_ret = f(0, 1);
                    closetrace();
                    return my_ret;
                }
            }
        }
    }
    closetrace();
}