#ifdef CALC
#include <stdio.h>
#include <stdlib.h>
#endif

int f(int x,int y)
{
    int a = 0;
    switch(x)
    {
        case 1:
            a = a+1;
            a = a+0;
            break;
        case 2:
            if(y>0)
            {
                a=a+20;
                switch(y)
                {
                    case 0:
                        a=a; // dead!
                    case 1:
                        a=a+100;
                    case 2:
                        a=a+50;
                        break;
                    default:
                        a=a+1000;
                }
            }
            else
            {
                a = a+2;
                break;
            }
            a = a + 3;
        case 3:
            a = a + 4;
        default:
            a = a + 10;
    }
    return a;
}

#ifdef CALC
int main(int argc, char* argv[])
{
    int x = atoi(argv[1]);
    int y = atoi(argv[2]);
    printf("f(x=%i,y=%i) =\n%i\n",x,y,f(x,y));
    return 0;
}
#endif
