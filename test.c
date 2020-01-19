//#include <stdio.h>
//#include <stdlib.h>

int x1 = 7;

enum Zahl { EINS, ZWEI };

typedef int MYINT2;
typedef MYINT2 MYINT;

float g(int x)
{
    if(x>=0)
    {
        return((float)(2*x));
    }
    else
    {
        return((float)(3*(-x)));
    }
}

short f(MYINT x,int y,enum Zahl z)
{
    MYINT2 x1 = x*2;
    x1 += 3;
    if(x1>g(y+1))
    {
        short abc;
        abc = x+(short)g(x1);
        return(abc*x1);
    }
    else
    {
        if(z==EINS)
        {
            x1 = (int)g(x1-2);
        }
    }

	return((short)(x1 % 0xffff));
}

/*
x_9 = -10;
g_8 = 30.0;
x_1 = 1;
x1_4 = -2;
x_7 = 0;
g_6 = 0.0;
y_2 = 0;
*/

//int main(int argc,char* argv[])
int main()
{
//    int x = atoi(argv[1]);
//    int y = atoi(argv[2]);
    int ret = f(1,0,EINS);
//    printf("f(%i)=%i\n",x,ret);
    return 0;
}
