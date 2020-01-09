//#include <stdio.h>
//#include <stdlib.h>

int x1 = 7;

enum Zahl { EINS, ZWEI };

typedef int MYINT2;
typedef MYINT2 MYINT;

short f(MYINT x,enum Zahl z)
{
    MYINT2 x1 = x*2;
    x1 += 3;
    if(x1>7)
    {
        short abc;
        abc = x+x1;
        return(abc*x1);
    }
    else
    {
        if(z==EINS)
        {
            x1 = x1-2;
        }
    }

	return((short)(x1 % 0xffff));
}

//int main(int argc,char* argv[])
int main()
{
//    int x = atoi(argv[1]);
//    int y = atoi(argv[2]);
    int ret = f(5,EINS);
//    printf("f(%i)=%i\n",x,ret);
    return 0;
}
