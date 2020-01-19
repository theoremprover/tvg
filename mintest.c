//#include <stdio.h>
//#include <stdlib.h>

typedef int MYINT2;
typedef MYINT2 MYINT;

short f(MYINT x,int y)
{
    MYINT2 x1 = x*2;
    x1 += 3;
	return((short)(x1 % 0xffff));
}

//int main(int argc,char* argv[])
int main()
{
//    int x = atoi(argv[1]);
//    int y = atoi(argv[2]);
    int ret = f(1,0);
//    printf("f(%i)=%i\n",x,ret);
    return 0;
}
