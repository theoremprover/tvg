//#include <stdio.h>
//#include <stdlib.h>

int x = 7;

enum Zahl { EINS, ZWEI };

typedef int MYINT;

float f(MYINT x)
{
    float z = 0.1;
    if(x>7)
    {
        short abc;
        z = z+1;
    }

	return(z+1);
}

//int main(int argc,char* argv[])
int main()
{
//    int x = atoi(argv[1]);
//    int y = atoi(argv[2]);
    int ret = f(x);
//    printf("f(%i)=%i\n",x,ret);
    return 0;
}
