#include <stdio.h>

int main(int argc,char *argv[])
{
	float f = 1.2;
	printf("%lx\n",*(long int*)&f);

	float g;
	int i = sscanf(argv[1],"%x",(long int*)(&g));
	if(i==1) printf("g=%f\n",g); else printf("ERROR!\n");
}
