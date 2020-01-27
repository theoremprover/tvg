int g(int x,int y)
{
    int erg = x;
    while(y>0)
    {
        erg = erg >>= 1;
    }
    return(erg);
}

int f(int y)
{
    if(g(2,y)>10) return 1;
    else return 0;
}

//int main(int argc,char* argv[])
int main()
{
//    int x = atoi(argv[1]);
//    int y = atoi(argv[2]);
    int ret = f(1);
//    printf("f(%i)=%i\n",x,ret);
    return 0;
}
