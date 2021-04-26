// This test will give three stimuli when subfuncovOpt is set, two stimuli otherwise.

int g(int x)
{
    if(x>0) return 1;
    else return 0;
}

int f(int x)
{
    if(x>=0) return g(x);
    else return 99;
}
