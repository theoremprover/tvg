/*
int f(int x)
{
    int a = 0;
    switch(x)
    {
        case 1:
            for(int i=0;i<4;i++)
            //          ^^^
            //          Dead Else because of if(i>=2) break;
            {
                if(i>=2) break;
                a++;
            }
            a++;
            break;
        default:
            a = a + 10;
    }
    return a;
}
*/
int f(int x)
{
    for(int i=0;i<4;i++)
    {
        if(i>=2) break;
        x++;
    }
    return x;
}