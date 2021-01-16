struct mystruct
{
    int a_s;
    float b_s;
};

typedef struct mystruct MyStruct;

int f(MyStruct* x)
{
    MyStruct* ps = x;
    return (ps->a_s);
}