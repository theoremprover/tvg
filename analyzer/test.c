union mystruct
{
    int i_s;
    float f_s;
};

typedef union mystruct MyStruct;

float f(int* x)
{
    return ( ((MyStruct*)(char*)x) -> f_s );
}
