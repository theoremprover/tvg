union _DVal
{
    int p_i;
    float p_d;
};

float f(double* px)
{
    _DVal* ps = (_DVal*)(char*) px;
    return ( ps->p_i );
}
