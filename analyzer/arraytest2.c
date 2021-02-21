typedef union
{
 unsigned short _Sh[3];
 float _Val;
} _Fval;

unsigned short f(unsigned short us)
{
    _Fval fval;
    _Fval *ps = &fval;

    ps->_Sh[0] = us;
    ps->_Sh[1] = ps->_Sh[0]+1;

    if(ps->_Sh[1] == 2) return ( ps->_Sh[0] + ps->_Sh[1] );
    else return ( ps->_Sh[0] + ps->_Sh[1] + 1 );

}
