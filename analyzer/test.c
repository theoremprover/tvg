/*
typedef union
 {
 unsigned short _Sh[sizeof(double)/sizeof(short)];
 double _Val;
 } __attribute__ ((__may_alias__)) _Dval;

short f(double *px)
 {
 _Dval *ps = (_Dval *)(char *)px;

    return ((short) (
        ((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) != 0) ||
           ps->_Sh[2] != 0 ||
           ps->_Sh[1] != 0 ||
           ps->_Sh[0] != 0 ? 2 : 1));
 }
*/
float f(float x)
{
    if(x==3.14) return (x*x+1.0);
    return(0.0);
}