


typedef long unsigned int _Sizet;
typedef _Sizet size_t;

typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;



short _FDint(float *px, short xexp)
 {
 _Fval *ps = (_Fval *)(char *)px;

 ps->_Sh[0] = 0x2000;   // PTR_px$$$1 -> PTR_px$$$1 [0] = 0x2000;
 ps->_Sh[0] = 0x2001;   // PTR_px$$$1 -> PTR_px$$$2 [0] = 0x2001;

 // COND PTR_px$$$2[1]==255
 if (ps->_Sh[1] == 255) return 2;

 // ps->_Sh = PTR_px$$$

  return 99;
}

/*
int _FDint(float *px, short xexp)
{
    int arr[] = { 1,2,3 };
    arr[0] = 1;
    arr[0] = 10;
    return arr[0];
}
*/

/*

short _FDint(float *px, short xexp)
 {
 _Fval *ps = (_Fval *)(char *)px;
 unsigned short frac;
 short xchar = (ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7;

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  return ((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) == 0 && ps->_Sh[0] == 0
   ? 1 : 2);
 else if ((ps->_Sh[1] & ~((unsigned short)0x8000)) == 0 && ps->_Sh[0] == 0)
  return (0);
 xchar = (0x7e + 16 + 7 + 1) - xchar - xexp;
 if (xchar <= 0)
  return (0);
 else if ((16 + 7 + 1) <= xchar)
  {
  ps->_Sh[1] &= ((unsigned short)0x8000);
  ps->_Sh[0] = 0;
  solver_find();
  return ((-1));
  }
 else
  {
  static const unsigned short mask[] = {
   0x0000, 0x0001, 0x0003, 0x0007,
   0x000f, 0x001f, 0x003f, 0x007f,
   0x00ff, 0x01ff, 0x03ff, 0x07ff,
   0x0fff, 0x1fff, 0x3fff, 0x7fff};
  static const size_t sub[] = {0, 1};

  frac = mask[xchar & 0xf];
  xchar >>= 4;
  frac &= ps->_Sh[sub[xchar]];
  ps->_Sh[sub[xchar]] ^= frac;

  if (0 < xchar)
   frac |= ps->_Sh[0], ps->_Sh[0] = 0;

  return (frac != 0 ? (-1) : 0);
  }
 }

 */





