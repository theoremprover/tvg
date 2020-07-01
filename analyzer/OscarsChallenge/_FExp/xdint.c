#include "defs.h"

/* OS: for better readability: original code = not precompiled (good to know that we have 100% in _Dint:-)


/ * _Dint function -- IEEE 754 version * /
#include "xmath.h"
_C_LIB_DECL

short _Dint(double *px, short xexp)
        {       / * test and drop (scaled) fraction bits * /
        _Dval *ps = (_Dval *)(char *)px;
        unsigned short frac;
        short xchar = (ps->_Sh[_D0] & _DMASK) >> _DOFF;

        if (xchar == _DMAX)
                return ((ps->_Sh[_D0] & _DFRAC) == 0 && ps->_Sh[_D1] == 0
                        && ps->_Sh[_D2] == 0 && ps->_Sh[_D3] == 0
                        ? _INFCODE : _NANCODE);
        else if ((ps->_Sh[_D0] & ~_DSIGN) == 0 && ps->_Sh[_D1] == 0
                && ps->_Sh[_D2] == 0 && ps->_Sh[_D3] == 0)
                return (0);
        xchar = (_DBIAS + 48 + _DOFF + 1) - xchar - xexp;
        if (xchar <= 0)
                return (0);     / * no frac bits to drop * /
        else if ((48 + _DOFF + 1) <= xchar)
                {       / * all frac bits * /
                ps->_Sh[_D0] &= _DSIGN;
                ps->_Sh[_D1] = 0;
                ps->_Sh[_D2] = 0;
                ps->_Sh[_D3] = 0;
                return (_FINITE);       / * report on frac, not result * /
                }
        else
                {       / * strip out frac bits * /
                _STATIC_CONST unsigned short mask[] = {
                        0x0000, 0x0001, 0x0003, 0x0007,
                        0x000f, 0x001f, 0x003f, 0x007f,
                        0x00ff, 0x01ff, 0x03ff, 0x07ff,
                        0x0fff, 0x1fff, 0x3fff, 0x7fff};
                _STATIC_CONST size_t sub[] = {_D3, _D2, _D1, _D0};
                frac = mask[xchar & 0xf];
                xchar >>= 4;
                frac &= ps->_Sh[sub[xchar]];
                ps->_Sh[sub[xchar]] ^= frac;
                switch (xchar)
                        {       / * cascade through! * /
                case 3:
                        frac |= ps->_Sh[_D1], ps->_Sh[_D1] = 0;
                case 2:
                        frac |= ps->_Sh[_D2], ps->_Sh[_D2] = 0;
                case 1:
                        frac |= ps->_Sh[_D3], ps->_Sh[_D3] = 0;
                        }
                return (frac != 0 ? _FINITE : 0);
                }
        }
_END_C_LIB_DECL
*/
short _Dint(double *px, short xexp)
 {
 _Dval *ps = (_Dval *)(char *)px;
 unsigned short frac;
 short xchar = (ps->_Sh[3] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 4) - 1))))) >> 4;
 if (xchar == ((unsigned short)((1 << (15 - 4)) - 1)))
  return ((ps->_Sh[3] & ((unsigned short)((1 << 4) - 1))) == 0 && ps->_Sh[2] == 0
   && ps->_Sh[1] == 0 && ps->_Sh[0] == 0
   ? 1 : 2);
 else if ((ps->_Sh[3] & ~((unsigned short)0x8000)) == 0 && ps->_Sh[2] == 0
  && ps->_Sh[1] == 0 && ps->_Sh[0] == 0)
  return (0);
 xchar = (0x3fe + 48 + 4 + 1) - xchar - xexp;
 if (xchar <= 0)
  return (0);
 else if ((48 + 4 + 1) <= xchar)
  {
  ps->_Sh[3] &= ((unsigned short)0x8000);
  ps->_Sh[2] = 0;
  ps->_Sh[1] = 0;
  ps->_Sh[0] = 0;
  return ((-1));
  }
 else
  {
  static const unsigned short mask[] = {
   0x0000, 0x0001, 0x0003, 0x0007,
   0x000f, 0x001f, 0x003f, 0x007f,
   0x00ff, 0x01ff, 0x03ff, 0x07ff,
   0x0fff, 0x1fff, 0x3fff, 0x7fff};
  static const size_t sub[] = {0, 1, 2, 3};
  frac = mask[xchar & 0xf];
  xchar >>= 4;
  frac &= ps->_Sh[sub[xchar]];
  ps->_Sh[sub[xchar]] ^= frac;
  switch (xchar)
   {
  case 3:
   frac |= ps->_Sh[2], ps->_Sh[2] = 0;
  case 2:
   frac |= ps->_Sh[1], ps->_Sh[1] = 0;
  case 1:
   frac |= ps->_Sh[0], ps->_Sh[0] = 0;
   }
  return (frac != 0 ? (-1) : 0);
  }
 }
