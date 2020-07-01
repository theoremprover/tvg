#include "defs.h"

static const double c[] = {
 0.00000000206374484196,
 -0.00000027555365134677,
 0.00002480157946764225,
 -0.00138888888730525966,
 0.04166666666651986722,
 -0.49999999999999547304,
 };
static const double s[] = {
 0.00000000015893606014,
 -0.00000002505069049138,
 0.00000275573131527032,
 -0.00019841269827816117,
 0.00833333333331908278,
 -0.16666666666666612594,
 };
double _Sinx(double x, unsigned int qoff, int quads)
 {
 switch (_Dtest(&x))
  {
 case 2:
  return (x);
 case 0:
  if ((qoff & 0x1) != 0)
   x = 1.0;
  return ((qoff & 0x2) != 0 ? -x : x);
 case 1:
  _Feraise(0x01);
  return (_Nan._Double);
 default:
  qoff += _Quad(&x, quads);
  if (-_Rteps._Double < x && x < _Rteps._Double)
   {
   if ((qoff & 0x1) != 0)
    x = 1.0;
   }
  else
   {
   double w = x * x;
   if ((qoff & 0x1) != 0)
    x = 1.0 + w * (((((c[0] * w + c[1]) * w + c[2]) * w + c[3]) * w + c[4]) * w + c[5]);
   else
    x += x * w * (((((s[0] * w + s[1]) * w + s[2]) * w + s[3]) * w + s[4]) * w + s[5]);
   }
  if (qoff & 0x2)
   ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
  return (x);
  }
 }
/* OS: dead code
double _Sincos(double x, double *pcos)
 {
 switch (_Dtest(&x))
  {
 case 2:
  *pcos = x;
  return (x);
 case 0:
  *pcos = 1.0;
  return (x);
 case 1:
  *pcos = _Nan._Double;
  _Feraise(0x01);
  return (_Nan._Double);
 default:
   {
  unsigned int qoff = _Quad(&x, 0);
  if (-_Rteps._Double < x && x < _Rteps._Double)
   if ((qoff & 0x1) == 0)
    *pcos = 1.0;
   else
    {
    *pcos = x;
    x = 1.0;
    }
  else
   {
   double w = x * x;
   if ((qoff & 0x1) != 0)
    x = 1.0 + w * (((((c[0] * w + c[1]) * w + c[2]) * w + c[3]) * w + c[4]) * w + c[5]);
   else
    x += x * w * (((((s[0] * w + s[1]) * w + s[2]) * w + s[3]) * w + s[4]) * w + s[5]);
   }
  if (((qoff + 1) & 0x2) != 0)
   *pcos = -*pcos;
  if (qoff & 0x2)
   ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
  return (x);
   }
  }
 }
double _Sin(double x, unsigned int qoff)
 {
 return (_Sinx(x, qoff, 0));
 }
*/
