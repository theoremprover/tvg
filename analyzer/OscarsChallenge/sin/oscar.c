#include "defs.h"
#include "xdtest.c"
#include "xferaise.c"
#include "xquad.c"
#include "xvalues.c"
#include "xdunscal.c"
#include "xdscale.c"
#include "xdint.c"
#include "xprec.c"
#include "xdnorm.c"
#include "errno.c"
#include "memcpy.c"

//gcc xsin.c xdtest.c xferaise.c xquad.c xvalues.c xdunscal.c xdscale.c xdint.c xprec.c xdnorm.c errno.c memcpy.c

/*
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
*/
static const double
    c0 = 0.00000000206374484196,
    c1 = -0.00000027555365134677,
    c2 = 0.00002480157946764225,
    c3 = -0.00138888888730525966,
    c4 = 0.04166666666651986722,
    c5 = -0.49999999999999547304;

static const double
    s0 = 0.00000000015893606014,
    s1 = -0.00000002505069049138,
    s2 = 0.00000275573131527032,
    s3 = -0.00019841269827816117,
    s4 = 0.00833333333331908278,
    s5 = -0.16666666666666612594;

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
    x = 1.0 + w * (((((c0 * w + c1) * w + c2) * w + c3) * w + c4) * w + c5);
   else
    x += x * w * (((((s0 * w + s1) * w + s2) * w + s3) * w + s4) * w + s5);
   }
  if (qoff & 0x2)
   ((*_Pmsw(&(x))) ^= ((unsigned short)0x8000));
  return (x);
  }
 }
