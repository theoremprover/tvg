#include "defs.h"

static const double p[] = {
 0.14821919759901168116,
 0.15310988435285788981,
 0.18183706393900951476,
 0.22222195289721309521,
 0.28571428781619503202,
 0.39999999999190497071,
 0.66666666666667803987,
 };
static const double loge2hi = (double)(5814539.0 / 8388608.0);
static const double loge2lo = 1.1730463525082348212145817656807550e-7;

static const double logx2hi = (double)(2525222.0 / 8388608.0);
static const double logx2lo = 7.5085978265526238894724493026768190e-8;

static const double logxehi = (double)(3643126.0 / 8388608.0);
static const double logxelo = 1.9699272335463628918916605082294398e-8;
static const double logxe = 0.43429448190325182765112891891660507;

static const double log2ehi = (double)(12102203.0 / 8388608.0);
static const double log2elo = 1.9259629911266174681001892137426646e-8;
static const double log2e = 1.4426950408889634073599246810018922;

static const double rthalf = 0.70710678118654752440084436210484905;

double _Log(double x, int baseflag)
 {
 short xexp;

 switch (_Dunscale(&xexp, &x))
  {
 case 2:
  return (x);

 case 0:
  _Feraise(0x02);
  return (-_Inf._Double);

 case 1:
  if (!((*_Pmsw(&(x))) & ((unsigned short)0x8000)))
   return (x);

 default:
  if (((*_Pmsw(&(x))) & ((unsigned short)0x8000)))
   {
   _Feraise(0x01);
   return (_Nan._Double);
   }
  else
   {
   double x1, w, z;

   if (x < rthalf)
    {
    x += x;
    --xexp;
    }
   x1 = x - 1.0;
   z = ((x1) / (x + 1.0));
   w = z * z;
   z *= x1 - w * ((((((p[0] * w + p[1]) * w + p[2]) * w + p[3]) * w + p[4]) * w + p[5]) * w + p[6]);

   w = xexp;
   if (baseflag == 0)
    z = x1 - (z - loge2lo * w)
     + loge2hi * w;
   else if (0 < baseflag)
    z = logxehi * x1
     + (logxelo * x1 - (logxe * z - logx2lo * w))
      + logx2hi * w;
   else
    z = log2ehi * x1
     + (log2elo * x1 - log2e * z)
      + w;
   return (z);
   }
  }
 }

double _Logpoly(double w)
 {
 return (((((((p[0] * w + p[1]) * w + p[2]) * w + p[3]) * w + p[4]) * w + p[5]) * w + p[6]));
 }

