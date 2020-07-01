#include "defs.h"

static const float p[] = {
 0.2988439998F,
 0.3997655209F,
 0.6666679125F,
 };

static const float loge2hi = (float)(5814539.0 / 8388608.0);
static const float loge2lo = 1.1730463525082348212145817656807550e-7F;

static const float logx2hi = (float)(2525222.0 / 8388608.0);
static const float logx2lo = 7.5085978265526238894724493026768190e-8F;

static const float logxehi = (float)(3643126.0 / 8388608.0);
static const float logxelo = 1.9699272335463628918916605082294398e-8F;
static const float logxe = 0.43429448190325182765112891891660507F;

static const float log2ehi = (float)(12102203.0 / 8388608.0);
static const float log2elo = 1.9259629911266174681001892137426646e-8F;
static const float log2e = 1.4426950408889634073599246810018922F;

static const float rthalf = 0.70710678118654752440084436210484905F;

float _FLog(float x, int baseflag)
 {
 short xexp;

 switch (_FDunscale(&xexp, &x))
  {
 case 2:
  return (x);

 case 0:
  _Feraise(0x02);
  return (-_FInf._Float);

 case 1:
  if (!((*_FPmsw(&(x))) & ((unsigned short)0x8000)))
   return (x);

 default:
  if (((*_FPmsw(&(x))) & ((unsigned short)0x8000)))
   {
   _Feraise(0x01);
   return (_FNan._Float);
   }
  else
   {
   float x1, w, z;

   if (x < rthalf)
    {
    x += x;
    --xexp;
    }
   x1 = x - 1.0F;
   z = ((x1) / (x + 1.0F));
   w = z * z;
   z *= x1 - w * ((p[0] * w + p[1]) * w + p[2]);

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

float _FLogpoly(float w)
 {
 return (((p[0] * w + p[1]) * w + p[2]));
 }


