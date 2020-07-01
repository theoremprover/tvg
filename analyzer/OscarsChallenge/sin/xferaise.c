#include "defs.h"
void (_Feraise)(int except)
 {

 int errh = (1 | 2);

 if ((errh & 2) != 0)
  {
  if ((except & (0x04 | 0x08)) != 0)
   except |= 0x10;
  feraiseexcept(except);
  }

 if ((errh & 1) == 0)
  ;
 else if ((except & 0x01) != 0)
  ( _Errno) = 0x0021;
 else if ((except & (0x02 | 0x08 | 0x04)) != 0)
  ( _Errno) = 0x0022;







 }
