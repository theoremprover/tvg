#include "tconfig.h"
#include "coretypes.h"
#include "tm.h"
#include "fp-bit.h"

const fp_number_type __thenan_sf = { CLASS_SNAN, 0, 0, {(fractype) 0} };

static int
iszero ( fp_number_type *  x)
{
  return x->class == CLASS_ZERO;
}


static fp_number_type *
nan (void)
{
  return (fp_number_type *) (& __thenan_sf);
}

fp_number_type* f(fp_number_type *  a)
{
  if (iszero (a))
	return nan ();
   else
   return a;
}
