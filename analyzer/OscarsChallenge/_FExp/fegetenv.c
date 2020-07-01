#include "defs.h"
int (fegetenv)(fenv_t *penv)
 {
	//OS: since this is just to get the current exceptions it is safe to set to 0 (no further impact, except writing back)
	*penv=0;
    //  __asm__("vmrs %0, fpscr" : "=r" (*penv));
 return (0);
 }
