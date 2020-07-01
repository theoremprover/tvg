/*************************************************************************

  MODULE       : $RCSfile: targdata.h $
  PART OF      : CTC/LIBRARY Host-Target support
  VERSION      : $Revision: 1.7 $, $Date: 2016/10/14 08:31:47 $
  AUTHOR       : $Author: olavi $
  LAST EDITED  : 14.10.2016/op
  DESCRIPTION  : The API that the instrumented programs use for CTC++
                 runtime (also) in HOTA case is defined in the general
                 ctc.h. This header declares some function prototypes that
                 are needed in the techncial arrangement of HOTA. It has
                 lived over years (initially the targcust.c and targsend.c
                 code was in targdata.c, but moved to user-modifiable
                 separate files)

                 This file should not be modified by the user.

                 Copyright (c) 1998-2013 Testwell Oy
                 Copyright (c) 2013-2016 Verifysoft Technology GmbH
**************************************************************************/

/* Revision history

   $Log: targdata.h $
   Revision 1.7  2016/10/14 08:31:47  olavi
   - completed/clarified some comments

   Revision 1.6  2007/05/17 14:08:50  olavi
   - added some clarifying header commenting to the file

   Revision 1.5  2005/06/07 05:51:06  seppo
   - Added #include for "ctctypes.h" so that including it is no longer
     needed before #include "targdata.h"

   Revision 1.4  2003/11/26 16:36:02  seppo
   - Added declarations of functions ctc_alloc, ctc_exit and ctc_show_error

   Revision 1.3  2000/05/22 12:22:08  tino
   CTC++ 5.0 updates for targetside declarations.

   Revision 1.2  1998/03/02 19:16:50  pasi
   - Updated comment on ctc_init_monitors()

   Revision 1.1  1998/02/24 21:08:48  pasi
   Initial revision

*/


#include "ctctypes.h"

/*************************************************************************

   FUNCTION  : ctc_send_data
   PURPOSE   : This is the function, which ctc_append_all() calls to
               transmit the coverage data from the target to the host.
               The implementation of this function needs to be written
               by the user using the other functions declared in this
               file. See readme.txt for further information.
   COMMENT   : As this function is now moved to other file (targsend.c),
               a prototype on this need to be intrioduced.
   ARGUMENTS : None
   RETURNS   : Nothing

 *************************************************************************/

void ctc_send_data(void);


/*************************************************************************

   FUNCTION  : ctc_prepare_upload
   PURPOSE   : Prepares the coverage data for transfer
   COMMENT   : ctc_send_data() call this
   ARGUMENTS : None
   RETURNS   : Nothing

 *************************************************************************/

void ctc_prepare_upload(void);


/*************************************************************************

   FUNCTION  : ctc_get_character
   PURPOSE   : Serializes the coverage data into a character stream.
               (The counters are initialized to zero by ctc_get_character()
               after each counter is sent, so that the same coverage data
               is not sent multiple times.)
   COMMENT   : ctc_send_data() call this
   ARGUMENTS : None
   RETURNS   : unsigned character or -1 when there is no more data

 *************************************************************************/

int ctc_get_character(void);


/*************************************************************************

   FUNCTION  : ctc_alloc
   PURPOSE   : Allocates block of memory from heap, i.e. a local 'malloc'
               implementation.
               The default implementation of this function just returns 0.
               The implementation of this function needs to be written by
               the user if coverage data areas are allocated dynamically
               at run time.
               NOTE This function never gets called if the coverage data
               areas are allocated statically inside the instrumented
               files themselves. Dynamic allocation is accomplished by
               defining the macro CTC_HEAP_COUNTERS at instrumentation
               time. The default is static allocation.
   COMMENT   : As this function is now moved to other file (targcust.c),
               a prototype on this needed to be introduced.
               
               See readme.txt for further information.

   ARGUMENTS : - size: bytes to allocate
   RETURNS   : a void pointer to the allocated space, or 0 if there is
               insufficient memory available

 *************************************************************************/

void* ctc_alloc(unsigned int size);


/*************************************************************************

   FUNCTION  : ctc_exit
   PURPOSE   : This function terminates the instrumented program, i.e.
               a local 'exit' implementation.
               This function is called if a fatal error is encountered
               or the user has ordered the program to end by inserting
               #pragma CTC QUIT in the source file.
               The default implementation of this function just loops
               infinitely because the instrumented program must not try
               to continue after a fatal error.
               The implementation of this function may be written by the
               user if there is available some proper way to terminate
               the program. See readme.txt for further information.
   COMMENT   : As this function is now moved to other file (targsend.c),
               a prototype on this needed to be introduced.
   ARGUMENTS : - status : exit status
   RETURNS   : Does not return

 *************************************************************************/

void ctc_exit(int status);


/*************************************************************************

   FUNCTION  : ctc_show_error
   PURPOSE   : Shows an error message.
               The default implementation of this function does nothing.
               The implementation of this function may be written by
               the user if there is available some proper way to report
               errors. See readme.txt for further information.
   COMMENT   : As this function is now moved to other file (targsend.c),
               a prototype on this needed to be introduced.
   ARGUMENTS : - status_code : error code according to which a message
                 can be selected
   RETURNS   : Nothing

 *************************************************************************/

void ctc_show_error(CTC_STATUS status_code);

/* EOF $RCSfile: targdata.h $ */
