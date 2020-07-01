/*************************************************************************

  MODULE       : $RCSfile: ctctypes.h $
  PART OF      : CTC/LIBRARY Host-Target support
  VERSION      : $Revision: 1.12 $, $Date: 2017/04/28 08:18:18 $
  AUTHOR       : $Author: olavi $
  LAST EDITED  : 28.04.2017/op
  DESCRIPTION  : This header defines the generally used types and macros
                 of CTC++ library

                 This file should not be modified by the user.

                 Copyright (c) 1998-2013 Testwell Oy
                 Copyright (c) 2013-2017 Verifysoft Technology GmbH
**************************************************************************/

/* Revision history

   $Log: ctctypes.h $
   Revision 1.12  2017/04/28 08:18:18  olavi
   - made the types CTC_LONG and CTC_ULONG still user-overriddable in the
     same way as in CTC++ v8.2 ctc.h. These types are used in struct
     CTC_MODULE_INFO and in some api function parameters, of which the
     instrumentation (ctc.h) and HOTA side must have compatible view.

   Revision 1.11  2017/03/28 13:29:40  olavi
   - enforced integral types CTC_COUNTER etc. that are used in data
     structures to be 32-bit in case the used compiler had type 'long'
     already 64-bit.

   Revision 1.10  2016/10/14 07:05:40  olavi
   - just some last-edited and copyright marking fine-tunings

   Revision 1.9  2015/11/17 09:59:29  seppo
   - Added 'const' qualifier to members source_path and dat_file of
     struct CTC_MODULE_INFO
   - Replaced "include guard" CTCTYPES_H by _CTCTYPES_H

   Revision 1.8  2013/11/27 07:48:35  seppo
   - MAX_COUNTER and MAX_TIMER are now defined using literals.
     Furthermore, they are now guarded by #if !defined(...)

   Revision 1.7  2013/10/15 06:33:57  seppo
   - Added bad_testcase_name to the CTC_STATUS enum type
     (as in the CTC++ v7.2 run-time library)
   - Added #define MAX_TESTCASE_NAME (max length for testcase names)

   Revision 1.6  2011/07/22 10:00:56  seppo
   - In struct CTC_MODULE_INFO, added new members 'ctc_maxtime'
     (max timers), and 'rewrite_count' and 'update_count'
     (module data's rewrite count and update count). Removed
     'ctc_e' (end counters). Renamed 'num_t' to 'num_time'.
     "start counters" -> "function counters"
   - The macro definitions of CTC_COUNTER and CTC_TIMER are now
     "guarded" by '#if !defined(...)'. This makes it easy to define
     them in some other way on the command line, if needed.
   - Added macros CTC_CWRAP_START and CTC_CWRAP_END
     (extern "C" wrappers)
   - Removed macro MAX_COUNTER_LIMIT
   - Updated the declaration of the CTC_STATUS type: the status
     codes are now at the level of the CTC++ v7.0 run-time library

   Revision 1.5  2009/09/23 05:24:21  seppo
   - Added definition of macro MAX_COUNTER_LIMIT
   - Updated the declaration of the CTC_STATUS type: the status codes
     are now at the level of the CTC++ v6.5.5 run-time library

   Revision 1.4  2007/05/17 14:10:33  olavi
   - updated in typedef enum {...} CTC_STATUS; the status codes to the same
     as in ctcdefs.h (v6.3) at host run-time library (err num encodings).
   - changed the time_stamp type from 'unsigned long' to 'long'

   Revision 1.3  2003/11/28 11:13:08  seppo
   - New datatype CTC_STATUS added (status codes are at the level of
     CTC++ version 5.2 run-time library)
   - Removed the const qualifier from source_path and dat_file in
     the struct CTC_MODULE_INFO
   - Documentation fix: path of the source file is NOT absolute
   - Defined CTCTYPES_H to make sure that the contents of this file
     are included only once

   Revision 1.2  2000/05/22 11:51:44  tino
   New datatypes added and obsolete ones removed.

   Revision 1.1  1998/02/24 21:02:06  pasi
   Initial revision
*/


#if !defined(_CTCTYPES_H)
#define _CTCTYPES_H

/************************* PUBLIC DEFINES ******************************/

/*
 * MANIFEST CONSTANTS:
 * - MAX_COUNTER: maximum value of a counter variable
 * - MAX_TIMER: maximum value of a timer variable
 * - MAX_TESTCASE_NAME: maximum length of a test case name
 */

#if !defined(MAX_COUNTER)
#  define MAX_COUNTER      4294967295UL
#endif

#if !defined(MAX_TIMER)
#  define MAX_TIMER        2147483647L
#endif

#define MAX_TESTCASE_NAME  32

/*
   CTC_CWRAP_START/END: extern "C" wrappers for C++ compilation
*/

#if !defined(CTC_CWRAP_START)
#  ifdef __cplusplus
#    define CTC_CWRAP_START extern "C" {
#  else
#    define CTC_CWRAP_START
#  endif
#endif
#if !defined(CTC_CWRAP_END)
#  ifdef __cplusplus
#    define CTC_CWRAP_END   }
#  else
#    define CTC_CWRAP_END
#  endif
#endif

/*************************************************************************
   Type definitions
   - CTC_COUNTER: type of the counter value, 32-bit
   - CTC_TIMER: type of the timer value, 32-bit
   - CTC_LONG: like 'long' but enforced to 32-bit if compiler has it
               already 64-bit
   - CTC_ULONG: like 'unsigned long' but ensured to be 32-bit
   - CTC_MODULE_INFO: type of the module information structure
   - CTC_STATUS : status codes
**************************************************************************/

#if !defined(CTC_COUNTER)
#  if defined(__SIZEOF_LONG__) && __SIZEOF_LONG__ > 4
     typedef unsigned int CTC_COUNTER;
#  else
     typedef unsigned long CTC_COUNTER;
#  endif
#endif

#if !defined(CTC_TIMER)
#  if defined(__HP_aCC) || defined(__HP_cc) || \
      defined(__hpux) || defined(__hpux__)
   /* HPUX aC++/cc/g++/gcc */
     typedef unsigned int CTC_TIMER;
#  else
#    if defined(__SIZEOF_LONG__) && __SIZEOF_LONG__ > 4
       typedef int CTC_TIMER;
#    else
       typedef long CTC_TIMER;
#    endif
#  endif
#endif

#if !defined(CTC_LONG)
#  if defined(__SIZEOF_LONG__) && __SIZEOF_LONG__ > 4
     typedef int CTC_LONG;
#  else			      
     typedef long CTC_LONG;
#  endif
#endif

#if !defined(CTC_ULONG)
#  if defined(__SIZEOF_LONG__) && __SIZEOF_LONG__ > 4
     typedef unsigned int CTC_ULONG;
#  else			      
     typedef unsigned long CTC_ULONG;
#  endif
#endif

typedef struct CTC_MODULE_INFO {
    /* Path of the source file */
    const char *source_path;

    /* Path of the datafile (usually absolute) */
    const char *dat_file;

    /* Time of the instrumentation */
    CTC_LONG time_stamp;

    /* Rewrite count and update count of module data */
    unsigned int rewrite_count;
    unsigned int update_count;

    /* Number of function, jump, true/false, multicondition and
       timer/maxtimer counters */
    CTC_ULONG num_s, num_j, num_tf, num_c, num_time;

    /* Pointers to counter arrays (function, jump, true, false,
       multicondition, timer and maxtimer) */
    CTC_COUNTER *ctc_s, *ctc_j, *ctc_t, *ctc_f, *ctc_c;
    CTC_TIMER   *ctc_time, *ctc_maxtime;

    /* Pointer to the next registered module */
    struct CTC_MODULE_INFO* next;
} CTC_MODULE_INFO;

typedef enum
{
    ok,
    bad_number_format,
    bad_stamp,
    bad_configuration_parameter,
    close_error,
    create_error,
    licence_error,
    missing_configuration_parameter,
    open_error,
    out_of_memory,
    read_error,
    write_error,
    lock_error,
    unlock_error,
    bad_environment_variable,
    corrupted_module,
    corrupted_datafile,
    bad_testcase_name
} CTC_STATUS;

#endif /* _CTCTYPES_H */

/* EOF $RCSfile: ctctypes.h $ */
