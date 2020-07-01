/*************************************************************************

  MODULE       : $RCSfile: transfer.h $
  PART OF      : CTC/LIBRARY Host-Target support
  VERSION      : $Revision: 1.8 $, $Date: 2016/10/14 07:05:42 $
  AUTHOR       : $Author: olavi $
  LAST EDITED  : 14.10.2016/op
  DESCRIPTION  : This header defines the internal protocol of coverage
                 data transfer.

                 This file should not be modified by the user.

                 Copyright (c) 1998-2013 Testwell Oy
                 Copyright (c) 2013-2016 Verifysoft Technology GmbH
**************************************************************************/

/* Revision history

   $Log: transfer.h $
   Revision 1.8  2016/10/14 07:05:42  olavi
   - just some last-edited and copyright marking fine-tunings

   Revision 1.7  2011/07/22 09:46:29  seppo
   - Corrected a typo in NUMBER_MAP: "...VW..." (was: "...WV...")
   - Added MODULE_MARKER for marking the start of module's coverage data
   - Added rule 'START_MODULE', and updated rules 'module' (added
     'START_MODULE'), 'size_array' (removed 'size_time', it's always 1),
     and 'data_arrays' (removed 'data_e', added 'data_maxtimer', and
     changed order, 'time' first)

   Revision 1.6  2009/09/24 07:14:55  seppo
   - Minor cosmetic changes (spacing and alignment)

   Revision 1.5  2003/11/26 08:32:50  seppo
   - Minor documentation fix for the 'array' rule

   Revision 1.4  2001/02/08 17:21:41  olavi
   Documentation fix: START/END_MARK --> START/END_SEQ

   Revision 1.3  2001/01/26 11:43:35  olavi
   NUMBER_MAP macro to one line (=> immune to DOS/Unix file formats...)

   Revision 1.2  2000/05/22 12:24:04  tino
   Data transfer protocol adjusted for usage with
   CTC++ 5.0 data structures.

   Revision 1.1  1998/02/24 21:01:32  pasi
   Initial revision

*/

/*************************************************************************

    The data is arranged as follows:

    data          ::= START_SEQ
                      modules
                      END_SEQ

    modules       ::= module
                    | modules module

    module        ::= START_MODULE size_array data_arrays

    START_MODULE  ::= MODULE_MARKER END_NUMBER

    size_array    ::= size_path size_datfile size_s size_j size_tf
                      size_c size_timer

    data_arrays   ::= time path datfile data_s data_j data_t data_f
                      data_c data_timer data_maxtimer

    -- time is a long number represented by a number array
       with length one
    -- path and datfile are represented as null-terminated
       strings, which are converted into number arrays
    -- size values are represented by a number
    -- data arrays are represented by number arrays

    array         ::= number
                    | array number

    number        ::= numberstr END_NUMBER

    numberstr     ::= numberset
                    | numberstr numberset

    numberset     ::= [0-9A-Za-z]

    START_SEQ     ::= "<START:"
    END_SEQ       ::= ">"
    END_NUMBER    ::= ','

    MODULE_MARKER ::= "MODUL"

**************************************************************************/



/************************* PUBLIC DEFINES ******************************/

/*
   - START_SEQ, END_SEQ: strings marking the start and end of coverage
     data transmission
   - END_NUMBER: string marking the end of a number
   - MODULE_MARKER: string marking the start of module's coverage data
   - NUMBER_MAP, NUMBER_BASE: character map for the number representation
     in the NUMBER_BASE base
   - MAX_CHARS: size of largest unsigned long given in NUMBER_BASE base,
     including the END_NUMBER string and '\0'
 */

#define START_SEQ     "<START:"
#define END_SEQ       ">"
#define END_NUMBER    ','

#define MODULE_MARKER 0x13B8632D   /* "MODUL" */

#define NUMBER_MAP    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

#define NUMBER_BASE   62

#define MAX_CHARS     20

/* EOF $RCSfile: transfer.h $ */
