/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_STDARG_H_
#define _TARGET_UNITTEST_STDARG_H_

/** Define va_arg, va_start and va_end */
#ifndef TARGET_NO_STDARG
#include <stdarg.h> /* If <stdarg.h> is not resolvable, pass -DTARGET_NO_STDARG */
#endif
#ifndef va_start
	#if defined(__GNUC__) || defined(__clang__)/** Use GCC builtins */
		typedef __builtin_va_list utt_va_list;
		#define utt_va_start(list, param) __builtin_va_start(list, param)
		#define utt_va_arg(list, type) __builtin_va_arg(list, type)
		#define utt_va_end(list) __builtin_va_end(list)
		#define utt_va_copy(dest, src) __builtin_va_copy(dest, src)
	#else
		typedef void* utt_va_list;
		#define utt_va_start(list, param) (list = ((void*)&(param)) + ( ( sizeof(param) + sizeof(int) - 1 ) / sizeof(int) ) * sizeof(int) )
		#define utt_va_arg(list, type) *(type*)( ( list += sizeof(type) ) - sizeof(type) )
		#define utt_va_end(list) (list = (void*)0)
		#define utt_va_copy(dest, src) (dest = src)
	#endif
#else
	typedef va_list utt_va_list;
	#define utt_va_start(list, param) va_start(list,param)
	#define utt_va_arg(list, type) va_arg(list, type)
	#define utt_va_end(list) va_end(list)
	#ifdef va_copy
		#define utt_va_copy(dest, src) va_copy(dest, src)
	#else
		#define utt_va_copy(dest, src) (dest = src)
	#endif
#endif

#endif