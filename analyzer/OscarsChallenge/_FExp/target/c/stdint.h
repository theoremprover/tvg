/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_STDINT_H_
#define _TARGET_UNITTEST_STDINT_H_

/** Define fixed-size 8 bit int */
#ifndef __UINT8_TYPE__
	#ifndef __INT8_TYPE__
		#define __INT8_TYPE__ char
	#endif
	#define __UINT8_TYPE__ unsigned __INT8_TYPE__
#endif

/** Define fixed-size 16 bit int */
#ifndef __INT16_TYPE__
	#define __INT16_TYPE__ short
#endif
#ifndef __UINT16_TYPE__
	#define __UINT16_TYPE__ unsigned __INT16_TYPE__
#endif

/** Define fixed-size 32 bit int */
#ifndef __UINT32_TYPE__
	#ifndef __INT32_TYPE__
		#ifdef __SIZEOF_INT__
			#if __SIZEOF_INT__ == 2
				#define __INT32_TYPE__ long
			#else
				#define __INT32_TYPE__ int
			#endif
		#elif defined(_MSC_VER)
			#define __INT32_TYPE__ __int32
		#else
			#define __INT32_TYPE__ int
		#endif
	#endif
#endif
#ifndef __UINT32_TYPE__
#define __UINT32_TYPE__ unsigned __INT32_TYPE__
#endif

/** Define fixed-size 64 bit int */
#ifndef __UINT64_TYPE__
	#ifndef __INT64_TYPE__
		#ifdef __SIZEOF_LONG__
			#if __SIZEOF_LONG__ == 4
				#define __INT64_TYPE__ long long
			#else
				#define __INT64_TYPE__ long
			#endif
		#elif defined(_MSC_VER)
			#define __INT64_TYPE__ __int64
		#else
			#define __INT64_TYPE__ long long
		#endif
	#endif
	#ifndef __UINT64_TYPE__
		#define __UINT64_TYPE__ unsigned __INT64_TYPE__
	#endif
#endif
#ifndef __UINT64_TYPE__
	#define __UINT64_TYPE__ unsigned __INT64_TYPE__
#endif

/** Define difference type for pointers */
#ifndef __PTRDIFF_TYPE__
	#ifdef __SIZEOF_PTRDIFF_T__
		#if __SIZEOF_PTRDIFF_T__ == 1
			#define __WINT_TYPE__ char
		#elif __SIZEOF_PTRDIFF_T__ == 2
			#define __WINT_TYPE__ __INT16_TYPE__
		#elif __SIZEOF_PTRDIFF_T__ == 4
			#define __WINT_TYPE__ __INT32_TYPE__
		#elif __SIZEOF_PTRDIFF_T__ == 8
			#define __WINT_TYPE__ __INT64_TYPE__
		#endif
	#elif defined(_MSC_VER)
		#define __WINT_TYPE__ __int64
	#else
		#define __PTRDIFF_TYPE__ long int
	#endif
#endif


/** Define fixed-size pointer-capable integer */
#ifndef __INTPTR_TYPE__
	#define __INTPTR_TYPE__ __PTRDIFF_TYPE__
#endif
#ifndef __UINTPTR_TYPE__
	#define __UINTPTR_TYPE__ unsigned __INTPTR_TYPE__
#endif

/** Define size type */
#ifndef __SIZE__TYPE__
	#define __SIZE__TYPE__ __UINTPTR_TYPE__
#endif

/** Define wide int type */
#ifndef __WINT_TYPE__
	#ifdef __SIZEOF_WINT_T__
		#if __SIZEOF_WINT_T__ == 1
			#define __WINT_TYPE__ char
		#elif __SIZEOF_WINT_T__ == 2
			#define __WINT_TYPE__ __INT16_TYPE__
		#elif __SIZEOF_WINT_T__ == 4
			#define __WINT_TYPE__ __INT32_TYPE__
		#elif __SIZEOF_WINT_T__ == 8
			#define __WINT_TYPE__ __INT64_TYPE__
		#endif
	#elif defined(_MSC_VER)
		#define __WINT_TYPE__ __int16
	#else
		#define __WINT_TYPE__ int
	#endif
#endif

/** Provide Convenient typedefs */
typedef __INT8_TYPE__		utt_int8_t;
typedef __UINT8_TYPE__		utt_uint8_t;
typedef __INT16_TYPE__		utt_int16_t;
typedef __UINT16_TYPE__		utt_uint16_t;
typedef __INT32_TYPE__		utt_int32_t;
typedef __UINT32_TYPE__		utt_uint32_t;
typedef __INT64_TYPE__		utt_int64_t;
typedef __UINT64_TYPE__		utt_uint64_t;
typedef __INTPTR_TYPE__		utt_intptr_t;
typedef __UINTPTR_TYPE__	utt_uintptr_t;
typedef __PTRDIFF_TYPE__	utt_ptrdiff_t;
typedef __SIZE__TYPE__		utt_size_t;
typedef __WINT_TYPE__		utt_wint_t;
typedef long long			utt_intmax_t;
typedef unsigned long long	utt_uintmax_t;


/** Define Maximum Values of DataTypes */
#define UTT_ULONGLONG_MAX ((unsigned long long)-1)
#define UTT_ULONG_MAX ((unsigned long)-1)
#define UTT_UINT_MAX ((unsigned int)-1)
#define UTT_USHORT_MAX ((unsigned short)-1)
#define UTT_SIZET_MAX ((utt_size_t)-1)
#define UTT_UINTMAX_MAX ((utt_uintmax_t)-1)

#define UTT_LONGLONG_MAX ((long long)(UTT_ULONGLONG_MAX>>1))
#define UTT_LONG_MAX ((long)(UTT_ULONG_MAX>>1))
#define UTT_INT_MAX ((int)(UTT_UINT_MAX>>1))
#define UTT_SHORT_MAX ((short)(UTT_USHORT_MAX>>1))
#define UTT_INTMAX_MAX ((utt_intmax_t)(UTT_UINTMAX_MAX>>1))

/** Define Minimum Values of DataTypes */
#define UTT_ULONGLONG_MIN ((unsigned long long)0)
#define UTT_ULONG_MIN ((unsigned long)0)
#define UTT_UINT_MIN ((unsigned int)0)
#define UTT_USHORT_MIN ((unsigned short)0)
#define UTT_SIZET_MIN ((utt_size_t)0)
#define UTT_UINTMAX_MIN ((utt_uintmax_t)0)

#define UTT_LONGLONG_MIN ((long long)(-UTT_LONGLONG_MAX-1))
#define UTT_LONG_MIN ((long)(-UTT_LONG_MAX-1))
#define UTT_INT_MIN ((int)(-UTT_INT_MAX-1))
#define UTT_SHORT_MIN ((short)(-UTT_SHORT_MAX-1))
#define UTT_INTMAX_MIN ((utt_intmax_t)(-UTT_INTMAX_MAX-1))

#endif
