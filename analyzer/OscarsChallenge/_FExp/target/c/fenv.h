/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_FENV_H_
#define _TARGET_UNITTEST_FENV_H_

#include <target/c/predefines.h>

#ifndef TARGET_NO_FENV
	#include <fenv.h> /* If <fenv.h> is not resolvable, pass -DTARGET_NO_FENV */
	#define UTT_FP_DIVBYZERO	FE_DIVBYZERO
	#define UTT_FP_INEXACT		FE_INEXACT
	#define UTT_FP_INVALID		FE_INVALID
	#define UTT_FP_OVERFLOW		FE_OVERFLOW
	#define UTT_FP_UNDERFLOW	FE_UNDERFLOW
	#define UTT_FP_ALL_EXCEPT	FE_ALL_EXCEPT
	#define UTT_FP_DOWNWARD		FE_DOWNWARD
	#define UTT_FP_TONEAREST	FE_TONEAREST
	#define UTT_FP_TOWARDZERO	FE_TOWARDZERO
	#define UTT_FP_UPWARD		FE_UPWARD
	#define UTT_FP_DFL_ENV		FE_DFL_ENV
	typedef fenv_t		utt_fp_state_t;
	typedef fexcept_t	utt_fp_except_t;
	typedef int			utt_fp_exception_info_t;
	UTT_LINKAGE int utt_fp_testexcept( int excepts ){ return fetestexcept( excepts ); }
	UTT_LINKAGE int utt_fp_clearexcept( int excepts ){ return feclearexcept( excepts ); }
	UTT_LINKAGE int utt_fp_raiseexcept( int excepts ){ return feraiseexcept( excepts ); }
	UTT_LINKAGE int utt_fp_getexceptflag( utt_fp_except_t* flagp , int excepts ){ return fegetexceptflag( flagp , excepts ); }
	UTT_LINKAGE int utt_fp_setexceptflag( const utt_fp_except_t* flagp , int excepts ){ return fesetexceptflag( flagp , excepts ); }
	UTT_LINKAGE int utt_fp_setround( int round ){ return fesetround( round ); }
	UTT_LINKAGE int utt_fp_getround(){ return fegetround(); }
	UTT_LINKAGE int utt_fp_updateenv( const utt_fp_state_t* envp ){ return feupdateenv( envp ); }
	UTT_LINKAGE int utt_fp_getenv( utt_fp_state_t* envp ){ return fegetenv( envp ); }
	UTT_LINKAGE int utt_fp_setenv( const utt_fp_state_t* envp ){ return fesetenv( envp ); }
	UTT_LINKAGE int utt_fp_holdexcept( utt_fp_state_t* envp ){ return feholdexcept( envp ); }
#else
	typedef struct utt_fp_state_t{}		utt_fp_state_t;
	typedef struct utt_fp_except_t{}	utt_fp_except_t;
	typedef int							utt_fp_exception_info_t;
	#define UTT_FP_DIVBYZERO 1
	#define UTT_FP_INEXACT 2
	#define UTT_FP_INVALID 4
	#define UTT_FP_OVERFLOW 8
	#define UTT_FP_UNDERFLOW 16
	#define UTT_FP_ALL_EXCEPT UTT_FP_DIVBYZERO | UTT_FP_INEXACT | UTT_FP_INVALID | UTT_FP_OVERFLOW | UTT_FP_UNDERFLOW
	#define UTT_FP_DOWNWARD 1
	#define UTT_FP_TONEAREST 2
	#define UTT_FP_TOWARDZERO 3
	#define UTT_FP_UPWARD 4
	#define UTT_FP_DFL_ENV ((const utt_fp_state_t*)NULL)
	UTT_LINKAGE int utt_fp_testexcept( int excepts ){ (void)excepts; return 0; }
	UTT_LINKAGE int utt_fp_clearexcept( int excepts ){ (void)excepts; return -1; }
	UTT_LINKAGE int utt_fp_raiseexcept( int excepts ){ (void)excepts; return -1; }
	UTT_LINKAGE int utt_fp_getexceptflag( utt_fp_except_t* flagp , int excepts ){ (void)flagp, (void)excepts; return -1; }
	UTT_LINKAGE int utt_fp_setexceptflag( const utt_fp_except_t* flagp , int excepts ){ (void)flagp, (void)excepts; return -1; }
	UTT_LINKAGE int utt_fp_setround( int round ){ (void)round; return -1; }
	UTT_LINKAGE int utt_fp_getround(){ return -1; }
	UTT_LINKAGE int utt_fp_updateenv( const utt_fp_state_t* envp ){ (void)envp; return -1; }
	UTT_LINKAGE int utt_fp_getenv( utt_fp_state_t* envp ){ (void)envp; return -1; }
	UTT_LINKAGE int utt_fp_setenv( const utt_fp_state_t* envp ){ (void)envp; return -1; }
	UTT_LINKAGE int utt_fp_holdexcept( utt_fp_state_t* envp ){ (void)envp; return -1; }
#endif

UTT_LINKAGE void utt_fp_enter( utt_fp_state_t* state ){
	utt_fp_holdexcept( state ); /* Enter safe state */
}

UTT_LINKAGE utt_fp_exception_info_t utt_fp_leave( utt_fp_state_t* state )
{
	/* Save all exceptions */
	utt_fp_exception_info_t status = utt_fp_testexcept( UTT_FP_ALL_EXCEPT );
	
	/* Clear all exceptions */
	utt_fp_clearexcept( UTT_FP_ALL_EXCEPT );
	
	/* Restore the floating point environment */
	utt_fp_updateenv( state );
	
	return status;
}

#endif
