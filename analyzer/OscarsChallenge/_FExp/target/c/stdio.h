/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_STDIO_H_
#define _TARGET_UNITTEST_STDIO_H_

#include <target/c/predefines.h>
#include <target/c/stdint.h>
#include <target/c/ctype.h>
#include <target/c/ieee754/print.h>
#include <target/c/bigfloat/print.h>
#include <target/c/stdarg.h>


/** Determine the standard way of printing a character */
#ifndef TARGET_SILENT
	extern
	#ifdef __cplusplus
		"C"
	#endif
	int putchar( int );
	UTT_LINKAGE void utt_putchar( char c ){ putchar(c); }
#else
	UTT_LINKAGE void utt_putchar( char ){}
#endif
UTT_LINKAGE utt_size_t utt_putchar_buf( char c , char* buffer ){
	if( buffer )
		*buffer = c;
	return 1;
}


/** Function to print a string */
UTT_LINKAGE void utt_putstr( const char* str ){
	if( !str )
		return;
	while( *str )
		utt_putchar( *str++ );
}
UTT_LINKAGE utt_size_t utt_putstr_buf( const char* str , char* buffer ){
	if( !str )
		return buffer[0] = '\0';
	utt_size_t len = 0;
	while( *str )
		len += utt_putchar_buf( *str++ , buffer++ );
	*buffer = '\0';
	return len;
}


/** Function to print a (signed) integer */
UTT_LINKAGE char* utt_putint_ex( int value , unsigned int base , const char* alphabet )
{
	static char	memory[65];
	bool		sign;
	char*		result = &memory[sizeof(memory)];
	*--result = '\0';
	
	if( value == UTT_INT_MIN )
	{
		/* Special treatment for the maximum possible, negative value */
		*--result = alphabet[ -(UTT_INT_MIN + base) % base ];
		value /= base;
		value = -value;
		sign = true;
	}
	else{
		sign = value < 0;
		value = sign ? -value : value;
	}
	
	do{
		*--result = alphabet[ value % base ];
		value /= base;
	}
	while( value );
	
	if( sign )
		*--result = '-';
	
	return result;
}
UTT_LINKAGE void utt_putint( int value ){
	utt_putstr( utt_putint_ex( value , 10 , "0123456789" ) );
}
UTT_LINKAGE utt_size_t utt_putint_buf( int value , char* buffer ){
	return utt_putstr_buf( utt_putint_ex( value , 10 , "0123456789" ) , buffer );
}


/** Function to print a (signed) long long int */
UTT_LINKAGE char* utt_putlong_ex( long long int value , unsigned int base , const char* alphabet )
{
	static char	memory[129];
	bool		sign;
	char*		result = &memory[sizeof(memory)];
	*--result = '\0';
	
	if( value == UTT_LONGLONG_MIN )
	{
		/* Special treatment for the maximum possible, negative value */
		*--result = alphabet[ -(UTT_LONGLONG_MIN + base) % base ];
		value /= base;
		value = -value;
		sign = true;
	}
	else{
		sign = value < 0;
		value = sign ? -value : value;
	}
	
	do{
		*--result = alphabet[ value % base ];
		value /= base;
	}
	while( value );
	
	if( sign )
		*--result = '-';
	
	return result;
}
UTT_LINKAGE void utt_putlong( long long int value ){
	utt_putstr( utt_putlong_ex( value , 10 , "0123456789" ) );
}
UTT_LINKAGE utt_size_t utt_putlong_buf( long long int value , char* buffer ){
	return utt_putstr_buf( utt_putlong_ex( value , 10 , "0123456789" ) , buffer );
}

/** Function to print an unsigned integer */
UTT_LINKAGE char* utt_putuint_ex( unsigned int value , unsigned int base , const char* alphabet ){
	static char	memory[65];
	char*		result = &memory[sizeof(memory)];
	*--result = '\0';
	do{
		*--result = alphabet[ value % base ] , value /= base;
	}
	while( value );
	return result;
}
UTT_LINKAGE void utt_putuint( unsigned int value ){
	utt_putstr( utt_putuint_ex( value , 10 , "0123456789" ) );
}
UTT_LINKAGE utt_size_t utt_putuint_buf(  unsigned int value , char* buffer ){
	return utt_putstr_buf( utt_putuint_ex( value , 10 , "0123456789" ) , buffer );
}


/** Function to print an unsigned long long */
UTT_LINKAGE char* utt_putulong_ex( unsigned long long value , unsigned int base , const char* alphabet ){
	static char	memory[129];
	char*		result = &memory[sizeof(memory)];
	*--result = '\0';
	do{
		*--result = alphabet[ value % base ] , value /= base;
	}
	while( value );
	return result;
}
UTT_LINKAGE void utt_putulong( unsigned long long value ){
	utt_putstr( utt_putulong_ex( value , 10 , "0123456789" ) );
}
UTT_LINKAGE utt_size_t utt_putulong_buf( unsigned long long value , char* buffer ){
	return utt_putstr_buf( utt_putulong_ex( value , 10 , "0123456789" ) , buffer );
}


/** Print the result of a test case */
UTT_LINKAGE void utt_print_tc_result( bool result , int test_case_number , const char* test_case_id ){
	if( result )
		return;
	utt_print( "Test Case #%d (%s) failed!\n" , test_case_number , test_case_id );
}


/** Print marker */
UTT_LINKAGE void utt_putcesura(){
	utt_putchar( 6 );
}


/** Print boolean */
UTT_LINKAGE void utt_putbool( bool val ){
	utt_putstr( val ? "true" : "false" );
}
UTT_LINKAGE utt_size_t utt_putbool_buf( bool val , char* buffer ){
	return utt_putstr_buf( val ? "true" : "false" , buffer );
}

/** Print a bigint */
UTT_LINKAGE char* utt_putbigint_ex( utt_bigint_t* value , unsigned int base , const char* alphabet )
{
	static char		memory[200];
	utt_uint8_t		shift;
	
	if( utt_bi_is_zero( value ) )
		return "0";
	
	/* Base 10? */
	if( base == 10 )
	{
		/* Find the greatest power of '10' that is smaller than 'value' */
		char*			iterator = &memory[0];
		utt_bigint_t	copy;
		utt_bi_assign( &copy , value );
		utt_uint32_t	power_base = 0;
		utt_bigint_t	divisor;
		
		do{
			++power_base;
			utt_bi_pow10( &divisor , power_base );
		}while( utt_bi_compare( &divisor , &copy ) <= 0 );
		
		/* Output digits */
		while( true )
		{
			/* Decrease the current power of '10' by '1' */
			power_base--;
			
			/* Determine how much '1' is for the current digit */
			utt_bi_pow10( &divisor , power_base );
			
			/* Subtract this value as many times as is possible */
			utt_uint8_t	digit = 0;
			while( !utt_bi_sub( &copy , &divisor ) )
				digit++;
			
			*iterator++ = alphabet[ digit ];
			
			/* Did we reach 0? */
			if( utt_bi_is_zero(&copy) )
				break;
			
			utt_bi_sub( &divisor , &copy );
			utt_bi_assign( &copy , &divisor );
		}
		
		/* Add trailing zeros */
		while( power_base-- > 0 )
			*iterator++ = alphabet[0];
		
		*iterator++ = '\0'; /* Add trailing '0' */
		return &memory[0];
	}
	/* Base a power of 2? */
	else if( base == ( 1 << ( shift = utt_log2_32(base) ) ) )
	{
		/* Output digits from low to high, since we can actually divide (shift right)! */
		char*			result = &memory[sizeof(memory)];
		*--result = '\0'; /* Add trailing '0' */
		utt_bigint_t	copy;
		utt_bi_assign( &copy , value );
		
		do{
			*--result = alphabet[ copy.blocks[0] & ( base - 1 ) ];
			utt_bi_shr( &copy , shift );
		}while( !utt_bi_is_zero( &copy ) );
		
		return result;
	}
	
	/* Generic base outputting */
	
	/* Find the greatest power of 'base' that is smaller than 'value' */
	char*			iterator = &memory[0];
	utt_bigint_t	copy;
	utt_bi_assign( &copy , value );
	utt_uint32_t	power_base = 0;
	utt_bigint_t	divisor;
	
	do{
		++power_base;
		utt_bi_set_32( &divisor , 1 );
		utt_uint32_t i = 0;
		while( i++ < power_base )
			utt_bi_times( &divisor , base );
	}while( utt_bi_compare( &divisor , &copy ) <= 0 );
	
	/* Output digits */
	while( true )
	{
		/* Decrease the current power of 'base' by '1' */
		power_base--;
		
		/* Determine how much '1' is for the current digit */
		utt_bi_set_32( &divisor , 1 );
		utt_uint32_t i = 0;
		while( i++ < power_base )
			utt_bi_times( &divisor , base );
		
		/* Subtract this value as many times as is possible */
		utt_uint8_t	digit = 0;
		while( !utt_bi_sub( &copy , &divisor ) )
			digit++;
		
		*iterator++ = alphabet[ digit ];
		
		/* Did we reach 0? */
		if( utt_bi_is_zero(&copy) )
			break;
		
		utt_bi_sub( &divisor , &copy );
		utt_bi_assign( &copy , &divisor );
	}
	
	/* Add trailing zeros */
	while( power_base-- > 0 )
		*iterator++ = alphabet[0];
	
	*iterator++ = '\0'; /* Add trailing '0' */
	return &memory[0];
}
UTT_LINKAGE void utt_putbigint( utt_bigint_t* value ){
	utt_putstr( utt_putbigint_ex( value , 10 , "0123456789" ) );
}
UTT_LINKAGE utt_size_t utt_putbigint_buf( utt_bigint_t* value , char* buffer ){
	return utt_putstr_buf( utt_putbigint_ex( value , 10 , "0123456789" ) , buffer );
}


/**
 * Generic formatting facility, printf-like.
 * 
 * Not supported Features:
 * - %a					Hexadecimal floating point printing
 * - %n					Returns the number of characters written so far
 *
 * Additional Features:
 * - %b					Prints a bool as either 'true' or 'false'
 * - %.9s				Prints at maximum 9 characters of 'str'. Put in any number as replacement for '9'
 * - %#.9s				Prints at maximum 9 characters of 'str'. If 'str' is longer than 9 characters,
 *						an ellipsis is written, replacing the last three of the '9' letters.
 * - %#c				Prints a char, but escapes control codes
 * - %hf, %hg, %he		Prints an IEEE 754 half float
 * - %#hf, %#hg, %#he	Prints a VFP alternative half float
 * - %jf, %jg, %je		Prints a utt_bigfloat_t
 * - %Ld, %Lo, %Lx, %Lu	Prints a utt_bigint_t >>THROUGH A POINTER TO THE PARAMETER<<
 */
#define PRINT_BUFFER_SIZE 256
UTT_LINKAGE void utt_vprint( const char* fmt , utt_va_list args )
{
	if( !fmt )
		return;
	
	bool			in_special_seq = false;
	unsigned int	type_spec = 2; /* hh = 0 ; h = 1 ; (none) = 2 ; l = 3 ; ll = 4 ; j = 5 ; z = 6 ; t = 7 ; L = 8 */
	bool			left_justified = false;
	unsigned char	force_sign = 0; /* 0 = omit a potential '+'; 1 = add a potential '+' ; 2 = replace potential '+' with a space */
	bool			alt_repr = false;
	bool			pad_with_zeros = false;
	int				field_width = 0;
	int				precision = UTT_INT_MIN;
	bool			precision_sign = false;
	bool			field_width_read = false;
	bool			precision_read = false;
	static char		buffer[PRINT_BUFFER_SIZE];
	unsigned int	data_width = 0;
	
	while( *fmt )
	{
		/* Conversion Sequence Identifier */
		if( *fmt == '%' ){
			if( in_special_seq ){
				utt_putchar('%');
				in_special_seq = false;
			}
			else
				in_special_seq = true;
		}
		
		/* If in conversion Sequence */
		else if( in_special_seq )
		{
			/* Conversion Flags */
			if( *fmt == '-' && !field_width_read && !precision_read )
				left_justified = true;
			if( *fmt == '-' && precision_read && !precision_sign && precision == UTT_INT_MIN )
				precision_sign = true;
			else if( *fmt == '+' && !field_width_read && !precision_read )
				force_sign = true;
			else if( *fmt == '0' && !field_width_read && !precision_read )
				pad_with_zeros = true;
			else if( *fmt == '#' && !field_width_read && !precision_read && !alt_repr )
				alt_repr = true;
			else if( *fmt >= '0' && *fmt <= '9' ){
				int value = *fmt - '0';
				if( precision_read ){
					if( precision < 0 && !precision_sign )
						precision = 0;
					precision = precision * 10 + ( precision_sign ? -value : value );
				}
				else{
					field_width = field_width * 10 + ( *fmt - '0' );
					field_width_read = true;
				}
			}
			else if( *fmt == '.' && !precision_read )
				precision_read = true;
			else if( *fmt == '*' && !field_width_read && !precision_read ){
				field_width = utt_va_arg( args , int );
				field_width_read = true;
				if( field_width < 0 ){
					field_width = -field_width;
					left_justified = true;
				}
			}
			else if( *fmt == '*' && precision_read ){
				precision = utt_va_arg( args , int );
				precision_read = true;
			}
			
			/* Conversion Specifiers */
			else if( *fmt == 'd' || *fmt == 'i' ){
				#define DO_PRINT( gen_fn , type ) \
					data_width = gen_fn( utt_va_arg( args , type ) , buffer )
				switch( type_spec ){
					case 0: DO_PRINT( utt_putint_buf , int /* promoted from signed char */ ); break;
					case 1: DO_PRINT( utt_putint_buf , int /* promoted from short */ ); break;
					case 2: DO_PRINT( utt_putint_buf , int ); break;
					case 3: DO_PRINT( utt_putlong_buf , long ); break;
					case 4: DO_PRINT( utt_putlong_buf , long long ); break;
					case 5: DO_PRINT( utt_putlong_buf , utt_intmax_t ); break;
					case 6: DO_PRINT( utt_putlong_buf , utt_intptr_t ); break;
					case 7: DO_PRINT( utt_putlong_buf , utt_ptrdiff_t ); break;
					case 8: DO_PRINT( utt_putbigint_buf , utt_bigint_t* ); break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				/* If precision and value is 0, output nothing */
				if( precision == 0 && buffer[0] == '0' && data_width == 1 ){
					buffer[0] = '\0';
					data_width = 0;
				}
				goto clear;
			}
			else if( *fmt == 'o' ){
				#define DO_PRINT( gen_fn , type ) \
					data_width = utt_putstr_buf( gen_fn( utt_va_arg( args , type ) , 8 , "01234567" ) , buffer )
				switch( type_spec ){
					case 0: DO_PRINT( utt_putuint_ex , int /* promoted from signed char */ ); break;
					case 1: DO_PRINT( utt_putuint_ex , int /* promoted from short */ ); break;
					case 2: DO_PRINT( utt_putuint_ex , int ); break;
					case 3: DO_PRINT( utt_putulong_ex , long ); break;
					case 4: DO_PRINT( utt_putulong_ex , long long ); break;
					case 5: DO_PRINT( utt_putulong_ex , utt_intmax_t ); break;
					case 6: DO_PRINT( utt_putulong_ex , utt_intptr_t ); break;
					case 7: DO_PRINT( utt_putulong_ex , utt_ptrdiff_t ); break;
					case 8: DO_PRINT( utt_putbigint_ex , utt_bigint_t* ); break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				/* If precision and value is 0, output nothing */
				if( precision == 0 && buffer[0] == '0' && data_width == 1 ){
					buffer[0] = '\0';
					data_width = 0;
				}
				goto clear;
			}
			else if( *fmt == 'x' ){
				#define DO_PRINT( gen_fn , type ) \
					data_width = utt_putstr_buf( gen_fn( utt_va_arg( args , type ) , 16 , "0123456789abcdef" ) , buffer )
				switch( type_spec ){
					case 0: DO_PRINT( utt_putuint_ex , int /* promoted from signed char */ ); break;
					case 1: DO_PRINT( utt_putuint_ex , int /* promoted from short */ ); break;
					case 2: DO_PRINT( utt_putuint_ex , int ); break;
					case 3: DO_PRINT( utt_putulong_ex , long ); break;
					case 4: DO_PRINT( utt_putulong_ex , long long ); break;
					case 5: DO_PRINT( utt_putulong_ex , utt_intmax_t ); break;
					case 6: DO_PRINT( utt_putulong_ex , utt_intptr_t ); break;
					case 7: DO_PRINT( utt_putulong_ex , utt_ptrdiff_t ); break;
					case 8: DO_PRINT( utt_putbigint_ex , utt_bigint_t* ); break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				/* If precision and value is 0, output nothing */
				if( precision == 0 && buffer[0] == '0' && data_width == 1 ){
					buffer[0] = '\0';
					data_width = 0;
				}
				goto clear;
			}
			else if( *fmt == 'X' ){
				#define DO_PRINT( gen_fn , type ) \
					data_width = utt_putstr_buf( gen_fn( utt_va_arg( args , type ) , 16 , "0123456789ABCDEF" ) , buffer )
				switch( type_spec ){
					case 0: DO_PRINT( utt_putuint_ex , unsigned int /* promoted from signed char */ ); break;
					case 1: DO_PRINT( utt_putuint_ex , unsigned int /* promoted from short */ ); break;
					case 2: DO_PRINT( utt_putuint_ex , unsigned int ); break;
					case 3: DO_PRINT( utt_putulong_ex , unsigned long ); break;
					case 4: DO_PRINT( utt_putulong_ex , unsigned long long ); break;
					case 5: DO_PRINT( utt_putulong_ex , utt_uintmax_t ); break;
					case 6: DO_PRINT( utt_putulong_ex , utt_size_t ); break;
					case 7: DO_PRINT( utt_putulong_ex , utt_uintptr_t /* unsigned ptrdiff_t */ ); break;
					case 8: DO_PRINT( utt_putbigint_ex , utt_bigint_t* ); break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				/* If precision and value is 0, output nothing */
				if( precision == 0 && buffer[0] == '0' && data_width == 1 ){
					buffer[0] = '\0';
					data_width = 0;
				}
				goto clear;
			}
			else if( *fmt == 'u' ){
				#define DO_PRINT( gen_fn , type ) \
					data_width = utt_putstr_buf( gen_fn( utt_va_arg( args , type ) , 10 , "0123456789" ) , buffer )
				switch( type_spec ){
					case 0: DO_PRINT( utt_putuint_ex , unsigned int /* promoted from signed char */ ); break;
					case 1: DO_PRINT( utt_putuint_ex , unsigned int /* promoted from short */ ); break;
					case 2: DO_PRINT( utt_putuint_ex , unsigned int ); break;
					case 3: DO_PRINT( utt_putulong_ex , unsigned long ); break;
					case 4: DO_PRINT( utt_putulong_ex , unsigned long long ); break;
					case 5: DO_PRINT( utt_putulong_ex , utt_uintmax_t ); break;
					case 6: DO_PRINT( utt_putulong_ex , utt_size_t ); break;
					case 7: DO_PRINT( utt_putulong_ex , utt_uintptr_t /* unsigned ptrdiff_t */ ); break;
					case 8: DO_PRINT( utt_putbigint_ex , utt_bigint_t* ); break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				/* If precision and value is 0, output nothing */
				if( precision == 0 && buffer[0] == '0' && data_width == 1 ){
					buffer[0] = '\0';
					data_width = 0;
				}
				goto clear;
			}
			else if( *fmt == 'f' ){
				if( !precision_read )
					precision = 6;
				#define DO_PRINT( gen_fn , act_type , prom_type ) \
					data_width = gen_fn( buffer , PRINT_BUFFER_SIZE , (act_type)( utt_va_arg( args , prom_type ) ) , UTT_FLOAT_FMT_POSITIONAL , precision , ".0123456789abcdefghijklmnopqrstuvwxyz" )
				switch( type_spec ){
					case 0:
						if( alt_repr ){
							DO_PRINT( utt_flt_printh2 , TARGET_IEEE_754_HALF_ALT_PRM_CAST , TARGET_IEEE_754_HALF_ALT_PRM_TYPE ); break;
						}
						else{
							DO_PRINT( utt_flt_printh , TARGET_IEEE_754_HALF_PRM_CAST , TARGET_IEEE_754_HALF_PRM_TYPE ); break;
						}
					case 1: DO_PRINT( utt_flt_printf , TARGET_IEEE_754_FLOAT_PRM_CAST , TARGET_IEEE_754_FLOAT_PRM_TYPE ); break;
					case 2: DO_PRINT( utt_flt_print , TARGET_IEEE_754_DOUBLE_PRM_CAST , TARGET_IEEE_754_DOUBLE_PRM_TYPE ); break;
					/* Big float support */
					case 5:
						data_width = utt_flt_printm( buffer , PRINT_BUFFER_SIZE , utt_va_arg( args , utt_bigfloat_t ) , UTT_FLOAT_FMT_POSITIONAL , precision , ".0123456789abcdefghijklmnopqrstuvwxyz" );
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				goto clear;
			}
			else if( *fmt == 'F' ){
				if( !precision_read )
					precision = 6;
				#define DO_PRINT( gen_fn , act_type , prom_type ) \
					data_width = gen_fn( buffer , PRINT_BUFFER_SIZE , (act_type)( utt_va_arg( args , prom_type ) ) , UTT_FLOAT_FMT_POSITIONAL , precision , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
				switch( type_spec ){
					case 0:
						if( alt_repr ){
							DO_PRINT( utt_flt_printh2 , TARGET_IEEE_754_HALF_ALT_PRM_CAST , TARGET_IEEE_754_HALF_ALT_PRM_TYPE ); break;
						}
						else{
							DO_PRINT( utt_flt_printh , TARGET_IEEE_754_HALF_PRM_CAST , TARGET_IEEE_754_HALF_PRM_TYPE ); break;
						}
					case 1: DO_PRINT( utt_flt_printf , TARGET_IEEE_754_FLOAT_PRM_CAST , TARGET_IEEE_754_FLOAT_PRM_TYPE ); break;
					case 2: DO_PRINT( utt_flt_print , TARGET_IEEE_754_DOUBLE_PRM_CAST , TARGET_IEEE_754_DOUBLE_PRM_TYPE ); break;
					/* Big float support */
					case 5:
						data_width = utt_flt_printm( buffer , PRINT_BUFFER_SIZE , utt_va_arg( args , utt_bigfloat_t ) , UTT_FLOAT_FMT_POSITIONAL , precision , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" );
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				goto clear;
			}
			else if( *fmt == 'e' ){
				if( !precision_read )
					precision = 6;
				#define DO_PRINT( gen_fn , act_type , prom_type ) \
					data_width = gen_fn( buffer , PRINT_BUFFER_SIZE , (act_type)( utt_va_arg( args , prom_type ) ) , UTT_FLOAT_FMT_SCIENTIFIC , precision , ".0123456789abcdefghijklmnopqrstuvwxyz" )
				switch( type_spec ){
					case 0:
						if( alt_repr ){
							DO_PRINT( utt_flt_printh2 , TARGET_IEEE_754_HALF_ALT_PRM_CAST , TARGET_IEEE_754_HALF_ALT_PRM_TYPE ); break;
						}
						else{
							DO_PRINT( utt_flt_printh , TARGET_IEEE_754_HALF_PRM_CAST , TARGET_IEEE_754_HALF_PRM_TYPE ); break;
						}
					case 1: DO_PRINT( utt_flt_printf , TARGET_IEEE_754_FLOAT_PRM_CAST , TARGET_IEEE_754_FLOAT_PRM_TYPE ); break;
					case 2: DO_PRINT( utt_flt_print , TARGET_IEEE_754_DOUBLE_PRM_CAST , TARGET_IEEE_754_DOUBLE_PRM_TYPE ); break;
					/* Big float support */
					case 5:
						data_width = utt_flt_printm( buffer , PRINT_BUFFER_SIZE , utt_va_arg( args , utt_bigfloat_t ) , UTT_FLOAT_FMT_SCIENTIFIC , precision , ".0123456789abcdefghijklmnopqrstuvwxyz" );
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				goto clear;
			}
			else if( *fmt == 'E' ){
				if( !precision_read )
					precision = 6;
				#define DO_PRINT( gen_fn , act_type , prom_type ) \
					data_width = gen_fn( buffer , PRINT_BUFFER_SIZE , (act_type)( utt_va_arg( args , prom_type ) ) , UTT_FLOAT_FMT_SCIENTIFIC , precision , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
				switch( type_spec ){
					case 0:
						if( alt_repr ){
							DO_PRINT( utt_flt_printh2 , TARGET_IEEE_754_HALF_ALT_PRM_CAST , TARGET_IEEE_754_HALF_ALT_PRM_TYPE ); break;
						}
						else{
							DO_PRINT( utt_flt_printh , TARGET_IEEE_754_HALF_PRM_CAST , TARGET_IEEE_754_HALF_PRM_TYPE ); break;
						}
					case 1: DO_PRINT( utt_flt_printf , TARGET_IEEE_754_FLOAT_PRM_CAST , TARGET_IEEE_754_FLOAT_PRM_TYPE ); break;
					case 2: DO_PRINT( utt_flt_print , TARGET_IEEE_754_DOUBLE_PRM_CAST , TARGET_IEEE_754_DOUBLE_PRM_TYPE ); break;
					/* Big float support */
					case 5:
						data_width = utt_flt_printm( buffer , PRINT_BUFFER_SIZE , utt_va_arg( args , utt_bigfloat_t ) , UTT_FLOAT_FMT_SCIENTIFIC , precision , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" );
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				goto clear;
			}
			else if( *fmt == 'g' ){
				if( !precision_read )
					precision = 6;
				#define DO_PRINT( gen_fn , act_type , prom_type ) \
					data_width = gen_fn( buffer , PRINT_BUFFER_SIZE , (act_type)( utt_va_arg( args , prom_type ) ) , UTT_FLOAT_FMT_MIXED , precision , ".0123456789abcdefghijklmnopqrstuvwxyz" )
				switch( type_spec ){
					case 0:
						if( alt_repr ){
							DO_PRINT( utt_flt_printh2 , TARGET_IEEE_754_HALF_ALT_PRM_CAST , TARGET_IEEE_754_HALF_ALT_PRM_TYPE ); break;
						}
						else{
							DO_PRINT( utt_flt_printh , TARGET_IEEE_754_HALF_PRM_CAST , TARGET_IEEE_754_HALF_PRM_TYPE ); break;
						}
					case 1: DO_PRINT( utt_flt_printf , TARGET_IEEE_754_FLOAT_PRM_CAST , TARGET_IEEE_754_FLOAT_PRM_TYPE ); break;
					case 2: DO_PRINT( utt_flt_print , TARGET_IEEE_754_DOUBLE_PRM_CAST , TARGET_IEEE_754_DOUBLE_PRM_TYPE ); break;
					/* Big float support */
					case 5:
						data_width = utt_flt_printm( buffer , PRINT_BUFFER_SIZE , utt_va_arg( args , utt_bigfloat_t ) , UTT_FLOAT_FMT_MIXED , precision , ".0123456789abcdefghijklmnopqrstuvwxyz" );
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				goto clear;
			}
			else if( *fmt == 'G' ){
				if( !precision_read )
					precision = 6;
				#define DO_PRINT( gen_fn , act_type , prom_type ) \
					data_width = gen_fn( buffer , PRINT_BUFFER_SIZE , (act_type)( utt_va_arg( args , prom_type ) ) , UTT_FLOAT_FMT_MIXED , precision , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" )
				switch( type_spec ){
					case 0:
						if( alt_repr ){
							DO_PRINT( utt_flt_printh2 , TARGET_IEEE_754_HALF_ALT_PRM_CAST , TARGET_IEEE_754_HALF_ALT_PRM_TYPE ); break;
						}
						else{
							DO_PRINT( utt_flt_printh , TARGET_IEEE_754_HALF_PRM_CAST , TARGET_IEEE_754_HALF_PRM_TYPE ); break;
						}
					case 1: DO_PRINT( utt_flt_printf , TARGET_IEEE_754_FLOAT_PRM_CAST , TARGET_IEEE_754_FLOAT_PRM_TYPE ); break;
					case 2: DO_PRINT( utt_flt_print , TARGET_IEEE_754_DOUBLE_PRM_CAST , TARGET_IEEE_754_DOUBLE_PRM_TYPE ); break;
					/* Big float support */
					case 5:
						data_width = utt_flt_printm( buffer , PRINT_BUFFER_SIZE , utt_va_arg( args , utt_bigfloat_t ) , UTT_FLOAT_FMT_MIXED , precision , ".0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" );
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				#undef DO_PRINT
				goto clear;
			}
			else if( *fmt == 'c' ){
				switch( type_spec ){
					case 2:{
						unsigned char value = utt_va_arg( args , int );
						if( alt_repr && !utt_isprint(value) ){
							switch( value ){
								case '\a':	data_width = utt_putstr_buf( "\\a" , buffer ); break;
								case '\b':	data_width = utt_putstr_buf( "\\b" , buffer ); break;
								case '\f':	data_width = utt_putstr_buf( "\\f" , buffer ); break;
								case '\n':	data_width = utt_putstr_buf( "\\n" , buffer ); break;
								case '\r':	data_width = utt_putstr_buf( "\\r" , buffer ); break;
								case '\t':	data_width = utt_putstr_buf( "\\t" , buffer ); break;
								case '\v':	data_width = utt_putstr_buf( "\\v" , buffer ); break;
								case '\\':	data_width = utt_putstr_buf( "\\\\" , buffer ); break;
								case '\'':	data_width = utt_putstr_buf( "\\'" , buffer ); break;
								case '\"':	data_width = utt_putstr_buf( "\\\"" , buffer ); break;
								default:{
									/* Default to octal constant */
									char* dest = buffer;
									*dest++ = '\\';
									if( value < 64 ){
										if( value < 8 )
											*dest++ = '0';
										*dest++ = '0';
									}
									data_width = utt_putstr_buf( utt_putuint_ex( value , 8 , "01234567" ) , dest ) + ( dest - buffer );
									buffer[data_width] = '\0';
									break;
								}
							}
						}
						else{
							data_width = utt_putchar_buf( value , buffer );
							buffer[data_width] = '\0';
						}
						break;
					}
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				goto clear;
			}
			else if( *fmt == 's' ){
				switch( type_spec ){
					case 2:
						/* If the precision is set, output at maximum  */
						if( precision_read ){
							const char* value = utt_va_arg( args , const char* );
							if( value )
								for( ; *value ; buffer[data_width++] = *value++ ) {
									if( alt_repr ){
										if( (int)data_width >= precision - 3 && value[1] && value[2] && value[3] ){ /* Value larger than precision minus '...' */
											buffer[data_width++] = '.';
											buffer[data_width++] = '.';
											buffer[data_width++] = '.';
											break;
										}
									}
									else if( (int)data_width >= precision )
										break;
								}
							buffer[data_width] = '\0';
						}
						else
							data_width = utt_putstr_buf( utt_va_arg( args , const char* ) , buffer );
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				goto clear;
			}
			else if( *fmt == 'p' ){
				switch( type_spec ){
					case 2:
						buffer[data_width++] = '0';
						buffer[data_width++] = 'x';
						data_width += utt_putstr_buf(
							utt_putulong_ex(
								(utt_intptr_t) utt_va_arg( args , const void* )
								, 16
								, "0123456789abcdef"
							)
							, buffer + data_width
						);
						break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				goto clear;
			}
			else if( *fmt == 'b' ){
				switch( type_spec ){
					case 2:	data_width = utt_putstr_buf( utt_va_arg( args , int /* promoted from bool */ ) ? "true" : "false" , buffer ); break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
				goto clear;
			}
			
			/* Argument Size Specifiers */
			else if( *fmt == 'h' )
				switch( type_spec ){
					case 1: type_spec = 0; break;
					case 2: type_spec = 1; break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
			else if( *fmt == 'l' )
				switch( type_spec ){
					case 2: type_spec = 3; break;
					case 3: type_spec = 4; break;
					default: utt_putstr( "<unsupported type spec>" ); break;
				}
			else if( *fmt == 'j' && in_special_seq && type_spec == 2 )
				type_spec = 5;
			else if( *fmt == 'z' && in_special_seq && type_spec == 2 )
				type_spec = 6;
			else if( *fmt == 't' && in_special_seq && type_spec == 2 )
				type_spec = 7;
			else if( *fmt == 'L' && in_special_seq && type_spec == 2 )
				type_spec = 8;
			
			/* Unsupported conversion sequence */
			else{
				utt_putstr( "<unsupported syntax at '" );
				utt_putchar( *fmt );
				utt_putstr( "'>" );
				goto clear;
			}
		}
		
		/* Normal Character */
		else
			utt_putchar( *fmt );
		
		++fmt;
		continue;
			
	clear:
			
		if( data_width > 0 )
		{
			if( force_sign && buffer[0] != '-' ){
				utt_putchar( force_sign == 1 ? '+' : ' ' );
				data_width++;
			}
			
			/* Pad on the left? */
			if( !left_justified )
				for( int cur = field_width - data_width ; cur > 0 ; --cur )
					utt_putchar( pad_with_zeros ? '0' : ' ' );
			
			/* Output the field */
			utt_putstr( buffer );
			
			/* Pad on the right? */
			if( left_justified )
				for( int cur = field_width - data_width ; cur > 0 ; --cur )
					utt_putchar( pad_with_zeros ? '0' : ' ' );
			
			buffer[0] = '\0';
			data_width = 0; /* Reset buffer size */
		}
		
		fmt++;
		in_special_seq = false;
		precision_read = false;
		precision_sign = false;
		field_width_read = false;
		pad_with_zeros = false;
		left_justified = false;
		alt_repr = false;
		force_sign = 0;
		type_spec = 2;
		precision = UTT_INT_MIN;
		field_width = 0;
	}
	
	/* Yet unsupported, remove warning about used but never read variables */
	(void)alt_repr;
}

UTT_LINKAGE void utt_print( const char* fmt , ... ){
	utt_va_list args;
	utt_va_start( args , fmt );
	utt_vprint( fmt , args );
	utt_va_end( args );
}

#endif
