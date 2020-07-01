/**
 * Copyright (C) 2017-2019 Jakob Riedle <riedle@validas.de>, Validas AG
 *
 * This file is part of the ForeC++ Target Framework.
 *
 * All Rights reserved. Unauthorized copying of this file via any medium
 * is strictly prohibited. Proprietary and confidential.
 */

#ifndef _TARGET_UNITTEST_PROPERTY_COVERAGE_H_
#define _TARGET_UNITTEST_PROPERTY_COVERAGE_H_

#include <target/c/c_upgrade.h>
#include <target/c/stdint.h>

#define TARGET_MAX_TRACE_LEN				1024
#define TARGET_MAX_NUM_PROPERTY_VALUES		32

/** Type of a property value */
typedef enum utt_pr_value_t{
	UTT_PR_VALUE_REGULAR = -9876543
	, UTT_PR_VALUE_THROWING = UTT_PR_VALUE_REGULAR + 1
} utt_pr_value_t;

/** Definition of a property */
typedef struct utt_property_t{
	const char*		name;
	utt_pr_value_t	values[TARGET_MAX_NUM_PROPERTY_VALUES];
	utt_size_t		default_value;
	utt_size_t		num_values;
} utt_property_t;


typedef struct utt_pr_trace_entry_t{
	utt_property_t*		property;
	utt_size_t			value;
}utt_pr_trace_entry_t;

typedef enum utt_pr_trace_cmd_t{
	UTT_PR_TRACE_CMD_RESET
	, UTT_PR_TRACE_CMD_ACCESS
	, UTT_PR_TRACE_CMD_COMPARE
	, UTT_PR_TRACE_CMD_FINISH
	, UTT_PR_TRACE_CMD_IS_COVERED
	, UTT_PR_TRACE_CMD_SET
	, UTT_PR_TRACE_CMD_START
	, UTT_PR_TRACE_CMD_GET
}utt_pr_trace_cmd_t;

typedef union utt_pr_trace_cmd_data_t{
	struct{
		utt_property_t*			property;
	}access;
	struct{
		utt_pr_trace_entry_t*	trace;
		utt_size_t				trace_len;
	}compare;
	struct{
		utt_pr_trace_entry_t*	trace;
		utt_size_t				trace_len;
	}set;
	struct{
		utt_pr_trace_entry_t**	trace;
		utt_size_t*				trace_len;
	}get;
}utt_pr_trace_cmd_data_t;


/**
 * Function for the administration of the global property trace
 */
UTT_LINKAGE utt_size_t utt_pr_trace( utt_pr_trace_cmd_t cmd , utt_pr_trace_cmd_data_t data )
{
	static utt_pr_trace_entry_t		trace[TARGET_MAX_TRACE_LEN];
	static utt_size_t				trace_len = 0;
	static utt_size_t				num_accesses = 0;
	static bool						executing = false;
	
	switch( cmd )
	{
	case UTT_PR_TRACE_CMD_RESET:
		utt_memset( trace , 0 , sizeof(trace) );
		trace_len = 0;
		num_accesses = 0;
		break;
		
	case UTT_PR_TRACE_CMD_SET:
		utt_memcpy( trace , data.set.trace , sizeof(utt_pr_trace_entry_t) * data.set.trace_len );
		data.set.trace_len = 0;
		break;
	
	case UTT_PR_TRACE_CMD_GET:
		*data.get.trace = trace;
		*data.get.trace_len = trace_len;
		break;
		
	case UTT_PR_TRACE_CMD_COMPARE:
		if( data.compare.trace_len != trace_len )
			return false;
		for( utt_size_t i = 0 ; i < trace_len ; i++ ){
			if( data.compare.trace[i].property != trace[i].property )
				return false;
			else if( data.compare.trace[i].value != trace[i].value )
				return false;
		}
		return true;
		
	case UTT_PR_TRACE_CMD_ACCESS:
		if( !executing )
			return data.access.property->default_value;
		if( num_accesses < trace_len ) /* Do we have a value prepared for this property access? */
		{
			utt_pr_trace_entry_t nth_access = trace[num_accesses++];
			
			/* Make sure, the accessed property is the same as before */
			if( nth_access.property != data.access.property )
				utt_print(
					"utt_pr_trace: Non-Reproducible series of calls to properties! Expected call to '%s', got '%s'."
					, nth_access.property->name
					, data.access.property->name
				);
			
			return nth_access.value;
		}
		if( !data.access.property->num_values ){ /* Does the property actually have values? */
			utt_print( "utt_pr_trace: Property '%s' cannot have a value." , data.access.property->name );
			return data.access.property->default_value;
		}
		utt_assert( trace_len < TARGET_MAX_TRACE_LEN );
		trace[trace_len++] = (utt_pr_trace_entry_t){ data.access.property , 0 }; /* Add the property value to the trace */
		num_accesses++;
		return 0;
		
	case UTT_PR_TRACE_CMD_START:
		if( executing )
			return 0;
		executing = true;
		num_accesses = 0;
		/* Find the last uncovered property in the access trace and increase its value by 1 */
		int iter = trace_len;
		while( iter-- ){
			if( ++trace[iter].value >= trace[iter].property->num_values )
				continue;
			if( trace[iter].value < trace[iter].property->num_values )
				break;
		}
		trace_len = iter + 1; /* Erase everything after the property we just changed */
		break;
		
	case UTT_PR_TRACE_CMD_FINISH:
		if( !executing )
			return 0;
		executing = false;
		break;
		
	case UTT_PR_TRACE_CMD_IS_COVERED:
	{
		/* Check if this trace is the lexicographically highest ordered trace we can achieve */
		int iter = trace_len;
		while( iter-- ){
			if( trace[iter].value + 1 >= trace[iter].property->num_values )
				continue; /* Check the next property, this one is covered! */
			if( trace[iter].value + 1 < trace[iter].property->num_values )
				return 0;
		}
		return 1;
	}
	
	default:
		return 0;
	}
	return 1;
}


/** Function to access the value of a certain property */
UTT_LINKAGE utt_pr_value_t utt_pr_access( utt_property_t* prop ){
	utt_size_t value_index = utt_pr_trace( UTT_PR_TRACE_CMD_ACCESS , (utt_pr_trace_cmd_data_t){ .access = { prop } } );
	#ifdef TARGET_VERBOSE
		utt_print( "Property Access: \"%s\" = %d\n" , prop->name , value_index );
	#endif
	return prop->values[ value_index ];
}

/** Function to start taking property coverage */
UTT_LINKAGE void utt_pr_reset(){
	utt_pr_trace( UTT_PR_TRACE_CMD_RESET , (utt_pr_trace_cmd_data_t){} );
}

/** Function to start taking property coverage */
UTT_LINKAGE void utt_pr_enter(){
	utt_pr_trace( UTT_PR_TRACE_CMD_START , (utt_pr_trace_cmd_data_t){} );
}

/** Function to finish taking property coverage */
UTT_LINKAGE void utt_pr_leave(){
	utt_pr_trace( UTT_PR_TRACE_CMD_FINISH , (utt_pr_trace_cmd_data_t){} );
}

/** Function to determine, whether the properties have been covered yet */
UTT_LINKAGE bool utt_pr_is_covered(){
	return utt_pr_trace( UTT_PR_TRACE_CMD_IS_COVERED , (utt_pr_trace_cmd_data_t){} );
}

/** Function to determine, whether the current trace is equal to the supplied one */
UTT_LINKAGE bool utt_pr_compare_trace( utt_pr_trace_entry_t* trace , utt_size_t trace_len ){
	return utt_pr_trace( UTT_PR_TRACE_CMD_COMPARE , (utt_pr_trace_cmd_data_t){ .compare = { trace , trace_len } } );
}

/** Function to assert, whether the supplied trace equals the observed one and optionally print a warning */
UTT_LINKAGE bool utt_pr_assert_trace( utt_pr_trace_entry_t* trace , utt_size_t trace_len ){
	if( !utt_pr_compare_trace( trace , trace_len ) ){
		utt_putstr("Unexpected call to properties, test cases can not cover the dynamic executing of the SUT!\n");
		return false;
	}
	return true;
}

/** Function to determine, whether the current trace is equal to the supplied one */
UTT_LINKAGE void utt_pr_get_trace( utt_pr_trace_entry_t** trace , utt_size_t* trace_len ){
	utt_pr_trace( UTT_PR_TRACE_CMD_GET , (utt_pr_trace_cmd_data_t){ .get = { trace , trace_len } } );
}

/** Function to output a trace in a format parseable by the import interface */
UTT_LINKAGE void utt_puttrace()
{
	utt_pr_trace_entry_t* trace;
	utt_size_t trace_len;
	utt_pr_get_trace( &trace , &trace_len );
	utt_putuint( trace_len );
	utt_putcesura();
	for( utt_size_t i = 0 ; i < trace_len ; i++ ){
		utt_putstr( trace[i].property->name );
		utt_putcesura();
		utt_putuint( trace[i].value );
	}
}

#endif
