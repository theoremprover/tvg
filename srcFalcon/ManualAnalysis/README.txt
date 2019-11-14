This folder contains the necessary to execute a test manually with code coverage information for the Falcon Library.
For this the correct procedure is as follows. 
	1. We need to update files (see Files to update)
	2. We need to execute the corresponding batch files (Files to execute)
	3. Code coverage information can be read in the generated file 'profile.txt'
	
Files to update:

	+ target.c : Test file
	+ target.c.h : ForeC++ headers
	
Files to execute:

	1. Instrument_library.bat : Generates Falcon Library with or without instrumentation.
	2. execute_manual.bat: Executes tests using the generated library from 'Instrument_library.bat'
	
Description of remaining files:

	+ Makefile : Reference file delivered by Sober
	+ Makefile_library: To generate Falcon Library
	+ Makefile_test: To execute test using generated library
	
	+ *.h, *.c : Files needed for Falcon Library