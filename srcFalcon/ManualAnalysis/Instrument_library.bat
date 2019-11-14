@echo off

title InstrumentLibrary

echo Instrument Library (n/ctc)
set /p instrument=

IF /i "%instrument%"=="ctc" goto CTCinstrumented
IF /i "%instrument%"=="n" goto noInstrumented

echo Not found.
goto commonexit

:CTCinstrumented
echo Instrumenting Library with CTC...
REM ctcwrap  -i m -C DATAFILE='^"../../../../TestsCoverage/MON.dat^"' make -f Makefile_library
REM make -f Makefile_library "CC=ctc -i m -C DATAFILE=^"MON.dat^" cl" "LNK=ctc link"
make -f Makefile_library CC="ctc -i m -C DATAFILE=MON.dat gcc" LNK="ctc link"
goto archive

:noInstrumented
echo Compiling Library (without instrumentation)
make -f Makefile_library
goto archive

:archive
echo Archiving library...
ar rcs libfalcon_instrumented.a libgcc2.o fp-bit.o dp-bit.o

:commonexit
pause


@echo on