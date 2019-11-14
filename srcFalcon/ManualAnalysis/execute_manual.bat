@echo off

title TestRun

echo Compiling test...
make -f Makefile_test

echo Executing test...
test.exe

echo Generating report...
ctcpost MON.sym MON.dat -p profile.txt
echo Creating HTML output...
ctc2html -i profile.txt -t 85 -nsb

:commonexit
echo ...
pause


@echo on