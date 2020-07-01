#!/bin/bash
#set -x 
echo starting coverage using CTC 8.2 which has to be installed before
# since my gcc 7.4 does not cooperate with ctc, I use mingw
# export PATH=/cygdrive/c/mingw/mingw32/bin:$PATH
rm MON.sym MON.dat *.exe report.txt
ctc -i m gcc -DNO_MAIN -DDEBUG -fno-builtin -I. *.c 
ctcpost -fmcdc MON.sym -p report.txt
#uncovered functions to measure
# res=`grep TER report.txt|grep FUNCTION|grep "_FExp("| sed "s/).*//g"| sed "s/.*(//g"`
# from=`echo $res|sed "s%.*/%%g"`
# cov=`echo $res|sed "s%/.*%%g"`
# echo coverage $cov goal: $from

# echo first call
./a.exe 
# ./a.exe -99.2 299

ctcpost -fmcdc MON.sym MON.dat -p report.txt
# res=`grep TER report.txt|grep FUNCTION|grep "_Pow("| sed "s/).*//g"| sed "s/.*(//g"`
# cov=`echo $res|sed "s%/.*%%g"`
# echo coverage: $cov from $from
# ctc2html.bat -i report.txt -nsb
# echo LTG: achieved $cov lines from $from
# while [ $from -gt $cov ] 
# do
# 	x1=`printf "%04d" $(( $RANDOM % 1000 ))`
# 	echo a 0.000$x1 1024
# 	./a.exe 0.000$x1 1024
# 	./a.exe 0.000$x1 2048
# 	./a.exe 0.000$x1 4095
# 	./a.exe 0.000$x1 9192
# 	./a.exe 0.000$x1 10000
# 	./a.exe 2.$x1 1024
# 	./a.exe 2.$x1 2048
# 	./a.exe 2.$x1 4095
# 	./a.exe 2.$x1 9192
# 	./a.exe 2.$x1 10000
# 	./a.exe 8.$x1 1024
# 	./a.exe 8.$x1 2048
# 	./a.exe 8.$x1 4095
# 	./a.exe 8.$x1 9192
# 	./a.exe 8.$x1 10000
# 	./a.exe -0.000$x1 1024
# 	./a.exe -0.000$x1 2048
# 	./a.exe -0.000$x1 4095
# 	./a.exe -0.000$x1 9192
# 	./a.exe -0.000$x1 10000
# 	./a.exe -2.$x1 1024
# 	./a.exe -2.$x1 2048
# 	./a.exe -2.$x1 4095
# 	./a.exe -2.$x1 9192
# 	./a.exe -2.$x1 10000
# 	./a.exe -8.$x1 1024
# 	./a.exe -8.$x1 2048
# 	./a.exe -8.$x1 4095
# 	./a.exe -8.$x1 9192
# 	./a.exe -8.$x1 10000
# 	ctcpost -fmcdc MON.sym MON.dat -p report.txt
# res1=`grep TER report.txt|grep FUNCTION|grep "_Pow("| sed "s/).*//g"| sed "s/.*(//g"`
# neu=`echo $res1|sed "s%/.*%%g"`
# 	if [ $cov -lt $neu ] 
# 	then
# 		echo "pow(0.000$x1 1024-10000) increased to $neu MCDC from $from";
# 		cov=$neu
# 		ctc2html.bat -i report.txt -nsb
# 	fi
# done;