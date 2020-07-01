#!/bin/bash
#set -x 
echo starting coverage using CTC 8.2 which has to be installed before
# since my gcc 7.4 does not cooperate with ctc, I use mingw
export PATH=/cygdrive/c/mingw/mingw32/bin:$PATH
rm MON.sym MON.dat *.exe report.txt
ctc -i m gcc *.c 
ctcpost -fmcdc MON.sym -p report.txt
#uncovered functions to measure
res1=`grep TER report.txt|grep FUNCTION|grep _Xp_getw| sed "s/).*//g"| sed "s/.*(//g"`
from1=`echo $res1|sed "s%.*/%%g"`
cov1=`echo $res1|sed "s%/.*%%g"`

res2=`grep TER report.txt|grep FUNCTION|grep _Xp_addh| sed "s/).*//g"| sed "s/.*(//g"`
from2=`echo $res2|sed "s%.*/%%g"`
cov2=`echo $res2|sed "s%/.*%%g"`

res3=`grep TER report.txt|grep FUNCTION|grep _Xp_mulh| sed "s/).*//g"| sed "s/.*(//g"`
from3=`echo $res3|sed "s%.*/%%g"`
cov3=`echo $res3|sed "s%/.*%%g"`

res4=`grep TER report.txt|grep FUNCTION|grep _Xp_setw| sed "s/).*//g"| sed "s/.*(//g"`
from4=`echo $res4|sed "s%.*/%%g"`
cov4=`echo $res4|sed "s%/.*%%g"`

res5=`grep TER report.txt|grep FUNCTION|grep "_Quad("| sed "s/).*//g"| sed "s/.*(//g"`
from5=`echo $res5|sed "s%.*/%%g"`
cov5=`echo $res5|sed "s%/.*%%g"`

from=$(( $from1 + $from2 + $from3 + $from4 + $from5 ))
cov=$(( $cov1 + $cov2 + $cov3 + $cov4 + $cov5 ))

echo try some integers and NAN 
echo trying to increase cover $cov lines to $from
echo NAN, INF cases first
declare -a arr=("NAN" "1" "2" "100" "1024" "4096" "3.14159265" "INF" "-INF" "0" "-1.5" "DEN" "-DEN" "-85445660" "85445660")
for i in "${arr[@]}" ; do
  echo ./a.exe $i
  ./a.exe $i
  ctcpost -fmcdc MON.sym MON.dat -p report.txt
res1=`grep TER report.txt|grep FUNCTION|grep _Xp_getw| sed "s/).*//g"| sed "s/.*(//g"`
cov1=`echo $res1|sed "s%/.*%%g"`
res2=`grep TER report.txt|grep FUNCTION|grep _Xp_addh| sed "s/).*//g"| sed "s/.*(//g"`
cov2=`echo $res2|sed "s%/.*%%g"`
res3=`grep TER report.txt|grep FUNCTION|grep _Xp_mulh| sed "s/).*//g"| sed "s/.*(//g"`
cov3=`echo $res3|sed "s%/.*%%g"`
res4=`grep TER report.txt|grep FUNCTION|grep _Xp_setw| sed "s/).*//g"| sed "s/.*(//g"`
cov4=`echo $res4|sed "s%/.*%%g"`
res5=`grep TER report.txt|grep FUNCTION|grep "_Quad("| sed "s/).*//g"| sed "s/.*(//g"`
cov5=`echo $res5|sed "s%/.*%%g"`
neu=$(( $cov1 + $cov2 + $cov3 + $cov4 + $cov5 ))
#echo coverage: $neu
  if [ $neu -gt $cov ] 
  then
	echo "sin($i) increased to $neu MCDC";
	cov=$neu
	ctc2html.bat -i report.txt -nsb
  fi
 done
ctcpost -fmcdc MON.sym MON.dat -p report.txt
res1=`grep TER report.txt|grep FUNCTION|grep _Xp_getw| sed "s/).*//g"| sed "s/.*(//g"`
cov1=`echo $res1|sed "s%/.*%%g"`
res2=`grep TER report.txt|grep FUNCTION|grep _Xp_addh| sed "s/).*//g"| sed "s/.*(//g"`
cov2=`echo $res2|sed "s%/.*%%g"`
res3=`grep TER report.txt|grep FUNCTION|grep _Xp_mulh| sed "s/).*//g"| sed "s/.*(//g"`
cov3=`echo $res3|sed "s%/.*%%g"`
res4=`grep TER report.txt|grep FUNCTION|grep _Xp_setw| sed "s/).*//g"| sed "s/.*(//g"`
cov4=`echo $res4|sed "s%/.*%%g"`
res5=`grep TER report.txt|grep FUNCTION|grep "_Quad("| sed "s/).*//g"| sed "s/.*(//g"`
cov5=`echo $res5|sed "s%/.*%%g"`
neu=$(( $cov1 + $cov2 + $cov3 + $cov4 + $cov5 ))




echo try some PI related values 
echo trying to increase cover $cov lines to $from
declare -a arr=("-3.1415926535897932384626433832795029" "3.1415926535897932384626433832795029" "-0.78539816339744830961566084581987572" "0.78539816339744830961566084581987572" "-0.63661977236758134307553505349005745" "0.63661977236758134307553505349005745" )
for i in "${arr[@]}" ; do
  echo ./a.exe $i
  ./a.exe $i
  ctcpost -fmcdc MON.sym MON.dat -p report.txt
res1=`grep TER report.txt|grep FUNCTION|grep _Xp_getw| sed "s/).*//g"| sed "s/.*(//g"`
cov1=`echo $res1|sed "s%/.*%%g"`
res2=`grep TER report.txt|grep FUNCTION|grep _Xp_addh| sed "s/).*//g"| sed "s/.*(//g"`
cov2=`echo $res2|sed "s%/.*%%g"`
res3=`grep TER report.txt|grep FUNCTION|grep _Xp_mulh| sed "s/).*//g"| sed "s/.*(//g"`
cov3=`echo $res3|sed "s%/.*%%g"`
res4=`grep TER report.txt|grep FUNCTION|grep _Xp_setw| sed "s/).*//g"| sed "s/.*(//g"`
cov4=`echo $res4|sed "s%/.*%%g"`
res5=`grep TER report.txt|grep FUNCTION|grep "_Quad("| sed "s/).*//g"| sed "s/.*(//g"`
cov5=`echo $res5|sed "s%/.*%%g"`
neu=$(( $cov1 + $cov2 + $cov3 + $cov4 + $cov5 ))
#echo coverage: $neu
  if [ $neu -gt $cov ] 
  then
	echo "sin($i) increased to $neu MCDC";
	cov=$neu
	ctc2html.bat -i report.txt -nsb
  fi
 done
ctcpost -fmcdc MON.sym MON.dat -p report.txt
res1=`grep TER report.txt|grep FUNCTION|grep _Xp_getw| sed "s/).*//g"| sed "s/.*(//g"`
cov1=`echo $res1|sed "s%/.*%%g"`
res2=`grep TER report.txt|grep FUNCTION|grep _Xp_addh| sed "s/).*//g"| sed "s/.*(//g"`
cov2=`echo $res2|sed "s%/.*%%g"`
res3=`grep TER report.txt|grep FUNCTION|grep _Xp_mulh| sed "s/).*//g"| sed "s/.*(//g"`
cov3=`echo $res3|sed "s%/.*%%g"`
res4=`grep TER report.txt|grep FUNCTION|grep _Xp_setw| sed "s/).*//g"| sed "s/.*(//g"`
cov4=`echo $res4|sed "s%/.*%%g"`
res5=`grep TER report.txt|grep FUNCTION|grep "_Quad("| sed "s/).*//g"| sed "s/.*(//g"`
cov5=`echo $res5|sed "s%/.*%%g"`
neu=$(( $cov1 + $cov2 + $cov3 + $cov4 + $cov5 ))








echo achieved $cov lines from $from
while [ $cov -lt 142 ] 
do
	x1=`printf "%02x" $((1 + $RANDOM % 256 -1))`
	x2=`printf "%02x" $((1 + $RANDOM % 256 -1))`
	x3=`printf "%02x" $((1 + $RANDOM % 256 -1))`
	x4=`printf "%02x" $((1 + $RANDOM % 256 -1))`	
	echo 0x$x1$x2$x3$x4
	./a.exe 0x$x1$x2$x3$x4 
	ctcpost -fmcdc MON.sym MON.dat -p report.txt
res1=`grep TER report.txt|grep FUNCTION|grep _Xp_getw| sed "s/).*//g"| sed "s/.*(//g"`
from1=`echo $res1|sed "s%.*/%%g"`
cov1=`echo $res1|sed "s%/.*%%g"`
res2=`grep TER report.txt|grep FUNCTION|grep _Xp_addh| sed "s/).*//g"| sed "s/.*(//g"`
from2=`echo $res2|sed "s%.*/%%g"`
cov2=`echo $res2|sed "s%/.*%%g"`
res3=`grep TER report.txt|grep FUNCTION|grep _Xp_mulh| sed "s/).*//g"| sed "s/.*(//g"`
from3=`echo $res3|sed "s%.*/%%g"`
cov3=`echo $res3|sed "s%/.*%%g"`
res4=`grep TER report.txt|grep FUNCTION|grep _Xp_setw| sed "s/).*//g"| sed "s/.*(//g"`
from4=`echo $res4|sed "s%.*/%%g"`
cov4=`echo $res4|sed "s%/.*%%g"`
res5=`grep TER report.txt|grep FUNCTION|grep "_Quad("| sed "s/).*//g"| sed "s/.*(//g"`
cov5=`echo $res5|sed "s%/.*%%g"`
neu=$(( $cov1 + $cov2 + $cov3 + $cov4 + $cov5 ))
	if [ $cov -lt $neu ] 
	then
		echo "sin(0x$x1$x2$x3$x4) increased to $neu MCDC";
		cov=$neu
		ctc2html.bat -i report.txt -nsb
	fi
done;