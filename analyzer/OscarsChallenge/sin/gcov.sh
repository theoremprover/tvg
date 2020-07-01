#!/bin/bash
echo starting coverage 
rm *.gcov *.gcda *.gcno *.exe
gcc -fprofile-arcs -ftest-coverage *.c 
#echo Start application with default 0 as initial coverage value
./a.exe 0.0
#main file to cover:
gcov xsin.c

un=`grep "#####" xsin.c.gcov |wc -l`
echo try some integers and NAN until 3 remain that cannot be covered
echo trying to cover $un lines
while [ $un -gt 3 ] 
do
	x=`echo $((1 + $RANDOM % 12 -6))`
	
	if [ $x == "3" ] 
	then x=NAN 
	fi
	if [ $x == "-5" ] 
	then x=INF 
	fi
	# echo $x
	./a.exe $x;
	gcov xsin.c>>log.txt
	neu=`grep "#####" xsin.c.gcov |wc -l`;
	if [ $neu -lt $un ] 
	then
		echo "sin($x) reduced to $neu lines";
		un=$neu
		# gcov *.c
		# gcovr --html-details
	fi
done;
exit
# not required
echo now normal doubles
while [ $un -gt 0 ] 
do
	x1=`echo $((1 + $RANDOM % 10 -5))`
	x2=`echo $((1 + $RANDOM % 100))`
	echo $x1.$x2
	./a.exe $x1.$x2
	gcov xsin.c>>log.txt
	neu=`grep "#####" xsin.c.gcov |wc -l`;
	if [ $neu -lt $un ] 
	then
		echo "sin($x,$y) reduced to $neu lines";
		un=$neu
	fi
done;