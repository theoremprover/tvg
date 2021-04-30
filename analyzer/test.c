/*
2021-04-30 11:47:58.1486224 Mitteleuropäische Sommerzeit

Compiler: gcc
Function: sqrtf
Source files: ["analyzer\\test.c"]
Options: ["-nohalt","-cutoffs","-writemodels","-subfuncov"]

---- Trace [1,3,1,3,3,1] -----------------------------------

sqrtf ( x = Infinity = 0x7f800000 = Infinity )
    = return_val = Infinity = 0x7f800000 = Infinity

	COVERED BRANCHES:
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Else branch 3 "(F||F)" at line 114, col 12, len 70
	Then branch 3 "case 1" at line 232, col 2, len 91
	Then branch 1 "if(!(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000))" at line 233, col 7, len 59

checkSolutionM [1,3,1,3,3,1] ok.


---- Trace [1,3,1,3,3,2,1,1,2,2,1] -----------------------------------

sqrtf ( x = -Infinity = 0xff800000 = -Infinity )
    = return_val = 0x0p+0 = 0x00000000 = 0.0

	COVERED BRANCHES:
	Then branch 1 "if((errh & 2) != 0)" at line 59, col 7, len 14
	Else branch 2 "if((0x1 & (0x8 | 0x10)) != 0)" at line 61, col 8, len 28
	Else branch 2 "if((errh & 1) == 0)" at line 66, col 7, len 14
	Then branch 1 "if((0x1 & 0x1) != 0)" at line 68, col 12, len 19
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Else branch 3 "(F||F)" at line 114, col 12, len 70
	Then branch 3 "case 1" at line 232, col 2, len 91
	Else branch 2 "if(!(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000))" at line 233, col 7, len 59
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55

checkSolutionM ERROR for return_val : exec_val=NaN(0x400000) = 0x7fc00000 = NaN /= predicted_result=0x0p+0 = 0x00000000 = 0.0


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,1,1] -----------------------------------

sqrtf ( x = 0x1.60bccp-127 = 0x00582f30 = 8.098451e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Then branch 1 "(T||_)" at line 145, col 20, len 70
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,1,2] -----------------------------------

sqrtf ( x = 0x1.2549ccp-127 = 0x00495273 = 6.733568e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Then branch 2 "(F||T)" at line 145, col 20, len 70
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,1,3] -----------------------------------

sqrtf ( x = 0x1.05f96cp-127 = 0x00417e5b = 6.014635e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(F||F)" at line 145, col 20, len 70
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.18801cp-127 = 0x00462007 = 6.439974e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,1,2,2] -----------------------------------

sqrtf ( x = 0x1.57f98p-127 = 0x0055fe60 = 7.89727e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.0fe738p-127 = 0x0043f9ce = 6.242591e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.5d6078p-127 = 0x0057581e = 8.021299e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.005p-127 = 0x00401400 = 5.884646e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.005p-127 = 0x00401400 = 5.884646e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.05067p-127 = 0x0041419c = 5.992843e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.6d209cp-127 = 0x005b4827 = 8.382914e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.f7844p-127 = 0x007de110 = 1.1560174e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.c0b4cp-127 = 0x00702d30 = 1.0301786e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.1be7ap-127 = 0x0046f9e8 = 6.518134e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.0f922p-127 = 0x0043e488 = 6.23496e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,4,3] -----------------------------------

sqrtf ( x = 0x1.0c892p-127 = 0x00432248 = 6.165276e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,1,1] -----------------------------------

sqrtf ( x = 0x1.2568a8p-127 = 0x00495a2a = 6.736336e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,1,2,1] -----------------------------------

sqrtf ( x = 0x1.144c6cp-127 = 0x0045131b = 6.343503e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 1 "(F&&_)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.a13d6p-127 = 0x00684f58 = 9.579355e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.13002cp-127 = 0x0044c00b = 6.313706e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.8b4ebcp-127 = 0x0062d3af = 9.075816e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.4488p-127 = 0x00512200 = 7.450872e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,3,1] -----------------------------------

sqrtf ( x = 0x1.349f8cp-127 = 0x004d27e3 = 7.085642e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,3,2,1] -----------------------------------

sqrtf ( x = 0x1.1ea858p-127 = 0x0047aa16 = 6.581336e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 1 "(F&&_)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.5d6334p-127 = 0x005758cd = 8.021544e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,4,1] -----------------------------------

sqrtf ( x = 0x1.193c4p-127 = 0x00464f10 = 6.456847e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.a4e3bp-127 = 0x006938ec = 9.663147e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.4104b4p-127 = 0x0050412d = 7.37022e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.04f198p-127 = 0x00413c66 = 5.990974e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,1,2,2] -----------------------------------

sqrtf ( x = 0x1.80b5f8p-127 = 0x00602d7e = 8.832527e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.402668p-127 = 0x0050099a = 7.350284e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.002b14p-127 = 0x00400ac5 = 5.881335e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.802a4cp-127 = 0x00600a93 = 8.820001e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.8be1bp-127 = 0x0062f86c = 9.088996e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.372d34p-127 = 0x004dcb4d = 7.144264e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.2100b8p-127 = 0x0048402e = 6.635179e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.f00004p-127 = 0x007c0001 = 1.1387603e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.0c7f78p-127 = 0x00431fde = 6.16441e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.a20a7p-127 = 0x0068829c = 9.597745e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.20a008p-127 = 0x00482802 = 6.626508e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,4,3] -----------------------------------

sqrtf ( x = 0x1.669c0cp-127 = 0x0059a703 = 8.233272e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,1,1] -----------------------------------

sqrtf ( x = 0x1.4cc4cp-127 = 0x00533130 = 7.639991e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,1,2,1] -----------------------------------

sqrtf ( x = 0x1.1ee89cp-127 = 0x0047ba27 = 6.587099e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 1 "(F&&_)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.002214p-127 = 0x00400885 = 5.880528e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.57af5cp-127 = 0x0055ebd7 = 7.89062e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.0499dp-127 = 0x00412674 = 5.983102e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.6b24bcp-127 = 0x005ac92f = 8.337366e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,3,1] -----------------------------------

sqrtf ( x = 0x1.170f7p-127 = 0x0045c3dc = 6.40691e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,3,2,1] -----------------------------------

sqrtf ( x = 0x1.408414p-127 = 0x00502105 = 7.358685e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 1 "(F&&_)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.01688p-127 = 0x00405a20 = 5.909803e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,4,1] -----------------------------------

sqrtf ( x = 0x1.004p-127 = 0x00401000 = 5.883211e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.b22a1p-127 = 0x006c8a84 = 9.967924e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.19bf0cp-127 = 0x00466fc3 = 6.468577e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.68741p-127 = 0x005a1d04 = 8.275603e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.1a01e4p-127 = 0x00468079 = 6.474572e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.00020cp-127 = 0x00400083 = 5.877655e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1p-127 = 0x00400000 = 5.877472e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.b132c4p-127 = 0x006c4cb1 = 9.945745e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.025ddp-127 = 0x00409774 = 5.931803e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.160304p-127 = 0x004580c1 = 6.382837e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1p-127 = 0x00400000 = 5.877472e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.563978p-127 = 0x00558e5e = 7.857089e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.c162p-127 = 0x00705880 = 1.0317323e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,1,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.191f94p-127 = 0x004647e5 = 6.454276e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.2ca60cp-127 = 0x004b2983 = 6.902554e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.204004p-127 = 0x00481001 = 6.617897e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.e2ee94p-127 = 0x0078bba5 = 1.1087574e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.690bc8p-127 = 0x005a42f2 = 8.28921e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.40081p-127 = 0x00500204 = 7.347563e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.7475p-127 = 0x005d1d40 = 8.551194e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.6c66c8p-127 = 0x005b19b2 = 8.366248e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.89594p-127 = 0x00625650 = 9.030842e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.450408p-127 = 0x00514102 = 7.461996e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.3fb08cp-127 = 0x004fec23 = 7.339714e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,2,3,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.ddff0cp-127 = 0x00777fc3 = 1.0974256e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.010be8p-127 = 0x004042fa = 5.901498e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,1,2,2] -----------------------------------

sqrtf ( x = 0x1.43748p-127 = 0x0050dd20 = 7.426164e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.8da91cp-127 = 0x00636a47 = 9.129839e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.233a1p-127 = 0x0048ce84 = 6.68624e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.04p-127 = 0x00410000 = 5.969307e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.766048p-127 = 0x005d9812 = 8.595254e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.40c3d8p-127 = 0x005030f6 = 7.364404e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.fc0b14p-127 = 0x007f02c5 = 1.1664102e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.05a5ep-127 = 0x00416978 = 6.007142e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.555c2cp-127 = 0x0055570b = 7.837242e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.13ea14p-127 = 0x0044fa85 = 6.334683e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,1,2,2,4,3] -----------------------------------

sqrtf ( x = 0x1.18ba9p-127 = 0x00462ea4 = 6.445216e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.40bca8p-127 = 0x00502f2a = 7.363759e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.52ff1cp-127 = 0x0054bfc7 = 7.782978e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.0d24fcp-127 = 0x0043493f = 6.179254e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,1,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.41c2bp-127 = 0x005070ac = 7.387259e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.ce9f18p-127 = 0x0073a7c6 = 1.0621268e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,1,2,2] -----------------------------------

sqrtf ( x = 0x1.49ff08p-127 = 0x00527fc2 = 7.576342e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.686138p-127 = 0x005a184e = 8.273914e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.955cdp-127 = 0x00655734 = 9.306668e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.15f728p-127 = 0x00457dca = 6.381774e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.e40244p-127 = 0x00790091 = 1.1112298e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.7e7028p-127 = 0x005f9c0a = 8.780348e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.6d06p-127 = 0x005b4180 = 8.380527e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.88013cp-127 = 0x0062004f = 8.99999e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.416524p-127 = 0x00505949 = 7.378869e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.6000bcp-127 = 0x0058002f = 8.08159e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,1,2,2,4,3] -----------------------------------

sqrtf ( x = 0x1.1f000cp-127 = 0x0047c003 = 6.589201e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.17f87p-127 = 0x0045fe1c = 6.427807e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.ca3608p-127 = 0x00728d82 = 1.052001e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.382208p-127 = 0x004e0882 = 7.166221e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,2,1,2,3,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.904178p-127 = 0x0064105e = 9.189421e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Then branch 1 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.40bf1p-127 = 0x00502fc4 = 7.363975e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.82903cp-127 = 0x0060a40f = 8.875061e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.fd5ac4p-127 = 0x007f56b1 = 1.1694207e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.808c8p-127 = 0x00602320 = 8.828808e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.c3a4dp-127 = 0x0070e934 = 1.0369233e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.012a8p-127 = 0x00404aa0 = 5.904242e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.77fd3p-127 = 0x005dff4c = 8.632284e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.1674a4p-127 = 0x00459d29 = 6.393028e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.85387cp-127 = 0x00614e1f = 8.936068e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.648cbcp-127 = 0x0059232f = 8.18598e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,1,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.e32564p-127 = 0x0078c959 = 1.109249e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.2502ep-127 = 0x004940b8 = 6.727208e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.be23d8p-127 = 0x006f88f6 = 1.0242872e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.f4616cp-127 = 0x007d185b = 1.1488174e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.4171p-127 = 0x00505c40 = 7.379933e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1p-127 = 0x00400000 = 5.877472e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.00ca48p-127 = 0x00403292 = 5.895613e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.080888p-127 = 0x00420222 = 6.061908e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1p-127 = 0x00400000 = 5.877472e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.f9e65p-127 = 0x007e7994 = 1.1614887e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.ce66dp-127 = 0x007399b4 = 1.061622e-38 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,2,1,1,2,2,3,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1.4d1ee8p-127 = 0x005347ba = 7.648077e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,3,1,1,1] -----------------------------------

sqrtf ( x = 0x1.000104p-127 = 0x00400041 = 5.877563e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 3 "(F||F)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Then branch 1 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,3,2] -----------------------------------

sqrtf ( x = 0x1.38p-127 = 0x004e0000 = 7.163169e-39 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Else branch 3 "(F||F)" at line 82, col 6, len 29
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Then branch 2 "(T&&T)" at line 153, col 6, len 27
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,1,1] -----------------------------------

sqrtf ( x = 0x1.2cp-142 = 0x00000096 = 2.1e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Then branch 1 "(T||_)" at line 145, col 20, len 70
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,1,2] -----------------------------------

sqrtf ( x = 0x1.34p-142 = 0x0000009a = 2.16e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Then branch 2 "(F||T)" at line 145, col 20, len 70
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,1,3] -----------------------------------

sqrtf ( x = 0x1.d2p-142 = 0x000000e9 = 3.27e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(F||F)" at line 145, col 20, len 70
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.26p-142 = 0x00000093 = 2.06e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,1,2,2] -----------------------------------

sqrtf ( x = 0x1.1cp-142 = 0x0000008e = 1.99e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.42p-142 = 0x000000a1 = 2.26e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.d2p-142 = 0x000000e9 = 3.27e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.08p-142 = 0x00000084 = 1.85e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.78p-142 = 0x000000bc = 2.63e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.52p-142 = 0x000000a9 = 2.37e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1.0ep-142 = 0x00000087 = 1.89e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.2p-142 = 0x00000090 = 2.02e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.bp-142 = 0x000000d8 = 3.03e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.7ap-142 = 0x000000bd = 2.65e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.86p-142 = 0x000000c3 = 2.73e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,1,2,2,4,3] -----------------------------------

sqrtf ( x = 0x1.4p-142 = 0x000000a0 = 2.24e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,1,1] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,1,2,1] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 1 "(F&&_)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.98p-142 = 0x000000cc = 2.86e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.2p-142 = 0x00000090 = 2.02e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.4ep-142 = 0x000000a7 = 2.34e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.72p-142 = 0x000000b9 = 2.59e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,3,1] -----------------------------------

sqrtf ( x = 0x1.2p-142 = 0x00000090 = 2.02e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,3,2,1] -----------------------------------

sqrtf ( x = 0x1.4ap-142 = 0x000000a5 = 2.31e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 1 "(F&&_)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.08p-142 = 0x00000084 = 1.85e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,4,1] -----------------------------------

sqrtf ( x = 0x1.3p-142 = 0x00000098 = 2.13e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,1,1,2,3,2,2,2,1] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 1 "(F&&_)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.9ap-142 = 0x000000cd = 2.87e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,1,2,2] -----------------------------------

sqrtf ( x = 0x1.7ap-142 = 0x000000bd = 2.65e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,1,2,3] -----------------------------------

sqrtf ( x = 0x1.5ap-142 = 0x000000ad = 2.42e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.8cp-142 = 0x000000c6 = 2.77e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1.26p-142 = 0x00000093 = 2.06e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1.98p-142 = 0x000000cc = 2.86e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.2ap-142 = 0x00000095 = 2.09e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,4,2] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,1,2,4,3] -----------------------------------

sqrtf ( x = 0x1.c4p-142 = 0x000000e2 = 3.17e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.3ap-142 = 0x0000009d = 2.2e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,2,3] -----------------------------------

sqrtf ( x = 0x1.fp-142 = 0x000000f8 = 3.48e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,3,2,3] -----------------------------------

sqrtf ( x = 0x1.98p-142 = 0x000000cc = 2.86e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,1,2,2,4,3] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Then branch 1 "if(xexp <= -16)" at line 185, col 8, len 11
	Else branch 2 "psx != 0 ? _ : _" at line 187, col 25, len 8
	Else branch 2 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 4 "(F||(T&&F))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,1,1] -----------------------------------

sqrtf ( x = 0x1.06p-142 = 0x00000083 = 1.84e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,1,2,1] -----------------------------------

sqrtf ( x = 0x1.4p-142 = 0x000000a0 = 2.24e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 1 "(F&&_)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,1,2,3] -----------------------------------

sqrtf ( x = 0x1.4ep-142 = 0x000000a7 = 2.34e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 1 "(T||(_&&_))" at line 201, col 8, len 64
	Else branch 2 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Else branch 3 "(T&&F)" at line 207, col 16, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,2,1] -----------------------------------

sqrtf ( x = 0x1.34p-142 = 0x0000009a = 2.16e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 1 "(F&&_)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,2,2] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Then branch 2 "(T&&T)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,2,3] -----------------------------------

sqrtf ( x = 0x1p-142 = 0x00000080 = 1.8e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Else branch 2 "(F||(F&&_))" at line 201, col 8, len 64
	Else branch 3 "(T&&F)" at line 210, col 13, len 37
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2


---- Trace [2,1,2,1,2,2,2,1,2,2,1,2,2,2,1,2,2,2,1,2,4,2,2,2,1,2,1,2,2,3,1,2,3,2,1,2,3,1] -----------------------------------

sqrtf ( x = 0x1.02p-142 = 0x00000081 = 1.81e-43 )
    = return_val = 0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2

	COVERED BRANCHES:
	Then branch 1 "(T||_)" at line 82, col 6, len 29
	Then branch 2 "(F||T)" at line 82, col 6, len 29
	Then branch 1 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Else branch 2 "if(ps->_Sh[1] == 0)" at line 84, col 7, len 15
	Then branch 1 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(ps->_Sh[1] < 1 << 7)" at line 86, col 9, len 43
	Else branch 2 "for/while(1 << 7 + 1 <= ps->_Sh[1])" at line 92, col 9, len 50
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 111, col 6, len 48
	Then branch 1 "if(0 >= xchar)" at line 120, col 8, len 8
	Else branch 2 "if(0 >= xchar)" at line 120, col 8, len 8
	Then branch 1 "(T||_)" at line 121, col 9, len 24
	Then branch 2 "(F||T)" at line 121, col 9, len 24
	Then branch 1 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(0 >= xchar)" at line 123, col 10, len 8
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 144, col 6, len 48
	Else branch 3 "(T&&F)" at line 153, col 6, len 27
	Else branch 1 "(F&&_)" at line 157, col 6, len 67
	Else branch 2 "if(-xchar < xexp / 2)" at line 162, col 11, len 13
	Else branch 3 "(F||F)" at line 174, col 7, len 33
	Else branch 2 "if(xexp <= -16)" at line 185, col 8, len 11
	Then branch 1 "if((xexp = (short) -xexp) != 0)" at line 192, col 9, len 25
	Else branch 2 "psx != 0 ? _ : _" at line 194, col 42, len 8
	Then branch 3 "(F||(T&&T))" at line 201, col 8, len 64
	Then branch 1 "if((ps->_Sh[0] & 0xffff) == 0)" at line 205, col 11, len 25
	Then branch 4 "default" at line 235, col 2, len 134
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 236, col 9, len 55
	Else branch 2 "if((unsigned int) xexp & 1)" at line 241, col 7, len 22

checkSolutionM ERROR for return_val : exec_val=0x0p+0 = 0x00000000 = 0.0 /= predicted_result=0x1.8be0dep-4 = 0x3dc5f06f = 9.665e-2
*/

typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


typedef union
 {
 unsigned short _Word[8];
 float _Float;
 double _Double;
 long double _Long_double;
 } __attribute__ ((__may_alias__)) _Dconst;

int (feraiseexcept)(int except)
 {
          return 0;
        }

void (_Feraise)(int except)
 {

int __attribute__((fardata)) _Errno;

 int errh = (1 | 2);

 if ((errh & 2) != 0)
  {
  if ((except & (0x08 | 0x10)) != 0)
   except |= 0x20;
  feraiseexcept(except);
  }

 if ((errh & 1) == 0)
  ;
 else if ((except & 0x01) != 0)
  ( _Errno) = 0x0021;
 else if ((except & (0x04 | 0x10 | 0x08)) != 0)
  ( _Errno) = 0x0022;
}


short _FDnorm(_Fval *ps)
 {
 short xchar;
 unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

 xchar = 1;
 ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1));
 if (ps->_Sh[1] != 0 || ps->_Sh[0])
  {
  if (ps->_Sh[1] == 0)
    ps->_Sh[1] = ps->_Sh[0], ps->_Sh[0] = 0, xchar -= 16;
  for (;solver_pragma(0,1) && (ps->_Sh[1] < 1 << 7); --xchar)
   {
   ps->_Sh[1] = (unsigned short)(ps->_Sh[1] << 1
    | ps->_Sh[0] >> 15);
   ps->_Sh[0] <<= 1;
   }
  for (;solver_pragma(0,1) && (1 << (7 + 1) <= ps->_Sh[1]); ++xchar)
   {
   ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> 1
    | ps->_Sh[1] << 15);
   ps->_Sh[1] >>= 1;
   }
  ps->_Sh[1] &= ((unsigned short)((1 << 7) - 1));
  }
 ps->_Sh[1] |= sign;
 return (xchar);
 }



short _FDunscale(short *pex, float *px)
 {
 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7;

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  {
  *pex = 0;
  return ((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1);
  }
 else
  {
    short xchar1;
    if(0>=xchar) xchar1 = _FDnorm(ps);
    if (0 < xchar || xchar1 <= 0)
    {
      if(0>=xchar) xchar = xchar1;
      ps->_Sh[1] = ps->_Sh[1] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1)))) | 0x7e << 7;
      *pex = xchar - 0x7e;
      return ((-1));
      }
     else
      {
      *pex = 0;
      return (0);
    }
  }
 }


short _FDscale(float *px, long lexp)
 {
 _Dconst _FInf = {{0, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};

 _Fval *ps = (_Fval *)(char *)px;
 short xchar = (short)((ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7);

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  return ((short)((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) != 0 || ps->_Sh[0] != 0
   ? 2 : 1));
 else
 {

 short xchar_old = xchar;
 xchar = _FDnorm(ps);

 if (xchar_old == 0 && 0 < xchar)
  return (0);
 }

 if (0 < lexp && ((unsigned short)((1 << (15 - 7)) - 1)) - xchar <= lexp)
  {
  *px = ps->_Sh[1] & ((unsigned short)0x8000) ? -_FInf._Float : _FInf._Float;
  return (1);
  }
 else if (-xchar < lexp)
  {
  ps->_Sh[1] = (unsigned short)(ps->_Sh[1] & ~((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))
   | (lexp + xchar) << 7);
  return ((-1));
  }
 else
  {
  unsigned short sign = (unsigned short)(ps->_Sh[1] & ((unsigned short)0x8000));

  ps->_Sh[1] = (unsigned short)(1 << 7 | ps->_Sh[1] & ((unsigned short)((1 << 7) - 1)));
  lexp += xchar - 1;
  if (lexp < -(16 + 1 + 7) || 0 <= lexp)
   {
   ps->_Sh[1] = sign;
   ps->_Sh[0] = 0;
   return (0);
   }
  else
   {
   short xexp = (short)lexp;
   unsigned short psx = 0;

   if (xexp <= -16)
    {
    psx = ps->_Sh[0] | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = ps->_Sh[1];
    ps->_Sh[1] = 0;
    xexp += 16;
    }
   if ((xexp = (short)-xexp) != 0)
    {
    psx = (ps->_Sh[0] << (16 - xexp)) | (psx != 0 ? 1 : 0);
    ps->_Sh[0] = (unsigned short)(ps->_Sh[0] >> xexp
     | ps->_Sh[1] << (16 - xexp));
    ps->_Sh[1] >>= xexp;
    }

   ps->_Sh[1] |= sign;
   if (0x8000 < psx
    || 0x8000 == psx && (ps->_Sh[0] & 0x0001) != 0)
    {
      ps->_Sh[0]++;
      if((ps->_Sh[0] & 0xffff) == 0)
        ++ps->_Sh[1];
      else if (ps->_Sh[1] == sign && ps->_Sh[0] == 0)
        return (0);
    }
   else if (ps->_Sh[1] == sign && ps->_Sh[0] == 0)
    return (0);
   return ((-1));
   }
  }
 }



float (sqrtf)(float x)
 {
   _Dconst _FNan = {{0, (((unsigned short)((1 << (15 - 7)) - 1)) << 7) | (1 << (7 - 1))}
      };

 short xexp;
 float y;

 switch (_FDunscale(&xexp, &x))
  {
 case 2:
 case 0:
  return (x);
 case 1:
  if (!(((_Fval *)(char *)&x)->_Sh[1] & ((unsigned short)0x8000)))
   return (x);
 default:
  if ((((_Fval *)(char *)&x)->_Sh[1] & ((unsigned short)0x8000)))
   {
   _Feraise(0x01);
   return (_FNan._Float);
   }
  if ((unsigned int)xexp & 1)
   x *= 2.0F, --xexp;
  y = (-0.09977F * x + 0.71035F) * x
   + 0.38660F;
  y += x / y;
  y = 0.25F * y + x / y;
  _FDscale(&y, xexp / 2);
  return (y);
  }
 }
