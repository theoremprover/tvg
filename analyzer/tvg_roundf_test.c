typedef long unsigned int _Sizet;
typedef _Sizet size_t;

typedef union
 {
 unsigned short _Sh[sizeof(float)/sizeof(short)];
 float _Val;
 } __attribute__ ((__may_alias__)) _Fval;


short _FDint(float *px, short xexp)
 {
 _Fval *ps = (_Fval *)(char *)px;
 unsigned short frac;
 short xchar = (ps->_Sh[1] & ((unsigned short)(0x7fff & ~((unsigned short)((1 << 7) - 1))))) >> 7;

 if (xchar == ((unsigned short)((1 << (15 - 7)) - 1)))
  return ((ps->_Sh[1] & ((unsigned short)((1 << 7) - 1))) == 0 && ps->_Sh[0] == 0
   ? 1 : 2);
 else if ((ps->_Sh[1] & ~((unsigned short)0x8000)) == 0 && ps->_Sh[0] == 0)
  return (0);

 xchar = (0x7e + 16 + 7 + 1) - xchar - xexp;

 if (xchar <= 0)
  return (0);
 else if ((16 + 7 + 1) <= xchar)
  {
  ps->_Sh[1] &= ((unsigned short)0x8000);
  ps->_Sh[0] = 0;
  return ((-1));
  }
 else
  {
  static const unsigned short mask[] = {
   0x0000, 0x0001, 0x0003, 0x0007,
   0x000f, 0x001f, 0x003f, 0x007f,
   0x00ff, 0x01ff, 0x03ff, 0x07ff,
   0x0fff, 0x1fff, 0x3fff, 0x7fff};
  static const size_t sub[] = {0, 1};

  frac = mask[xchar & 0xf];
  xchar >>= 4;
  frac &= ps->_Sh[sub[xchar]];
  ps->_Sh[sub[xchar]] ^= frac;
  if (0 < xchar)
   frac |= ps->_Sh[0], ps->_Sh[0] = 0;

  return (frac != 0 ? (-1) : 0);
  }
 }






float (roundf)(float x)
 {
 switch (_FDint(&x, 1))
  {
 case 2:
 case 1:
  break;

 default:
  if (_FDint(&x, 0) == 0)
   ;
  else if ((((_Fval *)(char *)&(x))->_Sh[1] & ((unsigned short)0x8000)))
   x -= 1.0F;
  else
   x += 1.0F;
  break;
  }
 return (x);
 }

/*
2021-06-02 15:49:55.5987802

Compiler: gcc
Function: roundf
Source files: ["analyzer\\tvg_roundf_test.c"]
Options: ["-nohalt","-cutoffs","-subfuncov"]

---- Trace [1,1,1] -----------------------------------

roundf ( x = NaN(0x040000) = 0x7f840000 = NaN )
    = return_val = NaN(0x000200) = 0x7f800200 = NaN

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 18, col 12, len 70
	Then branch 1 "case 2" at line 62, col 2, len 25

checkSolutionM [1,1,1] ok.


---- Trace [1,2,2] -----------------------------------

roundf ( x = Infinity = 0x7f800000 = Infinity )
    = return_val = Infinity = 0x7f800000 = Infinity

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Then branch 2 "(T&&T)" at line 18, col 12, len 70
	Then branch 2 "case 1" at line 63, col 2, len 16

checkSolutionM [1,2,2] ok.


---- Trace [1,3,1] -----------------------------------

roundf ( x = NaN(0x000200) = 0x7f800200 = NaN )
    = return_val = NaN(0x000020) = 0x7f800020 = NaN

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Then branch 1 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 3 "(T&&F)" at line 18, col 12, len 70
	Then branch 1 "case 2" at line 62, col 2, len 25

checkSolutionM [1,3,1] ok.


---- Trace [2,1,1,3,2,1,1,1] -----------------------------------

roundf ( x = 0x1p33 = 0x50000000 = 8.589935e9 )
    = return_val = 0x1p33 = 0x50000000 = 8.589935e9

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Then branch 1 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Then branch 1 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM [2,1,1,3,2,1,1,1] ok.


---- Trace [2,1,1,3,2,1,2,2,2,2,1] -----------------------------------

roundf ( x = 0x1p22 = 0x4a800000 = 4194304.0 )
    = return_val = 0x1p22 = 0x4a800000 = 4194304.0

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Then branch 1 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM [2,1,1,3,2,1,2,2,2,2,1] ok.


---- Trace [2,1,2,2,1,1,3,2,1,2,2,1,2,1] -----------------------------------

roundf ( x = 0x1.004p3 = 0x41002000 = 8.0078125 )
    = return_val = 0x1.004p3 = 0x41002000 = 8.0078125

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM ERROR for return_val : exec_val=0x1p3 = 0x41000000 = 8.0 /= predicted_result=0x1.004p3 = 0x41002000 = 8.0078125


---- Trace [2,1,2,2,1,1,3,2,1,2,1,2,1] -----------------------------------

roundf ( x = -0x1.00001p-1 = 0xbf000008 = -0.5000005 )
    = return_val = -0x1.800008p0 = 0xbfc00004 = -1.5000005

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 1 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=-0x1p0 = 0xbf800000 = -1.0 /= predicted_result=-0x1.800008p0 = 0xbfc00004 = -1.5000005


---- Trace [2,1,2,2,1,1,3,2,1,2,1,2,2] -----------------------------------

roundf ( x = 0x1.000006p-1 = 0x3f000003 = 0.5000002 )
    = return_val = 0x1.800004p0 = 0x3fc00002 = 1.5000002

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 1 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=0x1p0 = 0x3f800000 = 1.0 /= predicted_result=0x1.800004p0 = 0x3fc00002 = 1.5000002


---- Trace [2,1,2,2,1,1,3,2,1,2,2,1,1,2,1] -----------------------------------

roundf ( x = -0x1.400002p1 = 0xc0200001 = -2.5000002 )
    = return_val = -0x1.c00002p1 = 0xc0600001 = -3.5000002

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=-0x1.8p1 = 0xc0400000 = -3.0 /= predicted_result=-0x1.c00002p1 = 0xc0600001 = -3.5000002


---- Trace [2,1,2,2,1,1,3,2,1,2,2,1,1,2,2] -----------------------------------

roundf ( x = 0x1.802006p0 = 0x3fc01003 = 1.5004886 )
    = return_val = 0x1.401004p1 = 0x40200802 = 2.5004888

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=0x1p1 = 0x40000000 = 2.0 /= predicted_result=0x1.401004p1 = 0x40200802 = 2.5004888


---- Trace [2,1,2,2,1,2,3,2,1,2,2,1,2,1] -----------------------------------

roundf ( x = 0x1p3 = 0x41000000 = 8.0 )
    = return_val = 0x1p3 = 0x41000000 = 8.0

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM [2,1,2,2,1,2,3,2,1,2,2,1,2,1] ok.


---- Trace [2,1,2,2,1,2,3,2,1,2,1,2,1] -----------------------------------

roundf ( x = -0x1p-1 = 0xbf000000 = -0.5 )
    = return_val = -0x1.8p0 = 0xbfc00000 = -1.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 1 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=-0x1p0 = 0xbf800000 = -1.0 /= predicted_result=-0x1.8p0 = 0xbfc00000 = -1.5


---- Trace [2,1,2,2,1,2,3,2,1,2,1,2,2] -----------------------------------

roundf ( x = 0x1p-1 = 0x3f000000 = 0.5 )
    = return_val = 0x1.8p0 = 0x3fc00000 = 1.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 1 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=0x1p0 = 0x3f800000 = 1.0 /= predicted_result=0x1.8p0 = 0x3fc00000 = 1.5


---- Trace [2,1,2,2,1,2,3,2,1,2,2,1,1,2,1] -----------------------------------

roundf ( x = -0x1.8p0 = 0xbfc00000 = -1.5 )
    = return_val = -0x1.4p1 = 0xc0200000 = -2.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=-0x1p1 = 0xc0000000 = -2.0 /= predicted_result=-0x1.4p1 = 0xc0200000 = -2.5


---- Trace [2,1,2,2,1,2,3,2,1,2,2,1,1,2,2] -----------------------------------

roundf ( x = 0x1.fcp5 = 0x427e0000 = 63.5 )
    = return_val = 0x1.02p6 = 0x42810000 = 64.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=0x1p6 = 0x42800000 = 64.0 /= predicted_result=0x1.02p6 = 0x42810000 = 64.5


---- Trace [2,1,2,2,2,2,3,2,1,2,2,1,2,1] -----------------------------------

roundf ( x = 0x1p7 = 0x43000000 = 128.0 )
    = return_val = 0x1p7 = 0x43000000 = 128.0

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM [2,1,2,2,2,2,3,2,1,2,2,1,2,1] ok.


---- Trace [2,1,2,2,2,2,3,2,1,2,2,2,2,1] -----------------------------------

roundf ( x = 0x1p17 = 0x48000000 = 131072.0 )
    = return_val = 0x1p17 = 0x48000000 = 131072.0

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM [2,1,2,2,2,2,3,2,1,2,2,2,2,1] ok.


---- Trace [2,1,2,2,2,2,3,2,1,2,2,1,1,2,1] -----------------------------------

roundf ( x = -0x1.ffp7 = 0xc37f8000 = -255.5 )
    = return_val = -0x1.008p8 = 0xc3804000 = -256.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=-0x1p8 = 0xc3800000 = -256.0 /= predicted_result=-0x1.008p8 = 0xc3804000 = -256.5


---- Trace [2,1,2,2,2,2,3,2,1,2,2,1,1,2,2] -----------------------------------

roundf ( x = 0x1.ffp7 = 0x437f8000 = 255.5 )
    = return_val = 0x1.008p8 = 0x43804000 = 256.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 1 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=0x1p8 = 0x43800000 = 256.0 /= predicted_result=0x1.008p8 = 0x43804000 = 256.5


---- Trace [2,1,2,2,2,2,3,2,1,2,2,2,1,2,1] -----------------------------------

roundf ( x = -0x1.ff8p8 = 0xc3ffc000 = -511.5 )
    = return_val = -0x1.004p9 = 0xc4002000 = -512.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Then branch 1 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=-0x1p9 = 0xc4000000 = -512.0 /= predicted_result=-0x1.004p9 = 0xc4002000 = -512.5


---- Trace [2,1,2,2,2,2,3,2,1,2,2,2,1,2,2] -----------------------------------

roundf ( x = 0x1.001p11 = 0x45000800 = 2048.5 )
    = return_val = 0x1.003p11 = 0x45001800 = 2049.5

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Else branch 2 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 1 "(F&&_)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Else branch 2 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Else branch 2 "if(0 < xchar)" at line 46, col 7, len 9
	Then branch 1 "frac != 0 ? _ : _" at line 49, col 11, len 9
	Else branch 2 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18
	Else branch 2 "if(((_Fval *) (char *) &x)->_Sh[1] & (unsigned short) 0x8000)" at line 69, col 14, len 57

checkSolutionM ERROR for return_val : exec_val=0x1.002p11 = 0x45001000 = 2049.0 /= predicted_result=0x1.003p11 = 0x45001800 = 2049.5


---- Trace [2,2,3,2,2,1] -----------------------------------

roundf ( x = 0x0p+0 = 0x00000000 = 0.0 )
    = return_val = 0x0p+0 = 0x00000000 = 0.0

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Then branch 2 "(T&&T)" at line 20, col 12, len 63
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Then branch 2 "(T&&T)" at line 20, col 12, len 63
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM [2,2,3,2,2,1] ok.


---- Trace [2,3,2,1,3,2,2,1] -----------------------------------

roundf ( x = 0x1p-136 = 0x00002000 = 1.148e-41 )
    = return_val = 0x1p-136 = 0x00002000 = 1.148e-41

	DECISION POINTS (IN CONTROL FLOW ORDER):
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Else branch 3 "(T&&F)" at line 20, col 12, len 63
	Else branch 2 "if(xchar <= 0)" at line 25, col 6, len 10
	Then branch 1 "if(16 + 7 + 1 <= xchar)" at line 27, col 12, len 20
	Then branch 3 "default" at line 66, col 2, len 147
	Else branch 2 "if(xchar == (unsigned short) ((1 << 15 - 7) - 1))" at line 17, col 6, len 48
	Then branch 2 "(T&&T)" at line 20, col 12, len 63
	Then branch 1 "if(_FDint(&x, 0) == 0)" at line 67, col 7, len 18

checkSolutionM [2,3,2,1,3,2,2,1] ok.

================================

VERIFICATION ERRORS: 13
*/