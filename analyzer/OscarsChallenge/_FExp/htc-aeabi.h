/*
 * 4.1.2 The floating-point helper functions
 */

/*
 * Table 2, Standard double precision floating-point arithmetic helper functions
 */
__attribute__((__pcs__("aapcs"))) double __aeabi_dadd(double, double);
__attribute__((__pcs__("aapcs"))) double __aeabi_ddiv(double, double);
__attribute__((__pcs__("aapcs"))) double __aeabi_dmul(double, double);
__attribute__((__pcs__("aapcs"))) double __aeabi_drsub(double, double);
__attribute__((__pcs__("aapcs"))) double __aeabi_dsub(double, double);

/*
 * Table 3, double precision floating-point comparison helper functions
 */
__attribute__((__pcs__("aapcs"))) void __aeabi_cdcmpeq(double, double);
__attribute__((__pcs__("aapcs"))) void __aeabi_cdcmple(double, double);
__attribute__((__pcs__("aapcs"))) void __aeabi_cdrcmple(double, double);
__attribute__((__pcs__("aapcs"))) int  __aeabi_dcmpeq(double, double);
__attribute__((__pcs__("aapcs"))) int  __aeabi_dcmplt(double, double);
__attribute__((__pcs__("aapcs"))) int  __aeabi_dcmple(double, double);
__attribute__((__pcs__("aapcs"))) int  __aeabi_dcmpge(double, double);
__attribute__((__pcs__("aapcs"))) int  __aeabi_dcmpgt(double, double);
__attribute__((__pcs__("aapcs"))) int  __aeabi_dcmpun(double, double);

/*
 * Table 4, Standard single precision floating-point arithmetic helper functions
 */
__attribute__((__pcs__("aapcs"))) float __aeabi_fadd(float, float);
__attribute__((__pcs__("aapcs"))) float __aeabi_fdiv(float, float);
__attribute__((__pcs__("aapcs"))) float __aeabi_fmul(float, float);
__attribute__((__pcs__("aapcs"))) float __aeabi_frsub(float, float);
__attribute__((__pcs__("aapcs"))) float __aeabi_fsub(float, float);

/*
 * Table 5, Standard single precision floating-point comparison helper functions
 */
__attribute__((__pcs__("aapcs"))) void __aeabi_cfcmpeq(float, float);
__attribute__((__pcs__("aapcs"))) void __aeabi_cfcmple(float, float);
__attribute__((__pcs__("aapcs"))) void __aeabi_cfrcmple(float, float);
__attribute__((__pcs__("aapcs"))) int  __aeabi_fcmpeq(float, float);
__attribute__((__pcs__("aapcs"))) int  __aeabi_fcmplt(float, float);
__attribute__((__pcs__("aapcs"))) int  __aeabi_fcmple(float, float);
__attribute__((__pcs__("aapcs"))) int  __aeabi_fcmpge(float, float);
__attribute__((__pcs__("aapcs"))) int  __aeabi_fcmpgt(float, float);
__attribute__((__pcs__("aapcs"))) int  __aeabi_fcmpun(float, float);

/*
 * Table 6, Standard floating-point to integer conversions
 */
__attribute__((__pcs__("aapcs"))) int                __aeabi_d2iz(double);
__attribute__((__pcs__("aapcs"))) unsigned           __aeabi_d2uiz(double);
__attribute__((__pcs__("aapcs"))) long long          __aeabi_d2lz(double);
__attribute__((__pcs__("aapcs"))) unsigned long long __aeabi_d2ulz(double);
__attribute__((__pcs__("aapcs"))) int                __aeabi_f2iz(float);
__attribute__((__pcs__("aapcs"))) unsigned           __aeabi_f2uiz(float);
__attribute__((__pcs__("aapcs"))) long long          __aeabi_f2lz(float);
__attribute__((__pcs__("aapcs"))) unsigned long long __aeabi_f2ulz(float);

/*
 * Table 7, Standard conversions between floating types
 */
__attribute__((__pcs__("aapcs"))) float  __aeabi_d2f(double);
__attribute__((__pcs__("aapcs"))) double __aeabi_f2d(float);
__attribute__((__pcs__("aapcs"))) float  __aeabi_h2f(short);
__attribute__((__pcs__("aapcs"))) float  __aeabi_h2f_alt(short);
__attribute__((__pcs__("aapcs"))) short  __aeabi_f2h(float);
__attribute__((__pcs__("aapcs"))) short  __aeabi_f2h_alt(float);
__attribute__((__pcs__("aapcs"))) short  __aeabi_d2h(double);
__attribute__((__pcs__("aapcs"))) short  __aeabi_d2h_alt(double);

/*
 * Table 8, Standard integer to floating-point conversions
 */
__attribute__((__pcs__("aapcs"))) double __aeabi_i2d(int);
__attribute__((__pcs__("aapcs"))) double __aeabi_ui2d(unsigned);
__attribute__((__pcs__("aapcs"))) double __aeabi_l2d(long long);
__attribute__((__pcs__("aapcs"))) double __aeabi_ul2d(unsigned long long);
__attribute__((__pcs__("aapcs"))) float  __aeabi_i2f(int);
__attribute__((__pcs__("aapcs"))) float  __aeabi_ui2f(unsigned);
__attribute__((__pcs__("aapcs"))) float  __aeabi_l2f(long long);
__attribute__((__pcs__("aapcs"))) float  __aeabi_ul2f(unsigned long long);

/*
 * 4.2 The long long helper functions
 */

/*
 * Table 9, Long long functions
 */
typedef struct { long long quot; long long rem; } lldiv_t;
typedef struct { unsigned long long quot; unsigned long long rem; } ulldiv_t;
long long __aeabi_lmul(long long, long long);
lldiv_t   __aeabi_ldivmod(long long, long long);
ulldiv_t  __aeabi_uldivmod(unsigned long long, unsigned long long);
long long __aeabi_llsl(long long, int);
long long __aeabi_llsr(long long, int);
long long __aeabi_lasr(long long, int);
int       __aeabi_lcmp(long long, long long);
int       __aeabi_ulcmp(unsigned long long, unsigned long long);

/*
 * 4.3 Other C and assembly language helper functions
 */

/*
 * 4.3.1 Integer (32/32 -> 32) division functions
 */

int      __aeabi_idiv(int, int);
unsigned __aeabi_uidiv(unsigned, unsigned);

typedef struct { int quot; int rem; } idiv_return;
typedef struct { unsigned quot; unsigned rem; } uidiv_return;
idiv_return  __aeabi_idivmod(int, int);
uidiv_return __aeabi_uidivmod(unsigned, unsigned);

/*
 * 4.3.2 Division by zero
 */
__attribute__((__pcs__("aapcs"))) int       __aeabi_idiv0(int);
__attribute__((__pcs__("aapcs"))) long long __aeabi_ldiv0(long long);

/*
 * 4.3.4 Memory copying, clearing, and setting
 */
void __aeabi_memcpy8(void *, const void *, unsigned int);
void __aeabi_memcpy4(void *, const void *, unsigned int);
void __aeabi_memcpy(void *, const void *, unsigned int);

void __aeabi_memmove8(void *, const void *, unsigned int);
void __aeabi_memmove4(void *, const void *, unsigned int);
void __aeabi_memmove(void *, const void *, unsigned int);

void __aeabi_memset8(void *, unsigned int, int);
void __aeabi_memset4(void *, unsigned int, int);
void __aeabi_memset(void *, unsigned int, int);

void __aeabi_memclr8(void *, unsigned int);
void __aeabi_memclr4(void *, unsigned int);
void __aeabi_memclr(void *, unsigned int);
