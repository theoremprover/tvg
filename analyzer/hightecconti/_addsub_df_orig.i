enum machine_mode
{
  VOIDmode,
  BLKmode,
  CCmode,
  BImode,
  QImode,
  HImode,
  SImode,
  DImode,
  TImode,
  PDImode,
  QQmode,
  HQmode,
  SQmode,
  DQmode,
  TQmode,
  UQQmode,
  UHQmode,
  USQmode,
  UDQmode,
  UTQmode,
  HAmode,
  SAmode,
  DAmode,
  TAmode,
  UHAmode,
  USAmode,
  UDAmode,
  UTAmode,
  HFmode,
  SFmode,
  DFmode,
  SDmode,
  DDmode,
  TDmode,
  CQImode,
  CHImode,
  CSImode,
  CDImode,
  CTImode,
  HCmode,
  SCmode,
  DCmode,
  MAX_MACHINE_MODE,

  MIN_MODE_RANDOM = VOIDmode,
  MAX_MODE_RANDOM = BLKmode,

  MIN_MODE_CC = CCmode,
  MAX_MODE_CC = CCmode,

  MIN_MODE_INT = QImode,
  MAX_MODE_INT = TImode,

  MIN_MODE_PARTIAL_INT = PDImode,
  MAX_MODE_PARTIAL_INT = PDImode,

  MIN_MODE_FRACT = QQmode,
  MAX_MODE_FRACT = TQmode,

  MIN_MODE_UFRACT = UQQmode,
  MAX_MODE_UFRACT = UTQmode,

  MIN_MODE_ACCUM = HAmode,
  MAX_MODE_ACCUM = TAmode,

  MIN_MODE_UACCUM = UHAmode,
  MAX_MODE_UACCUM = UTAmode,

  MIN_MODE_FLOAT = HFmode,
  MAX_MODE_FLOAT = DFmode,

  MIN_MODE_DECIMAL_FLOAT = SDmode,
  MAX_MODE_DECIMAL_FLOAT = TDmode,

  MIN_MODE_COMPLEX_INT = CQImode,
  MAX_MODE_COMPLEX_INT = CTImode,

  MIN_MODE_COMPLEX_FLOAT = HCmode,
  MAX_MODE_COMPLEX_FLOAT = DCmode,

  MIN_MODE_VECTOR_INT = VOIDmode,
  MAX_MODE_VECTOR_INT = VOIDmode,

  MIN_MODE_VECTOR_FRACT = VOIDmode,
  MAX_MODE_VECTOR_FRACT = VOIDmode,

  MIN_MODE_VECTOR_UFRACT = VOIDmode,
  MAX_MODE_VECTOR_UFRACT = VOIDmode,

  MIN_MODE_VECTOR_ACCUM = VOIDmode,
  MAX_MODE_VECTOR_ACCUM = VOIDmode,

  MIN_MODE_VECTOR_UACCUM = VOIDmode,
  MAX_MODE_VECTOR_UACCUM = VOIDmode,

  MIN_MODE_VECTOR_FLOAT = VOIDmode,
  MAX_MODE_VECTOR_FLOAT = VOIDmode,

  NUM_MACHINE_MODES = MAX_MACHINE_MODE
};

       
typedef float SFtype __attribute__ ((mode (SF)));
typedef float DFtype __attribute__ ((mode (DF)));




typedef int HItype __attribute__ ((mode (HI)));
typedef int SItype __attribute__ ((mode (SI)));
typedef int DItype __attribute__ ((mode (DI)));







typedef int CMPtype __attribute__ ((mode (__libgcc_cmp_return__)));


typedef unsigned int UHItype __attribute__ ((mode (HI)));
typedef unsigned int USItype __attribute__ ((mode (SI)));
typedef unsigned int UDItype __attribute__ ((mode (DI)));
 typedef UDItype fractype;
 typedef USItype halffractype;
 typedef DFtype FLO_type;
 typedef DItype intfrac;

typedef enum
{
  CLASS_SNAN,
  CLASS_QNAN,
  CLASS_ZERO,
  CLASS_NUMBER,
  CLASS_INFINITY
} fp_class_type;

typedef struct
{





  fp_class_type class;
  unsigned int sign;
  int normal_exp;


  union
    {
      fractype ll;
      halffractype l[2];
    } fraction;
} fp_number_type;

const fp_number_type __thenan_df = { CLASS_SNAN, 0, 0, {(fractype) 0} };

typedef union
{
  FLO_type value;
  fractype value_raw;





  halffractype words[2];




  struct
    {
      fractype fraction:52 __attribute__ ((packed));
      unsigned int exp:11 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
}
FLO_union_type;


static const fp_number_type *
makenan (void)
{





  return & __thenan_df;

}

__inline__
static int
isnan (const fp_number_type *x)
{
  return __builtin_expect (x->class == CLASS_SNAN || x->class == CLASS_QNAN,
      0);
}

__inline__
static int
isinf (const fp_number_type * x)
{
  return __builtin_expect (x->class == CLASS_INFINITY, 0);
}



__inline__
static int
iszero (const fp_number_type * x)
{
  return x->class == CLASS_ZERO;
}

__inline__
static void
flip_sign ( fp_number_type * x)
{
  x->sign = !x->sign;
}


void
__unpack_d (FLO_union_type * src, fp_number_type * dst)
{



  fractype fraction;
  int exp;
  int sign;
  fraction = src->bits.fraction;
  exp = src->bits.exp;
  sign = src->bits.sign;
  dst->sign = sign;
  if (exp == 0)
    {

      if (fraction == 0



   )
 {

   dst->class = CLASS_ZERO;
 }
      else
 {



   dst->normal_exp = exp - 1023 + 1;
   fraction <<= 8L;

   dst->class = CLASS_NUMBER;

   while (fraction < ((fractype)1<<(52 +8L)))
     {
       fraction <<= 1;
       dst->normal_exp--;
     }

   dst->fraction.ll = fraction;
 }
    }
  else if (!0
    && __builtin_expect (exp == (0x7ff), 0))
    {

      if (fraction == 0)
 {

   dst->class = CLASS_INFINITY;
 }
      else
 {




   if (fraction & 0x8000000000000LL)

     {
       dst->class = CLASS_QNAN;
     }
   else
     {
       dst->class = CLASS_SNAN;
     }


   fraction &= ~0x8000000000000LL;
   dst->fraction.ll = fraction << 8L;
 }
    }
  else
    {

      dst->normal_exp = exp - 1023;
      dst->class = CLASS_NUMBER;
      dst->fraction.ll = (fraction << 8L) | ((fractype)1<<(52 +8L));
    }

 return(dst->fraction.ll);
}

FLO_type
__pack_d (const fp_number_type *src)
{
  FLO_union_type dst;
  fractype fraction = src->fraction.ll;
  int sign = src->sign;
  int exp = 0;

  if (0 && (isnan (src) || isinf (src)))
    {



      exp = (0x7ff);
      fraction = ((fractype) 1 << 52) - 1;
    }
  else if (isnan (src))
    {
      exp = (0x7ff);

      fraction >>= 8L;
      fraction &= 0x8000000000000LL - 1;
      if (src->class == CLASS_QNAN || 1)
 {







   fraction |= 0x8000000000000LL;

 }
    }
  else if (isinf (src))
    {
      exp = (0x7ff);
      fraction = 0;
    }
  else if (iszero (src))
    {
      exp = 0;
      fraction = 0;
    }
  else if (fraction == 0)
    {
      exp = 0;
    }
  else
    {
      if (__builtin_expect (src->normal_exp < (-(1023)+1), 0))
 {
   int shift = (-(1023)+1) - src->normal_exp;

   exp = 0;

   if (shift > 64 - 8L)
     {

       fraction = 0;
     }
   else
     {
       int lowbit = (fraction & (((fractype)1 << shift) - 1)) ? 1 : 0;
       fraction = (fraction >> shift) | lowbit;
     }
   if ((fraction & 0xff) == 0x80)
     {
       if ((fraction & (1 << 8L)))
  fraction += 0x7f + 1;
     }
   else
     {

       fraction += 0x7f;
     }


   if (fraction >= ((fractype)1<<(52 +8L)))
     {
       exp += 1;
     }
   fraction >>= 8L;

 }
      else if (!0
        && __builtin_expect (src->normal_exp > 1023, 0))
 {
   exp = (0x7ff);
   fraction = 0;
 }
      else
 {
   exp = src->normal_exp + 1023;
   if (!0)
     {



       if ((fraction & 0xff) == 0x80)
  {
    if (fraction & (1 << 8L))
      fraction += 0x7f + 1;
  }
       else
  {

    fraction += 0x7f;
  }
       if (fraction >= ((fractype)1<<(52 +1+8L)))
  {
    fraction >>= 1;
    exp += 1;
  }
     }
   fraction >>= 8L;

   if (0 && exp > (0x7ff))
     {

       exp = (0x7ff);
       fraction = ((fractype) 1 << 52) - 1;
     }
 }
    }





  dst.bits.fraction = fraction;
  dst.bits.exp = exp;
  dst.bits.sign = sign;
  return dst.value;
}



struct _dont_use_rtx_here_;
struct _dont_use_rtvec_here_;
union _dont_use_tree_here_;
enum function_class {
  function_c94,
  function_c99_misc,
  function_c99_math_complex,
  function_sincos
};



enum memmodel
{
  MEMMODEL_RELAXED = 0,
  MEMMODEL_CONSUME = 1,
  MEMMODEL_ACQUIRE = 2,
  MEMMODEL_RELEASE = 3,
  MEMMODEL_ACQ_REL = 4,
  MEMMODEL_SEQ_CST = 5,
  MEMMODEL_LAST = 6
};






typedef void (*gt_pointer_operator) (void *, void *);


typedef unsigned char uchar;
typedef struct
{

  const char *option;




  int by_compiler;



  int by_assembler;



  const char *macro;



  int fixit;


  const char *ifx_name;
} tric_erratum_t;

enum
  {


tric_errata_cpu048,



tric_errata_cpu060,


tric_errata_cpu069,


tric_errata_cpu070,


tric_errata_cpu072,


tric_errata_cpu076,



tric_errata_cpu081,



tric_errata_cpu082,


tric_errata_cpu083,



tric_errata_cpu094,



tric_errata_cpu095,

tric_errata_cpu096,


tric_errata_cpu101,

tric_errata_cpu116,






tric_errata_cpu114,

    tric_errata_max
  };



typedef struct
{

  const char *name;


  int id;


  const char *libname;
} tric_core_t;



typedef struct
{

  const char *name;


  const char *target_name;


  const char *core_mtc;


  const char *ld_mcpu;
} tric_device_t;

extern tric_erratum_t tric_errata[];
extern const tric_core_t tric_cores[];
extern const tric_core_t *tric_core;
extern const tric_device_t tric_devices[];
extern const tric_device_t *tric_device;

extern void tric_set_core (const char*);
extern void tric_set_device (const char*);
extern const char *tric_device_to_startfile (int argc, const char **argv);
extern const char *tric_device_to_ld (int argc, const char **argv);
extern const char *tric_device_to_as (int argc, const char **argv);
extern const char *tric_self_specs (int argc, const char **argv);
extern const char *insert_tooldir_spec_function (int argc, const char **argv);
typedef struct
{
  const char * name;
  const char * args;
  char ret;
  int fast;
} tric_libfunc_info_t;
typedef struct
{
  int call_cookie;
  int argno;
  union _dont_use_tree_here_ * fntype;
  int libfunc_p;
  int outgoing;
  int update;
  int this_argno;
  unsigned int args_mask;
  tric_libfunc_info_t libfunc;
} CUMULATIVE_ARGS;
enum reg_class
{
    NO_REGS,

    REGCLASS_D15,
    REGCLASS_A10,
    REGCLASS_A15,

    REGCLASS_ND15,
    REGCLASS_D,
    REGCLASS_NA10,
    REGCLASS_NA15,
    REGCLASS_A,
    REGCLASS_R,

    ALL_REGS,

    LIM_REG_CLASSES
};
extern int tric_move_ratio (int);
extern int tric_clear_ratio (int);
extern int tric_set_ratio (int);
struct machine_function
{

  int is_leaf;



  int is_interrupt;






  int sibcall_fails;




  int bogus_pxhndcall;

  int anchor_completed;




  struct
  {
    struct _dont_use_rtx_here_ * reg;



    struct _dont_use_rtx_here_ * symbol;
  } pic_offset;
};

struct tric_segment_trap
{

  int have_pre_inc;


  int have_pre_dec;


  int do_ivopts;


  int do_ivopts_base_costs;


  int do_ivopts_use_address;



  int do_sched_change_address;
};


__inline__
static int
clzusi (USItype n)
{
  extern int __clzsi2 (USItype);
  if (sizeof (USItype) == sizeof (unsigned int))
    return __builtin_clz (n);
  else if (sizeof (USItype) == sizeof (unsigned long))
    return __builtin_clzl (n);
  else if (sizeof (USItype) == sizeof (unsigned long long))
    return __builtin_clzll (n);
  else
    return __clzsi2 (n);
}

extern FLO_type __pack_d (const fp_number_type * );
static const fp_number_type *
_fpadd_parts (fp_number_type * a,
       fp_number_type * b,
       fp_number_type * tmp)
{
  intfrac tfraction;


  int a_normal_exp;
  int b_normal_exp;
  fractype a_fraction;
  fractype b_fraction;

  if (isnan (a))
    {
      return a;
    }
  if (isnan (b))
    {
      return b;
    }
  if (isinf (a))
    {

      if (isinf (b) && a->sign != b->sign)
 return makenan ();
      return a;
    }
  if (isinf (b))
    {
      return b;
    }
  if (iszero (b))
    {
      if (iszero (a))
 {
   *tmp = *a;
   tmp->sign = a->sign & b->sign;
   return tmp;
 }
      return a;
    }
  if (iszero (a))
    {
      return b;
    }



  {
    int diff;
    int sdiff;

    a_normal_exp = a->normal_exp;
    b_normal_exp = b->normal_exp;
    a_fraction = a->fraction.ll;
    b_fraction = b->fraction.ll;

    diff = a_normal_exp - b_normal_exp;
    sdiff = diff;

    if (diff < 0)
      diff = -diff;
    if (diff < 64)
      {
 if (sdiff > 0)
   {
     b_normal_exp += diff;
     { b_fraction = (b_fraction >> diff) | !!(b_fraction & (((fractype) 1 << diff) - 1)); };
   }
 else if (sdiff < 0)
   {
     a_normal_exp += diff;
     { a_fraction = (a_fraction >> diff) | !!(a_fraction & (((fractype) 1 << diff) - 1)); };
   }
      }
    else
      {

 if (a_normal_exp > b_normal_exp)
   {
     b_normal_exp = a_normal_exp;
     b_fraction = 0;
   }
 else
   {
     a_normal_exp = b_normal_exp;
     a_fraction = 0;
   }
      }
  }

  if (a->sign != b->sign)
    {
      if (a->sign)
 {
   tfraction = -a_fraction + b_fraction;
 }
      else
 {
   tfraction = a_fraction - b_fraction;
 }
      if (tfraction >= 0)
 {
   tmp->sign = 0;
   tmp->normal_exp = a_normal_exp;
   tmp->fraction.ll = tfraction;
 }
      else
 {
   tmp->sign = 1;
   tmp->normal_exp = a_normal_exp;
   tmp->fraction.ll = -tfraction;
 }


      while (tmp->fraction.ll < ((fractype)1<<(52 +8L)) && tmp->fraction.ll)
 {
   tmp->fraction.ll <<= 1;
   tmp->normal_exp--;
 }
    }
  else
    {
      tmp->sign = a->sign;
      tmp->normal_exp = a_normal_exp;
      tmp->fraction.ll = a_fraction + b_fraction;
    }
  tmp->class = CLASS_NUMBER;



  if (tmp->fraction.ll >= ((fractype)1<<(52 +1+8L)))
    {
      { tmp->fraction.ll = (tmp->fraction.ll >> 1) | !!(tmp->fraction.ll & (((fractype) 1 << 1) - 1)); };
      tmp->normal_exp++;
    }
  return tmp;
}

FLO_type
__adddf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  const fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_d (&au, &a);
  __unpack_d (&bu, &b);

  res = _fpadd_parts (&a, &b, &tmp);

  return __pack_d (res);
}

FLO_type
__subdf3 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  fp_number_type tmp;
  const fp_number_type *res;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_d (&au, &a);
  __unpack_d (&bu, &b);

  b.sign ^= 1;

  res = _fpadd_parts (&a, &b, &tmp);

  return __pack_d (res);
}
extern SFtype __make_fp (fp_class_type, unsigned int, int, USItype);
