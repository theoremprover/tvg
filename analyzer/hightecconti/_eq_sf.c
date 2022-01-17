





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
 typedef USItype fractype;
 typedef UHItype halffractype;
 typedef SFtype FLO_type;
 typedef SItype intfrac;
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

typedef union
{
  FLO_type value;
  fractype value_raw;
  struct
    {
      fractype fraction:23 __attribute__ ((packed));
      unsigned int exp:8 __attribute__ ((packed));
      unsigned int sign:1 __attribute__ ((packed));
    }
  bits;
}
FLO_union_type;







extern void __unpack_f (FLO_union_type *, fp_number_type *);
extern int __fpcmp_parts_f (fp_number_type *, fp_number_type *);






extern CMPtype __eqsf2 (FLO_type, FLO_type);
extern DFtype __make_dp (fp_class_type, unsigned int, int, UDItype);
extern const fp_number_type __thenan_sf;




__inline__
static const fp_number_type *
makenan (void)
{



  return & __thenan_sf;



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

extern FLO_type __pack_f (const fp_number_type * );
CMPtype
__eqsf2 (FLO_type arg_a, FLO_type arg_b)
{
  fp_number_type a;
  fp_number_type b;
  FLO_union_type au, bu;

  au.value = arg_a;
  bu.value = arg_b;

  __unpack_f (&au, &a);
  __unpack_f (&bu, &b);

  if (isnan (&a) || isnan (&b))
    return 1;

  return __fpcmp_parts_f (&a, &b) ;
}
