

/* Node: Storage Layout */

#define BITS_BIG_ENDIAN          0
#define BYTES_BIG_ENDIAN         0
#define WORDS_BIG_ENDIAN         0

#define UNITS_PER_WORD           4

#define PARM_BOUNDARY           32
#define STACK_BOUNDARY          32
#define FUNCTION_BOUNDARY        8

#define BIGGEST_ALIGNMENT       32
#define EMPTY_FIELD_BOUNDARY     8
#define STRUCTURE_SIZE_BOUNDARY  8

#define STRICT_ALIGNMENT         1

#define MAX_FIXED_MODE_SIZE     32

/* For compatibility and historical reasons, a char should be signed.  */
#define DEFAULT_SIGNED_CHAR 1

/* Note that WCHAR_TYPE_SIZE is used in cexp.y,
   where TARGET_SHORT is not available.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Node: Frame Layout */
#define STACK_GROWS_DOWNWARD 1
#define FRAME_GROWS_DOWNWARD 1

