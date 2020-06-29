#include <stdio.h>
#include <stdlib.h>

#include "fp-bit.i"

int main(int argc, char* argv[])
{
    //printf("IMPLICIT_1=%i\n",IMPLICIT_1);
    //printf("FRACBITS=%i, NGARDS=%i\n",FRACBITS,NGARDS);
    // FRACBITS=23, NGARDS=7
    //printf("ROUND_TOWARDS_ZERO=%i, GARDMASK=%i, GARDMSB=%i\n",ROUND_TOWARDS_ZERO,GARDMASK,GARDMSB);

    int i = 1 ;
    int arga0 = atoi(argv[i++]); // a
    int arga1 = atoi(argv[i++]); // fp_class_type class;
    int arga2 = atoi(argv[i++]); // unsigned int sign;
    int arga3 = atoi(argv[i++]); // int normal_exp;
    int arga4 = atoi(argv[i++]); // fractype ll; }

    int argb0 = atoi(argv[i++]); // b
    int argb1 = atoi(argv[i++]); // fp_class_type class;
    int argb2 = atoi(argv[i++]); // unsigned int sign;
    int argb3 = atoi(argv[i++]); // int normal_exp;
    int argb4 = atoi(argv[i++]); // fractype ll; }

    fp_number_type a = { arga1, arga2, arga3, { arga4 } };
    fp_number_type b = { argb1, argb2, argb3, { argb4 } };

    fp_number_type* r = _fpdiv_parts(&a,&b);
    printf("f(a=%i, a={ %i,%i,%i, fraction={%i} },   b=%i, b={ %i,%i,%i, fraction={%i} }) =\n%i %i %i %i %i\n",
        arga0,arga1,arga2,arga3,arga4,
        argb0,argb1,argb2,argb3,argb4,
        r,r->class,r->sign,r->normal_exp,r->fraction.ll);
    return 0;
}

