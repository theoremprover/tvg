#include "defs.h"

const _Dconst _FDenorm = {{1, 0}};
const _Dconst _FEps = {
 {0, (0x7e - (16 + 7) - 1) << 7}};
const _Dconst _FInf = {{0, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};
const _Dconst _FNan = {{0, (((unsigned short)((1 << (15 - 7)) - 1)) << 7) | (1 << (7 - 1))}};

const _Dconst _FSnan = {{1, ((unsigned short)((1 << (15 - 7)) - 1)) << 7}};
const _Dconst _FRteps = {
 {0, (0x7e - (16 + 7) / 2) << 7}};

const float _FZero = 0.0F;
