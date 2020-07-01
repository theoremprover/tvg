#include "defs.h"
const _Dconst _Denorm = {{1, 0, 0, 0}};
const _Dconst _Hugeval = {{0, 0, 0, ((unsigned short)((1 << (15 - 4)) - 1)) << 4}};
const _Dconst _Inf = {{0, 0, 0, ((unsigned short)((1 << (15 - 4)) - 1)) << 4}};
const _Dconst _Nan = {{0, 0, 0, (((unsigned short)((1 << (15 - 4)) - 1)) << 4) | (1 << (4 - 1))}};
const _Dconst _Snan = {{1, 0, 0, ((unsigned short)((1 << (15 - 4)) - 1)) << 4}};
const _Dconst _Eps = {{0, 0, 0, (0x3fe - (48 + 4) - 1) << 4}};
const _Dconst _Rteps = {{0, 0, 0, (0x3fe - (48 + 4) / 2) << 4}};
const double _Zero = 0.0;

