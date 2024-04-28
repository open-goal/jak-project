#pragma once
#include "game/mips2c/mips2c_private.h"

namespace Mips2C::jak3 {
extern ExecutionContext sky_regs_vfs;

namespace clip_polygon_against_positive_hyperplane {
extern u64 execute(void* ctxt);
}

namespace clip_polygon_against_negative_hyperplane {
extern u64 execute(void* ctxt);
}
}  // namespace Mips2C::jak3