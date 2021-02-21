#pragma once

#include "common/common_types.h"

namespace ee {
struct sceDmaChan {};

int sceDmaSync(sceDmaChan* chan, s32 mode, s32 timeout);
}  // namespace ee