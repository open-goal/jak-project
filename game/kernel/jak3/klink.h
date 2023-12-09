#pragma once

#include "common/common_types.h"

#include "game/kernel/common/Ptr.h"
#include "game/kernel/common/kmalloc.h"

namespace jak3 {
Ptr<uint8_t> link_and_exec(Ptr<uint8_t> data,
                           const char* name,
                           int32_t size,
                           Ptr<kheapinfo> heap,
                           uint32_t flags,
                           bool jump_from_c_to_goal);
}