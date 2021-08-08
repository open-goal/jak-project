#pragma once

#include "goalc/regalloc/allocator_interface.h"

// Allocator v2's interface
AllocationResult allocate_registers_v2(const AllocationInput& input);
