#pragma once

#include <string>
#include "common/common_types.h"

std::string disassemble_x86(u8* data, int len, u64 base_addr);
std::string disassemble_x86(u8* data, int len, u64 base_addr, u64 highlight_addr);