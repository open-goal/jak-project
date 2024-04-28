#pragma once
#include <optional>
#include <string>

#include "common/common_types.h"
#include "common/util/json_util.h"

namespace kmachine_extras {
void pc_set_levels(u32 lev_list);
void pc_set_active_levels(u32 lev_list);
u32 alloc_vagdir_names(u32 heap_sym);
inline u64 bool_to_symbol(const bool val);
// TODO - move to common
void encode_utf8_string(u32 src_str_ptr, u32 str_dest_ptr);

}  // namespace kmachine_extras
