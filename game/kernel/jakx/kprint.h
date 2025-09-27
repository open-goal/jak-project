#pragma once

#include "common/common_types.h"

namespace jakx {
void output_sql_query(char* query_name);
}

// todo, do we actually have to do this, now that we aren't calling it from asm?
extern "C" {
s32 format_impl_jakx(uint64_t* args);
}