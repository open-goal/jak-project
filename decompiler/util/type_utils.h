#pragma once
#include "common/goal_constants.h"
#include "common/type_system/TypeSpec.h"
#include "common/versions/versions.h"

namespace decompiler {

/*!
 * Is ts something we can use symbol->string on?
 * Ideally, this would just check for symbol, but it's convenient for this to work on
 * structure/basic, as we sometimes get symbols out of res-lump lookups or similar.
 */
bool allowable_base_type_for_symbol_to_string(const TypeSpec& ts);

constexpr PerGameVersion<int> SYMBOL_TO_STRING_MEM_OFFSET_DECOMP = {8167 * 8,
                                                                    jak2::SYM_TO_STRING_OFFSET};

constexpr PerGameVersion<int> OFFSET_OF_NEXT_STATE_STORE = {72, 64};
}  // namespace decompiler
