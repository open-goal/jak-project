#pragma once

#include "common/common_types.h"
#include "common/goos/Object.h"
#include "common/type_system/TypeSystem.h"
#include "common/versions/versions.h"

#include "decompiler/ObjectFile/LinkedWord.h"

namespace decompiler {
goos::Object decompile_sparticle_field_init(const std::vector<decompiler::LinkedWord>& words,
                                            u16 field_id,
                                            u16 flags,
                                            goos::Object sound_spec,
                                            goos::Object userdata,
                                            const TypeSystem& ts,
                                            GameVersion version);
}  // namespace decompiler
