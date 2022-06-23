#pragma once

#include "common/goos/Object.h"
#include "common/type_system/TypeSpec.h"

#include "decompiler/Disasm/DecompilerLabel.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {
goos::Object decompile_sparticle_field_init(const TypeSpec& type,
                                            const DecompilerLabel& label,
                                            const std::vector<DecompilerLabel>& labels,
                                            const std::vector<std::vector<LinkedWord>>& words,
                                            const TypeSystem& ts,
                                            const LinkedObjectFile* file);
goos::Object decompile_sparticle_field_init(const DefpartElement::StaticInfo::PartField& field,
                                            const TypeSystem& ts);
goos::Object decompile_sparticle_group_item(const TypeSpec& type,
                                            const DecompilerLabel& label,
                                            const std::vector<DecompilerLabel>& labels,
                                            const std::vector<std::vector<LinkedWord>>& words,
                                            const TypeSystem& ts,
                                            const LinkedObjectFile* file);
}  // namespace decompiler
