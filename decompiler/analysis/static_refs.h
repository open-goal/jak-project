#pragma once

#include "decompiler/IR2/Form.h"

namespace decompiler {
int insert_static_refs(Form* top_level_form,
                       FormPool& pool,
                       const Function& function,
                       const DecompilerTypeSystem& dts);
}
