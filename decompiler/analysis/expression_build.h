#pragma once

namespace decompiler {
class Form;
class Function;
class FormPool;
class DecompilerTypeSystem;
bool convert_to_expressions(Form* top_level_form,
                            FormPool& pool,
                            Function& f,
                            const DecompilerTypeSystem& dts);
}  // namespace decompiler