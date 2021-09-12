#pragma once

#include <string>
#include <unordered_map>
#include <vector>

namespace decompiler {
class Form;
class Function;
class FormPool;
class DecompilerTypeSystem;
struct LocalVarOverride;
bool convert_to_expressions(
    Form* top_level_form,
    FormPool& pool,
    Function& f,
    const std::vector<std::string>& arg_names,
    const std::unordered_map<std::string, LocalVarOverride>& var_override_map,
    const DecompilerTypeSystem& dts);
}  // namespace decompiler