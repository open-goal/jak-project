#pragma once

namespace decompiler {
class Form;
class Function;
class FormPool;
bool convert_to_expressions(Form* top_level_form, FormPool& pool, const Function& f);
}  // namespace decompiler