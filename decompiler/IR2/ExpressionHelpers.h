#pragma once

#include <vector>

namespace decompiler {
class FormElement;
class Form;
class Env;
class FormPool;

FormElement* handle_get_property_value_float(const std::vector<Form*>& forms,
                                             FormPool& pool,
                                             const Env& env);
FormElement* handle_get_property_data(const std::vector<Form*>& forms,
                                      FormPool& pool,
                                      const Env& env);
FormElement* handle_get_property_struct(const std::vector<Form*>& forms,
                                        FormPool& pool,
                                        const Env& env);

FormElement* handle_get_property_value(const std::vector<Form*>& forms,
                                       FormPool& pool,
                                       const Env& env);
}  // namespace decompiler
