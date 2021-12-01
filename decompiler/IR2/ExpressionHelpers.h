#pragma once

#include <vector>

namespace decompiler {
class FormElement;
class Form;
class Env;
class FormPool;
class FormStack;

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

FormElement* last_two_in_and_to_handle_get_proc(Form* first,
                                                Form* second,
                                                const Env& env,
                                                FormPool& pool,
                                                FormStack& stack,
                                                bool part_of_longer_sc);
}  // namespace decompiler
