#include "ExpressionHelpers.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/Env.h"
#include "common/goal_constants.h"

namespace decompiler {

FormElement* handle_get_property_value_float(const std::vector<Form*>& forms,
                                             FormPool& pool,
                                             const Env& env) {
  assert(forms.size() == 7);
  // lump object
  // name
  // 'interp
  // -1000000000.0 = DEFAULT_RES_TIME
  // default value
  // tag pointer
  // *res-static-buf*

  // get the res-lump. This can be anything.
  Form* lump_object = forms.at(0);

  // get the name of the the thing we're looking up. This can be anything.
  Form* property_name = forms.at(1);

  // get the mode. It must be interp.
  auto mode_atom = form_as_atom(forms.at(2));
  if (!mode_atom || !mode_atom->is_sym_ptr("interp")) {
    fmt::print("fail: bad mode {}\n", forms.at(2)->to_string(env));
    return nullptr;
  }

  // get the time. It must be DEFAULT_RES_TIME
  auto lookup_time = forms.at(3)->try_as_element<ConstantFloatElement>();
  if (!lookup_time || lookup_time->value() != DEFAULT_RES_TIME) {
    fmt::print("fail: bad time {}\n", forms.at(3)->to_string(env));
    return nullptr;
  }

  // get the default value. It can be anything...
  Form* default_value = forms.at(4);
  // but let's see if it's 0, because that's the default in the macro
  auto default_value_float = default_value->try_as_element<ConstantFloatElement>();
  if (default_value_float && default_value_float->value() == 0) {
    default_value = nullptr;
  }

  // get the tag pointer. It can be anything...
  Form* tag_pointer = forms.at(5);
  // but let's see if it's (the-as (pointer res-tag) #f)
  if (tag_pointer->to_string(env) == "(the-as (pointer res-tag) #f)") {
    tag_pointer = nullptr;
  }

  // get the buffer. It should be *res-static-buf*
  auto buf_atom = form_as_atom(forms.at(6));
  if (!buf_atom || !buf_atom->is_sym_val("*res-static-buf*")) {
    return nullptr;
  }

  // (lump name &key (tag-ptr (the-as (pointer res-tag) #f)) &key (default 0.0))
  std::vector<Form*> macro_args;
  macro_args.push_back(lump_object);
  macro_args.push_back(property_name);
  if (default_value) {
    macro_args.push_back(pool.form<ConstantTokenElement>(":default"));
    macro_args.push_back(default_value);
  }

  if (tag_pointer) {
    macro_args.push_back(pool.form<ConstantTokenElement>(":tag-ptr"));
    macro_args.push_back(tag_pointer);
  }

  GenericOperator op =
      GenericOperator::make_function(pool.form<ConstantTokenElement>("res-lump-float"));
  return pool.alloc_element<GenericElement>(op, macro_args);
}

namespace {
FormElement* handle_get_property_data_or_structure(const std::vector<Form*>& forms,
                                                   FormPool& pool,
                                                   const Env& env,
                                                   ResLumpMacroElement::Kind kind,
                                                   const std::string& expcted_default,
                                                   const TypeSpec& default_type) {
  //   (-> obj entity)
  //   'water-anim-fade-dist
  //   'interp
  //   -1000000000.0
  //   (the-as pointer #f)
  //   (the-as (pointer res-tag) #f)
  //   *res-static-buf*

  // get the res-lump. This can be anything.
  Form* lump_object = forms.at(0);

  // get the name of the the thing we're looking up. This can be anything.
  Form* property_name = forms.at(1);

  // get the mode. It must be interp.
  auto mode_atom = form_as_atom(forms.at(2));
  if (!mode_atom || !mode_atom->is_sym_ptr("interp")) {
    fmt::print("fail data: bad mode {}\n", forms.at(2)->to_string(env));
    return nullptr;
  }

  // get the time. It can be anything, but there's a default.
  auto time = forms.at(3);
  auto lookup_time = time->try_as_element<ConstantFloatElement>();
  if (lookup_time && lookup_time->value() == DEFAULT_RES_TIME) {
    time = nullptr;
  }

  // get the default value. It must be (the-as pointer #f)
  Form* default_value = forms.at(4);
  // but let's see if it's 0, because that's the default in the macro
  if (default_value->to_string(env) != expcted_default) {
    fmt::print("fail data: bad default {}\n", default_value->to_string(env));
    return nullptr;
  }

  // get the tag pointer. It can be anything...
  Form* tag_pointer = forms.at(5);
  // but let's see if it's (the-as (pointer res-tag) #f)
  if (tag_pointer->to_string(env) == "(the-as (pointer res-tag) #f)") {
    tag_pointer = nullptr;
  }

  // get the buffer. It should be *res-static-buf*
  auto buf_atom = form_as_atom(forms.at(6));
  if (!buf_atom || !buf_atom->is_sym_val("*res-static-buf*")) {
    return nullptr;
  }

  return pool.alloc_element<ResLumpMacroElement>(kind, lump_object, property_name,
                                                 nullptr,  // default, must be #f
                                                 tag_pointer, time, default_type);
}
}  // namespace

FormElement* handle_get_property_data(const std::vector<Form*>& forms,
                                      FormPool& pool,
                                      const Env& env) {
  return handle_get_property_data_or_structure(forms, pool, env, ResLumpMacroElement::Kind::DATA,
                                               "(the-as pointer #f)", TypeSpec("pointer"));
}

FormElement* handle_get_property_struct(const std::vector<Form*>& forms,
                                        FormPool& pool,
                                        const Env& env) {
  return handle_get_property_data_or_structure(forms, pool, env, ResLumpMacroElement::Kind::STRUCT,
                                               "#f", TypeSpec("structure"));
}

FormElement* handle_get_property_value(const std::vector<Form*>& forms,
                                       FormPool& pool,
                                       const Env& env) {
  // get the res-lump. This can be anything.
  Form* lump_object = forms.at(0);

  // get the name of the the thing we're looking up. This can be anything.
  Form* property_name = forms.at(1);

  // get the mode. It must be interp.
  auto mode_atom = form_as_atom(forms.at(2));
  if (!mode_atom || !mode_atom->is_sym_ptr("interp")) {
    fmt::print("fail data: bad mode {}\n", forms.at(2)->to_string(env));
    return nullptr;
  }

  // get the time. It can be anything, but there's a default.
  auto time = forms.at(3);
  auto lookup_time = time->try_as_element<ConstantFloatElement>();
  if (lookup_time && lookup_time->value() == DEFAULT_RES_TIME) {
    time = nullptr;
  }

  // get the default value. It can be whatever.
  Form* default_value = forms.at(4);
  // but let's see if it's 0, because that's the default in the macro
  if (default_value->to_string(env) == "(the-as uint128 0)") {
    default_value = nullptr;
  }

  // get the tag pointer. It can be anything...
  Form* tag_pointer = forms.at(5);
  // but let's see if it's (the-as (pointer res-tag) #f)
  if (tag_pointer->to_string(env) == "(the-as (pointer res-tag) #f)") {
    tag_pointer = nullptr;
  }

  // get the buffer. It should be *res-static-buf*
  auto buf_atom = form_as_atom(forms.at(6));
  if (!buf_atom || !buf_atom->is_sym_val("*res-static-buf*")) {
    return nullptr;
  }

  return pool.alloc_element<ResLumpMacroElement>(ResLumpMacroElement::Kind::VALUE, lump_object,
                                                 property_name, default_value, tag_pointer, time,
                                                 TypeSpec("uint128"));
}
}  // namespace decompiler