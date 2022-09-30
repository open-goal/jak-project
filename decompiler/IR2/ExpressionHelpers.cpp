#include "ExpressionHelpers.h"

#include "common/goal_constants.h"
#include "common/log/log.h"

#include "decompiler/IR2/Env.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"
#include "decompiler/IR2/GenericElementMatcher.h"

namespace decompiler {

// needed for jak 2.
std::optional<float> try_get_const_float(const Form* form) {
  auto* as_cfe = form->try_as_element<ConstantFloatElement>();
  if (as_cfe) {
    return as_cfe->value();
  }

  auto atom = form_as_atom(form);
  if (atom && atom->is_integer_promoted_to_float()) {
    return atom->get_integer_promoted_to_float();
  }
  return {};
}

FormElement* handle_get_property_value_float(const std::vector<Form*>& forms,
                                             FormPool& pool,
                                             const Env& env) {
  ASSERT(forms.size() == 7);
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
    lg::error("fail: bad mode {}", forms.at(2)->to_string(env));
    return nullptr;
  }

  // get the time. It must be DEFAULT_RES_TIME
  auto lookup_time = try_get_const_float(forms.at(3));
  if (!lookup_time || *lookup_time != DEFAULT_RES_TIME) {
    lg::error("fail: bad time {}\n", forms.at(3)->to_string(env));
    return nullptr;
  }

  // get the default value. It can be anything...
  Form* default_value = forms.at(4);
  // but let's see if it's 0, because that's the default in the macro
  auto default_value_float = try_get_const_float(default_value);
  if (default_value_float && *default_value_float == 0) {
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
    lg::error("fail data: bad mode {}", forms.at(2)->to_string(env));
    return nullptr;
  }

  // get the time. It can be anything, but there's a default.
  auto time = forms.at(3);
  auto lookup_time = try_get_const_float(time);
  if (lookup_time && *lookup_time == DEFAULT_RES_TIME) {
    time = nullptr;
  }

  // get the default value. It must be (the-as pointer #f)
  Form* default_value = forms.at(4);
  // but let's see if it's 0, because that's the default in the macro
  if (default_value->to_string(env) != expcted_default) {
    lg::error("fail data: bad default {}", default_value->to_string(env));
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
  return handle_get_property_data_or_structure(
      forms, pool, env, ResLumpMacroElement::Kind::STRUCT,
      env.version == GameVersion::Jak2 ? "(the-as structure #f)" : "#f", TypeSpec("structure"));
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
    lg::error("fail data: bad mode {}", forms.at(2)->to_string(env));
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

namespace {
Form* var_to_form(const RegisterAccess& var, FormPool& pool) {
  return pool.alloc_single_element_form<SimpleAtomElement>(nullptr, SimpleAtom::make_var(var));
}

/*!
 * Try to see if this form is a variable, or variable in a cast. If so, return the variable,
 * and set cast_out if there was a cast.
 */
std::optional<RegisterAccess> try_strip_cast_get_var(Form* in, std::optional<TypeSpec>& cast_out) {
  auto* elt = in->try_as_single_element();
  if (!elt) {
    return {};
  }
  auto* as_cast_elt = dynamic_cast<CastElement*>(elt);
  if (as_cast_elt) {
    cast_out = as_cast_elt->type();
    elt = as_cast_elt->source()->try_as_single_element();
  }
  if (!elt) {
    return {};
  }
  auto as_atom = form_element_as_atom(elt);
  if (!as_atom || !as_atom->is_var()) {
    return {};
  }
  return as_atom->var();
}

/*!
 * Return (the-as <type> <in>) or <in>.
 */
FormElement* maybe_cast(FormElement* in, std::optional<TypeSpec>& maybe_cast_type, FormPool& pool) {
  if (maybe_cast_type) {
    return pool.alloc_element<CastElement>(*maybe_cast_type, pool.alloc_single_form(nullptr, in));
  } else {
    return in;
  }
}

}  // namespace

/*!
 * Recognize the handle->process macro.
 * If it occurs inside of another and, the part_of_longer_sc argument should be set.
 *
 * Will move the cast out from the `if` to surround the output.
 */
FormElement* last_two_in_and_to_handle_get_proc(Form* first,
                                                Form* second,
                                                const Env& env,
                                                FormPool& pool,
                                                FormStack& stack,
                                                bool part_of_longer_sc) {
  constexpr int reg_input_1 = 0;
  constexpr int reg_input_2 = 1;
  constexpr int reg_input_3 = 2;
  constexpr int reg_temp_1 = 10;
  constexpr int reg_temp_2 = 11;
  constexpr int kValInIf = 12;

  // only used if part of a longer sc.
  Form* longer_sc_src = nullptr;  // the source (can be found without repopping)
  Form longer_sc_first_form;      // the equivalent of the first form for a normal handle->proc
  RegisterAccess longer_sc_var;   // the second temp var that only appears in this case.

  // check first.
  Matcher first_matcher =
      Matcher::op(GenericOpMatcher::condition(IR2_Condition::Kind::NONZERO),
                  {Matcher::op(GenericOpMatcher::fixed(FixedOperatorKind::L32_NOT_FALSE_CBOOL),
                               {Matcher::any_reg(reg_input_1)})});

  // if we're part of a longer, the first will actually be (being (set! foo <>) (... foo))
  // so let's strip out the set, then remember what was being set. If it all works out,
  // we can just substitute the <> into the macro
  if (part_of_longer_sc) {
    if (first->size() != 2) {
      return nullptr;
    }

    auto as_var_set = dynamic_cast<SetVarElement*>(first->elts().at(0));
    if (!as_var_set) {
      return nullptr;
    }

    longer_sc_var = as_var_set->dst();
    longer_sc_src = as_var_set->src();

    longer_sc_first_form.elts().push_back(first->elts().at(1));
    first = &longer_sc_first_form;
  }

  auto first_result = match(first_matcher, first);
  if (!first_result.matched) {
    return nullptr;
  }

  // auto first_use_of_in = *first_result.maps.regs.at(reg_input_1);
  // lg::print("reg1: {}\n", first_use_of_in.to_string(env));

  auto setup_matcher = Matcher::set_var(
      Matcher::deref(Matcher::any_reg(reg_input_2), false,
                     {DerefTokenMatcher::string("process"), DerefTokenMatcher::integer(0)}),
      reg_temp_1);

  auto if_matcher = Matcher::if_no_else(
      Matcher::op(
          GenericOpMatcher::fixed(FixedOperatorKind::EQ),
          {Matcher::deref(Matcher::any_reg(reg_input_3), false, {DerefTokenMatcher::string("pid")}),
           Matcher::deref(Matcher::any_reg(reg_temp_2), false,
                          {DerefTokenMatcher::string("pid")})}),
      Matcher::any(kValInIf));

  auto second_matcher = Matcher::begin({setup_matcher, if_matcher});

  auto second_result = match(second_matcher, second);
  if (!second_result.matched) {
    return nullptr;
  }

  auto in1 = *first_result.maps.regs.at(reg_input_1);
  auto in2 = *second_result.maps.regs.at(reg_input_2);
  auto in3 = *second_result.maps.regs.at(reg_input_3);

  auto in_name = in1.to_string(env);
  if (in_name != in2.to_string(env)) {
    return nullptr;
  }

  if (in_name != in3.to_string(env)) {
    return nullptr;
  }

  auto temp_name = second_result.maps.regs.at(reg_temp_1)->to_string(env);
  if (temp_name != second_result.maps.regs.at(reg_temp_2)->to_string(env)) {
    return nullptr;
  }

  std::optional<TypeSpec> cast_type_for_result;
  auto val_in_if =
      try_strip_cast_get_var(second_result.maps.forms.at(kValInIf), cast_type_for_result);

  if (!val_in_if || temp_name != val_in_if->to_string(env)) {
    return nullptr;
  }

  const auto& temp_use_def = env.get_use_def_info(*second_result.maps.regs.at(reg_temp_1));
  if (temp_use_def.use_count() != 2 || temp_use_def.def_count() != 1) {
    lg::error("failed usedef: {} {}", temp_use_def.use_count(), temp_use_def.def_count());
    return nullptr;
  }

  if (part_of_longer_sc) {
    // check that our temporary name matches (it's the var used inside the macro)
    if (in_name != longer_sc_var.to_string(env)) {
      lg::error("failed var name: {} vs {}", temp_name, longer_sc_var.to_string(env));
      return nullptr;
    }

    // check that our temporary has the right usage pattern.
    const auto& outer_temp_usedef = env.get_use_def_info(longer_sc_var);
    if (outer_temp_usedef.use_count() != 3 || outer_temp_usedef.def_count() != 1) {
      lg::error("failed usedef2: {} {}", outer_temp_usedef.use_count(),
                outer_temp_usedef.def_count());
      return nullptr;
    }

    return maybe_cast(
        pool.alloc_element<GenericElement>(
            GenericOperator::make_function(
                pool.alloc_single_element_form<ConstantTokenElement>(nullptr, "handle->process")),
            longer_sc_src),
        cast_type_for_result, pool);
  } else {
    // modify use def:
    auto* menv = const_cast<Env*>(&env);
    menv->disable_use(in2);
    menv->disable_use(in3);

    auto repopped = stack.pop_reg(in1, {}, env, true);
    // lg::print("repopped: {}\n", repopped->to_string(env));

    if (!repopped) {
      repopped = var_to_form(in1, pool);
    }

    return maybe_cast(
        pool.alloc_element<GenericElement>(
            GenericOperator::make_function(
                pool.alloc_single_element_form<ConstantTokenElement>(nullptr, "handle->process")),
            repopped),
        cast_type_for_result, pool);
  }
}
}  // namespace decompiler
