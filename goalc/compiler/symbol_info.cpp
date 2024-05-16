#include "symbol_info.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

namespace symbol_info {
void SymbolInfo::update_args_from_docstring() {
  if (m_docstring.empty()) {
    return;
  }
  auto lines = str_util::split(m_docstring);
  for (const auto& line : lines) {
    const auto trimmed_line = str_util::ltrim(line);
    if (str_util::starts_with(trimmed_line, "@param")) {
      // Get the info from the @param line
      const auto& tokens =
          str_util::regex_get_capture_groups(trimmed_line, "(@param.)\\s?([^\\s]*)\\s(.*)");
      if (tokens.size() != 3) {
        lg::warn("invalid docstring line - {}, skipping", trimmed_line);
        continue;
      }
      const auto& param_type = str_util::trim(tokens[0]);
      const auto& param_name = str_util::trim(tokens[1]);
      const auto& param_description = str_util::trim(tokens[2]);
      // Locate the appropriate arg based on the name
      for (auto& arg : m_args) {
        if (arg.name == param_name) {
          arg.description = param_description;
          if (param_type == "@param") {
            // a normal arg, nothing fancy
          } else if (param_type == "@param_") {
            // it's unused
            arg.is_unused = true;
          } else if (param_type == "@param!") {
            // the params value is mutated within the function body
            arg.is_mutated = true;
          } else if (param_type == "@param?") {
            // the param is optional -- there are checks to see if it was provided or not so its
            // safe to pass "nothing"
            arg.is_optional = true;
          }
        }
      }
    }
  }
}

void SymbolInfo::set_definition_location(const goos::TextDb* textdb) {
  const auto& goos_info = textdb->get_short_info_for(m_def_form);
  if (goos_info) {
    DefinitionLocation def_loc;
    def_loc.line_idx = goos_info->line_idx_to_display;
    def_loc.char_idx = goos_info->pos_in_line;
    def_loc.file_path = file_util::convert_to_unix_path_separators(goos_info->filename);
    m_def_location = def_loc;
  }
}

void SymbolInfoMap::add_symbol_to_file_index(const std::string& file_path, SymbolInfo* symbol) {
  if (m_file_symbol_index.find(file_path) == m_file_symbol_index.end()) {
    m_file_symbol_index[file_path] = {};
  }
  m_file_symbol_index[file_path].push_back(symbol);
}

void SymbolInfoMap::add_global(const std::string& name,
                               const std::string& type,
                               const goos::Object& defining_form,
                               const std::string& docstring) {
  SymbolInfo info = {
      .m_kind = Kind::GLOBAL_VAR,
      .m_name = name,
      .m_def_form = defining_form,
      .m_docstring = docstring,
      .m_type = type,
  };
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_fwd_dec(const std::string& name, const goos::Object& defining_form) {
  SymbolInfo info = {.m_kind = Kind::FWD_DECLARED_SYM, .m_name = name, .m_def_form = defining_form};
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_function(const std::string& name,
                                 const std::string& return_type,
                                 const std::vector<GoalArg>& args,
                                 const goos::Object& defining_form,
                                 const std::string& docstring) {
  SymbolInfo info = {
      .m_kind = Kind::FUNCTION,
      .m_name = name,
      .m_def_form = defining_form,
      .m_docstring = docstring,
      .m_return_type = return_type,
  };
  for (const auto& goal_arg : args) {
    ArgumentInfo arg_info;
    arg_info.name = goal_arg.name;
    // TODO - is this reliable?
    arg_info.type = goal_arg.type.base_type();
    info.m_args.push_back(arg_info);
  }
  info.update_args_from_docstring();
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_type(const std::string& name,
                             Type* type_info,
                             const goos::Object& defining_form,
                             const std::string& docstring) {
  SymbolInfo info = {
      .m_kind = Kind::TYPE,
      .m_name = name,
      .m_def_form = defining_form,
      .m_docstring = docstring,
      .m_parent_type = type_info->get_parent(),
      .m_type_size = type_info->get_size_in_memory(),
  };
  // Only structure types have fields
  auto as_structure_type = dynamic_cast<StructureType*>(type_info);
  if (as_structure_type) {  // generate the inspect method
    for (const auto& field : as_structure_type->fields()) {
      // TODO - field docstrings arent a thing, yet!
      FieldInfo field_info = {
          .name = field.name(),
          .description = "",
          .type = field.type().base_type(),
          .is_array = field.is_array(),
          .is_dynamic = field.is_dynamic(),
          .is_inline = field.is_inline(),
      };
      info.m_type_fields.push_back(field_info);
    }
  }
  for (const auto& method : type_info->get_methods_defined_for_type()) {
    if (method.type.base_type() == "state") {
      TypeStateInfo state_info = {
          .name = method.name,
          .is_virtual = true,
          .id = method.id,
      };
      info.m_type_states.push_back(state_info);
    } else {
      TypeMethodInfo method_info = {
          .id = method.id,
          .name = method.name,
          .is_override = method.overrides_parent,
      };
      info.m_type_methods.push_back(method_info);
    }
  }
  for (const auto& [state_name, state_info] : type_info->get_states_declared_for_type()) {
    TypeStateInfo type_state_info = {
        .name = state_name,
        .is_virtual = false,
    };
    info.m_type_states.push_back(type_state_info);
  }
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_constant(const std::string& name,
                                 const goos::Object& defining_form,
                                 const std::string& docstring) {
  SymbolInfo info = {
      .m_kind = Kind::CONSTANT,
      .m_name = name,
      .m_def_form = defining_form,
      .m_docstring = docstring,
      // TODO - unfortunately, constants are not properly typed
      .m_type = "unknown",
  };
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_macro(const std::string& name,
                              const goos::ArgumentSpec arg_spec,
                              const goos::Object& defining_form,
                              const std::string& docstring) {
  SymbolInfo info = {
      .m_kind = Kind::MACRO,
      .m_name = name,
      .m_def_form = defining_form,
      .m_docstring = docstring,
  };
  for (const auto& arg : arg_spec.unnamed) {
    info.m_macro_args.push_back(arg);
  }
  for (const auto& arg : arg_spec.named) {
    std::optional<std::string> def_value;
    if (arg.second.has_default) {
      def_value = arg.second.default_value.print();
    }
    info.m_macro_kwargs.push_back({arg.first, def_value});
  }
  if (!arg_spec.rest.empty()) {
    info.m_variadic_arg = arg_spec.rest;
  }
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_builtin(const std::string& name, const std::string& docstring) {
  SymbolInfo info = {
      .m_kind = Kind::LANGUAGE_BUILTIN,
      .m_name = name,
      .m_docstring = docstring,
  };
  info.set_definition_location(m_textdb);
  m_symbol_map.insert(name, info);
}

void SymbolInfoMap::add_method(const std::string& method_name,
                               const std::vector<GoalArg>& args,
                               const MethodInfo& method_info,
                               const goos::Object& defining_form) {
  SymbolInfo info = {
      .m_kind = Kind::METHOD,
      .m_name = method_name,
      .m_def_form = defining_form,
      .m_method_info = method_info,
      .m_method_builtin = method_info.id <= 9,
  };
  if (method_info.docstring) {
    info.m_docstring = method_info.docstring.value();
  }
  for (const auto& goal_arg : args) {
    ArgumentInfo arg_info;
    arg_info.name = goal_arg.name;
    // TODO - is this reliable?
    arg_info.type = goal_arg.type.base_type();
    info.m_args.push_back(arg_info);
  }
  info.update_args_from_docstring();
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(method_name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_state(const std::string& name,
                              const std::string& related_type,
                              const bool is_virtual,
                              const std::vector<ArgumentInfo>& code_args,
                              const goos::Object& defining_form,
                              const std::string& docstring) {
  SymbolInfo info = {
      .m_kind = Kind::STATE,
      .m_name = name,
      .m_def_form = defining_form,
      .m_docstring = docstring,
      .m_state_related_type = related_type,
      .m_state_virtual = is_virtual,
  };
  // TODO - split up docstring into individual handlers
  // TODO - update args from docstring
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

void SymbolInfoMap::add_enum(EnumType* enum_info,
                             const goos::Object& defining_form,
                             const std::string& docstring) {
  SymbolInfo info = {.m_kind = Kind::ENUM,
                     .m_name = enum_info->get_name(),
                     .m_def_form = defining_form,
                     .m_docstring = docstring,
                     .m_enum_info = enum_info};
  info.set_definition_location(m_textdb);
  const auto inserted_symbol = m_symbol_map.insert(info.m_name, info);
  if (info.m_def_location) {
    add_symbol_to_file_index(info.m_def_location->file_path, inserted_symbol);
  }
}

std::vector<SymbolInfo*> SymbolInfoMap::lookup_symbols_by_file(const std::string& file_path) const {
  if (m_file_symbol_index.find(file_path) != m_file_symbol_index.end()) {
    return m_file_symbol_index.at(file_path);
  }
  return {};
}

std::vector<SymbolInfo*> SymbolInfoMap::lookup_exact_name(const std::string& name) const {
  return m_symbol_map.retrieve_with_exact(name);
}

std::vector<SymbolInfo*> SymbolInfoMap::lookup_exact_name(const std::string& name,
                                                          const Kind sym_kind) const {
  const auto query_results = m_symbol_map.retrieve_with_exact(name);
  std::vector<SymbolInfo*> filtered_results = {};
  for (const auto& result : query_results) {
    if (result->m_kind == sym_kind) {
      filtered_results.push_back(result);
    }
  }
  return filtered_results;
}

std::vector<SymbolInfo*> SymbolInfoMap::lookup_exact_method_name(
    const std::string& name,
    const std::string& defining_type_name) const {
  const auto query_results = m_symbol_map.retrieve_with_exact(name);
  std::vector<SymbolInfo*> filtered_results = {};
  for (const auto& result : query_results) {
    if (result->m_kind == Kind::METHOD && result->m_method_info.type_name == defining_type_name) {
      filtered_results.push_back(result);
    }
  }
  return filtered_results;
}

std::vector<SymbolInfo*> SymbolInfoMap::lookup_exact_virtual_state_name(
    const std::string& name,
    const std::string& defining_type_name) const {
  const auto query_results = m_symbol_map.retrieve_with_exact(name);
  std::vector<SymbolInfo*> filtered_results = {};
  for (const auto& result : query_results) {
    if (result->m_kind == Kind::STATE && result->m_state_virtual &&
        result->m_state_related_type == defining_type_name) {
      filtered_results.push_back(result);
    }
  }
  return filtered_results;
}

std::vector<SymbolInfo*> SymbolInfoMap::lookup_symbols_starting_with(
    const std::string& prefix) const {
  std::vector<SymbolInfo*> symbols;
  const auto lookup = m_symbol_map.retrieve_with_prefix(prefix);
  for (const auto& result : lookup) {
    symbols.push_back(result);
  }
  return symbols;
}

std::vector<SymbolInfo*> SymbolInfoMap::get_all_symbols() const {
  return m_symbol_map.get_all_elements();
}

std::set<std::string> SymbolInfoMap::lookup_names_starting_with(const std::string& prefix,
                                                                int max_count) const {
  std::set<std::string> names;
  const auto lookup = m_symbol_map.retrieve_with_prefix(prefix, max_count);
  for (const auto& result : lookup) {
    names.insert(result->m_name);
  }
  return names;
}

int SymbolInfoMap::symbol_count() const {
  return m_symbol_map.size();
}

void SymbolInfoMap::evict_symbols_using_file_index(const std::string& file_path) {
  const auto standardized_path = file_util::convert_to_unix_path_separators(file_path);
  if (m_file_symbol_index.find(standardized_path) != m_file_symbol_index.end()) {
    for (const auto& symbol : m_file_symbol_index.at(standardized_path)) {
      m_symbol_map.remove(symbol->m_name, symbol);
    }
    m_file_symbol_index.erase(standardized_path);
  }
}
}  // namespace symbol_info
