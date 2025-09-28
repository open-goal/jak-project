#include "DocTypes.h"

#include "common/log/log.h"
#include "common/util/string_util.h"

namespace Docs {

void to_json(json& j, const DefinitionLocation& obj) {
  j = json{{"filename", obj.filename}, {"line_idx", obj.line_idx}, {"char_idx", obj.char_idx}};
}

void to_json(json& j, const BuiltinDocumentation& obj) {
  j = json{{"name", obj.name}, {"description", obj.description}};
}

void to_json(json& j, const VariableDocumentation& obj) {
  j = json{{"name", obj.name},
           {"description", obj.description},
           {"type", obj.type},
           {"is_constant", obj.is_constant}};
  if (obj.def_location) {
    j["def_location"] = obj.def_location.value();
  }
}

void to_json(json& j, const ArgumentDocumentation& obj) {
  j = json{{"name", obj.name},
           {"description", obj.description},
           {"type", obj.type},
           {"is_mutated", obj.is_mutated},
           {"is_unused", obj.is_unused}};
}

void to_json(json& j, const FunctionDocumentation& obj) {
  j = json{{"name", obj.name},
           {"description", obj.description},
           {"return_type", obj.return_type},
           {"args", obj.args},
           {"is_unused", obj.is_unused}};
  if (obj.def_location) {
    j["def_location"] = obj.def_location.value();
  }
}

void to_json(json& j, const FieldDocumentation& obj) {
  j = json{
      {"name", obj.name},         {"description", obj.description}, {"type", obj.type},
      {"is_array", obj.is_array}, {"is_dynamic", obj.is_dynamic},   {"is_inline", obj.is_inline}};
}

void to_json(json& j, const TypeMethodDocumentation& obj) {
  j = json{{"name", obj.name}, {"id", obj.id}, {"is_override", obj.is_override}};
}

void to_json(json& j, const TypeStateDocumentation& obj) {
  j = json{{"name", obj.name}, {"is_virtual", obj.is_virtual}};
  if (obj.id) {
    j["id"] = obj.id.value();
  }
}

void to_json(json& j, const TypeDocumentation& obj) {
  j = json{{"name", obj.name},
           {"description", obj.description},
           {"parent_type", obj.parent_type},
           {"size", obj.size},
           {"fields", obj.fields},
           {"method_count", obj.method_count},
           {"methods", obj.methods},
           {"states", obj.states}};
  if (obj.def_location) {
    j["def_location"] = obj.def_location.value();
  }
}

void to_json(json& j, const MethodDocumentation& obj) {
  j = json{{"name", obj.name},
           {"id", obj.id},
           {"is_builtin", obj.is_builtin},
           {"description", obj.description},
           {"type", obj.type},
           {"is_override", obj.is_override},
           {"is_unused", obj.is_unused},
           {"args", obj.args},
           {"return_type", obj.return_type}};
  if (obj.def_location) {
    j["def_location"] = obj.def_location.value();
  }
}

void to_json(json& j, const FileDocumentation& obj) {
  j = json{{"description", obj.description},
           {"global_vars", obj.global_vars},
           {"functions", obj.functions},
           {"types", obj.types},
           {"constants", obj.constants},
           {"methods", obj.methods},
           {"macros", obj.macros}};
}

void to_json(json& j, const SymbolDocumentation& obj) {
  j = json{{"description", obj.description},
           {"name", obj.name},
           {"kind", obj.kind},
           {"forward_declared_in", obj.forward_declared_in}};
  if (obj.def_location) {
    j["def_location"] = obj.def_location.value();
  }
}

void to_json(json& j, const MacroDocumentation& obj) {
  j = json{{"name", obj.name}, {"args", obj.args}, {"description", obj.description}};
  auto kwargs = json{};
  for (const auto& kwarg : obj.kwargs) {
    kwargs[kwarg.first] = kwarg.second.has_value() ? kwarg.second.value() : "";
  }
  j["kwargs"] = kwargs;
  if (obj.variadic_arg) {
    j["variadic_arg"] = obj.variadic_arg.value();
  }
  if (obj.def_location) {
    j["def_location"] = obj.def_location.value();
  }
}

std::vector<ArgumentDocumentation> get_args_from_docstring(
    std::vector<symbol_info::ArgumentInfo> args,
    std::string docstring) {
  std::vector<ArgumentDocumentation> arg_docs;
  for (const auto& arg : args) {
    ArgumentDocumentation arg_doc;
    arg_doc.name = arg.name;
    // TODO - is this type reliable?
    arg_doc.type = arg.type;
    arg_docs.push_back(arg_doc);
  }
  if (docstring.empty()) {
    return arg_docs;
  }
  auto lines = str_util::split(docstring);
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
      for (auto& arg : arg_docs) {
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
  return arg_docs;
}

}  // namespace Docs
