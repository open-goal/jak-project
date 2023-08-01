#include "MakeSystem.h"

#include "common/goos/ParseHelpers.h"
#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/string_util.h"

#include "goalc/make/Tools.h"

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"

std::string MakeStep::print() const {
  std::string result = fmt::format("Tool {} with inputs", tool);
  int i = 0;
  for (auto& in : input) {
    if (i++ > 0) {
      result += ", ";
    }
    result += fmt::format("\"{}\"", in);
  }
  result += " and deps:\n ";
  for (auto& dep : deps) {
    result += dep;
    result += '\n';
    result += ' ';
  }
  result.pop_back();

  result += fmt::format("will produce outputs:\n ");
  for (auto& o : outputs) {
    result += o;
    result += '\n';
    result += ' ';
  }
  result.pop_back();
  return result;
}

MakeSystem::MakeSystem(const std::optional<REPL::Config> repl_config, const std::string& username)
    : m_goos(username), m_repl_config(repl_config) {
  m_goos.register_form("defstep", [=](const goos::Object& obj, goos::Arguments& args,
                                      const std::shared_ptr<goos::EnvironmentObject>& env) {
    return handle_defstep(obj, args, env);
  });

  m_goos.register_form("basename", [=](const goos::Object& obj, goos::Arguments& args,
                                       const std::shared_ptr<goos::EnvironmentObject>& env) {
    return handle_basename(obj, args, env);
  });

  m_goos.register_form("stem", [=](const goos::Object& obj, goos::Arguments& args,
                                   const std::shared_ptr<goos::EnvironmentObject>& env) {
    return handle_stem(obj, args, env);
  });

  m_goos.register_form("get-gsrc-path", [=](const goos::Object& obj, goos::Arguments& args,
                                            const std::shared_ptr<goos::EnvironmentObject>& env) {
    return handle_get_gsrc_path(obj, args, env);
  });

  m_goos.register_form("map-path!", [=](const goos::Object& obj, goos::Arguments& args,
                                        const std::shared_ptr<goos::EnvironmentObject>& env) {
    return handle_map_path(obj, args, env);
  });

  m_goos.register_form("set-output-prefix",
                       [=](const goos::Object& obj, goos::Arguments& args,
                           const std::shared_ptr<goos::EnvironmentObject>& env) {
                         return handle_set_output_prefix(obj, args, env);
                       });

  m_goos.register_form("set-gsrc-folder!",
                       [=](const goos::Object& obj, goos::Arguments& args,
                           const std::shared_ptr<goos::EnvironmentObject>& env) {
                         return handle_set_gsrc_folder(obj, args, env);
                       });

  m_goos.register_form("get-gsrc-folder", [=](const goos::Object& obj, goos::Arguments& args,
                                              const std::shared_ptr<goos::EnvironmentObject>& env) {
    return handle_get_gsrc_folder(obj, args, env);
  });

  m_goos.register_form("get-game-version-folder",
                       [=](const goos::Object& obj, goos::Arguments& args,
                           const std::shared_ptr<goos::EnvironmentObject>& env) {
                         return handle_get_game_version_folder(obj, args, env);
                       });

  m_goos.set_global_variable_to_symbol("ASSETS", "#t");

  set_constant("*iso-data*", file_util::get_file_path({"iso_data"}));
  set_constant("*use-iso-data-path*", false);

  add_tool<DgoTool>();
  add_tool<TpageDirTool>();
  add_tool<CopyTool>();
  add_tool<GameCntTool>();
  add_tool<GroupTool>();
  add_tool<TextTool>();
  add_tool<SubtitleTool>();
  add_tool<SubtitleV2Tool>();
  add_tool<BuildLevelTool>();
}

/*!
 * Load a project file, clearing any project info previously loaded.
 */
void MakeSystem::load_project_file(const std::string& file_path) {
  Timer timer;
  // clear the previous project
  clear_project();
  // read the file
  auto data = m_goos.reader.read_from_file({file_path});
  // interpret it, which will call various handlers.
  m_goos.eval(data, m_goos.global_environment.as_env_ptr());
  lg::print("Loaded project {} with {} steps in {} ms\n", file_path, m_output_to_step.size(),
            (int)timer.getMs());
}

goos::Object MakeSystem::handle_defstep(const goos::Object& form,
                                        goos::Arguments& args,
                                        const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {},
           {{"out", {true, {goos::ObjectType::PAIR}}},
            {"tool", {true, {goos::ObjectType::SYMBOL}}},
            {"in", {false, {}}},
            {"dep", {false, {}}},
            {"arg", {false, {}}}});

  auto step = std::make_shared<MakeStep>();

  goos::for_each_in_list(args.get_named("out"), [&](const goos::Object& obj) {
    step->outputs.push_back(m_path_map.apply_remaps(obj.as_string()->data));
  });

  step->tool = args.get_named("tool").as_symbol()->name;

  if (m_tools.find(step->tool) == m_tools.end()) {
    throw std::runtime_error(fmt::format("The tool {} is unknown.", step->tool));
  }

  if (args.has_named("in")) {
    const auto& in = args.get_named("in");
    if (in.is_pair()) {
      step->input.clear();
      goos::for_each_in_list(in, [&](const goos::Object& o) {
        step->input.push_back(m_path_map.apply_remaps(o.as_string()->data));
      });
    } else {
      step->input = {m_path_map.apply_remaps(in.as_string()->data)};
    }
  }

  if (args.has_named("dep")) {
    goos::for_each_in_list(args.get_named("dep"), [&](const goos::Object& obj) {
      step->deps.push_back(m_path_map.apply_remaps(obj.as_string()->data));
    });
  }

  if (args.has_named("arg")) {
    step->arg = args.get_named("arg");
  } else {
    step->arg = goos::Object::make_empty_list();
  }

  for (auto& output : step->outputs) {
    auto existing = m_output_to_step.find(output);
    if (existing != m_output_to_step.end()) {
      throw std::runtime_error(fmt::format("There are multiple ways to make output {}:\n{}\n{}\n",
                                           output, step->print(), existing->second->print()));
    }
    m_output_to_step.insert({output, step});
  }

  return goos::Object::make_empty_list();
}

/*!
 * Fully clear the state of the project.
 *
 */
void MakeSystem::clear_project() {
  m_output_to_step.clear();
}

void MakeSystem::va_check(
    const goos::Object& form,
    const goos::Arguments& args,
    const std::vector<std::optional<goos::ObjectType>>& unnamed,
    const std::unordered_map<std::string, std::pair<bool, std::optional<goos::ObjectType>>>&
        named) {
  std::string err;
  if (!goos::va_check(args, unnamed, named, &err)) {
    m_goos.throw_eval_error(form, err);
  }
}

goos::Object MakeSystem::handle_basename(const goos::Object& form,
                                         goos::Arguments& args,
                                         const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {goos::ObjectType::STRING}, {});
  fs::path input(args.unnamed.at(0).as_string()->data);

  return goos::StringObject::make_new(input.filename().u8string());
}

goos::Object MakeSystem::handle_stem(const goos::Object& form,
                                     goos::Arguments& args,
                                     const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {goos::ObjectType::STRING}, {});
  fs::path input(args.unnamed.at(0).as_string()->data);

  return goos::StringObject::make_new(input.stem().u8string());
}

goos::Object MakeSystem::handle_get_gsrc_path(const goos::Object& form,
                                              goos::Arguments& args,
                                              const std::shared_ptr<goos::EnvironmentObject>& env) {
  if (m_gsrc_folder.empty()) {
    throw std::runtime_error("`set-gsrc-folder!` was not called before a `get-gsrc-path`");
  }
  m_goos.eval_args(&args, env);
  va_check(form, args, {goos::ObjectType::STRING}, {});

  const auto& file_name = args.unnamed.at(0).as_string()->data;

  if (m_gsrc_files.count(file_name) != 0) {
    return goos::StringObject::make_new(m_gsrc_files.at(file_name));
  } else {
    return goos::SymbolObject::make_new(m_goos.reader.symbolTable, "#f");
  }
}

goos::Object MakeSystem::handle_map_path(const goos::Object& form,
                                         goos::Arguments& args,
                                         const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {goos::ObjectType::STRING, goos::ObjectType::STRING}, {});
  auto old_path = args.unnamed.at(0).as_string()->data;
  if (old_path.empty() || old_path[0] != '$') {
    throw std::runtime_error(fmt::format("Invalid path remap {}, must start with $", old_path));
  }
  auto new_path = args.unnamed.at(1).as_string()->data;
  m_path_map.path_remap[old_path] = new_path;
  return goos::Object::make_empty_list();
}

goos::Object MakeSystem::handle_set_output_prefix(
    const goos::Object& form,
    goos::Arguments& args,
    const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {goos::ObjectType::STRING}, {});
  m_path_map.output_prefix = args.unnamed.at(0).as_string()->data;
  return goos::Object::make_empty_list();
}

goos::Object MakeSystem::handle_set_gsrc_folder(
    const goos::Object& form,
    goos::Arguments& args,
    const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {goos::ObjectType::STRING}, {});

  const auto& folder = args.unnamed.at(0).as_string()->data;
  m_gsrc_folder = str_util::split(folder, '/');
  m_gsrc_files.clear();

  auto folder_scan = file_util::get_file_path(m_gsrc_folder);
  auto src_files = file_util::find_files_recursively(folder_scan, std::regex(".*\\.gc"));

  for (const auto& path : src_files) {
    auto name = file_util::base_name_no_ext(path.u8string());
    auto gsrc_path =
        file_util::convert_to_unix_path_separators(file_util::split_path_at(path, m_gsrc_folder));
    // TODO - this is only "safe" because the current OpenGOAL system requires globally unique
    // file names
    m_gsrc_files.emplace(name, gsrc_path);
  }

  return args.unnamed.at(0);
}

goos::Object MakeSystem::handle_get_gsrc_folder(
    const goos::Object& form,
    goos::Arguments& args,
    const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {}, {});

  std::string out;
  int idx = 0;
  for (const auto& part : m_gsrc_folder) {
    if (idx++ > 0) {
      out += '/';
    }
    out += part;
  }
  return goos::StringObject::make_new(out);
}

goos::Object MakeSystem::handle_get_game_version_folder(
    const goos::Object& form,
    goos::Arguments& args,
    const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {}, {});
  if (m_repl_config) {
    return goos::StringObject::make_new(m_repl_config->game_version_folder);
  } else {
    return goos::StringObject::make_new("");
  }
}

void MakeSystem::get_dependencies(const std::string& master_target,
                                  const std::string& output,
                                  std::vector<std::string>* result,
                                  std::unordered_set<std::string>* result_set) const {
  // fmt::print(output + "\n");
  if (result_set->find(output) != result_set->end()) {
    return;
  }

  const auto& rule_it = m_output_to_step.find(output);
  if (rule_it == m_output_to_step.end()) {
    throw std::runtime_error(
        fmt::format("No rule to make {}, required for {}\n", output, master_target));
  }

  // what deps do we need?
  for (auto& dep : rule_it->second->deps) {
    get_dependencies(master_target, dep, result, result_set);
  }

  const auto& rule = rule_it->second;
  for (auto& dep : m_tools.at(rule->tool)
                       ->get_additional_dependencies(
                           {rule->input, rule->deps, rule->outputs, rule->arg}, m_path_map)) {
    get_dependencies(master_target, dep, result, result_set);
  }

  result->push_back(output);
  for (auto& op : rule->outputs) {
    result_set->insert(op);
  }
}

std::vector<std::string> MakeSystem::get_dependencies(const std::string& target) const {
  Timer timer;

  std::vector<std::string> result;
  std::unordered_set<std::string> added_deps;

  get_dependencies(target, target, &result, &added_deps);

  lg::print("Successfully found all {} dependencies for target in {:.3f}s\n", result.size(),
            timer.getSeconds());
  return result;
}

void MakeSystem::add_tool(std::shared_ptr<Tool> tool) {
  auto& name = tool->name();
  ASSERT(m_tools.find(name) == m_tools.end());
  m_tools[name] = tool;
}

std::vector<std::string> MakeSystem::filter_dependencies(const std::vector<std::string>& all_deps) {
  Timer timer;
  std::vector<std::string> result;
  std::unordered_set<std::string> stale_deps;

  std::unordered_set<std::string> compiler_sym_needed;

  for (auto& to_make : all_deps) {
    auto& rule = m_output_to_step.at(to_make);
    auto& tool = m_tools.at(rule->tool);
    const ToolInput task = {rule->input, rule->deps, rule->outputs, rule->arg};

    bool added = false;

    if (tool->needs_run(task, m_path_map)) {
      result.push_back(to_make);
      stale_deps.insert(to_make);
      added = true;
    }

    if (!added) {
      // check transitive dependencies
      for (auto& dep : rule->deps) {
        if (stale_deps.find(dep) != stale_deps.end()) {
          result.push_back(to_make);
          stale_deps.insert(to_make);
          added = true;
          break;
        }
      }
    }

    if (!added) {
      // check transitive dependencies
      for (auto& dep : tool->get_additional_dependencies(task, m_path_map)) {
        if (stale_deps.find(dep) != stale_deps.end()) {
          result.push_back(to_make);
          stale_deps.insert(to_make);
          added = true;
          break;
        }
      }
    }
  }

  lg::print("Found that {} of {} targets do need rebuilding in {:.3f}s\n", result.size(),
            all_deps.size(), timer.getSeconds());
  return result;
}

namespace {
void print_input(const std::vector<std::string>& in, char end) {
  int i = 0;
  std::string all_names;
  for (auto& name : in) {
    if (i++ > 0) {
      all_names += ", ";
    }
    all_names += name;
  }
  if (all_names.length() > 70) {
    lg::print("{}...{}", all_names.substr(0, 70 - 3), end);
  } else {
    lg::print("{}{}{}", all_names, std::string(70 - all_names.length(), ' '), end);
  }
}
}  // namespace

bool MakeSystem::make(const std::string& target_in, bool force, bool verbose) {
  std::string target = m_path_map.apply_remaps(target_in);
  auto deps = get_dependencies(target);
  //  lg::print("All deps:\n");
  //  for (auto& dep : deps) {
  //    lg::print("{}\n", dep);
  //  }
  if (!force) {
    deps = filter_dependencies(deps);
  }

  //  lg::print("Filt deps:\n");
  //  for (auto& dep : filtered_deps) {
  //    lg::print("{}\n", dep);
  //  }

  Timer make_timer;
  lg::print("Building {} targets...\n", deps.size());
  int i = 0;
  for (auto& to_make : deps) {
    Timer step_timer;
    auto& rule = m_output_to_step.at(to_make);
    auto& tool = m_tools.at(rule->tool);
    int percent = (100.0 * (1 + (i++)) / (deps.size())) + 0.5;
    if (verbose) {
      lg::print("[{:3d}%] [{:8s}] {}{}\n", percent, tool->name(), rule->input.at(0),
                rule->input.size() > 1 ? ", ..." : "");
    } else {
      lg::print("[{:3d}%] [{:8s}]       ", percent, tool->name());
      print_input(rule->input, '\r');
    }

    bool success = false;
    try {
      success = tool->run({rule->input, rule->deps, rule->outputs, rule->arg}, m_path_map);
    } catch (std::exception& e) {
      lg::print("\n");
      lg::print("Error: {}\n", e.what());
    }
    if (!success) {
      lg::print("Build failed on {}{}\n", rule->input.at(0), rule->input.size() > 1 ? ", ..." : "");
      throw std::runtime_error("Build failed.");
      return false;
    }

    if (verbose) {
      if (step_timer.getSeconds() > 0.05) {
        lg::print(fg(fmt::color::yellow), " {:.3f}\n", step_timer.getSeconds());
      } else {
        lg::print(" {:.3f}\n", step_timer.getSeconds());
      }
    } else {
      if (step_timer.getSeconds() > 0.05) {
        lg::print("[{:3d}%] [{:8s}] ", percent, tool->name());
        lg::print(fg(fmt::color::yellow), "{:.3f} ", step_timer.getSeconds());
        print_input(rule->input, '\n');
      } else {
        lg::print("[{:3d}%] [{:8s}] {:.3f} ", percent, tool->name(), step_timer.getSeconds());
        print_input(rule->input, '\n');
      }
    }
  }
  lg::print("\nSuccessfully built all {} targets in {:.3f}s\n", deps.size(),
            make_timer.getSeconds());
  return true;
}

void MakeSystem::set_constant(const std::string& name, const std::string& value) {
  m_goos.set_global_variable_by_name(name, goos::StringObject::make_new(value));
}

void MakeSystem::set_constant(const std::string& name, bool value) {
  m_goos.set_global_variable_to_symbol(name, value ? "#t" : "#f");
}
