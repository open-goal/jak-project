#include <filesystem>

#include "MakeSystem.h"

#include "third-party/fmt/color.h"
#include "third-party/fmt/core.h"

#include "common/goos/ParseHelpers.h"
#include "common/util/Timer.h"

#include "goalc/make/Tools.h"

std::string MakeStep::print() const {
  std::string result = fmt::format("Tool {} with input \"{}\" and deps:\n ", tool, input);
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

MakeSystem::MakeSystem() {
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

  m_goos.set_global_variable_to_symbol("ASSETS", "#t");

  add_tool<DgoTool>();
  add_tool<TpageDirTool>();
  add_tool<CopyTool>();
  add_tool<GameCntTool>();
  add_tool<TextTool>();
  add_tool<GroupTool>();
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
}

goos::Object MakeSystem::handle_defstep(const goos::Object& form,
                                        goos::Arguments& args,
                                        const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {},
           {{"out", {true, {goos::ObjectType::PAIR}}},
            {"tool", {true, {goos::ObjectType::SYMBOL}}},
            {"in", {false, {goos::ObjectType::STRING}}},
            {"dep", {false, {}}}});

  auto step = std::make_shared<MakeStep>();

  goos::for_each_in_list(args.get_named("out"), [&](const goos::Object& obj) {
    step->outputs.push_back(obj.as_string()->data);
  });

  step->tool = args.get_named("tool").as_symbol()->name;

  if (m_tools.find(step->tool) == m_tools.end()) {
    throw std::runtime_error(fmt::format("The tool {} is unknown.", step->tool));
  }

  if (args.has_named("in")) {
    step->input = args.get_named("in").as_string()->data;
  }

  if (args.has_named("dep")) {
    goos::for_each_in_list(args.get_named("dep"), [&](const goos::Object& obj) {
      step->deps.push_back(obj.as_string()->data);
    });
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
  std::filesystem::path input(args.unnamed.at(0).as_string()->data);

  return goos::StringObject::make_new(input.filename().u8string());
}

goos::Object MakeSystem::handle_stem(const goos::Object& form,
                                     goos::Arguments& args,
                                     const std::shared_ptr<goos::EnvironmentObject>& env) {
  m_goos.eval_args(&args, env);
  va_check(form, args, {goos::ObjectType::STRING}, {});
  std::filesystem::path input(args.unnamed.at(0).as_string()->data);

  return goos::StringObject::make_new(input.stem().u8string());
}

void MakeSystem::get_dependencies(const std::string& master_target,
                                  const std::string& output,
                                  std::vector<std::string>* result,
                                  std::unordered_set<std::string>* result_set) const {
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
                       ->get_additional_dependencies({rule->input, rule->deps, rule->outputs})) {
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

  fmt::print("Successfully found all {} dependencies for target in {:.3f}s\n", result.size(),
             timer.getSeconds());
  return result;
}

void MakeSystem::add_tool(std::shared_ptr<Tool> tool) {
  auto& name = tool->name();
  assert(m_tools.find(name) == m_tools.end());
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

    bool added = false;

    if (tool->needs_run({rule->input, rule->deps, rule->outputs})) {
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
      for (auto& dep :
           tool->get_additional_dependencies({rule->input, rule->deps, rule->outputs})) {
        if (stale_deps.find(dep) != stale_deps.end()) {
          result.push_back(to_make);
          stale_deps.insert(to_make);
          added = true;
          break;
        }
      }
    }
  }

  fmt::print("Found that {} of {} targets do need rebuilding in {:.3f}s\n", result.size(),
             all_deps.size(), timer.getSeconds());
  return result;
}

namespace {
void print_input(const std::string& in, char end) {
  if (in.length() > 70) {
    fmt::print("{}...{}", in.substr(0, 70 - 3), end);
  } else {
    fmt::print("{}{}{}", in, std::string(70 - in.length(), ' '), end);
  }
}
}  // namespace

bool MakeSystem::make(const std::string& target, bool force, bool verbose) {
  auto deps = get_dependencies(target);
  //  fmt::print("All deps:\n");
  //  for (auto& dep : deps) {
  //    fmt::print("{}\n", dep);
  //  }
  if (!force) {
    deps = filter_dependencies(deps);
  }

  //  fmt::print("Filt deps:\n");
  //  for (auto& dep : filtered_deps) {
  //    fmt::print("{}\n", dep);
  //  }

  Timer make_timer;
  fmt::print("Building {} targets...\n", deps.size());
  int i = 0;
  for (auto& to_make : deps) {
    Timer step_timer;
    auto& rule = m_output_to_step.at(to_make);
    auto& tool = m_tools.at(rule->tool);
    int percent = (100.0 * (1 + (i++)) / (deps.size())) + 0.5;
    if (verbose) {
      fmt::print("[{:3d}%] [{:8s}] {}\n", percent, tool->name(), rule->input);
    } else {
      fmt::print("[{:3d}%] [{:8s}]       ", percent, tool->name());
      print_input(rule->input, '\r');
    }

    bool success = false;
    try {
      success = tool->run({rule->input, rule->deps, rule->outputs});
    } catch (std::exception& e) {
      fmt::print("\n");
      fmt::print("Error: {}\n", e.what());
    }
    if (!success) {
      fmt::print("Build failed on {}.\n", rule->input);
      throw std::runtime_error("Build failed.");
      return false;
    }

    if (verbose) {
      if (step_timer.getSeconds() > 0.05) {
        fmt::print(fg(fmt::color::yellow), " {:.3f}\n", step_timer.getSeconds());
      } else {
        fmt::print(" {:.3f}\n", step_timer.getSeconds());
      }
    } else {
      if (step_timer.getSeconds() > 0.05) {
        fmt::print("[{:3d}%] [{:8s}] ", percent, tool->name());
        fmt::print(fg(fmt::color::yellow), "{:.3f} ", step_timer.getSeconds());
        print_input(rule->input, '\n');
      } else {
        fmt::print("[{:3d}%] [{:8s}] {:.3f} ", percent, tool->name(), step_timer.getSeconds());
        if (tool->name() == "goalc") {
          print_input(rule->input, '\r');
        } else {
          print_input(rule->input, '\n');
        }
      }
    }
  }
  fmt::print("\nSuccessfully built all {} targets in {:.3f}s\n", deps.size(),
             make_timer.getSeconds());
  return true;
}