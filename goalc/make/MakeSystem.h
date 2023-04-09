#pragma once

#include "common/goos/Interpreter.h"

#include "goalc/make/Tool.h"

struct MakeStep {
  std::vector<std::string> input;
  std::vector<std::string> deps, outputs;
  goos::Object arg;
  std::string tool;

  std::string print() const;
};

class MakeSystem {
 public:
  MakeSystem(const std::optional<REPL::Config> repl_config, const std::string& username = "#f");
  void load_project_file(const std::string& file_path);

  goos::Object handle_defstep(const goos::Object& obj,
                              goos::Arguments& args,
                              const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_basename(const goos::Object& obj,
                               goos::Arguments& args,
                               const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_stem(const goos::Object& obj,
                           goos::Arguments&,
                           const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_get_gsrc_path(const goos::Object& obj,
                                    goos::Arguments&,
                                    const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_map_path(const goos::Object& obj,
                               goos::Arguments& args,
                               const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_set_output_prefix(const goos::Object& obj,
                                        goos::Arguments& args,
                                        const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_set_gsrc_folder(const goos::Object& obj,
                                      goos::Arguments& args,
                                      const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_get_gsrc_folder(const goos::Object& obj,
                                      goos::Arguments& args,
                                      const std::shared_ptr<goos::EnvironmentObject>& env);

  goos::Object handle_get_game_version_folder(const goos::Object& obj,
                                              goos::Arguments&,
                                              const std::shared_ptr<goos::EnvironmentObject>& env);

  std::vector<std::string> get_dependencies(const std::string& target) const;
  std::vector<std::string> filter_dependencies(const std::vector<std::string>& all_deps);

  bool make(const std::string& target, bool force, bool verbose);

  void add_tool(std::shared_ptr<Tool> tool);
  void set_constant(const std::string& name, const std::string& value);
  void set_constant(const std::string& name, bool value);

  template <typename T>
  void add_tool() {
    add_tool(std::make_shared<T>());
  }

  void clear_project();

  /*!
   * Get the prefix that the project has requested for all compiler outputs
   */
  const std::string& compiler_output_prefix() const { return m_path_map.output_prefix; }

 private:
  void va_check(const goos::Object& form,
                const goos::Arguments& args,
                const std::vector<std::optional<goos::ObjectType>>& unnamed,
                const std::unordered_map<std::string,
                                         std::pair<bool, std::optional<goos::ObjectType>>>& named);

  void get_dependencies(const std::string& master_target,
                        const std::string& output,
                        std::vector<std::string>* result_order,
                        std::unordered_set<std::string>* result_set) const;

  goos::Interpreter m_goos;

  std::optional<REPL::Config> m_repl_config;

  std::unordered_map<std::string, std::shared_ptr<MakeStep>> m_output_to_step;
  std::unordered_map<std::string, std::shared_ptr<Tool>> m_tools;
  PathMap m_path_map;
  std::vector<std::string> m_gsrc_folder;
  std::map<std::string, std::string> m_gsrc_files = {};
};
