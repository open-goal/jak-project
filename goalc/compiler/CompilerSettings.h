#pragma once

#include <string>
#include <unordered_map>

#include "common/goos/Object.h"

class CompilerSettings {
 public:
  CompilerSettings();
  bool debug_print_ir = false;
  bool debug_print_regalloc = false;
  bool disable_math_const_prop = false;
  bool emit_move_after_return = true;
  bool check_for_requires = false;  // check for missing 'require' statements (TODO - does not work
                                    // for virtual state usages or macro usages)

  void set(const std::string& name, const goos::Object& value);

 private:
  void link(bool& val, const std::string& name);

  enum class SettingKind { BOOL, STRING, INVALID };

  struct SettingsEntry {
    SettingKind kind = SettingKind::INVALID;
    goos::Object value;
    bool* boolp = nullptr;
  };

  std::unordered_map<std::string, SettingsEntry> m_settings;
};
