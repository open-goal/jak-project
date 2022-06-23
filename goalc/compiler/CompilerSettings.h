#pragma once

#ifndef JAK_COMPILERSETTINGS_H
#define JAK_COMPILERSETTINGS_H

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
  bool print_timing = false;

  void set(const std::string& name, const goos::Object& value);

 private:
  void link(bool& val, const std::string& name);
  enum class SettingKind { BOOL, INVALID };

  struct SettingsEntry {
    SettingKind kind = SettingKind::INVALID;
    goos::Object value;
    bool* boolp = nullptr;
  };

  std::unordered_map<std::string, SettingsEntry> m_settings;
};

#endif  // JAK_COMPILERSETTINGS_H
