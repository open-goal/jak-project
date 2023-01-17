#pragma once

#include <optional>
#include <string>

#include "third-party/imgui/imgui.h"

struct DebugTextFilter {
  enum class Type { CONTAINS, NOT_CONTAINS, REGEX };

  std::string content;
  Type type;
};

class FiltersMenu {
 public:
  FiltersMenu();
  void draw_window();
};
