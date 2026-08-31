#pragma once

#include <optional>
#include <string>

#include "common/util/json_util.h"

#if defined(__SWITCH__)
#include "game/switch/imgui_stub.h"
#else
#include "third-party/imgui/imgui.h"
#endif

struct DebugTextFilter {
  enum class Type { CONTAINS, NOT_CONTAINS, REGEX };

  std::string content;
  Type type;
};

void to_json(json& j, const DebugTextFilter& obj);
void from_json(const json& j, DebugTextFilter& obj);

class FiltersMenu {
 public:
  FiltersMenu();
  void draw_window();
};
