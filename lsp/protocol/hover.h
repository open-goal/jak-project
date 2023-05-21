#pragma once

#include "common_types.h"

namespace LSPSpec {
struct Hover {
  /// @brief The hover's content
  MarkupContent m_contents;
  /// @brief An optional range is a range inside a text document that is used to visualize a hover,
  /// e.g. by changing the background color.
  std::optional<Range> m_range;
};

void to_json(json& j, const Hover& obj);
void from_json(const json& j, Hover& obj);

}  // namespace LSPSpec
