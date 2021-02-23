#pragma once

#include <string>

namespace decompiler {
/*!
 * A label to a location in an object file.
 * Doesn't have to be word aligned.
 */
struct DecompilerLabel {
  std::string name;
  int target_segment = 0;
  int offset;  // in bytes
};
}  // namespace decompiler