#pragma once

/*!
 * @file DataParser.h
 * A parser for the decompiled GOAL data format.
 */

#include <vector>
#include <string>
#include "decompiler/ObjectFile/LinkedWord.h"
#include "decompiler/Disasm/DecompilerLabel.h"

namespace decompiler {
struct ParsedData {
  std::vector<LinkedWord> words;
  std::vector<DecompilerLabel> labels;
  std::string print() const;
  const DecompilerLabel& label(const std::string& name) const;
};

ParsedData parse_data(const std::string& str);

}  // namespace decompiler