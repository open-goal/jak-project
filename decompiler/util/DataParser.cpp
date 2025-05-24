#include "DataParser.h"

#include <stdexcept>
#include <unordered_map>

#include "common/util/Assert.h"

#include "fmt/core.h"

/*
 * Allowable lines:
 * L123:             - label
 * L123: (offset 2)  - label with an offset (only 2 allowed)
 * .word 0xbeef      - a hex word
 * .word L123        - a label word
 * .symbol sym       - a symbol
 * .empty-list       - the empty list
 * .type typ         - a type
 */

namespace decompiler {
namespace {
std::vector<std::string> string_to_lines(const std::string& str) {
  std::vector<std::string> result;
  std::string::size_type i;
  std::string::size_type start = 0;
  while (true) {
    i = str.find('\n', start);
    if (i == std::string::npos) {
      if (start < str.length()) {
        result.push_back(str.substr(start));
      }
      return result;
    } else {
      result.push_back(str.substr(start, i - start));
      start = i + 1;
    }
  }
}

std::string get_until_space(std::string& instr) {
  ASSERT(!instr.empty());
  size_t i;
  for (i = 0; i < instr.length(); i++) {
    if (instr[i] == ' ') {
      break;
    }
  }
  auto name = instr.substr(0, i);
  if (i == instr.length()) {
    instr.clear();
  } else {
    instr = instr.substr(i + 1);
  }
  return name;
}

}  // namespace
ParsedData parse_data(const std::string& str) {
  ParsedData result;
  struct LabelInfo {
    int idx = -1;
    bool defined = false;
  };

  std::unordered_map<std::string, LabelInfo> label_map;

  const std::string offset_2 = ": (offset 2)";
  auto lines = string_to_lines(str);
  int byte_offset = 0;
  for (auto& line : lines) {
    // strip off leading white space
    size_t i;
    for (i = 0; i < line.length(); i++) {
      if (line[i] != ' ') {
        line = line.substr(i);
        break;
      }
    }

    if (line.empty()) {
      continue;
    }

    // try as label definition.
    if (line.front() == 'L') {
      int offset = 0;
      if (line.back() == ':') {
        line.pop_back();
      } else {
        if (line.length() >= (2 + offset_2.length()) &&
            line.substr(line.length() - offset_2.length()) == offset_2) {
          line = line.substr(0, line.length() - offset_2.length());
          offset = 2;
        } else {
          throw std::runtime_error(fmt::format("Invalid label line: {}", line));
        }
      }

      auto& l = label_map[line];
      if (l.defined) {
        throw std::runtime_error(fmt::format("Label {} is multiply defined.", line));
      }

      l.defined = true;
      if (l.idx == -1) {
        l.idx = result.labels.size();
        result.labels.emplace_back();
      }

      auto& label = result.labels.at(l.idx);
      label.target_segment = 0;
      label.offset = byte_offset + offset;
      label.name = line;
      continue;
    }

    auto first_thing = get_until_space(line);

    // try as .type
    if (first_thing == ".type") {
      LinkedWord word(0);
      word.set_to_symbol(LinkedWord::TYPE_PTR, line);
      result.words.push_back(word);
      byte_offset += 4;
      continue;
    }

    if (first_thing == ".symbol") {
      LinkedWord word(0);
      word.set_to_symbol(LinkedWord::SYM_PTR, line);
      result.words.push_back(word);
      byte_offset += 4;
      continue;
    }

    if (first_thing == ".empty-list") {
      if (!line.empty()) {
        throw std::runtime_error("Got something after .empty-list, this is not allowed");
      }
      LinkedWord word(0);
      word.set_to_empty_ptr();
      result.words.push_back(word);
      byte_offset += 4;
      continue;
    }

    if (first_thing == ".word") {
      if (!line.empty() && line.at(0) == 'L') {
        auto& l = label_map[line];
        if (l.idx == -1) {
          l.idx = result.labels.size();
          result.labels.emplace_back();
        }
        LinkedWord word(0);
        word.set_to_pointer(LinkedWord::PTR, l.idx);
        result.words.push_back(word);
        byte_offset += 4;
        continue;
      } else {
        auto val = std::stoull(line, nullptr, 16);
        ASSERT(val <= UINT32_MAX);
        LinkedWord word(val);
        word.set_to_plain_data();
        result.words.push_back(word);
        byte_offset += 4;
        continue;
      }
    }
  }

  for (auto& kv : label_map) {
    if (!kv.second.defined) {
      throw std::runtime_error(fmt::format("Label {} was used but not defined.", kv.first));
    }
  }

  return result;
}

std::string ParsedData::print() const {
  std::string result;
  std::unordered_map<int, const DecompilerLabel*> label_map;
  for (const auto& x : labels) {
    label_map[x.offset] = &x;
  }

  for (size_t idx = 0; idx < words.size(); idx++) {
    // print label
    auto kv = label_map.find(idx * 4);
    if (kv != label_map.end()) {
      result += fmt::format("{}:\n", kv->second->name);
    }
    auto kv_offset = label_map.find(idx * 4 + 2);
    if (kv_offset != label_map.end()) {
      result += fmt::format("{}: (offset 2)\n", kv_offset->second->name);
    }

    // print word
    auto& word = words.at(idx);
    switch (word.kind()) {
      case LinkedWord::PLAIN_DATA:
        result += fmt::format("    .word 0x{:x}\n", word.data);
        break;
      case LinkedWord::PTR:
        result += fmt::format("    .word {}\n", labels.at(word.label_id()).name);
        break;
      case LinkedWord::SYM_PTR:
        result += fmt::format("    .symbol {}\n", word.symbol_name());
        break;
      case LinkedWord::TYPE_PTR:
        result += fmt::format("    .type {}\n", word.symbol_name());
        break;
      case LinkedWord::EMPTY_PTR:
        result += "    .empty-list\n";
        break;
      default:
        ASSERT(false);
    }
  }

  return result;
}

const DecompilerLabel& ParsedData::label(const std::string& name) const {
  for (auto& x : labels) {
    if (x.name == name) {
      return x;
    }
  }
  throw std::runtime_error("Could not find label " + name);
}
}  // namespace decompiler
