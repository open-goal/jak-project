#include <iostream>
#include <stdexcept>
#include <string>
#include <vector>

#include "common/util/FileUtil.h"
#include "decompiler/ObjectFile/LinkedWord.h"
#include "decompiler/VuDisasm/VuDisassembler.h"
#include "decompiler/util/DataParser.h"

int main(int argc, char** argv) {
  if (argc != 3) {
    std::cerr << "usage: vu-disasm <vu0|vu1> <word-data-file>\n";
    return 1;
  }

  try {
    decompiler::VuDisassembler::VuKind kind;
    const std::string kind_name = argv[1];
    if (kind_name == "vu0") {
      kind = decompiler::VuDisassembler::VuKind::VU0;
    } else if (kind_name == "vu1") {
      kind = decompiler::VuDisassembler::VuKind::VU1;
    } else {
      throw std::runtime_error("kind must be vu0 or vu1");
    }

    const auto parsed =
        decompiler::parse_data(file_util::read_text_file(std::string(argv[2])));
    std::vector<u32> words;
    words.reserve(parsed.words.size());
    for (const auto& word : parsed.words) {
      if (word.kind() != decompiler::LinkedWord::Kind::PLAIN_DATA) {
        throw std::runtime_error("VU input must contain only plain .word data");
      }
      words.push_back(word.data);
    }
    if (words.empty() || words.size() % 2) {
      throw std::runtime_error("VU input must contain a nonempty whole number of instruction pairs");
    }

    decompiler::VuDisassembler disassembler(kind);
    const auto program =
        disassembler.disassemble(words.data(), static_cast<int>(words.size() * sizeof(u32)));
    std::cout << disassembler.to_string(program);
  } catch (const std::exception& error) {
    std::cerr << "vu-disasm: " << error.what() << '\n';
    return 1;
  }
  return 0;
}
