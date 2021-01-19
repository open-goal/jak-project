#pragma once

#include "common/common_types.h"
#include <string>
#include <map>
#include <vector>

class DataObjectGenerator {
 public:
  int add_word(u32 word);
  void link_word_to_word(int source, int target, int offset = 0);
  void link_word_to_byte(int source_word, int target_byte);
  int add_ref_to_string_in_pool(const std::string& str);
  int add_type_tag(const std::string& str);
  int add_symbol_link(const std::string& str);
  std::vector<u8> generate_v2();
  std::vector<u8> generate_v4();
  void align(int alignment_words);
  int words() const;

 private:
  void add_strings();
  std::vector<u8> generate_link_table();

  struct PointerLinkRecord {
    int source_word;
    int target_byte;
  };

  std::map<std::string, std::vector<int>> m_string_pool;
  std::vector<u32> m_words;
  std::vector<PointerLinkRecord> m_ptr_links;

  // both alphabetical.
  // symbols before types.
  std::map<std::string, std::vector<int>> m_type_links, m_symbol_links;
};
