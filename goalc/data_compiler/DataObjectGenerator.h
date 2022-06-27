#pragma once

#include <map>
#include <string>
#include <vector>

#include "common/common_types.h"

class DataObjectGenerator {
 public:
  int add_word(u32 word);
  int add_word_float(float f);
  void set_word(u32 word_idx, u32 val);
  void link_word_to_word(int source, int target, int offset = 0);
  void link_word_to_byte(int source_word, int target_byte);
  int add_ref_to_string_in_pool(const std::string& str);
  void link_word_to_string_in_pool(const std::string& str, int word_idx);
  int add_type_tag(const std::string& str);
  int add_symbol_link(const std::string& str);
  void link_word_to_symbol(const std::string& str, int word_idx);
  std::vector<u8> generate_v2();
  std::vector<u8> generate_v4();
  void align(int alignment_words);
  void align_to_basic();
  int words() const;
  size_t current_offset_bytes() const { return m_words.size() * sizeof(u32); }
  u8* data() { return (u8*)m_words.data(); }

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
