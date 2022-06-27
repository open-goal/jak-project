#pragma once
#include <cstring>
#include <stdexcept>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/util/Assert.h"

#include "decompiler/ObjectFile/LinkedWord.h"

namespace decompiler {
class LinkedWordReader {
 public:
  explicit LinkedWordReader(const std::vector<LinkedWord>* words) : m_words(words) {}
  std::string get_type_tag() {
    if (m_words->at(m_offset).kind() == LinkedWord::TYPE_PTR) {
      auto result = m_words->at(m_offset).symbol_name();
      m_offset++;
      return result;
    } else {
      ASSERT_MSG(false, "LinkedWordReader::get_type_tag failed");
    }
  }

  template <typename T>
  T get_word() {
    static_assert(sizeof(T) == 4, "size of word in get_word");
    T result;
    ASSERT(m_words->at(m_offset).kind() == LinkedWord::PLAIN_DATA);
    memcpy(&result, &m_words->at(m_offset).data, 4);
    m_offset++;
    return result;
  }

  u32 words_left() {
    ASSERT(m_words->size() >= m_offset);
    return m_words->size() - m_offset;
  }

 private:
  const std::vector<LinkedWord>* m_words = nullptr;
  u32 m_offset = 0;
};
}  // namespace decompiler
