#pragma once
#include <vector>
#include <cassert>
#include "common/common_types.h"

class IRegSet {
 public:
  IRegSet() = default;
  void insert(int x) {
    resize(x + 1);
    assert(m_bits > x);
    auto word = x / 64;
    auto bit = x % 64;
    m_data.at(word) |= (1ll << bit);
  }

  void clear() {
    for (auto& x : m_data) {
      x = 0;
    }
  }

  bool operator[](int x) {
    resize(x + 1);
    auto word = x / 64;
    auto bit = x % 64;
    return m_data.at(word) & (1ll << bit);
  }

  int size() const { return m_bits; }

  bool operator==(const IRegSet& other) const {
    auto compare_size = std::min(m_data.size(), other.m_data.size());
    size_t i;
    for (i = 0; i < compare_size; i++) {
      if (m_data[i] != other.m_data[i]) {
        return false;
      }
    }

    for (size_t j = i; j < m_data.size(); j++) {
      if (m_data[j]) {
        return false;
      }
    }

    for (size_t j = i; j < other.m_data.size(); j++) {
      if (other.m_data[j]) {
        return false;
      }
    }
    return true;
  }

  bool operator!=(const IRegSet& other) const { return !((*this) == other); }

  void resize(int bits) {
    if (bits > m_bits) {
      auto new_vector_size = (bits + 63) / 64;
      m_data.resize(new_vector_size);
      m_bits = new_vector_size * 64;
    }
  }

 private:
  std::vector<u64> m_data;
  int m_bits = 0;
};