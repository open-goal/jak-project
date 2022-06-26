#pragma once
/*!
 * @file IRegSet.h
 * An efficient implementation of a set of IRegs.
 * This takes advantage of the fact that:
 *  - IReg id values are pretty densely packed (ie no huge gaps in ids)
 *  - Generally there are ~hundreds of IRegs in complicated functions
 * and has a bit per ireg.
 *
 * The container dynamically increases size as needed. Uninitialized values are 0.
 * Doing operations on pairs of sets will resize both to be the max size.
 * This was found to be more efficient in the end.
 *
 * The space used is (highest_set_reg + 63) / 64 + constant overhead (vector + int)
 */
#include <vector>

#include "common/common_types.h"
#include "common/util/Assert.h"

class IRegSet {
 public:
  IRegSet() { resize(64 * 4); }

  /*!
   * Add the given ireg to the set.
   * Resizes if needed.
   */
  void insert(int x) {
    resize(x + 1);
    ASSERT(m_bits > x);
    auto word = x / 64;
    auto bit = x % 64;
    m_data.at(word) |= (1ll << bit);
  }

  /*!
   * Remove everything from the set.
   */
  void clear() {
    for (auto& x : m_data) {
      x = 0;
    }
  }

  /*!
   * Is the given register in the set?
   * Will resize if needed. (todo - is this better than not resizing?)
   */
  bool operator[](int x) {
    resize(x + 1);
    auto word = x / 64;
    auto bit = x % 64;
    return m_data.at(word) & (1ll << bit);
  }

  /*!
   * The size is (maximum value we can access with operator[] without resizing) - 1
   */
  int size() const { return m_bits; }

  /*!
   * Is this the same set?
   * Doesn't resize either.
   */
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

  void make_max_size(IRegSet& other) {
    if (other.size() < size()) {
      other.resize(size());
    } else {
      resize(other.size());
    }
  }

  /*!
   * this = (this & !other).
   * Resizes the smaller one.
   */
  void bitwise_and_not(IRegSet& other) {
    // make same size
    make_max_size(other);

    for (size_t i = 0; i < m_data.size(); i++) {
      m_data.at(i) &= ~other.m_data.at(i);
    }
  }

  /*!
   * this = (this | other)
   * Resizes the smaller one
   */
  void bitwise_or(IRegSet& other) {
    make_max_size(other);

    for (size_t i = 0; i < m_data.size(); i++) {
      m_data.at(i) |= other.m_data.at(i);
    }
  }

 private:
  std::vector<u64> m_data;
  int m_bits = 0;
};
