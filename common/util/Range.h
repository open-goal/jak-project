#pragma once

#include <iterator>

/*!
 * A Range is similar to a Python range. It can represent an empty range or a range of consecutive
 * values.  It supports iterators so you can write a loop like:
 *   for (auto x : Range<int>(1, 4)) {
 *   }
 * and it will work like you expect, without allocating memory for all the elements.
 *
 * A Range's iterator actually store values, not references, so this is designed to be used
 * with small things, like integers.
 *
 * Note - for size and iterator distances, a Range will use the same type as its elements.
 */
template <typename T>
class Range {
 public:
  Range() = default;
  Range(const T& start, const T& end) : m_start(start), m_end(end) {}
  const T& first() const { return m_start; }
  const T& last() const { return m_end; }
  T& first() { return m_start; }
  T& last() { return m_end; }
  bool contains(T& val) const { return val >= m_start && val < m_end; }
  bool empty() const { return m_end <= m_start; }
  T size() const { return m_end - m_start; }
  bool operator==(const Range<T>& other) const {
    return m_start == other.m_start && m_end == other.m_end;
  }
  bool operator!=(const Range<T>& other) const { return !((*this) == other); }

  struct Iterator {
    using iterator_category = std::input_iterator_tag;
    using difference_type = T;
    using value_type = T;
    using pointer = const T*;
    using reference = const T&;

    Iterator(const T& val) : m_val(val) {}

    reference operator*() const { return m_val; }
    pointer operator->() { return &m_val; }

    Iterator& operator++() {
      m_val++;
      return *this;
    }

    const Iterator operator++(int) {
      Iterator old = *this;
      ++(*this);
      return old;
    }

    friend bool operator==(const Iterator& a, const Iterator& b) { return a.m_val == b.m_val; }
    friend bool operator!=(const Iterator& a, const Iterator& b) { return a.m_val != b.m_val; }

   private:
    T m_val;
  };

  Iterator begin() { return Iterator(m_start); }
  Iterator end() { return Iterator(m_end); }

 private:
  T m_start = {};
  T m_end = {};
};