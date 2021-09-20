#pragma once

#include "common/util/assert.h"
#include <cstring>
#include <string>
#include <vector>

class Serializer {
 public:
  Serializer() : m_writing(true) {
    const size_t initial_size = 32;
    m_data = (u8*)malloc(initial_size);
    m_size = initial_size;
  }
  Serializer(const u8* data, size_t size) : m_size(size), m_writing(false) {
    m_data = (u8*)malloc(size);
    memcpy(m_data, data, size);
  }

  template <typename T>
  void from_ptr(T* ptr) {
    read_or_write(ptr, sizeof(T));
  }

  void from_raw_data(void* ptr, size_t size) { read_or_write(ptr, size); }

  template <typename T>
  T load() {
    assert(!m_writing);
    T result;
    read_or_write(&result, sizeof(T));
    return result;
  }

  template <typename T>
  void save(const T& thing) {
    assert(m_writing);
    read_or_write(const_cast<T*>(&thing), sizeof(T));
  }

  std::pair<const u8*, size_t> get_save_result() {
    assert(m_writing);
    return {m_data, m_offset};
  }

  bool get_load_finished() const {
    assert(!m_writing);
    return m_offset == m_size;
  }

  bool is_saving() const { return m_writing; }

  bool is_loading() const { return !m_writing; }

  void save_str(const std::string* str) {
    assert(is_saving());
    from_str(const_cast<std::string*>(str));
  }

  std::string load_string() {
    assert(is_loading());
    std::string s;
    from_str(&s);
    return s;
  }

  void from_str(std::string* str) {
    if (is_loading()) {
      str->resize(load<size_t>());
    } else {
      save<size_t>(str->size());
    }
    from_raw_data(str->data(), str->size());
  }

  template <typename T>
  void from_pod_vector(std::vector<T>* vec) {
    if (is_saving()) {
      save<size_t>(vec->size());
    } else {
      vec->resize(load<size_t>());
    }
    from_raw_data(vec->data(), vec->size());
  }

  Serializer(const Serializer& other) = delete;
  Serializer& operator=(const Serializer& other) = delete;

  Serializer& operator=(Serializer&& other) noexcept {
    if (this == &other) {
      return *this;
    }

    m_data = other.m_data;
    m_size = other.m_size;
    m_offset = other.m_offset;
    m_writing = other.m_writing;

    other.m_data = nullptr;
    other.m_size = 0;

    return *this;
  }

  ~Serializer() { free(m_data); }

  void reset_load() {
    assert(is_loading());
    m_offset = 0;
  }

  size_t data_size() const { return m_size; }

 private:
  void read_or_write(void* data, size_t size) {
    if (m_writing) {
      if (m_offset + size > m_size) {
        m_data = (u8*)realloc(m_data, (m_offset + size) * 2);
      }
      memcpy(m_data + m_offset, data, size);
    } else {
      assert(m_offset + size <= m_size);
      memcpy(data, m_data + m_offset, size);
    }
    m_offset += size;
  }

  u8* m_data = nullptr;
  size_t m_size = 0;
  size_t m_offset = 0;
  bool m_writing = false;
};