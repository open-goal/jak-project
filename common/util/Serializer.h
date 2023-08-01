#pragma once

#include <cstring>
#include <string>
#include <vector>

#include "common/common_types.h"
#include "common/util/Assert.h"

/*!
 * The Serializer is a tool to load or save data from a buffer.
 * It's currently used to save graphics dumps, but could also be used for savestates in the future.
 *
 * The serializer can be constructed in either a "save" or a "load" mode, for saving or loading.
 * When saving, it copies stuff into a buffer.
 * When loading, it copies stuff out of this buffer.  This will only work if you do things in
 * exactly the same order. To make this easier, most of the commonly used functions can work for
 * either saving or loading, so you can have a method like
 *
 * void MyObject::serialize(Serializer& ser) {
 *    ser.from_ptr(&m_foo);
 *    ser.from_ptr(&m_bar);
 * }
 *
 * and it will work for either saving or loading.
 *
 * Methods that are named like "save_" can only be used in save mode.
 *
 * Methods like "from_ptr" can work in either save or load mode, and will work in either.
 * In general, you can only use most of these on POD, and saving more complicated data structures
 * requires special handling.
 */
class Serializer {
 public:
  /*!
   * Construct a serializer in writing mode. This saves data from the program into a buffer that can
   * later be accessed with get_save_result.
   */
  Serializer() : m_writing(true) {
    constexpr size_t initial_size = 32;
    m_data = (u8*)malloc(initial_size);
    m_size = initial_size;
  }

  /*!
   * Construct a serializer that reads from the given data.
   * The data is copied to an internal buffer managed by the serializer, there is no need to keep
   * the input data around.
   */
  Serializer(const u8* data, size_t size) : m_size(size), m_writing(false) {
    m_data = (u8*)malloc(size);
    memcpy(m_data, data, size);
  }

  // don't allow copying, assigning, or move constructing.
  Serializer(const Serializer& other) = delete;
  Serializer& operator=(const Serializer& other) = delete;
  Serializer(Serializer&& other) = delete;

  // move assignment is supported.
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

  /*!
   * Save or load the thing pointed to by ptr.
   * T must be POD.
   */
  template <typename T>
  void from_ptr(T* ptr) {
    read_or_write(ptr, sizeof(T));
  }

  /*!
   * Save or load size bytes from ptr.
   */
  void from_raw_data(void* ptr, size_t size) { read_or_write(ptr, size); }

  /*!
   * Load a T. T should be POD.
   */
  template <typename T>
  T load() {
    ASSERT(!m_writing);
    T result;
    read_or_write(&result, sizeof(T));
    return result;
  }

  /*!
   * Save a T. T should be POD.
   */
  template <typename T>
  void save(const T& thing) {
    ASSERT(m_writing);
    read_or_write(const_cast<T*>(&thing), sizeof(T));
  }

  /*!
   * Save or load a string.
   */
  void from_str(std::string* str) {
    // size, then data.
    if (is_loading()) {
      str->resize(load<size_t>());
    } else {
      save<size_t>(str->size());
    }
    from_raw_data(str->data(), str->size());
  }

  /*!
   * Save a std::string. This is just so you can save a const string without needing to copy or do a
   * const_cast.
   */
  void save_str(const std::string* str) {
    ASSERT(is_saving());
    // safe, we're saving, so from_str will only read.
    from_str(const_cast<std::string*>(str));
  }

  /*!
   * Load a std::string and return it.
   */
  std::string load_string() {
    ASSERT(is_loading());
    std::string s;
    from_str(&s);
    return s;
  }

  /*!
   * Save or load a vector of POD. This won't work on vectors of vectors, for example.
   */
  template <typename T>
  void from_pod_vector(std::vector<T>* vec) {
    if (is_saving()) {
      save<size_t>(vec->size());
    } else {
      vec->resize(load<size_t>());
    }
    from_raw_data(vec->data(), sizeof(T) * vec->size());
  }

  void from_string_vector(std::vector<std::string>* vec) {
    if (is_saving()) {
      save<size_t>(vec->size());
    } else {
      vec->resize(load<size_t>());
    }
    for (auto& str : *vec) {
      from_str(&str);
    }
  }

  /*!
   * Are we saving?
   */
  bool is_saving() const { return m_writing; }

  /*!
   * Are we loading?
   */
  bool is_loading() const { return !m_writing; }

  /*!
   * Reset a load back to the beginning.
   */
  void reset_load() {
    ASSERT(is_loading());
    m_offset = 0;
  }

  /*!
   * Get the result of the save. This is a view of the buffer owned by the Serializer.
   */
  std::pair<const u8*, size_t> get_save_result() {
    ASSERT(m_writing);
    return {m_data, m_offset};
  }

  /*!
   * Have we reached the end of the load?
   */
  bool get_load_finished() const {
    ASSERT(!m_writing);
    return m_offset == m_size;
  }

  /*!
   * Size of buffer, in bytes.
   */
  size_t data_size() const { return m_size; }

 private:
  /*!
   * Main function to read and write the buffer.
   */
  void read_or_write(void* data, size_t size) {
    if (m_writing) {
      // if we would overflow, just resize the buffer.
      if (m_offset + size > m_size) {
        m_size = (m_offset + size) * 2;
        m_data = (u8*)realloc(m_data, m_size);
      }
      memcpy(m_data + m_offset, data, size);
    } else {
      // if we would overflow, it's an error.
      ASSERT(m_offset + size <= m_size);
      memcpy(data, m_data + m_offset, size);
    }
    m_offset += size;
  }

  u8* m_data = nullptr;
  size_t m_size = 0;
  size_t m_offset = 0;
  bool m_writing = false;
};
