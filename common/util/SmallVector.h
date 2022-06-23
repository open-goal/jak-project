#pragma once

#include <cstdint>
#include <iterator>
#include <new>
#include <type_traits>
#include <utility>

#include "common/util/Assert.h"

namespace cu {
// This might seem stupid, but compiling an empty file with #include <algorithm> takes 0.5 seconds.
// Avoiding the include for <algorithm> is a huge win for compile times.
template <typename T>
constexpr const T& max(const T& a, const T& b) {
  return (a < b) ? b : a;
}

template <typename T>
constexpr const T& min(const T& a, const T& b) {
  return (b < a) ? b : a;
}
/*!
 * A SmallVector is a replacement for std::vector.  It is optimized for the case where there
 * are only a few elements by having a small inline array of objects. This uses more memory, but
 * handles the very common case of having only a few elements much more efficiently.
 * It will work correctly even with many elements by falling back to allocating on the heap.
 * Even when it falls back to being a normal heap allocation, it is the same speed as a
 * std::vector for most operations. Resizing operations may be a tiny bit slower, but this
 * doesn't seem to cause issues in practice.
 *
 * You can specify the number of inline elements. If you don't exceed this number, it won't
 * allocate. By default, it picks the largest nonzero number of elements that keeps the size
 * under 128-bytes, which is somewhat arbitrary.
 *
 * It does not implement all features of a std::vector.
 * Missing:
 *  - the "at" operator asserts instead of throwing an exception if the index is invalid.
 *  - no custom Allocators
 *  - no reverse iterators
 *  - max_size is not implemented
 *  - Can't have a SmallVector<X> as a member of X.
 *
 */
template <typename T, std::size_t inline_elt_count = max(std::size_t(1), 128 / sizeof(T))>
class SmallVector {
 private:
  template <typename U>
  constexpr U* launder(U* in) const {
#if __cpp_lib_launder >= 201606
    return std::launder(in);
#else
    return in;
#endif
  }
  // how much to increase the storage amount when we run out.
  static constexpr double GROW_AMOUNT = 1.5;

  // by default, store stuff in here. C++ always uses sizeof(T) as the stride of an array, so this
  // is just like an uninitialized array of T's, which starts at the proper alignment.
  // We can't use an array of T's because that would default construct them, which we don't want.
  typename std::aligned_storage<sizeof(T), alignof(T)>::type m_inline[inline_elt_count];

  // get a T* at the beginning of our inline storage.
  constexpr const T* inline_begin() const { return launder(reinterpret_cast<const T*>(m_inline)); }
  constexpr T* inline_begin() { return launder(reinterpret_cast<T*>(m_inline)); }

  // regardless of our storage mode, these hold the beginning and end of the storage.
  // by default, they are initialized to the inline storage.
  T* m_storage_begin;
  T* m_storage_end;

  // the number of in-use elements
  std::size_t m_size;

  //// STORAGE HELPERS

  /*!
   * Allocate new heap storage and set it as the current.
   * The objects in storage are uninitialized.
   */
  void allocate_and_set_heap_storage(std::size_t elt_count) {
    m_storage_begin = launder(reinterpret_cast<T*>(new uint8_t[elt_count * sizeof(T)]));
    m_storage_end = m_storage_begin + elt_count;
  }

  /*!
   * Free heap storage, without calling destructors of objects.
   */
  void free_heap_storage(T* ptr) { delete[] launder(reinterpret_cast<uint8_t*>(ptr)); }

  /*!
   * Set the current storage to the inline memory.
   * We rely on m_storage_begin == inline_begin() to determine if we are using the inline memory.
   * This ends up being faster than storing a bool for the mode.
   */
  void set_inline_storage() {
    m_storage_begin = inline_begin();
    m_storage_end = inline_begin() + inline_elt_count;
  }

  bool using_heap_storage() const { return m_storage_begin != inline_begin(); }

  /*!
   * Allocate (if needed) storage to hold size objects.
   * Sets the container size.
   * Assumes the current storage is stack (the default)
   */
  void initialize_storage_and_size(std::size_t size) {
    if (size > inline_elt_count) {
      allocate_and_set_heap_storage(size);
    } else {
      set_inline_storage();
    }
    m_size = size;
  }

  void free_current_storage() {
    if (using_heap_storage()) {
      free_heap_storage(m_storage_begin);
    }
  }

  //// CONSTRUCTION HELPERS

  /*!
   * Construct objects using the copy constructor, copying val for each.
   */
  void construct_objects_by_filling(T* ptr, std::size_t count, const T& val) {
    for (std::size_t i = 0; i < count; i++) {
      new (ptr + i) T(val);
    }
  }

  /*!
   * Construct objects using the default constructor.
   */
  void construct_objects_by_default_ctor(T* ptr, std::size_t count) {
    for (std::size_t i = 0; i < count; i++) {
      new (ptr + i) T();
    }
  }

  /*!
   * Call destructor on an array of objects.
   */
  void destroy_objects(T* ptr, std::size_t count) {
    for (std::size_t i = 0; i < count; i++) {
      ptr[i].~T();
    }
  }

  /*!
   * Move objects from src to dst, deleting them in src.
   * Doing this in a single pass is more cache friendly for huge vectors.
   * Compilers seem to be smart enough to turn this into a memcpy for trivial types still.
   */
  void move_and_destroy(T* dst, T* src, std::size_t count) {
    for (std::size_t i = 0; i < count; i++) {
      new (dst + i) T(std::move(src[i]));
      src[i].~T();
    }
  }

  /*!
   * Move objects from src to dst, starting at the highest memory address. This is useful
   * if src and dst overlap.
   */
  void move_and_destroy_reverse(T* dst, T* src, std::size_t count) {
    for (std::size_t i = count; i-- > 0;) {
      new (dst + i) T(std::move(src[i]));
      src[i].~T();
    }
  }

  /*!
   * Copy objects from src to dst, using the copy constructor.
   */
  void copy_construct_objects(T* dst, const T* src, std::size_t count) {
    for (std::size_t i = 0; i < count; i++) {
      new (dst + i) T(src[i]);
    }
  }

  template <typename InputIt>
  void copy_objects_from_range(T* dst, InputIt first, InputIt last) {
    auto iter = first;
    while (iter != last) {
      new (dst) T(*iter);
      iter++;
      dst++;
    }
  }

 public:
  using iterator = T*;
  using const_iterator = const T*;
  using reverse_iterator = std::reverse_iterator<T*>;
  using const_reverse_iterator = std::reverse_iterator<const T*>;
  //// CONSTRUCTORS
  /*!
   * Constructor a SmallVector. By default the size is 0 and no elements are constructed.
   */
  SmallVector() { initialize_storage_and_size(0); }

  /*!
   * Initialize vector with count elements that are default initialized
   */
  explicit SmallVector(std::size_t count) {
    initialize_storage_and_size(count);
    construct_objects_by_default_ctor(m_storage_begin, count);
  }

  /*!
   * Initialize vector with count values that are copied from the given value.
   */
  SmallVector(std::size_t count, const T& value) {
    // set up storage:
    initialize_storage_and_size(count);
    construct_objects_by_filling(m_storage_begin, count, value);
  }

  /*!
   * Initialize vector from iterators.
   * In the case where your input iterator doesn't support random access, this ends up
   * making two passes through your data. The first to check the size, then the second to copy.
   */
  template <typename InputIt>
  SmallVector(InputIt first, InputIt last) {
    // this does a subtraction for random access iterators, or increments one-by-one for others.
    initialize_storage_and_size(std::distance(first, last));
    copy_objects_from_range(m_storage_begin, first, last);
  }

  /*!
   * Initialize vector using values from another vector.
   */
  SmallVector(const SmallVector<T, inline_elt_count>& other) {
    initialize_storage_and_size(other.size());
    copy_construct_objects(m_storage_begin, other.m_storage_begin, other.size());
  }

  /*!
   * Initialize vector by moving from existing vector. Will leave other vector with size = 0.
   */
  SmallVector(SmallVector<T, inline_elt_count>&& other) {
    if (other.using_heap_storage()) {
      // steal the storage from the existing vector
      m_storage_begin = other.m_storage_begin;
      m_storage_end = other.m_storage_end;
      // don't let the other vector reuse the storage by setting it back to inline.
      other.set_inline_storage();
    } else {
      set_inline_storage();
      // move from inline to inline
      // we're going to set the other's size to 0, so we have to destroy its objects.
      move_and_destroy(m_storage_begin, other.m_storage_begin, other.size());
    }

    m_size = other.m_size;
    other.m_size = 0;
  }

  /*!
   * Initialize vector from an initializer list.
   */
  SmallVector(std::initializer_list<T> lst) {
    initialize_storage_and_size(lst.size());
    copy_objects_from_range(m_storage_begin, lst.begin(), lst.end());
  }

  //// ASSIGNMENT
  /*!
   * Copy all data from another vector.
   * Uses copy ctor in all cases, never copy assignment.
   */
  SmallVector& operator=(const SmallVector<T, inline_elt_count>& other) {
    if (&other == this) {
      return *this;
    }

    // destroy all existing objects
    destroy_objects(m_storage_begin, size());

    // if we're bothering to copy, let's resize the storage to exactly what we need.
    if (capacity() != other.size()) {
      free_current_storage();
      initialize_storage_and_size(other.size());
    }
    m_size = other.size();

    copy_construct_objects(m_storage_begin, other.m_storage_begin, other.size());
    return *this;
  }

  /*!
   * Move all data from another vector. Leaves the other vector with nothing.
   */
  SmallVector& operator=(SmallVector<T, inline_elt_count>&& other) {
    if (&other == this) {
      return *this;
    }

    // destroy existing objects
    destroy_objects(m_storage_begin, size());

    // kill old storage. We will either steal it from other or use stack.
    free_current_storage();

    if (other.size() > inline_elt_count) {
      // steal the heap array
      m_storage_begin = other.m_storage_begin;
      m_storage_end = other.m_storage_end;
      other.set_inline_storage();
    } else {
      // move from inline storage
      set_inline_storage();
      move_and_destroy(m_storage_begin, other.m_storage_begin, other.size());
    }
    m_size = other.size();
    other.m_size = 0;
    return *this;
  }

  /*!
   * Set the contents to count copies of a given value.
   * Note - this is not super efficient and could avoid reallocating the heap memory in some cases.
   */
  void assign(std::size_t count, const T& value) {
    *this = SmallVector<T, inline_elt_count>(count, value);
  }

  /*!
   * See note on other assign.
   */
  template <typename InputIt>
  void assign(InputIt first, InputIt last) {
    *this = SmallVector<T, inline_elt_count>(first, last);
  }

  /*!
   * See note on other assign.
   */
  void assign(std::initializer_list<T> lst) { *this = SmallVector<T, inline_elt_count>(lst); }

  /*!
   * Get the element at the index. Assert the index is valid.
   */
  T& at(std::size_t idx) {
    ASSERT(idx < m_size);
    return m_storage_begin[idx];
  }

  /*!
   * Get the element at the index. Assert the index is valid.
   */
  const T& at(std::size_t idx) const {
    ASSERT(idx < m_size);
    return m_storage_begin[idx];
  }

  /*!
   * Get the element at the index. No range checking.
   */
  T& operator[](std::size_t idx) { return m_storage_begin[idx]; }

  /*!
   * Get the element at the index. No range checking.
   */
  const T& operator[](std::size_t idx) const { return m_storage_begin[idx]; }

  /*!
   * Get the first element. Not valid if size == 0
   */
  T& front() { return m_storage_begin[0]; }

  /*!
   * Get the first element. Not valid if size == 0
   */
  const T& front() const { return m_storage_begin[0]; }

  /*!
   * Get the last element. Not valid if size == 0
   */
  T& back() { return m_storage_begin[m_size - 1]; }

  /*!
   * Get the last element. Not valid if size == 0
   */
  const T& back() const { return m_storage_begin[m_size - 1]; }

  /*!
   * Get a pointer to data. This may become invalid after any resize, just like a std::vector.
   */
  T* data() { return m_storage_begin; }

  /*!
   * Get a pointer to data. This may become invalid after any resize, just like a std::vector.
   */
  const T* data() const { return m_storage_begin; }

  iterator begin() { return m_storage_begin; }
  const_iterator begin() const { return m_storage_begin; }
  const_iterator cbegin() const { return m_storage_begin; }
  iterator end() { return m_storage_begin + m_size; }
  const_iterator end() const { return m_storage_begin + m_size; }
  const_iterator cend() const { return m_storage_begin + m_size; }

  reverse_iterator rbegin() { return m_storage_begin + m_size; }
  const_reverse_iterator rbegin() const { return m_storage_begin + m_size; }
  const_reverse_iterator crbegin() const { return m_storage_begin + m_size; }
  reverse_iterator rend() { return m_storage_begin; }
  const_reverse_iterator rend() const { return m_storage_begin; }
  const_reverse_iterator crend() const { return m_storage_begin; }

  bool empty() const { return m_size == 0; }
  std::size_t size() const { return m_size; }

  /*!
   * Make sure we have enough storage to hold desired_count without allocating.
   */
  void reserve(std::size_t desired_count) {
    auto old_count = std::size_t(m_storage_end - m_storage_begin);
    if (desired_count > old_count) {
      // we need more storage.
      auto old_storage = m_storage_begin;
      auto old_uses_heap = using_heap_storage();

      // replace current storage
      allocate_and_set_heap_storage(desired_count);
      // move objects into the new location and destroy the old ones.
      move_and_destroy(m_storage_begin, old_storage, size());
      // free old, if needed.
      if (old_uses_heap) {
        free_heap_storage(old_storage);
      }
    }
  }

  /*!
   * The number of elements we can hold without allocating more memory.
   */
  std::size_t capacity() const { return m_storage_end - m_storage_begin; }

  /*!
   * Shrink our heap allocation to the smallest possible size that can possibly hold the
   * current elements. This can shrink it to zero and move us back to inline storage.
   */
  void shrink_to_fit() {
    if (using_heap_storage() && capacity() > size()) {
      auto old_storage = m_storage_begin;
      if (size() > inline_elt_count) {
        // go to smaller heap
        allocate_and_set_heap_storage(size());
      } else {
        // go back to inline
        set_inline_storage();
      }
      move_and_destroy(m_storage_begin, old_storage, size());
      free_heap_storage(old_storage);  // was heap.
    }
  }

  /*!
   * Destroy all objects and free all of our memory. Sets our size to 0.
   */
  void clear() {
    destroy_objects(m_storage_begin, m_size);
    if (using_heap_storage()) {
      free_heap_storage(m_storage_begin);
    }
    m_size = 0;
  }

  /*!
   * Insert value _before_ pos.
   */
  iterator insert(iterator pos, const T& value) {
    auto objects_before_insert = pos - begin();
    auto objects_after_insert = end() - pos;
    iterator result;
    if (size() + 1 > capacity()) {
      // not enough room, need to reallocate
      auto old_storage = m_storage_begin;
      auto old_used_heap = using_heap_storage();
      allocate_and_set_heap_storage(GROW_AMOUNT * (size() + 1));  // todo grow policy here.

      // copy the objects before the insert
      move_and_destroy(m_storage_begin, old_storage, objects_before_insert);
      // copy the object to insert
      result = new (m_storage_begin + objects_before_insert) T(value);
      // copy the objects after the insert
      move_and_destroy(m_storage_begin + objects_before_insert + 1,
                       old_storage + objects_before_insert, objects_after_insert);

      if (old_used_heap) {
        free_heap_storage(old_storage);
      }
    } else {
      // we have enough room, just move.
      move_and_destroy_reverse(pos + 1, pos, objects_after_insert);
      // and insert!
      result = new (pos) T(value);
    }

    m_size++;
    return result;
  }

  // insert iterator T&& value
  // insert iterator count value
  // insert iterator (it, it)

  // emplace
  // erase

  // push_back
  void push_back(const T& value) {
    if (size() + 1 > capacity()) {
      // not enough room!
      reserve(GROW_AMOUNT * (size() + 1));
    }
    // copy
    new (m_storage_begin + size()) T(value);
    m_size++;
  }

  void push_back(const T&& value) {
    if (size() + 1 > capacity()) {
      // not enough room!
      reserve(GROW_AMOUNT * (size() + 1));
    }
    // move:
    new (m_storage_begin + size()) T(std::move(value));
    m_size++;
  }

  template <typename... Args>
  void emplace_back(Args&&... args) {
    if (size() + 1 > capacity()) {
      // not enough room!
      reserve(GROW_AMOUNT * (size() + 1));
    }
    new (m_storage_begin + size()) T(std::forward<Args>(args)...);
    m_size++;
  }

  void pop_back() { m_size--; }

  void relocate_some_and_destroy_rest(T* dst,
                                      T* src,
                                      std::size_t dst_count,
                                      std::size_t src_count) {
    std::size_t i = 0;
    while (i < dst_count) {
      new (dst + i) T(std::move(src[i]));
      src[i].~T();
      i++;
    }

    while (i < src_count) {
      src[i].~T();
      i++;
    }
  }

  void resize(std::size_t new_size) {
    if (size() == new_size) {
      return;
    }

    auto copy_count = min(size(), new_size);
    bool old_used_heap = using_heap_storage();
    auto old_storage = m_storage_begin;

    if (new_size < inline_elt_count) {
      // heap to inline
      set_inline_storage();
    } else {
      allocate_and_set_heap_storage(new_size);
    }

    relocate_some_and_destroy_rest(m_storage_begin, old_storage, copy_count, size());
    if (old_used_heap) {
      free_heap_storage(old_storage);
    }
    construct_objects_by_default_ctor(m_storage_begin + copy_count, new_size - copy_count);
    m_size = new_size;
  }

  void resize(std::size_t new_size, const T& value) {
    if (size() == new_size) {
      return;
    }

    auto copy_count = min(size(), new_size);
    bool old_used_heap = using_heap_storage();
    auto old_storage = m_storage_begin;

    if (new_size < inline_elt_count) {
      // heap to inline
      set_inline_storage();
    } else {
      allocate_and_set_heap_storage(new_size);
    }

    relocate_some_and_destroy_rest(m_storage_begin, old_storage, copy_count, size());
    if (old_used_heap) {
      free_heap_storage(old_storage);
    }
    construct_objects_by_filling(m_storage_begin + copy_count, new_size - copy_count, value);
    m_size = new_size;
  }

  // swap

  bool operator==(const SmallVector<T, inline_elt_count>& other) const {
    if (other.size() != size()) {
      return false;
    }
    for (std::size_t i = 0; i < size(); i++) {
      if (operator[](i) != other[i]) {
        return false;
      }
    }
    return true;
  }

  bool operator!=(const SmallVector<T, inline_elt_count>& other) const {
    return !((*this) == other);
  }

  // std::swap

  ~SmallVector() {
    // destroy allocated objects
    destroy_objects(m_storage_begin, m_size);
    // free heap storage.
    if (using_heap_storage()) {
      free_heap_storage(m_storage_begin);
    }
  }
};
}  // namespace cu
