#ifndef JAK_V2_UTIL_H
#define JAK_V2_UTIL_H

#include <memory>

template <typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args) {
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

template <typename T>
T align(T current, T alignment, T offset) {
  while ((current % alignment) != 0) {
    current++;
  }
  return current + offset;
}

#endif  // JAK_V2_UTIL_H
