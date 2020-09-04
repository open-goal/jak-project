#ifndef JAK_TYPE_UTIL_H
#define JAK_TYPE_UTIL_H

template <typename T>
T align(T current, T alignment, T offset = 0) {
  while ((current % alignment) != 0) {
    current++;
  }
  return current + offset;
}

#endif  // JAK_TYPE_UTIL_H
