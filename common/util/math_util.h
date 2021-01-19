#pragma once

template <typename T>
T align(T current, T alignment, T offset = 0) {
  while ((current % alignment) != 0) {
    current++;
  }
  return current + offset;
}