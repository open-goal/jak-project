// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#pragma once

#include <array>
#include <cstddef>

// This class is only valid for sizes that are power of two
template <typename Tp, size_t Nm>
class fifo {
 public:
  Tp Pop() { return array[Mask(read++)]; }
  void Push(Tp val) { array[Mask(write++)] = val; }

  Tp& Front() { return array[Mask(read)]; }
  Tp& Back() { return array[Mask(write)]; }

  Tp Peek() { return array[Mask(read + 1)]; }
  Tp Peek(size_t offset) { return array[Mask(read + offset)]; }

  size_t Size() { return write - read; }
  bool Full() { return Size() == capacity; }
  bool Empty() { return read == write; }

  void Reset() {
    array.fill(Tp{});
    read = 0;
    write = 0;
  }

 private:
  static constexpr bool is_power_of_two(int v) { return v && ((v & (v - 1)) == 0); }
  static_assert(is_power_of_two(Nm), "FIFO size must be power of 2 for correct operation");

  size_t Mask(size_t val) { return val & (capacity - 1); }

  std::array<Tp, Nm> array = {};
  size_t capacity = Nm;
  size_t read = {};
  size_t write = {};
};
