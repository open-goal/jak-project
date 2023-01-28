#pragma once

#include <unordered_set>

namespace set_util {
template <typename T>
std::unordered_set<T> intersection(std::unordered_set<T> set1, std::unordered_set<T> set2) {
  if (set2.size() < set1.size()) {
    auto temp = set1;
    set1 = set2;
    set2 = temp;
  }
  std::unordered_set<T> m(set1.begin(), set1.end());
  std::unordered_set<T> res;
  for (auto a : set2)
    if (m.count(a)) {
      res.insert(a);
      m.erase(a);
    }
  return res;
}
}  // namespace set_util
