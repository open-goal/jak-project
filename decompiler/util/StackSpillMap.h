#pragma once

#include <string>
#include <unordered_map>
#include <vector>

namespace decompiler {
/*!
 * Memory on the stack used to spill a register.
 * This just has a size/is_signed? and doesn't know anything about types.
 */
struct StackSpillSlot {
  int offset = -1;         // relative to sp
  int size = -1;           // bytes
  bool is_signed = false;  // set to false for quadwords/doublewords

  bool operator==(const StackSpillSlot& other) const {
    return offset == other.offset && size == other.size && is_signed == other.is_signed;
  }

  bool operator!=(const StackSpillSlot& other) const { return !((*this) == other); }

  std::string print() const;
};

/*!
 * Map of StackSpillSlots for a function.
 */
class StackSpillMap {
 public:
  void add_access(const StackSpillSlot& access);
  void finalize();
  const StackSpillSlot& lookup(int offset) const;
  int size() const;
  const std::unordered_map<int, StackSpillSlot>& map() const { return m_slot_map; }

 private:
  std::unordered_map<int, StackSpillSlot> m_slot_map;
};
}  // namespace decompiler