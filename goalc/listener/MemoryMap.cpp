#include "MemoryMap.h"

#include <algorithm>
#include <stdexcept>

#include "common/link_types.h"
#include "common/util/Assert.h"
#include "common/util/BitUtils.h"

#include "third-party/fmt/core.h"

namespace listener {
std::string LoadEntry::print() const {
  std::string result;
  const SegmentTypes types[3] = {MAIN_SEGMENT, DEBUG_SEGMENT, TOP_LEVEL_SEGMENT};
  for (int i = 0; i < 3; i++) {
    result += fmt::format("{} : 0x{:x} size 0x{:x}\n", SEGMENT_NAMES[i], segments[int(types[i])],
                          segment_sizes[int(types[i])]);
  }
  return result;
}

std::string MemoryMapEntry::print() const {
  std::string result;
  result += fmt::format("0x{:x} to 0x{:x}: ", start_addr, end_addr);
  if (empty) {
    result += "emtpy!\n";
  } else {
    result += fmt::format("{} seg {}\n", obj_name, seg_id);
  }
  return result;
}

MemoryMap::MemoryMap(const std::unordered_map<std::string, LoadEntry>& load_entries) {
  std::vector<MemoryMapEntry> entries;

  for (const auto& x : load_entries) {
    for (int i = 0; i < N_SEG; i++) {
      if (i != TOP_LEVEL_SEGMENT && x.second.segments[i] != 0 && x.second.segment_sizes[i] != 0) {
        MemoryMapEntry entry;
        entry.start_addr = x.second.segments[i];
        // we cheat the segment sizes to be 16 byte aligned. This avoids tiny gaps < 16 bytes in the
        // memory map.
        entry.end_addr = entry.start_addr + align16(x.second.segment_sizes[i]);
        entry.seg_id = i;
        entry.obj_name = x.first;
        entry.empty = false;
        entries.push_back(entry);
      }
    }
  }

  std::sort(entries.begin(), entries.end(), [](const MemoryMapEntry& a, const MemoryMapEntry& b) {
    return a.start_addr < b.start_addr;
  });

  u32 last_addr = 0;
  for (const auto& entry : entries) {
    if (entry.start_addr < last_addr) {
      // this is bad.
      printf("[Listener Error] We could not build a memory map.\n");
      MemoryMap temp(m_entries);
      printf("%s\n", temp.print().c_str());
      printf("Can't add %s\n", entry.print().c_str());

      ASSERT(false);  // todo, handle this more gracefully
    } else if (entry.start_addr > last_addr) {
      // this is fine, there's just a gap.
      MemoryMapEntry gap;
      gap.start_addr = last_addr;
      gap.end_addr = entry.start_addr;
      gap.empty = true;
      m_entries.push_back(gap);
    }

    m_entries.push_back(entry);
    last_addr = entry.end_addr;
  }

  MemoryMapEntry last_gap;
  last_gap.empty = true;
  last_gap.start_addr = last_addr;
  last_gap.end_addr = UINT32_MAX;
  m_entries.push_back(last_gap);
}

std::string MemoryMap::print() const {
  std::string result;
  result += std::string(40, '-');
  result += '\n';
  for (auto& entry : m_entries) {
    if (entry.empty) {
      result += fmt::format(" [0x{:08x}] GAP of 0x{:x} bytes, until 0x{:x}\n", entry.start_addr,
                            entry.end_addr - entry.start_addr, entry.end_addr);
    } else {
      result += fmt::format(
          " [0x{:08x}] SEGMENT of 0x{:x} bytes, until 0x{:x}\n    name: {}\n    kind: {}\n",
          entry.start_addr, entry.end_addr - entry.start_addr, entry.end_addr, entry.obj_name,
          SEGMENT_NAMES[entry.seg_id]);
    }
    result += std::string(40, '-');
    result += '\n';
  }
  return result;
}

const MemoryMapEntry& MemoryMap::lookup(u32 addr) {
  for (auto& entry : m_entries) {
    if (addr >= entry.start_addr && addr < entry.end_addr) {
      return entry;
    }
  }
  ASSERT(false);
  throw std::runtime_error("MemoryMap::lookup failed");
}

bool MemoryMap::lookup(const std::string& obj_name, u8 seg_id, MemoryMapEntry* out) {
  for (auto& entry : m_entries) {
    if (!entry.empty && entry.obj_name == obj_name && entry.seg_id == seg_id) {
      *out = entry;
      return true;
    }
  }
  return false;
}

bool LoadEntry::overlaps_with(const LoadEntry& other) const {
  for (int seg = 0; seg < 2; seg++) {
    if (std::max(segments[seg], other.segments[seg]) <
        std::min(segments[seg] + segment_sizes[seg],
                 other.segments[seg] + other.segment_sizes[seg])) {
      return true;
    }
  }
  return false;
}
}  // namespace listener
