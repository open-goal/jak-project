#include "jakx_texture_remap.h"

#include <unordered_map>
#include <utility>
#include <vector>

namespace {
const std::unordered_map<int, std::vector<std::pair<int, int>>> data = {
    // TODO - jakx
};

};

int lookup_jakx_texture_dest_offset(int tpage, int texture_idx) {
  auto it = data.find(tpage);
  if (it == data.end()) {
    return 0;
  }
  for (auto& p : it->second) {
    if (p.first == texture_idx) {
      return p.second;
    }
  }
  return 0;
}