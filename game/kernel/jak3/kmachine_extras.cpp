#include "kmachine_extras.h"

#include <bitset>
#include <regex>

#include "kscheme.h"

#include "common/symbols.h"
#include "common/util/FontUtils.h"

#include "game/kernel/common/Symbol4.h"
#include "game/kernel/common/kmachine.h"
#include "game/kernel/common/kscheme.h"

namespace kmachine_extras {
using namespace jak3;

void pc_set_levels(u32 lev_list) {
  if (!Gfx::GetCurrentRenderer()) {
    return;
  }
  std::vector<std::string> levels;
  for (int i = 0; i < LEVEL_MAX; i++) {
    u32 lev = *Ptr<u32>(lev_list + i * 4);
    std::string ls = Ptr<String>(lev).c()->data();
    if (ls != "none" && ls != "#f" && ls != "") {
      levels.push_back(ls);
    }
  }

  Gfx::GetCurrentRenderer()->set_levels(levels);
}

void pc_set_active_levels(u32 lev_list) {
  if (!Gfx::GetCurrentRenderer()) {
    return;
  }
  std::vector<std::string> levels;
  for (int i = 0; i < LEVEL_MAX; i++) {
    u32 lev = *Ptr<u32>(lev_list + i * 4);
    std::string ls = Ptr<String>(lev).c()->data();
    if (ls != "none" && ls != "#f" && ls != "") {
      levels.push_back(ls);
    }
  }

  Gfx::GetCurrentRenderer()->set_active_levels(levels);
}

inline u64 bool_to_symbol(const bool val) {
  return val ? static_cast<u64>(s7.offset) + true_symbol_offset(g_game_version) : s7.offset;
}

inline bool symbol_to_bool(const u32 symptr) {
  return symptr != s7.offset;
}

// TODO - move to common
void encode_utf8_string(u32 src_str_ptr, u32 str_dest_ptr) {
  auto str = std::string(Ptr<String>(src_str_ptr).c()->data());
  std::string converted = get_font_bank(GameTextVersion::JAK3)->convert_utf8_to_game(str);
  strcpy(Ptr<String>(str_dest_ptr).c()->data(), converted.c_str());
}

}  // namespace kmachine_extras
