#ifndef LFO_H_
#define LFO_H_

#include "common/common_types.h"

namespace snd {

enum class lfo_type { OFF, SINE, SQUARE, TRIANGLE, SAW, RAND };
enum class lfo_target { NONE, VOLUME, PAN, PMOD, PBEND, UNK1, UNK2 };

class blocksound_handler;

class LFOTracker {
 public:
  LFOTracker(blocksound_handler& handler) : m_handler(handler) {}
  lfo_type m_type{lfo_type::OFF};
  lfo_target m_target{0};
  u8 m_target_extra{0};
  u8 m_setup_flags{0};
  u8 m_running_flags{0};
  s16 m_depth{0};
  u32 m_orig_depth{0};
  s32 m_next_step{0};
  u32 m_step_size{0};
  u32 m_orig_step_size{0};
  s32 m_state_hold1{0};
  s32 m_state_hold2{0};
  s32 m_range{0};
  s32 m_last_lfo{0};

  u32 m_tick{0};

  void init();
  void calc_depth();
  void tick();
  s32 get_lfo(s32 step_mult);

  blocksound_handler& m_handler;
};

}  // namespace snd

#endif  // LFO_H_
