#pragma once
#include "sound_handler.h"
#include "vagvoice.h"

#include "common/common_types.h"

#include "game/sound/989snd/lfo.h"
#include "game/sound/989snd/sfxblock.h"

namespace snd {

extern std::array<s8, 32> g_block_reg;

class BlockSoundVoice : public VagVoice {
 public:
  BlockSoundVoice(Tone& t) : VagVoice(t) {}
  s32 g_vol;
  s32 g_pan;
};

class BlockSoundHandler : public SoundHandler {
 public:
  BlockSoundHandler(SoundBank& bank,
                    SFXBlock::SFX& sfx,
                    VoiceManager& vm,
                    s32 sfx_vol,
                    s32 sfx_pan,
                    SndPlayParams& params);

  ~BlockSoundHandler() override;
  bool Tick() override;
  SoundBank& Bank() override { return m_bank; };

  void Pause() override;
  void Unpause() override;
  void Stop() override;
  u8 Group() override { return m_group; };
  void SetVolPan(s32 vol, s32 pan) override;
  void SetPMod(s32 mod) override;
  void SetRegister(u8 reg, u8 value) override { m_registers.at(reg) = value; };
  void SetPBend(s32 bend) override;

  void DoGrain();

  void UpdatePitch();

  bool m_paused{false};

  u8 m_group{0};
  bool m_done{false};

  u32 m_grains_to_play{0};
  u32 m_grains_to_skip{0};
  bool m_skip_grains{false};

  SFXBlock::SFX& m_sfx;
  VoiceManager& m_vm;

  std::list<std::weak_ptr<BlockSoundVoice>> m_voices;

  std::list<std::unique_ptr<SoundHandler>> m_children;

  s32 m_orig_volume{0};
  s32 m_orig_pan{0};
  s32 m_cur_volume{0};
  s32 m_cur_pan{0};
  s32 m_cur_pm{0};
  s32 m_cur_pb{0};
  s32 m_app_volume{0};
  s32 m_app_pan{0};
  s32 m_app_pm{0};
  s32 m_app_pb{0};

  s32 m_lfo_volume{0};
  s32 m_lfo_pan{0};
  s32 m_lfo_pm{0};
  s32 m_lfo_pb{0};

  SoundBank& m_bank;

  u8 m_note{60};
  u8 m_fine{0};

  std::array<s8, 4> m_registers{};
  std::array<LFOTracker, 4> m_lfo{{*this, *this, *this, *this}};

  s32 m_countdown{0};
  u32 m_next_grain{0};
};
}  // namespace snd
