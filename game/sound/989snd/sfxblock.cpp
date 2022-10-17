#include "sfxblock.h"

#include "sfxgrain.h"

#include "common/log/log.h"

#include "blocksound_handler.h"

namespace snd {

SFXBlock::SFXBlock(locator& loc, u32 id, BankTag* tag)
    : SoundBank(id, BankType::SFX), m_locator(loc) {
  auto data = (SFXBlockData*)tag;

  auto sounddata = (SFX2Data*)((uintptr_t)data + data->FirstSound);
  for (int i = 0; i < data->NumSounds; i++) {
    SFX2 sound;
    sound.d = sounddata[i];
    m_sounds.push_back(std::move(sound));
  }

  for (auto& sound : m_sounds) {
    auto graindata = (SFXGrain*)((uintptr_t)data + data->FirstGrain + sound.d.FirstGrain);
    for (int i = 0; i < sound.d.NumGrains; i++) {
      SFXGrain& grain = graindata[i];
      sound.grains.push_back(new_grain((grain_type)grain.Type, grain));
    }
  }
}

std::unique_ptr<sound_handler> SFXBlock::make_handler(voice_manager& vm,
                                                      u32 sound_id,
                                                      s32 vol,
                                                      s32 pan,
                                                      s32 pm,
                                                      s32 pb) {
  auto& SFX = m_sounds[sound_id];

  if (SFX.grains.empty()) {
    // fmt::print("skipping empty sfx\n");
    return nullptr;
  }

  auto handler =
      std::make_unique<blocksound_handler2>(m_sounds[sound_id], vm, vol, pan, pm, pb, bank_id);
  handler->init();
  return handler;
}

}  // namespace snd
