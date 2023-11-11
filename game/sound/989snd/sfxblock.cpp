#include "sfxblock.h"

#include "blocksound_handler.h"
#include "sfxgrain.h"

#include "common/log/log.h"

namespace snd {

std::optional<std::unique_ptr<sound_handler>> SFXBlock::make_handler(voice_manager& vm,
                                                                     u32 sound_id,
                                                                     s32 vol,
                                                                     s32 pan,
                                                                     SndPlayParams& params) {
  auto& SFX = Sounds[sound_id];

  if (SFX.Grains.empty()) {
    return std::nullopt;
  }

  auto handler = std::make_unique<blocksound_handler>(*this, SFX, vm, vol, pan, params);
  return handler;
}

std::optional<u32> SFXBlock::get_sound_by_name(const char* name) {
  auto sound = Names.find(name);
  if (sound != Names.end()) {
    return sound->second;
  }

  return std::nullopt;
}

}  // namespace snd
