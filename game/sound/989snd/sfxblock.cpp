#include "sfxblock.h"

#include "blocksound_handler.h"
#include "sfxgrain.h"

#include "common/log/log.h"

namespace snd {

SoundHandler* SFXBlock::MakeHandler(u32 sound_id, s32 vol, s32 pan, SndPlayParams& params) {
  auto& SFX = Sounds[sound_id];

  if (SFX.Grains.empty()) {
    return nullptr;
  }

  auto handler = BlockSoundHandler::MakeBlockSound(*this, SFX, vol, pan, params);
  return handler;
}

std::optional<u32> SFXBlock::GetSoundByName(const char* name) {
  auto sound = Names.find(name);
  if (sound != Names.end()) {
    return sound->second;
  }

  return std::nullopt;
}

}  // namespace snd
