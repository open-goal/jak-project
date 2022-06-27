#include "sfxblock.h"

#include "blocksound_handler.h"

namespace snd {
std::unique_ptr<sound_handler> SFXBlock::make_handler(voice_manager& vm,
                                                      u32 sound_id,
                                                      s32 vol,
                                                      s32 pan,
                                                      s32 pm,
                                                      s32 pb) {
  std::unique_ptr<blocksound_handler> handler;
  auto& SFX = sounds[sound_id];

  if (SFX.grains.empty()) {
    // fmt::print("skipping empty sfx\n");
    return nullptr;
  }

  handler = std::make_unique<blocksound_handler>(sounds[sound_id], vm, vol, pan, pm, pb, bank_id);
  handler->init();
  return handler;
}

}  // namespace snd
