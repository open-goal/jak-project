#include "sfxblock2.h"

#include "blocksound_handler.h"

#include "common/log/log.h"

namespace snd {
SFXBlock2::SFXBlock2(locator& loc, u32 id, BankTag* tag)
    : SoundBank(id, BankType::SFX), m_locator(loc) {
  m_version = tag->Version;

  auto data = (SFXBlockData2*)tag;
  auto names = (SFXBlockNames*)((uintptr_t)data + data->BlockNames);
  lg::warn("{:x} {:x} {}", names->BlockName[0], names->BlockName[1], names->SFXNameTableOffset);
  if (names->SFXNameTableOffset != 0) {
    // The sound names are hashed and divided up into 32 buckets
    // to reduce the number of comparisons needed to search the list.
    // An empty name entry signifies the end of each bucket.
    // Let's go through all the buckets and collect the names.

    auto name_table = (SFXName*)((uintptr_t)names + names->SFXNameTableOffset);
    for (int i = 0; i < 32; i++) {
      auto name = &name_table[names->SFXHashOffsets[i]];
      while (name->Name[0] != 0) {
        char buf[16];
        strncpy(buf, (char*)name->Name, 16);

        std::string str(buf);
        m_names[str] = name->Index;

        name++;
      }
    }
  }
}

std::unique_ptr<sound_handler> SFXBlock2::make_handler(voice_manager& vm,
                                                       u32 sound_id,
                                                       s32 vol,
                                                       s32 pan,
                                                       s32 pm,
                                                       s32 pb) {
  return nullptr;
}

}  // namespace snd
