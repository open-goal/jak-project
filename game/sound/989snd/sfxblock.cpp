#include "sfxblock.h"

#include "blocksound_handler.h"

#include "common/log/log.h"

namespace snd {

SFXBlock::SFXBlock(locator& loc, u32 id, BankTag* tag)
    : SoundBank(id, BankType::SFX), m_locator(loc) {
  auto data = (SFXBlockData*)tag;

  lg::warn("BlockNames {:x}", data->BlockNames);
  lg::warn("FirstSound {:x}", data->FirstSound);
  lg::warn("SFXUD {:x}", data->SFXUD);

  m_version = data->Version;

  auto sounddata = (SFXData*)((uintptr_t)data + data->FirstSound);
  for (int i = 0; i < data->NumSounds; i++) {
    SFX sound;
    sound.d = sounddata[i];
    m_sounds.push_back(sound);
  }

  for (auto& sound : m_sounds) {
    auto graindata = (SFXGrain*)((uintptr_t)data + data->FirstGrain + sound.d.FirstGrain);
    for (int i = 0; i < sound.d.NumGrains; i++) {
      SFXGrain grain = graindata[i];
      if (grain.Type == 1) {
        grain.GrainParams.tone.BankID = id;
      }
      sound.grains.push_back(grain);
    }
  }

  if (m_version > 1) {
    auto data2 = (SFXBlockData2*)data;
    read_names((SFXBlockNames*)((uintptr_t)data2 + data2->BlockNames));
  }
}

void SFXBlock::read_names(SFXBlockNames* names) {
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

std::unique_ptr<sound_handler> SFXBlock::make_handler(voice_manager& vm,
                                                      u32 sound_id,
                                                      s32 vol,
                                                      s32 pan,
                                                      s32 pm,
                                                      s32 pb) {
  if (m_version > 1) {
    lg::warn("version {}", m_version);
    return nullptr;
  }

  std::unique_ptr<blocksound_handler> handler;
  auto& SFX = m_sounds[sound_id];

  if (SFX.grains.empty()) {
    // fmt::print("skipping empty sfx\n");
    return nullptr;
  }

  handler = std::make_unique<blocksound_handler>(m_sounds[sound_id], vm, vol, pan, pm, pb, bank_id);
  handler->init();
  return handler;
}

}  // namespace snd
