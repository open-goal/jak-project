#include "sfxblock2.h"

#include "blocksound_handler.h"

#include "common/log/log.h"

namespace snd {
SFXBlock2::SFXBlock2(locator& loc, u32 id, BankTag* tag)
    : SoundBank(id, BankType::SFX), m_locator(loc) {
  auto data = (SFXBlockData2*)tag;

  auto sounddata = (SFX2Data*)((uintptr_t)data + data->FirstSound);
  auto userdata = (SFXUserData*)((uintptr_t)data + data->SFXUD);
  for (int i = 0; i < data->NumSounds; i++) {
    SFX2 sound;
    sound.index = i;
    sound.d = sounddata[i];
    sound.user_data = userdata[i];
    m_sounds.push_back(std::move(sound));
  }

  for (auto& sound : m_sounds) {
    auto graindata = (SFXGrain2*)((uintptr_t)data + data->FirstGrain + sound.d.FirstGrain);
    for (int i = 0; i < sound.d.NumGrains; i++) {
      SFXGrain2& grain = graindata[i];
      sound.grains.push_back(new_grain((grain_type)grain.OpcodeData.type, grain,
                                       (u8*)((uintptr_t)data + data->GrainData)));
    }
  }

  auto names = (SFXBlockNames*)((uintptr_t)data + data->BlockNames);
  char buf[8];
  strncpy(buf, (char*)names->BlockName, 8);
  m_name = buf;

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
        if (m_names.find(str) == m_names.end()) {
          m_names[str] = name->Index;
        }
        m_sounds.at(name->Index).name = str;

        name++;
      }
    }
  }

  // auto idx = 0;
  // for (auto& s : m_sounds) {
  // lg::warn("sound {} : {}", idx, s.name);
  // idx++;
  //}
}

std::optional<std::unique_ptr<sound_handler>> SFXBlock2::make_handler(voice_manager& vm,
                                                                      u32 sound_id,
                                                                      s32 vol,
                                                                      s32 pan,
                                                                      SndPlayParams& params) {
  if (sound_id >= m_sounds.size()) {
    lg::error("out of bounds sound_id");
    return std::nullopt;
  }

  auto& SFX = m_sounds[sound_id];

  if (SFX.grains.empty()) {
    return std::nullopt;
  }

  auto handler =
      std::make_unique<blocksound_handler>(*this, m_sounds[sound_id], vm, vol, pan, params);
  return handler;
}

std::optional<u32> SFXBlock2::get_sound_by_name(const char* name) {
  // lg::error("searching for sound {}", name);
  // for (auto& s : m_names) {
  //   lg::error("{}", s.first);
  // }

  auto sound = m_names.find(name);
  if (sound != m_names.end()) {
    return sound->second;
  }

  return std::nullopt;
}

}  // namespace snd
