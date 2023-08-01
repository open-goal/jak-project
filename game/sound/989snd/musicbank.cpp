#include "musicbank.h"

#include "ame_handler.h"
#include "midi_handler.h"

#include "../common/synth.h"

namespace snd {

MusicBank::MusicBank(locator& loc, u32 id, BankTag* tag)
    : SoundBank(id, BankType::Music), m_locator(loc) {
  auto data = (SoundBankData*)tag;

  auto sound = (MIDISound*)((uintptr_t)data + data->FirstSound);
  for (int i = 0; i < data->NumSounds; i++) {
    m_sounds.emplace_back(sound[i]);
  }

  auto progdata = (ProgData*)((uintptr_t)data + data->FirstProg);
  for (int i = 0; i < data->NumProgs; i++) {
    Prog prog;
    prog.d = progdata[i];
    m_programs.emplace_back(std::move(prog));
  }

  for (auto& prog : m_programs) {
    auto tonedata = (Tone*)((uintptr_t)data + prog.d.FirstTone);
    for (int i = 0; i < prog.d.NumTones; i++) {
      prog.tones.emplace_back(tonedata[i]);
    }
  }

  bank_name = data->BankID;
}

std::optional<std::unique_ptr<sound_handler>> MusicBank::make_handler(voice_manager& vm,
                                                                      u32 sound_id,
                                                                      s32 vol,
                                                                      s32 pan,
                                                                      s32 pm,
                                                                      s32 pb) {
  auto& sound = m_sounds[sound_id];

  if (sound.Type == 4) {  // midi
    auto midi = static_cast<MIDIBlockHeader*>(m_locator.get_midi(sound.MIDIID));
    return std::make_unique<midi_handler>(midi, vm, sound, vol, pan, m_locator, *this);
  } else if (sound.Type == 5) {  // ame
    auto midi = static_cast<MultiMIDIBlockHeader*>(m_locator.get_midi(sound.MIDIID));
    return std::make_unique<ame_handler>(midi, vm, sound, vol, pan, m_locator, *this);
  } else {
    lg::error("Invalid music sound type");
    return std::nullopt;
    // error
  }
}

std::optional<std::unique_ptr<sound_handler>> MusicBank::make_handler(voice_manager& vm,
                                                                      u32 sound_id,
                                                                      s32 vol,
                                                                      s32 pan,
                                                                      SndPlayParams& params) {
  return std::nullopt;
}

}  // namespace snd
