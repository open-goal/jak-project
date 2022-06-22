#include "musicbank.h"

#include "ame_handler.h"
#include "midi_handler.h"

#include "../common/synth.h"

namespace snd {
std::unique_ptr<sound_handler> MusicBank::make_handler(voice_manager& vm,
                                                       u32 sound_id,
                                                       s32 vol,
                                                       s32 pan,
                                                       s32 pm,
                                                       s32 pb) {
  auto& sound = sounds[sound_id];
  std::unique_ptr<sound_handler> handler;

  if (sound.Type == 4) {  // midi
    auto midi = static_cast<MIDIBlockHeader*>(m_locator.get_midi(sound.MIDIID));
    handler = std::make_unique<midi_handler>(midi, vm, sound, vol, pan, m_locator, bank_id);
  } else if (sound.Type == 5) {  // ame
    auto midi = static_cast<MultiMIDIBlockHeader*>(m_locator.get_midi(sound.MIDIID));
    handler = std::make_unique<ame_handler>(midi, vm, sound, vol, pan, m_locator, bank_id);
  } else {
    // error
  }

  return handler;
}

}  // namespace snd
