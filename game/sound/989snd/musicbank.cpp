#include "musicbank.h"

#include "ame_handler.h"
#include "midi_handler.h"

#include "common/log/log.h"

namespace snd {

SoundHandler* MusicBank::MakeHandler(u32 sound_id, s32 vol, s32 pan, s32 pm, s32 pb) {
  auto& sound = Sounds[sound_id];

  // FIXME: global midi list
  // search only local bank for now
  // (always the case in jak games anyway)

  if (sound.Type == 4) {
    auto& midi = std::get<Midi>(MidiData);
    if (sound.MIDIID == midi.ID) {
      return AllocMidiSound(&midi, sound, vol, pan, *this);
    }
    return nullptr;
  } else if (sound.Type == 5) {
    auto& midi = std::get<MultiMidi>(MidiData);
    if (sound.MIDIID == midi.ID) {
      return AllocAmeSound(&midi, sound, vol, pan, *this);
    }
    return nullptr;
  } else {
    lg::error("Invalid music sound type");
    return nullptr;
    // error
  }
}

SoundHandler* MusicBank::MakeHandler(u32 sound_id, s32 vol, s32 pan, SndPlayParams& params) {
  return nullptr;
}

}  // namespace snd
