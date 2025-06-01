#pragma once
#include <span>
#include <variant>
#include <vector>

#include "soundbank.h"

namespace snd {

struct Midi {
  u32 DataID;
  s16 Version;
  s8 Flags;
  u32 ID;
  u32 BankID;
  u8* DataStart;
  u32 Tempo;
  s32 PPQ;
};

struct MidiSegment : Midi {
  // For MultiMidi data where the sound handler struct is baked into the
  // file, we need to know the handler type to determine if we should
  // play the segment. For unknown reasons the files contain segments
  // that have invalid/unknown handler types that should not be played as midi.
  u32 SoundHandle;
};

struct MultiMidi {
  u32 DataID;
  u32 Version;
  s8 Flags;
  u32 ID;

  std::vector<MidiSegment> midi;
};

class MusicBank : public SoundBank {
 public:
  struct MIDISound {  // 0x1c
    s32 Type;
    SoundBank* Bank;
    void* OrigBank;
    u32 MIDIID;
    s16 Vol;
    s8 Repeats;
    s8 VolGroup;
    s16 Pan;
    s8 Index;
    u8 Flags;
  };

  struct Prog {
    s8 Vol;
    s16 Pan;
    std::vector<Tone> Tones;
  };

  std::vector<MIDISound> Sounds;
  std::vector<Prog> Progs;
  std::unique_ptr<u8[]> SampleData;
  std::unique_ptr<u8[]> SeqData;
  std::variant<Midi, MultiMidi> MidiData;

  static MusicBank* ReadBank(std::span<u8> bank_data,
                             std::span<u8> samples,
                             std::span<u8> midi_data);

  std::optional<std::unique_ptr<SoundHandler>>
  MakeHandler(VoiceManager& vm, u32 sound_id, s32 vol, s32 pan, s32 pm, s32 pb, s32 tick) override;

  std::optional<std::unique_ptr<SoundHandler>> MakeHandler(VoiceManager& vm,
                                                           u32 sound_id,
                                                           s32 vol,
                                                           s32 pan,
                                                           SndPlayParams& params,
                                                           s32 tick) override;
};
}  // namespace snd
