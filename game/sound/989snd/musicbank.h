#pragma once
#include <vector>

#include "soundbank.h"

namespace snd {
struct SoundBankData : BankTag {
  /*  10 */ s8 BankNum;
  /*  11 */ s8 pad1;
  /*  12 */ s16 pad2;
  /*  14 */ s16 NumSounds;
  /*  16 */ s16 NumProgs;
  /*  18 */ s16 NumTones;
  /*  1a */ s16 NumVAGs;
  /*  1c */ /*Sound**/ u32 FirstSound;
  /*  20 */ /*Prog**/ u32 FirstProg;
  /*  24 */ /*Tone**/ u32 FirstTone;
  /*  28 */ /*void**/ u32 VagsInSR;
  /*  2c */ u32 VagDataSize;
  /*  30 */ /*SoundBank**/ u32 NextBank;
};

struct MIDIBlock {
  /*   0 */ u32 DataID;
  /*   4 */ s16 Version;
  /*   6 */ s8 Flags;
  /*   7 */ s8 NumMIDIBlocks;
  /*   8 */ u32 ID;
};

struct MIDIBlockHeader : MIDIBlock {
  /*   c */ /*void**/ u32 NextMIDIBlock;
  /*  10 */ u32 BankID;
  /*  14 */ /*SoundBank**/ u32 BankPtr;
  /*  18 */ /*s8**/ u32 DataStart;
  /*  1c */ /*s8**/ u32 MultiMIDIParent;
  /*  20 */ u32 Tempo;
  /*  24 */ u32 PPQ;
};

struct MultiMIDIBlockHeader : MIDIBlock {
  /*   c */ /*void**/ u32 NextMIDIBlock;
  /*  10 */ /*s8**/ u32 BlockPtr[0];
};

struct MIDISound {
  /*   0 */ s32 Type;
  /*   4 */ /*SoundBank**/ u32 Bank;
  /*   8 */ /*void**/ u32 OrigBank;
  /*   c */ u32 MIDIID;
  /*  10 */ s16 Vol;
  /*  12 */ s8 Repeats;
  /*  13 */ s8 VolGroup;
  /*  14 */ s16 Pan;
  /*  16 */ s8 Index;
  /*  17 */ s8 Flags;
  /*  18 */ /*void**/ u32 MIDIBlock;
};

// Annoyingly enough we need some data out of this struct
struct MIDISoundHandler {
  u32 OwnerID;
};

struct Prog;
class MusicBank : public SoundBank {
 public:
  MusicBank(locator& loc, u32 id, BankTag* tag);
  std::optional<std::unique_ptr<sound_handler>> make_handler(voice_manager& vm,
                                                             u32 sound_id,
                                                             s32 vol,
                                                             s32 pan,
                                                             s32 pm,
                                                             s32 pb) override;

  std::optional<std::unique_ptr<sound_handler>> make_handler(voice_manager& vm,
                                                             u32 sound_id,
                                                             s32 vol,
                                                             s32 pan,
                                                             SndPlayParams& params) override;

  std::vector<Prog> m_programs;
  std::vector<MIDISound> m_sounds;

 private:
  locator& m_locator;
};
}  // namespace snd
