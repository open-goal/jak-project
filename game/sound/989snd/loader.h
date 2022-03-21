// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#pragma once
#include "../common/synth.h"
#include "common/common_types.h"
#include <filesystem>
#include <memory>
#include <vector>

namespace snd {

#define FOURCC(a, b, c, d) ((u32)(((d) << 24) | ((c) << 16) | ((b) << 8) | (a)))

struct BankTag {
  /*   0 */ u32 DataID;
  /*   4 */ u32 Version;
  /*   8 */ u32 Flags;
  /*   c */ u32 BankID;
};

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

struct SFXBlockData : BankTag {
  /*  10 */ s8 BlockNum;
  /*  11 */ s8 pad1;
  /*  12 */ s16 pad2;
  /*  14 */ s16 pad3;
  /*  16 */ s16 NumSounds;
  /*  18 */ s16 NumGrains;
  /*  1a */ s16 NumVAGs;
  /*  1c */ u32 FirstSound;
  /*  20 */ u32 FirstGrain;
  /*  24 */ void* VagsInSR;
  /*  28 */ u32 VagDataSize;
  /*  2c */ u32 SRAMAllocSize;
  /*  30 */ u32 NextBlock;
  /*  34 */ u32 BlockNames;
  /*  38 */ u32 SFXUD;
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

struct XREFGrainParams {
  /*   0 */ u32 BankID;
  /*   4 */ u32 SoundIndex;
  /*   8 */ s32 PitchMod;
  /*   c */ u32 Flags;
};

struct RandDelayParams {
  /*   0 */ s32 Amount;
};

struct ControlParams {
  /*   0 */ s16 param[4];
};

struct LFOParams {
  /*   0 */ u8 which_lfo;
  /*   1 */ u8 target;
  /*   2 */ u8 target_extra;
  /*   3 */ u8 shape;
  /*   4 */ u16 duty_cycle;
  /*   6 */ u16 depth;
  /*   8 */ u16 flags;
  /*   a */ u16 start_offset;
  /*   c */ u32 step_size;
};

struct PlaySoundParams {
  /*   0 */ s32 vol;
  /*   4 */ s32 pan;
  /*   8 */ s8 reg_settings[4];
  /*   c */ s32 sound_id;
  /*  10 */ char snd_name[16];
};

struct PluginParams {
  /*   0 */ u32 id;
  /*   4 */ u32 index;
  /*   8 */ u8 data[24];
};

struct LargestGrainParamStruct {
  /*   0 */ char blank[32];
};

struct SFXGrain {
  /*   0 */ u32 Type;
  /*   4 */ s32 Delay;
  union {
    /*   8 */ Tone tone;
    /*   8 */ XREFGrainParams xref;
    /*   8 */ RandDelayParams delay;
    /*   8 */ ControlParams control;
    /*   8 */ LFOParams lfo;
    /*   8 */ PlaySoundParams play_sound;
    /*   8 */ PluginParams plugin_params;
    /*   8 */ LargestGrainParamStruct junk;
  } GrainParams;
};

struct SFX {
  /*   0 */ s8 Vol;
  /*   1 */ s8 VolGroup;
  /*   2 */ s16 Pan;
  /*   4 */ s8 NumGrains;
  /*   5 */ s8 InstanceLimit;
  /*   6 */ u16 Flags;
  /*   8 */ u32 FirstGrain;
};

struct Prog;
struct SoundBank {
  SoundBankData d;
  std::vector<Prog> programs;
  std::vector<MIDISound> sounds;
  std::unique_ptr<u8[]> sampleBuf;
};

struct SFXBlock {
  SFXBlockData d;
  std::vector<SFX> sounds;
  std::unique_ptr<u8[]> sampleBuf;
};

struct LocAndSize {
  /*   0 */ u32 offset;
  /*   4 */ u32 size;
};

template <size_t chunks>
struct FileAttributes {
  /*   0 */ u32 type;
  /*   4 */ u32 num_chunks;
  /*   8 */ LocAndSize where[chunks];
};

class loader : public locator {
 public:
  SoundBank& get_bank(u32 id) override;
  MIDIBlock* get_midi(u32 id);
  u16* get_bank_samples(u32 id) override;

  void unload_bank(u32 id);

  u32 read_bank(std::fstream& in);
  void load_midi(std::fstream& in);

  bool read_midi();

 private:
  void load_samples(u32 bank, std::unique_ptr<u8[]> samples);
  u32 read_music_bank(SoundBankData* data);
  u32 read_sfx_bank(SFXBlockData* data);

  std::unordered_map<u32, std::unique_ptr<u8[]>> m_bank_samples;

  std::unordered_map<u32, SoundBank> m_soundbanks;
  std::vector<SoundBank> m_soundblocks;

  std::vector<std::unique_ptr<u8[]>> m_midi_chunks;

  std::unordered_map<u32, MIDIBlock*> m_midi;

  u32 m_next_id{0};
};
}  // namespace snd
