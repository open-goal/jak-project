// Copyright: 2021 - 2024, Ziemas
// SPDX-License-Identifier: ISC
#include "loader.h"

#include <fstream>
#include <optional>

#include "sfxblock.h"

#include "common/log/log.h"
#include "common/util/BinaryReader.h"

#include "game/sound/989snd/musicbank.h"

#include "fmt/core.h"

namespace snd {

enum chunk : u32 { bank, samples, midi };

inline static constexpr u32 fourcc(std::string_view p) {
  return p[3] << 24 | p[2] << 16 | p[1] << 8 | p[0];
}

void FileAttributes::Read(BinaryReader& data) {
  type = data.read<u32>();
  num_chunks = data.read<u32>();

  where.resize(num_chunks);
  for (size_t i = 0; i < num_chunks; i++) {
    where[i].offset = data.read<u32>();
    where[i].size = data.read<u32>();
  }
}

static Tone ReadTone(BinaryReader& data, u8* samples) {
  Tone tone{};

  tone.Priority = data.read<s8>();
  tone.Vol = data.read<s8>();
  tone.CenterNote = data.read<s8>();
  tone.CenterFine = data.read<s8>();
  tone.Pan = data.read<s16>();
  tone.MapLow = data.read<s8>();
  tone.MapHigh = data.read<s8>();
  tone.PBLow = data.read<s8>();
  tone.PBHigh = data.read<s8>();
  tone.ADSR1 = data.read<u16>();
  tone.ADSR2 = data.read<u16>();
  tone.Flags = data.read<u16>();
  u32 SampleOffset = data.read<u32>();
  tone.Sample = &samples[SampleOffset];

  data.read<u32>();  // reserved1

  return tone;
};

static Midi ReadMidi(BinaryReader& data) {
  Midi mid;
  u8* base = const_cast<u8*>(data.here());

  mid.DataID = data.read<u32>();
  mid.Version = data.read<s16>();
  mid.Flags = data.read<s8>();
  data.read<s8>();
  mid.ID = data.read<u32>();
  data.read<u32>();
  mid.BankID = data.read<u32>();
  data.read<u32>();
  u32 DataStart = data.read<u32>();
  mid.DataStart = base + DataStart;
  data.read<u32>();
  mid.Tempo = data.read<u32>();
  mid.PPQ = data.read<s32>();

  return mid;
}

static MultiMidi ReadMultiMidi(BinaryReader& data) {
  MultiMidi mmid;

  mmid.DataID = data.read<u32>();
  mmid.Version = data.read<s16>();
  mmid.Flags = data.read<s8>();
  s8 NumSegments = data.read<s8>();
  mmid.midi.resize(NumSegments);
  mmid.ID = data.read<u32>();
  data.read<u32>();

  for (auto& midi : mmid.midi) {
    u32 offset = data.read<u32>();
    auto mr = data.at(offset);
    midi.Midi::operator=(ReadMidi(mr));
    midi.SoundHandle = mr.read<u32>();
  }

  return mmid;
}

static Grain ReadGrainV1(BinaryReader& data, u8* samples) {
  Grain grain{};

  u32 pos = data.get_seek();

  grain.Type = static_cast<GrainType>(data.read<u32>());
  grain.Delay = data.read<s32>();

  switch (grain.Type) {
    case GrainType::TONE:
    case GrainType::TONE2: {
      grain.data = ReadTone(data, samples);
    } break;
    case GrainType::LFO_SETTINGS: {
      grain.data = data.read<LFOParams>();
    } break;
    case GrainType::BRANCH:
    case GrainType::STARTCHILDSOUND:
    case GrainType::STOPCHILDSOUND:
      grain.data = data.read<PlaySoundParams>();
      break;
    case GrainType::PLUGIN_MESSAGE:
      grain.data = data.read<PluginParams>();
      break;
    case GrainType::RAND_DELAY:
      grain.data = data.read<RandDelayParams>();
      break;
    default:
      grain.data = data.read<ControlParams>();
      break;
  }

  data.set_seek(pos);
  data.ffwd(0x28);

  return grain;
};

static Grain ReadGrainV2(BinaryReader& data, BinaryReader grainData, u8* samples) {
  union OpcodeData {
    struct {
      s8 arg[3];
      u8 type;
    };

    u32 Opcode;
  } op;

  Grain grain{};

  op = data.read<OpcodeData>();
  grain.Type = static_cast<GrainType>(op.type);
  grain.Delay = data.read<s32>();

  u32 value = op.Opcode & 0xFFFFFF;

  switch (grain.Type) {
    case GrainType::TONE:
    case GrainType::TONE2: {
      grainData.set_seek(value);
      grain.data = ReadTone(grainData, samples);
    } break;
    case GrainType::LFO_SETTINGS: {
      grainData.set_seek(value);
      grain.data = grainData.read<LFOParams>();
    } break;
    case GrainType::BRANCH:
    case GrainType::STARTCHILDSOUND:
    case GrainType::STOPCHILDSOUND:
      grainData.set_seek(value);
      grain.data = grainData.read<PlaySoundParams>();
      break;
    case GrainType::PLUGIN_MESSAGE:
      grainData.set_seek(value);
      grain.data = grainData.read<PluginParams>();
      break;
    case GrainType::RAND_DELAY:
      RandDelayParams rp;
      rp.Amount = (s32)value + 1;
      grain.data = rp;
      break;
    default:
      ControlParams p;
      p.param[0] = op.arg[0];
      p.param[1] = op.arg[1];
      p.param[2] = op.arg[2];
      grain.data = p;
      break;
  }

  return grain;
};

SFXBlock* SFXBlock::ReadBlock(std::span<u8> bank_data, std::span<u8> samples) {
  BinaryReader data(bank_data);
  // auto block = std::make_unique<SFXBlock>();
  auto block = new SFXBlock();
  block->DataID = data.read<u32>();
  if (block->DataID != fourcc("SBlk")) {
    return nullptr;
  }

  block->SampleData = std::make_unique<u8[]>(samples.size());
  std::copy(samples.begin(), samples.end(), block->SampleData.get());

  block->Version = data.read<u32>();
  block->Flags.flags = data.read<u32>();
  block->BankID = data.read<u32>();
  block->BankNum = data.read<s8>();

  // discard padding
  data.read<s8>();
  data.read<s16>();
  data.read<s16>();

  s16 NumSounds = data.read<s16>();
  block->Sounds.resize(NumSounds);
  [[maybe_unused]] s16 NumGrains = data.read<s16>();
  [[maybe_unused]] s16 NumVAGs = data.read<s16>();

  u32 FirstSound = data.read<u32>();
  u32 FirstGrain = data.read<u32>();

  [[maybe_unused]] u32 VagsInSR = data.read<u32>();
  [[maybe_unused]] u32 VagDataSize = data.read<u32>();
  [[maybe_unused]] u32 SRAMAllocSize = data.read<u32>();
  [[maybe_unused]] u32 NextBlock = data.read<u32>();
  u32 GrainData = 0;
  if (block->Version >= 2) {
    GrainData = data.read<u32>();
  }
  u32 BlockNames = data.read<u32>();
  u32 SFXUD = data.read<u32>();

  data.set_seek(FirstSound);
  for (auto& sfx : block->Sounds) {
    sfx.Vol = data.read<s8>();
    sfx.VolGroup = data.read<s8>();
    sfx.Pan = data.read<s16>();

    s8 NumGrains = data.read<s8>();
    sfx.Grains.resize(NumGrains);

    sfx.InstanceLimit = data.read<s8>();
    sfx.Flags.flags = data.read<u16>();

    u32 FirstSFXGrain = data.read<u32>();
    if (NumGrains) {
      auto grains = data.at(FirstGrain + FirstSFXGrain);
      for (auto& grain : sfx.Grains) {
        if (block->Version < 2) {
          grain = ReadGrainV1(grains, block->SampleData.get());
        } else {
          grain = ReadGrainV2(grains, data.at(GrainData), block->SampleData.get());
        }
      }
    }
  }

  if (block->Flags.hasNames()) {
    struct SFXBlockNames {  // 0x98
      /* 0x00 */ u32 BlockName[2];
      /* 0x08 */ u32 SFXNameTableOffset;
      /* 0x0c */ u32 VAGNameTableOffset;
      /* 0x10 */ u32 VAGImportsTableOffset;
      /* 0x14 */ u32 VAGExportsTableOffset;
      /* 0x18 */ s16 SFXHashOffsets[32];
      /* 0x58 */ s16 VAGHashOffsets[32];
    };

    struct SFXName {  // 0x14
      /* 0x00 */ u32 Name[4];
      /* 0x10 */ s16 Index;
      /* 0x12 */ s16 reserved;
    };

    data.set_seek(BlockNames);
    auto names = data.read<SFXBlockNames>();

    char buf[17];
    buf[16] = 0;
    strncpy(buf, (char*)names.BlockName, 8);
    block->Name = buf;

    data.set_seek(BlockNames + names.SFXNameTableOffset);

    auto name_table = (SFXName*)(data.here());
    for (auto SFXHashOffset : names.SFXHashOffsets) {
      auto name = &name_table[SFXHashOffset];
      while (name->Name[0] != 0) {
        strncpy(buf, (char*)name->Name, 16);

        std::string str(buf);
        if (block->Names.find(str) == block->Names.end()) {
          block->Names[str] = name->Index;
        }

        name++;
      }
    }
  }

  if (block->Flags.hasUserdata()) {
    data.set_seek(SFXUD);
    for (auto& sfx : block->Sounds) {
      sfx.UserData.data[0] = data.read<u32>();
      sfx.UserData.data[1] = data.read<u32>();
      sfx.UserData.data[2] = data.read<u32>();
      sfx.UserData.data[3] = data.read<u32>();
    }
  }

  return block;
}

MusicBank* MusicBank::ReadBank(std::span<u8> bank_data,
                               std::span<u8> samples,
                               std::span<u8> midi_data) {
  BinaryReader data(bank_data);
  // auto bank = std::make_unique<MusicBank>();
  auto bank = new MusicBank();

  bank->DataID = data.read<u32>();
  if (bank->DataID != fourcc("SBv2")) {
    return nullptr;
  }

  bank->SampleData = std::make_unique<u8[]>(samples.size_bytes());
  std::copy(samples.begin(), samples.end(), bank->SampleData.get());

  bank->SeqData = std::make_unique<u8[]>(midi_data.size_bytes());
  std::copy(midi_data.begin(), midi_data.end(), bank->SeqData.get());

  bank->Version = data.read<u32>();
  bank->Flags.flags = data.read<u32>();
  bank->BankID = data.read<u32>();
  bank->BankNum = data.read<s8>();

  // discard padding
  data.read<s8>();
  data.read<s16>();

  s16 NumSounds = data.read<s16>();
  bank->Sounds.resize(NumSounds);
  s16 NumProgs = data.read<s16>();
  bank->Progs.resize(NumProgs);
  [[maybe_unused]] s16 NumTones = data.read<s16>();
  [[maybe_unused]] s16 NumVAGs = data.read<s16>();

  u32 FirstSound = data.read<u32>();
  u32 FirstProg = data.read<u32>();
  [[maybe_unused]] u32 FirstTone = data.read<u32>();

  // vagsinsr
  data.read<u32>();
  // vagdatasize
  data.read<u32>();

  data.set_seek(FirstSound);
  for (auto& sound : bank->Sounds) {
    sound.Type = data.read<s32>();
    data.read<u32>();  // bankptr
    data.read<u32>();  // ogbankptr
    sound.MIDIID = data.read<s32>();
    sound.Vol = data.read<s16>();
    sound.Repeats = data.read<s8>();
    sound.VolGroup = data.read<s8>();
    sound.Pan = data.read<s16>();
    sound.Index = data.read<s8>();
    sound.Flags = data.read<u8>();
    data.read<u32>();  // midiblockptr

    sound.Bank = bank;
    sound.OrigBank = bank;
  }

  data.set_seek(FirstProg);
  for (auto& prog : bank->Progs) {
    s8 NumTones = data.read<s8>();
    prog.Vol = data.read<s8>();
    prog.Pan = data.read<s16>();
    u32 FirstTone = data.read<u32>();
    prog.Tones.resize(NumTones);

    auto tones = data.at(FirstTone);
    for (auto& tone : prog.Tones) {
      tone = ReadTone(tones, bank->SampleData.get());
    }
  }

  auto seq_buf = std::span<u8>(bank->SeqData.get(), midi_data.size_bytes());
  BinaryReader seq_data(seq_buf);
  FileAttributes fa;
  fa.Read(seq_data);

  BinaryReader midi(seq_buf.subspan(fa.where[0].offset));
  u32 id = midi.read<u32>();
  midi.set_seek(0);
  if (id == fourcc("MID ")) {
    bank->MidiData = ReadMidi(midi);
  } else if (id == fourcc("MMID")) {
    bank->MidiData = ReadMultiMidi(midi);
  } else {
    lg::error("Invalid midi\n");
  }

  return bank;
}

BankHandle Loader::BankLoad(std::span<u8> bank) {
  BinaryReader reader(bank);
  FileAttributes fa;
  fa.Read(reader);

  if (fa.type != 1 && fa.type != 3) {
    fmt::print("bad file type\n");
    return nullptr;
  }

  reader.set_seek(fa.where[0].offset);
  u32 fourcc = reader.read<u32>();
  std::span<u8> bank_data(std::span<u8>(bank).subspan(fa.where[0].offset, fa.where[0].size));
  std::span<u8> sample_data(std::span<u8>(bank).subspan(fa.where[1].offset, fa.where[1].size));

  if (fourcc == snd::fourcc("SBv2")) {
    if (fa.num_chunks != 3) {
      fmt::print("SBv2 without midi data not supported\n");
      return 0;
    }
    std::span<u8> midi_data(std::span<u8>(bank).subspan(fa.where[2].offset, fa.where[2].size));

    auto bank = MusicBank::ReadBank(bank_data, sample_data, midi_data);
    mBanks.emplace_back(bank);

    return bank;
  } else if (fourcc == snd::fourcc("SBlk")) {
    auto block = SFXBlock::ReadBlock(bank_data, sample_data);
    mBanks.emplace_back(block);

    return block;
  }

  return nullptr;
}

SoundBank* Loader::GetBankByHandle(BankHandle handle) {
  auto bank = std::find_if(mBanks.begin(), mBanks.end(),
                           [handle](auto& bank) { return bank.get() == handle; });
  if (bank == mBanks.end()) {
    return nullptr;
  }

  return bank->get();
}

SoundBank* Loader::GetBankByName(const char* name) {
  for (auto& b : mBanks) {
    auto bankname = b->GetName();
    if (bankname.has_value()) {
      if (bankname->compare(name) == 0) {
        return b.get();
      }
    }
  }

  return nullptr;
}

SoundBank* Loader::GetBankWithSound(const char* name) {
  for (auto& b : mBanks) {
    auto sound = b->GetSoundByName(name);
    if (sound.has_value()) {
      return b.get();
    }
  }

  return nullptr;
}

void Loader::UnloadBank(BankHandle handle) {
  auto bank = std::find_if(mBanks.begin(), mBanks.end(),
                           [handle](auto& bank) { return bank.get() == handle; });
  if (bank != mBanks.end()) {
    mBanks.erase(bank);
  }
}

}  // namespace snd
