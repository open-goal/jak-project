// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "loader.h"

#include <fstream>
#include <optional>

#include "midi_handler.h"

#include <third-party/fmt/core.h>

namespace snd {
enum chunk : u32 { bank, samples, midi };

#define FOURCC(a, b, c, d) ((u32)(((d) << 24) | ((c) << 16) | ((b) << 8) | (a)))

u32 loader::read_music_bank(SoundBankData* data) {
  u32 handle = m_id_allocator.get_id();

  auto bank = std::make_unique<MusicBank>(*this);

  auto sound = (MIDISound*)((uintptr_t)data + data->FirstSound);
  for (int i = 0; i < data->NumSounds; i++) {
    bank->sounds.emplace_back(sound[i]);
  }

  auto progdata = (ProgData*)((uintptr_t)data + data->FirstProg);
  for (int i = 0; i < data->NumProgs; i++) {
    Prog prog;
    prog.d = progdata[i];
    bank->programs.emplace_back(std::move(prog));
  }

  for (auto& prog : bank->programs) {
    auto tonedata = (Tone*)((uintptr_t)data + prog.d.FirstTone);
    for (int i = 0; i < prog.d.NumTones; i++) {
      Tone tone = tonedata[i];
      tone.BankID = handle;
      prog.tones.emplace_back(tone);
    }
  }

  bank->type = BankType::Music;

  bank->bank_id = handle;
  bank->bank_name = data->BankID;
  m_soundbanks.emplace(handle, std::move(bank));

  return handle;
}

u32 loader::read_sfx_bank(SFXBlockData* data) {
  u32 handle = m_id_allocator.get_id();

  auto bank = std::make_unique<SFXBlock>(*this);

  auto sounddata = (SFXData*)((uintptr_t)data + data->FirstSound);
  for (int i = 0; i < data->NumSounds; i++) {
    SFX sound;
    sound.d = sounddata[i];
    bank->sounds.push_back(sound);
  }

  for (auto& sound : bank->sounds) {
    auto graindata = (SFXGrain*)((uintptr_t)data + data->FirstGrain + sound.d.FirstGrain);
    for (int i = 0; i < sound.d.NumGrains; i++) {
      SFXGrain grain = graindata[i];
      if (grain.Type == 1) {
        grain.GrainParams.tone.BankID = handle;
      }
      sound.grains.push_back(grain);
    }
  }

  bank->type = BankType::SFX;

  bank->bank_id = handle;
  m_soundbanks.emplace(handle, std::move(bank));
  return handle;
}

u32 loader::read_bank(std::fstream& in) {
  size_t origin = in.tellg();
  FileAttributes<3> attr;
  in.read((char*)(&attr), sizeof(attr));

  if (attr.type != 1 && attr.type != 3) {
    fmt::print("Error: File type {} not supported.", attr.type);
    return -1;
  }

  if (attr.num_chunks > 2) {
    // Fix for bugged tooling I assume?
    attr.where[chunk::bank].size += 4;
  }

  // auto pos = in.tellg();
  auto bank_buf = std::make_unique<u8[]>(attr.where[chunk::bank].size);
  in.seekg(origin + attr.where[chunk::bank].offset, std::fstream::beg);
  in.read((char*)bank_buf.get(), attr.where[chunk::bank].size);
  auto bank = (BankTag*)bank_buf.get();

  u32 bank_id = 0;

  if (bank->DataID == FOURCC('S', 'B', 'v', '2')) {
    bank_id = read_music_bank((SoundBankData*)bank_buf.get());
  } else if (bank->DataID == FOURCC('S', 'B', 'l', 'k')) {
    bank_id = read_sfx_bank((SFXBlockData*)bank_buf.get());
  } else {
    throw std::runtime_error("Unknown bank ID, bad file?");
  }

  if (attr.num_chunks >= 2) {
    in.seekg(origin + attr.where[chunk::samples].offset, std::fstream::beg);
    auto samples = std::make_unique<u8[]>(attr.where[chunk::samples].size);
    in.read((char*)samples.get(), attr.where[chunk::samples].size);
    load_samples(bank_id, std::move(samples));
  }

  if (attr.num_chunks >= 3) {
    in.seekg(origin + attr.where[chunk::midi].offset, std::fstream::beg);
    load_midi(in);
  }

  return bank_id;
}

void loader::load_midi(std::fstream& in) {
  FileAttributes<1> attr;
  u32 cur = in.tellg();

  in.read((char*)&attr, sizeof(attr));
  in.seekg(cur + attr.where[0].offset, std::fstream::beg);

  auto midi = std::make_unique<u8[]>(attr.where[0].size);
  in.read((char*)midi.get(), attr.where[0].size);

  auto h = (MIDIBlock*)midi.get();
  fmt::print("Loaded midi {:.4}\n", (char*)&h->ID);

  m_midi.emplace(h->ID, (MIDIBlock*)midi.get());
  m_midi_chunks.emplace_back(std::move(midi));
}

SoundBank* loader::get_bank_by_handle(u32 id) {
  if (m_soundbanks.find(id) == m_soundbanks.end()) {
    return nullptr;
  }

  return m_soundbanks[id].get();
}

MusicBank* loader::get_bank_by_name(u32 id) {
  for (auto& b : m_soundbanks) {
    if (b.second->type == BankType::Music) {
      auto* bank = static_cast<MusicBank*>(b.second.get());
      if (bank->bank_name == id) {
        return bank;
      }
    }
  }

  return nullptr;
}

MIDIBlock* loader::get_midi(u32 id) {
  return m_midi.at(id);
}

u8* loader::get_bank_samples(u32 id) {
  return m_soundbanks.at(id).get()->sampleBuf.get();
}

void loader::load_samples(u32 bank_id, std::unique_ptr<u8[]> samples) {
  auto& bank = m_soundbanks.at(bank_id);
  bank->sampleBuf = std::move(samples);
}

void loader::unload_bank(u32 id) {
  fmt::print("Deleting bank {}\n", id);
  for (auto it = m_midi_chunks.begin(); it != m_midi_chunks.end();) {
    bool del = false;
    // FIXME delete midi

    if (del) {
      it = m_midi_chunks.erase(it);
    } else {
      ++it;
    }
  }

  m_soundbanks.erase(id);
  m_id_allocator.free_id(id);
}

}  // namespace snd
