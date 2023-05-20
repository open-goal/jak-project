// Copyright: 2021 - 2022, Ziemas
// SPDX-License-Identifier: ISC
#include "loader.h"

#include <fstream>
#include <optional>

#include "midi_handler.h"
#include "sfxblock.h"

#include "common/log/log.h"

#include "sfxblock2.h"

#include "third-party/fmt/core.h"

namespace snd {
enum chunk : u32 { bank, samples, midi };

#define FOURCC(a, b, c, d) ((u32)(((d) << 24) | ((c) << 16) | ((b) << 8) | (a)))

u32 loader::read_bank(std::fstream& in) {
  size_t origin = in.tellg();
  FileAttributes<3> attr;
  in.read((char*)(&attr), sizeof(attr));

  if (attr.type != 1 && attr.type != 3) {
    lg::error("Error: File type {} not supported.", attr.type);
    return -1;
  }

  /*
   * if there's midi data the pointer to the allocated memory is stored
   * just before the sound bank data...
  if (attr.num_chunks > 2) {
    attr.where[chunk::bank].size += 4;
  }
  */

  // auto pos = in.tellg();
  auto bank_buf = std::make_unique<u8[]>(attr.where[chunk::bank].size);
  in.seekg(origin + attr.where[chunk::bank].offset, std::fstream::beg);
  in.read((char*)bank_buf.get(), attr.where[chunk::bank].size);
  auto bank_tag = (BankTag*)bank_buf.get();

  u32 bank_id = m_id_allocator.get_id();
  std::unique_ptr<SoundBank> bank;

  if (bank_tag->DataID == FOURCC('S', 'B', 'v', '2')) {
    bank = std::make_unique<MusicBank>(*this, bank_id, bank_tag);
  } else if (bank_tag->DataID == FOURCC('S', 'B', 'l', 'k')) {
    if (bank_tag->Version < 2) {
      bank = std::make_unique<SFXBlock>(*this, bank_id, bank_tag);
    } else {
      bank = std::make_unique<SFXBlock2>(*this, bank_id, bank_tag);
    }
  } else {
    m_id_allocator.free_id(bank_id);
    throw std::runtime_error("Unknown bank ID, bad file?");
  }

  m_soundbanks.emplace(bank_id, std::move(bank));

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
  lg::info("Loaded midi {:.4}", (char*)&h->ID);

  m_midi.emplace(h->ID, (MIDIBlock*)midi.get());
  m_midi_chunks.emplace_back(std::move(midi));
}

SoundBank* loader::get_bank_by_handle(u32 id) {
  if (m_soundbanks.find(id) == m_soundbanks.end()) {
    return nullptr;
  }

  return m_soundbanks[id].get();
}

MusicBank* loader::get_bank_by_id(u32 id) {
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

SoundBank* loader::get_bank_by_name(const char* name) {
  for (auto& b : m_soundbanks) {
    auto bankname = b.second->get_name();
    if (bankname.has_value()) {
      if (bankname->compare(name) == 0) {
        return b.second.get();
      }
    }
  }

  return nullptr;
}

SoundBank* loader::get_bank_with_sound(const char* name) {
  for (auto& b : m_soundbanks) {
    auto sound = b.second->get_sound_by_name(name);
    if (sound.has_value()) {
      return b.second.get();
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
