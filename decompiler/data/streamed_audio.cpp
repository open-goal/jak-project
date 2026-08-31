
#include "streamed_audio.h"
#include <cstdint>

#include "common/audio/audio_formats.h"
#include "common/log/log.h"
#include "common/util/BinaryReader.h"
#include "common/util/FileUtil.h"
#include "game/sound/989snd/fakeplayer.h"
#include "game/sound/989snd/musicbank.h"
#include "game/sound/989snd/sfxblock.h"
#include "game/sound/common/sound_types.h"
#include "game/sound/common/flava.h"
#include "fmt/format.h"
#include "third-party/json.hpp"

namespace decompiler {
using std::string;

// number of bytes per "audio page" in the VAG directory file.
constexpr int AUDIO_PAGE_SIZE = 2048;

// Swap endian of 32-bit value.
uint32_t swap32(uint32_t in) {
  return ((in << 24) | ((in & 0xff00) << 8) | ((in & 0xff0000) >> 8) | (in >> 24));
}

/*!
 * A processed version of the VAGDIR file containing a map from 8-char name to location in the
 * WAD files.
 */
struct AudioDir {
  struct Entry {
    std::string name;
    bool stereo = false;
    bool international = false;
    s64 start_byte = -1;
  };

  std::vector<Entry> entries;
  u32 version = 1;

  int entry_count() const { return entries.size(); }

  void debug_print() const {
    // for (auto& e : entries) {
    // lg::debug("\"{}\" 0x{:07x} - 0x{:07x}", e.name, e.start_byte, e.end_byte);
    // }
  }
};

/*!
 * Matches the format in file.
 */
struct VagFileHeader {
  u32 magic;
  u32 version;
  u32 zero;
  u32 size;
  u32 sample_rate;
  u32 z[3];
  char name[16];

  VagFileHeader swap_endian() const {
    VagFileHeader result(*this);
    result.version = swap32(result.version);
    result.size = swap32(result.size);
    result.sample_rate = swap32(result.sample_rate);
    return result;
  }

  void debug_print() {
    char temp_name[17];
    memcpy(temp_name, name, 16);
    temp_name[16] = '\0';
    lg::debug("{:x} v {} zero {} chan {} samp {} z {} {} {} name {}", magic, version, zero, size,
              sample_rate, z[0], z[1], z[2], temp_name);
  }
};

static std::string unpack_vag_name_jak3(u64 compressed) {
  const char* char_map = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-";
  u32 chars = compressed & 0x1fffff;
  std::array<char, 9> buf;
  buf.fill(0);
  for (int i = 0; i < 8; i++) {
    if (i == 4) {
      chars = (compressed >> 21) & 0x1fffff;
    }
    buf[7 - i] = char_map[chars % 38];
    chars /= 38;
  }

  return {buf.data()};
}

/*!
 * Read the DIR file into an AudioDir
 */
AudioDir read_audio_dir(const decompiler::Config& config, const fs::path& path) {
  auto data = file_util::read_binary_file(path);
  lg::info("Got {} bytes of audio dir.", data.size());
  auto reader = BinaryReader(data);
  AudioDir result;
  if (config.game_version == GameVersion::Jak1) {
    u32 count = reader.read<u32>();
    // matches the format in file.
    struct DirEntryJak1 {
      char name[8];
      u32 value;
    };
    u32 data_end = sizeof(u32) + sizeof(DirEntryJak1) * count;
    ASSERT(data_end <= data.size());
    std::vector<DirEntryJak1> entries;
    for (u32 i = 0; i < count; i++) {
      entries.push_back(reader.read<DirEntryJak1>());
    }

    while (reader.bytes_left()) {
      ASSERT(reader.read<u8>() == 0);
    }
    ASSERT(!entries.empty());
    for (size_t i = 0; i < entries.size(); i++) {
      AudioDir::Entry e;
      for (auto c : entries[i].name) {
        // padded with spaces, no null terminator.
        e.name.push_back(c);
      }
      e.start_byte = AUDIO_PAGE_SIZE * entries[i].value;
      result.entries.push_back(e);
    }
  } else if (config.game_version == GameVersion::Jak2) {
    u32 count = reader.read<u32>();
    // matches the format in file.
    struct DirEntryJak2 {
      char name[8];
      u32 value;
      u32 stereo;
    };
    u32 data_end = sizeof(u32) + sizeof(DirEntryJak2) * count;
    ASSERT(data_end <= data.size());
    std::vector<DirEntryJak2> entries;
    for (u32 i = 0; i < count; i++) {
      entries.push_back(reader.read<DirEntryJak2>());
    }

    while (reader.bytes_left()) {
      ASSERT(reader.read<u8>() == 0);
    }
    ASSERT(!entries.empty());
    for (size_t i = 0; i < entries.size(); i++) {
      AudioDir::Entry e;
      for (auto c : entries[i].name) {
        // padded with spaces, no null terminator.
        e.name.push_back(c);
      }
      e.stereo = entries[i].stereo;
      e.start_byte = AUDIO_PAGE_SIZE * entries[i].value;
      result.entries.push_back(e);
    }
  } else if (config.game_version == GameVersion::Jak3 || config.game_version == GameVersion::JakX) {
    struct VagDirJak3 {
      u32 id[2];
      u32 version;
      u32 count;
    } dir;
    struct DirEntryJak3 {
      union {
        u64 data;
        struct {
          u64 name : 42;
          u64 stereo : 1;
          u64 international : 1;
          u64 param : 4;
          u64 offset : 16;
        };
      };
    };

    static_assert(sizeof(DirEntryJak3) == sizeof(u64));

    dir = reader.read<VagDirJak3>();
    ASSERT(dir.id[0] == 0x41574756);
    ASSERT(dir.id[1] == 0x52494444);
    lg::warn("version {} count {}", dir.version, dir.count);

    result.version = dir.version;

    std::vector<DirEntryJak3> entries;

    for (size_t i = 0; i < dir.count; i++) {
      entries.push_back(reader.read<DirEntryJak3>());
    }

    for (size_t i = 0; i < entries.size(); i++) {
      AudioDir::Entry e;
      e.name = unpack_vag_name_jak3(entries[i].name);
      e.stereo = entries[i].stereo;
      e.international = entries[i].international;
      e.start_byte = 0x8000 * entries[i].offset;
      result.entries.push_back(e);
    }
  } else {
    ASSERT_MSG(false, "Unsupported game version for extracting streaming audio");
  }
  return result;
}

std::string remove_trailing_spaces(const std::string& in) {
  auto short_name = in;
  while (!short_name.empty() && short_name.back() == ' ') {
    short_name.pop_back();
  }
  return short_name;
}

struct AudioFileInfo {
  std::string filename;
  double length_seconds;
};

AudioFileInfo process_audio_file(const fs::path& output_folder,
                                 std::span<const uint8_t> data,
                                 const std::string& name,
                                 const std::string& suffix,
                                 bool stereo,
                                 u32 version) {
  BinaryReader reader(data);

  auto header = reader.read<VagFileHeader>();
  if (header.magic == 0x70474156 /* big endian (VAGp)*/) {
    header = header.swap_endian();
  } else if (header.magic != 0x56414770 /* little endian (pGAV) */) {
    ASSERT(false);
  }
  header.debug_print();

  reader = BinaryReader(data.subspan(0, header.size));
  const auto [left_samples, right_samples] = decode_adpcm(reader, stereo, version);

  while (reader.bytes_left()) {
    ASSERT(reader.read<u8>() == 0);
  }

  file_util::create_dir_if_needed(output_folder / suffix);
  auto file_name = fmt::format("{}.wav", remove_trailing_spaces(name));
  write_wave_file(left_samples, right_samples, header.sample_rate,
                  output_folder / suffix / file_name);

  std::string vag_filename;
  for (int i = 0; i < 16; i++) {
    if (header.name[i]) {
      vag_filename.push_back(header.name[i]);
    }
  }
  return {vag_filename,
          ((double)left_samples.size() + (double)right_samples.size()) / header.sample_rate};
}

size_t find_bank_offset(const std::vector<u8>& d) {
  auto u32_at = [&](size_t p) {
    u32 v = 0;
    std::memcpy(&v, d.data() + p, sizeof(v));
    return v;
  };
  auto is_fourcc = [&](size_t p, const char* cc) {
    return p + 4 <= d.size() && d[p] == static_cast<u8>(cc[0]) &&
           d[p + 1] == static_cast<u8>(cc[1]) && d[p + 2] == static_cast<u8>(cc[2]) &&
           d[p + 3] == static_cast<u8>(cc[3]);
  };

  for (size_t off = 0; off + 16 <= d.size(); off += 2048) {
    const u32 type = u32_at(off);
    if (type != 1 && type != 3) {
      continue;
    }
    const u32 num_chunks = u32_at(off + 4);
    if (num_chunks < 2 || num_chunks > 3 || off + 8 + static_cast<size_t>(num_chunks) * 8 > d.size()) {
      continue;
    }
    bool chunks_ok = true;
    for (u32 i = 0; i < num_chunks; i++) {
      const u32 coff = u32_at(off + 8 + i * 8);
      const u32 csz = u32_at(off + 8 + i * 8 + 4);
      if (off + static_cast<size_t>(coff) + csz > d.size()) {
        chunks_ok = false;
        break;
      }
    }
    if (chunks_ok && (is_fourcc(off + u32_at(off + 8), "SBlk") ||
                      is_fourcc(off + u32_at(off + 8), "SBv2"))) {
      return off;
    }
  }
  return SIZE_MAX;
}

void parse_jak1_name_table(const std::vector<u8>& d,
                           size_t limit,
                           std::string& bank_name,
                           std::vector<std::string>& names) {
  bank_name.clear();
  names.clear();
  if (limit < 0x18 || limit > d.size()) {
    return;
  }
  auto read_name16 = [&](size_t p) {
    char buf[17];
    std::memcpy(buf, d.data() + p, 16);
    buf[16] = 0;
    return std::string(buf);
  };
  u32 count = 0;
  std::memcpy(&count, d.data() + 0x14, sizeof(count));

  bank_name = read_name16(0);
  size_t pos = 0x18;
  for (u32 i = 0; i < count && pos + 20 <= limit; i++, pos += 20) {
    names.push_back(read_name16(pos));
  }
}

void process_music(const fs::path& output_path, 
                const fs::path& input_dir) {
  auto dir = input_dir / "MUS";
  std::vector<fs::path> musFiles = file_util::find_files_in_dir(dir, std::regex(".*\\.MUS"));
  double audio_len = 0.f;

  //Create a fake player that will generate the samples to play the music tracks exactly as they are in the games :)
  snd::FakePlayer fakeplayer; 
  std::vector<s16> left_samples, right_samples;
  //Generate three minutes worth of music
  const s64 THREE_MINUTES = snd::SAMPLE_RATE * 180;
  left_samples.reserve(THREE_MINUTES);
  right_samples.reserve(THREE_MINUTES);
  for(auto& mus : musFiles){
    auto mus_name = remove_trailing_spaces(mus.filename().replace_extension("").string());
    auto data = file_util::read_binary_file( mus );

    //Skip TWEAKVAL which is not a real musicbank
    if(mus_name == "TWEAKVAL")
      continue;

    std::vector<std::string> sfx_names;
    const size_t bank_offset = find_bank_offset(data); //Find where the music bank starts
    if (bank_offset == SIZE_MAX) {
      lg::error("'{}' is not a valid .MUS bank.", mus_name);
      return;
    }

    auto output_folder= output_path / mus_name;
    file_util::create_dir_if_needed(output_folder);
    snd::MusicBank *bank = (snd::MusicBank*)fakeplayer.LoadBank(std::span<u8>(data).subspan(bank_offset));

    const auto flava_set = flava::lookup(mus_name);
    if(flava_set)
    {
      for(auto &flavaVariant : flava_set->variants)
      {
        const auto variantName = std::string(flavaVariant.name);
        if (variantName == "none")
          continue;

        fakeplayer.PlaySound(bank, 0, snd::MAX_VOLUME, 0, 0, 0);
        if (flavaVariant.value > 0){
          //Play for a tenth of a second before setting the register, then clear left/right samples so they don't get added to track.
          //This seems to help ensure that the correct flava actually plays.
          fakeplayer.Tick(left_samples, right_samples, snd::SAMPLE_RATE / 10);
          fakeplayer.SetSoundReg(flava_set->reg, flavaVariant.value);
          left_samples.clear();
          right_samples.clear();
        }
        fakeplayer.Tick(left_samples, right_samples, THREE_MINUTES);
  
        auto file_name = variantName == "default" ? mus_name : mus_name + '_' + variantName;
        file_name = fmt::format("{}.wav", file_name);
        write_wave_file(left_samples, right_samples, snd::SAMPLE_RATE,
                output_folder / file_name);
        audio_len += left_samples.size() / (float)snd::SAMPLE_RATE;
  
        left_samples.clear();
        right_samples.clear();
        fakeplayer.StopSound();
      }
    }
    //If no flavaset, just convert sound 0 with no fuss
    else{
      fakeplayer.PlaySound(bank, 0, snd::MAX_VOLUME, 0, 0, 0);
      fakeplayer.Tick(left_samples, right_samples, THREE_MINUTES);

      auto file_name = fmt::format("{}.wav", mus_name);
      write_wave_file(left_samples, right_samples, snd::SAMPLE_RATE,
              output_folder / file_name);
      audio_len += left_samples.size() / (float)snd::SAMPLE_RATE;

      left_samples.clear();
      right_samples.clear();
    }
    
    fakeplayer.UnloadBank(bank);
    lg::info("File {}, total {:.2f} minutes", mus.filename().string(), audio_len / 60.0);
  }
}

void process_sfx(const fs::path& output_path, 
                const fs::path& input_dir) {
  auto dir = input_dir / "SBK";
  std::vector<fs::path> sbkFiles = file_util::find_files_in_dir(dir, std::regex(".*\\.SBK"));
  double audio_len = 0.f;

  //Create a fake player that will generate the samples to play the sounds exactly as they are in the games :)
  snd::FakePlayer fakeplayer; 
  std::vector<s16> left_samples, right_samples;
  const s64 TEN_SECONDS = snd::SAMPLE_RATE * 10;
  left_samples.reserve(TEN_SECONDS);
  right_samples.reserve(TEN_SECONDS);
  for(auto& sbk : sbkFiles){
    auto sbk_name = sbk.filename().replace_extension("");
    auto data = file_util::read_binary_file(sbk);


    const size_t bank_offset = find_bank_offset(data); //Find where the sfx bank starts
    std::vector<std::string> sfx_names;
    if (bank_offset == SIZE_MAX) {
      lg::error("'{}' is not a valid .SBK bank.", sbk_name.string());
      return;
    }
    if (bank_offset > 0) {
      std::string bank_name;
      parse_jak1_name_table(data, bank_offset, bank_name, sfx_names);
    }

    auto output_folder= output_path / sbk_name;
    file_util::create_dir_if_needed(output_folder);
    snd::SFXBlock *block = (snd::SFXBlock*)fakeplayer.LoadBank(std::span<u8>(data).subspan(bank_offset));


    std::map<u32, std::string> names_by_index;
    for (const auto& [name, index] : block->Names) {
      names_by_index.insert({index,name});
    }

    for(u32 sound_index = 0; sound_index < block->Sounds.size(); ++sound_index)
    {
      fakeplayer.PlaySound(block, sound_index, snd::MAX_VOLUME, 0, 0, 0);
      fakeplayer.Tick(left_samples, right_samples, TEN_SECONDS);

      std::string name;
      if (auto it = names_by_index.find(sound_index); it != names_by_index.end())
        name = it->second;
      else if (sound_index < sfx_names.size())
        name = sfx_names[sound_index];
      else
        name = "sound" + std::to_string(sound_index);


      auto file_name = fmt::format("{}.wav", remove_trailing_spaces(name));
      write_wave_file(left_samples, right_samples, snd::SAMPLE_RATE,
              output_folder / file_name);
      audio_len += left_samples.size() / (float)snd::SAMPLE_RATE;

      left_samples.clear();
      right_samples.clear();
    }
    
    fakeplayer.UnloadBank(block);
    lg::info("File {}, total {:.2f} minutes", sbk.filename().string(), audio_len / 60.0);
  }
}

void process_streamed_audio(const decompiler::Config& config,
                            const fs::path& output_path,
                            const fs::path& input_dir,
                            const std::vector<std::string>& audio_files) {
  auto dir_data = read_audio_dir(config, input_dir / "VAG" / "VAGDIR.AYB");
  double audio_len = 0.f;

  std::vector<std::string> langs;
  std::vector<std::vector<std::string>> filename_data;
  for (auto& e : dir_data.entries) {
    std::vector<std::string> placeholders = {remove_trailing_spaces(e.name)};
    for (size_t i = 0; i < audio_files.size(); i++) {
      placeholders.push_back("????");
    }
    filename_data.push_back(placeholders);
  }

  for (size_t lang_id = 0; lang_id < audio_files.size(); lang_id++) {
    auto& file = audio_files[lang_id];
    auto wad_data = file_util::read_binary_file(input_dir / "VAG" / file);
    auto suffix = fs::path(file).extension().string().substr(1);
    bool int_bank_p = suffix.compare("INT") == 0;
    langs.push_back(suffix);
    for (int i = 0; i < dir_data.entry_count(); i++) {
      auto entry = dir_data.entries.at(i);
      if (entry.international != int_bank_p) {
        continue;
      }

      lg::info("File {}, total {:.2f} minutes", entry.name, audio_len / 60.0);
      auto data = std::span(wad_data).subspan(entry.start_byte);
      auto info =
          process_audio_file(output_path, data, entry.name, suffix, entry.stereo, dir_data.version);
      audio_len += info.length_seconds;
      filename_data[i][lang_id + 1] = info.filename;
    }
  }

  nlohmann::json file_list;
  file_list["names"] = filename_data;
  file_list["languages"] = langs;

  file_util::write_text_file(output_path / "file_list.txt", file_list.dump(2));
}

}  // namespace decompiler
