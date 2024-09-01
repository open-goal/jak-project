
#include "streamed_audio.h"

#include "common/audio/audio_formats.h"
#include "common/log/log.h"
#include "common/util/BinaryReader.h"
#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "fmt/core.h"
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
  } else if (config.game_version == GameVersion::Jak3) {
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
                                 bool stereo) {
  BinaryReader reader(data);

  auto header = reader.read<VagFileHeader>();
  if (header.magic == 0x70474156 /* big endian (VAGp)*/) {
    header = header.swap_endian();
  } else if (header.magic != 0x56414770 /* little endian (pGAV) */) {
    ASSERT(false);
  }
  header.debug_print();

  reader = BinaryReader(data.subspan(0, header.size));
  const auto [left_samples, right_samples] = decode_adpcm(reader, stereo);

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
      auto info = process_audio_file(output_path, data, entry.name, suffix, entry.stereo);
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
