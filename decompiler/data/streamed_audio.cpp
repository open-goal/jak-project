
#include "streamed_audio.h"

#include "common/audio/audio_formats.h"
#include "common/log/log.h"
#include "common/util/BinaryReader.h"
#include "common/util/FileUtil.h"

#include "third-party/fmt/core.h"
#include "third-party/json.hpp"

namespace decompiler {

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
    s64 start_byte = -1;
    s64 end_byte = -1;
  };

  std::vector<Entry> entries;

  void set_file_size(u64 size) {
    if (!entries.empty()) {
      entries.back().end_byte = size;
    }
  }

  int entry_count() const { return entries.size(); }

  void debug_print() const {
    for (auto& e : entries) {
      lg::debug("\"{}\" 0x{:07x} - 0x{:07x}", e.name, e.start_byte, e.end_byte);
    }
  }
};

/*!
 * Read an entry from a WAD and return the binary data.
 */
std::vector<u8> read_entry(const AudioDir& dir, const std::vector<u8>& data, int entry_idx) {
  const auto& entry = dir.entries.at(entry_idx);
  ASSERT(entry.end_byte > 0);
  return std::vector<u8>(data.begin() + entry.start_byte, data.begin() + entry.end_byte);
}

/*!
 * Matches the format in file.
 */
struct VagFileHeader {
  char magic[4];
  u32 version;
  u32 zero;
  u32 channel_size;
  u32 sample_rate;
  u32 z[3];
  char name[16];

  VagFileHeader swapped_endian() const {
    VagFileHeader result(*this);
    result.version = swap32(result.version);
    result.channel_size = swap32(result.channel_size);
    result.sample_rate = swap32(result.sample_rate);
    return result;
  }

  void debug_print() {
    char temp_name[17];
    memcpy(temp_name, name, 16);
    temp_name[16] = '\0';
    lg::debug("{}{}{}{} v {} zero {} chan {} samp {} z {} {} {} name {}", magic[0], magic[1],
              magic[2], magic[3], version, zero, channel_size, sample_rate, z[0], z[1], z[2],
              temp_name);
  }
};

/*!
 * Read the DIR file into an AudioDir
 */
AudioDir read_audio_dir(const fs::path& path) {
  // matches the format in file.
  struct DirEntry {
    char name[8];
    u32 value;
  };
  auto data = file_util::read_binary_file(path);
  lg::info("Got {} bytes of audio dir.", data.size());
  auto reader = BinaryReader(data);

  u32 count = reader.read<u32>();
  u32 data_end = sizeof(u32) + sizeof(DirEntry) * count;
  ASSERT(data_end <= data.size());
  std::vector<DirEntry> entries;
  for (u32 i = 0; i < count; i++) {
    entries.push_back(reader.read<DirEntry>());
  }

  while (reader.bytes_left()) {
    ASSERT(reader.read<u8>() == 0);
  }

  AudioDir result;

  ASSERT(!entries.empty());
  for (size_t i = 0; i < entries.size(); i++) {
    AudioDir::Entry e;
    for (auto c : entries[i].name) {
      // padded with spaces, no null terminator.
      e.name.push_back(c);
    }
    e.start_byte = AUDIO_PAGE_SIZE * entries[i].value;
    if (i + 1 < (entries.size())) {
      e.end_byte = AUDIO_PAGE_SIZE * entries[i + 1].value;
    } else {
      e.end_byte = -1;
    }
    result.entries.push_back(e);
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
                                 const std::vector<u8>& data,
                                 const std::string& name,
                                 const std::string& suffix) {
  BinaryReader reader(data);

  auto header = reader.read<VagFileHeader>();
  if (header.magic[0] == 'V') {
    header = header.swapped_endian();
  } else {
    ASSERT(false);
  }
  header.debug_print();

  for (int i = 0; i < 16; i++) {
    ASSERT(reader.read<u8>() == 0);
  }

  std::vector<s16> decoded_samples = decode_adpcm(reader);

  while (reader.bytes_left()) {
    ASSERT(reader.read<u8>() == 0);
  }

  file_util::create_dir_if_needed(output_folder / suffix);
  auto file_name = fmt::format("{}.wav", remove_trailing_spaces(name));
  write_wave_file_mono(decoded_samples, header.sample_rate, output_folder / suffix / file_name);

  std::string vag_filename;
  for (int i = 0; i < 16; i++) {
    if (header.name[i]) {
      vag_filename.push_back(header.name[i]);
    }
  }
  return {vag_filename, (double)decoded_samples.size() / header.sample_rate};
}

void process_streamed_audio(const fs::path& output_path,
                            const fs::path& input_dir,
                            const std::vector<std::string>& audio_files) {
  auto dir_data = read_audio_dir(input_dir / "VAG" / "VAGDIR.AYB");
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
    auto suffix = fs::path(file).extension().u8string().substr(1);
    langs.push_back(suffix);
    dir_data.set_file_size(wad_data.size());
    for (int i = 0; i < dir_data.entry_count(); i++) {
      auto audio_data = read_entry(dir_data, wad_data, i);
      lg::info("File {}, total {:.2f} minutes", dir_data.entries.at(i).name, audio_len / 60.0);
      auto info = process_audio_file(output_path, audio_data, dir_data.entries.at(i).name, suffix);
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
