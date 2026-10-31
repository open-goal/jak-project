#include "vag.h"

#include <algorithm>
#include <cstring>
#include <stdexcept>

#include "common/audio/audio_formats.h"
#include "common/util/Assert.h"
#include "common/util/BinaryReader.h"

#include "fmt/format.h"

namespace vag {

namespace {

constexpr int BLOCK_SIZE = 16;

constexpr u32 JAK3_DIR_MAGIC_0 = 0x41574756;  // "VGWA" (little endian)
constexpr u32 JAK3_DIR_MAGIC_1 = 0x52494444;  // "DDIR"
constexpr const char* JAK3_CHAR_MAP = " ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-";
constexpr u32 JAK3_CHAR_COUNT = 38;

constexpr u32 MAGIC_BIG_ENDIAN_FIELDS = 0x70474156;     // bytes "VAGp"
constexpr u32 MAGIC_LITTLE_ENDIAN_FIELDS = 0x56414770;  // bytes "pGAV"

u32 read_u32(const u8* p) {
  return p[0] | (p[1] << 8) | (p[2] << 16) | ((u32)p[3] << 24);
}

u64 read_u64(const u8* p) {
  return (u64)read_u32(p) | ((u64)read_u32(p + 4) << 32);
}

void write_u32(std::vector<u8>& out, u32 v) {
  for (int i = 0; i < 4; i++) {
    out.push_back((v >> (8 * i)) & 0xff);
  }
}

void write_u64(std::vector<u8>& out, u64 v) {
  write_u32(out, v & 0xffffffff);
  write_u32(out, v >> 32);
}

u32 swap32(u32 v) {
  return (v << 24) | ((v & 0xff00) << 8) | ((v & 0xff0000) >> 8) | (v >> 24);
}

std::string unpack_name_jak3(u64 packed) {
  u32 chars = packed & 0x1fffff;
  std::string buf(8, ' ');
  for (int i = 0; i < 8; i++) {
    if (i == 4) {
      chars = (packed >> 21) & 0x1fffff;
    }
    buf[7 - i] = JAK3_CHAR_MAP[chars % JAK3_CHAR_COUNT];
    chars /= JAK3_CHAR_COUNT;
  }
  return buf;
}

u64 pack_name_jak3(const std::string& name) {
  std::string padded = pad_name(name);
  auto pack4 = [&](int start) {
    u32 result = 0;
    for (int i = 0; i < 4; i++) {
      const char* pos = strchr(JAK3_CHAR_MAP, padded[start + i]);
      ASSERT(pos && padded[start + i]);
      result = result * JAK3_CHAR_COUNT + (pos - JAK3_CHAR_MAP);
    }
    return result;
  };
  return (u64)pack4(4) | ((u64)pack4(0) << 21);
}

std::vector<u8> make_block(u8 shift_filter, u8 flags) {
  std::vector<u8> result(BLOCK_SIZE, 0);
  result[0] = shift_filter;
  result[1] = flags;
  return result;
}

/*!
 * The stream for a single channel, this is what goes after the header.
 * Like the original files, it starts with a silent block. Jak 1 and 3 end with a block with the
 * "end" flag, followed by a silent block that loops forever. Jak 2 has no flags at all.
 */
std::vector<u8> build_channel_stream(Game game, const std::vector<s16>& samples, size_t length) {
  std::vector<s16> padded = samples;
  padded.resize(length, 0);
  auto audio = encode_adpcm(padded);

  std::vector<u8> stream = make_block(0, 0);
  if (audio.empty()) {
    // encode at least one block so there is something to flag.
    audio.resize(16, 0);
  }
  if (game != Game::Jak2) {
    audio[audio.size() - 16 + 1] = 1;
  }
  stream.insert(stream.end(), audio.begin(), audio.end());
  if (game != Game::Jak2) {
    auto loop = make_block(0x0c, 7);
    stream.insert(stream.end(), loop.begin(), loop.end());
  }
  return stream;
}

void write_header(std::vector<u8>& out,
                  Game game,
                  const std::string& name,
                  u32 size,
                  u32 sample_rate) {
  size_t start = out.size();
  const bool big_endian_fields = game == Game::Jak1;
  auto field = [&](u32 v) { write_u32(out, big_endian_fields ? swap32(v) : v); };
  write_u32(out, big_endian_fields ? MAGIC_BIG_ENDIAN_FIELDS : MAGIC_LITTLE_ENDIAN_FIELDS);
  field(0x20);  // version
  field(0);
  field(size);
  field(sample_rate);
  field(0);
  field(0);
  field(0);
  // 16 byte name
  for (int i = 0; i < 16; i++) {
    out.push_back(i < (int)name.size() ? name[i] : 0);
  }
  ASSERT(out.size() - start == HEADER_SIZE);
}

size_t stereo_channel_chunk_count(size_t channel_bytes) {
  const size_t first_chunk_data = STEREO_CHUNK_SIZE - HEADER_SIZE;
  if (channel_bytes <= first_chunk_data) {
    return 1;
  }
  return 1 + (channel_bytes - first_chunk_data + STEREO_CHUNK_SIZE - 1) / STEREO_CHUNK_SIZE;
}

}  // namespace

std::optional<Game> game_from_name(const std::string& name) {
  if (name == "jak1") {
    return Game::Jak1;
  }
  if (name == "jak2") {
    return Game::Jak2;
  }
  if (name == "jak3" || name == "jakx") {
    return Game::Jak3;
  }
  return std::nullopt;
}

const char* game_name(Game game) {
  switch (game) {
    case Game::Jak1:
      return "jak1";
    case Game::Jak2:
      return "jak2";
    case Game::Jak3:
      return "jak3";
  }
  return "";
}

u32 page_size(Game game) {
  return game == Game::Jak3 ? 0x8000 : 2048;
}

u32 max_dir_entries(Game game) {
  switch (game) {
    case Game::Jak1:
      return 868;
    case Game::Jak2:
      return 2728;
    case Game::Jak3:
      return 4096;
  }
  return 0;
}

std::string trim_name(const std::string& name) {
  std::string result = name;
  while (!result.empty() && result.back() == ' ') {
    result.pop_back();
  }
  return result;
}

std::string pad_name(const std::string& name) {
  std::string result = name;
  result.resize(8, ' ');
  return result;
}

std::optional<std::string> check_name(Game game, const std::string& name) {
  if (name.empty() || name.size() > 8) {
    return fmt::format("'{}' must be between 1 and 8 characters long", name);
  }
  if (game == Game::Jak3) {
    for (char c : name) {
      if (!c || !strchr(JAK3_CHAR_MAP, c)) {
        return fmt::format(
            "'{}' contains a character that is not allowed in Jak 3 names, only "
            "upper case letters, digits, '-' and space are",
            name);
      }
    }
  }
  return std::nullopt;
}

Dir parse_dir(Game game, const std::vector<u8>& data) {
  Dir result;
  auto require = [&](size_t bytes) {
    if (bytes > data.size()) {
      throw std::runtime_error(
          fmt::format("VAGDIR is too small: needs {} bytes, has {}", bytes, data.size()));
    }
  };

  if (game == Game::Jak3) {
    require(16);
    if (read_u32(data.data()) != JAK3_DIR_MAGIC_0 ||
        read_u32(data.data() + 4) != JAK3_DIR_MAGIC_1) {
      throw std::runtime_error("VAGDIR doesn't have the expected Jak 3 header");
    }
    result.version = read_u32(data.data() + 8);
    u32 count = read_u32(data.data() + 12);
    require(16 + (size_t)count * 8);
    for (u32 i = 0; i < count; i++) {
      u64 v = read_u64(data.data() + 16 + i * 8);
      DirEntry e;
      e.name = unpack_name_jak3(v & ((1ull << 42) - 1));
      e.stereo = (v >> 42) & 1;
      e.international = (v >> 43) & 1;
      e.param = (v >> 44) & 0xf;
      e.page = v >> 48;
      result.entries.push_back(e);
    }
  } else {
    const size_t entry_size = game == Game::Jak1 ? 12 : 16;
    require(4);
    u32 count = read_u32(data.data());
    require(4 + (size_t)count * entry_size);
    for (u32 i = 0; i < count; i++) {
      const u8* p = data.data() + 4 + i * entry_size;
      DirEntry e;
      e.name = std::string((const char*)p, 8);
      e.page = read_u32(p + 8);
      if (game == Game::Jak2) {
        e.stereo = read_u32(p + 12) != 0;
      }
      result.entries.push_back(e);
    }
  }
  return result;
}

std::vector<u8> write_dir(Game game, const Dir& dir, size_t min_file_size) {
  std::vector<u8> out;
  if (game == Game::Jak3) {
    write_u32(out, JAK3_DIR_MAGIC_0);
    write_u32(out, JAK3_DIR_MAGIC_1);
    write_u32(out, dir.version);
    write_u32(out, dir.entries.size());
    for (auto& e : dir.entries) {
      ASSERT(e.page < 0x10000);
      u64 v = pack_name_jak3(e.name);
      v |= (u64)e.stereo << 42;
      v |= (u64)e.international << 43;
      v |= (u64)(e.param & 0xf) << 44;
      v |= (u64)e.page << 48;
      write_u64(out, v);
    }
  } else {
    write_u32(out, dir.entries.size());
    for (auto& e : dir.entries) {
      std::string padded = pad_name(e.name);
      out.insert(out.end(), padded.begin(), padded.end());
      write_u32(out, e.page);
      if (game == Game::Jak2) {
        write_u32(out, e.stereo ? 1 : 0);
      }
    }
  }
  if (out.size() < min_file_size) {
    out.resize(min_file_size, 0);
  }
  return out;
}

std::optional<Header> parse_header(const u8* data, size_t len) {
  if (len < HEADER_SIZE) {
    return std::nullopt;
  }
  u32 magic = read_u32(data);
  Header result;
  result.size = read_u32(data + 12);
  result.sample_rate = read_u32(data + 16);
  if (magic == MAGIC_BIG_ENDIAN_FIELDS) {
    result.size = swap32(result.size);
    result.sample_rate = swap32(result.sample_rate);
  } else if (magic != MAGIC_LITTLE_ENDIAN_FIELDS) {
    return std::nullopt;
  }
  return result;
}

size_t entry_length(const Header& header, bool stereo) {
  if (!stereo) {
    return HEADER_SIZE + (size_t)header.size;
  }
  size_t chunks = stereo_channel_chunk_count(header.size / 2);
  return chunks * 2 * STEREO_CHUNK_SIZE;
}

std::vector<u8> build_vag(Game game,
                          const std::string& name,
                          const std::vector<s16>& left,
                          const std::vector<s16>& right,
                          u32 sample_rate) {
  const bool stereo = !right.empty();
  const size_t length = std::max(left.size(), right.size());
  const std::string header_name = trim_name(name);

  auto left_stream = build_channel_stream(game, left, length);
  std::vector<u8> out;

  if (!stereo) {
    write_header(out, game, header_name, left_stream.size(), sample_rate);
    out.insert(out.end(), left_stream.begin(), left_stream.end());
    return out;
  }

  auto right_stream = build_channel_stream(game, right, length);
  ASSERT(left_stream.size() == right_stream.size());
  const size_t channel_bytes = left_stream.size();
  const size_t chunk_count = stereo_channel_chunk_count(channel_bytes);
  const u32 size_field = channel_bytes * 2;

  size_t pos = 0;
  for (size_t chunk = 0; chunk < chunk_count; chunk++) {
    for (const auto* stream : {&left_stream, &right_stream}) {
      size_t chunk_start = out.size();
      size_t capacity = STEREO_CHUNK_SIZE;
      if (chunk == 0) {
        write_header(out, game, header_name, size_field, sample_rate);
        capacity -= HEADER_SIZE;
      }
      size_t n = pos < channel_bytes ? std::min(capacity, channel_bytes - pos) : 0;
      if (n) {
        out.insert(out.end(), stream->begin() + pos, stream->begin() + pos + n);
      }
      out.resize(chunk_start + STEREO_CHUNK_SIZE, 0);
    }
    pos += chunk == 0 ? STEREO_CHUNK_SIZE - HEADER_SIZE : STEREO_CHUNK_SIZE;
  }
  return out;
}

std::optional<DecodedVag> decode_vag(const u8* data, size_t len, bool stereo) {
  auto header = parse_header(data, len);
  if (!header) {
    return std::nullopt;
  }
  size_t expected = entry_length(*header, stereo);
  if (expected > len) {
    return std::nullopt;
  }

  BinaryReader reader(std::span<const u8>(data, expected));
  auto [left, right] = decode_adpcm(reader, stereo, 0);

  // both the stereo padding and the last incomplete chunk decode to silence, remove them.
  const size_t channel_bytes = stereo ? header->size / 2 : header->size;
  const size_t samples = channel_bytes / 16 * 28;
  if (left.size() > samples) {
    left.resize(samples);
  }
  if (right.size() > samples) {
    right.resize(samples);
  }

  DecodedVag result;
  result.left = std::move(left);
  result.right = std::move(right);
  result.sample_rate = header->sample_rate;
  return result;
}

}  // namespace vag
