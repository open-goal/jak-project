#pragma once

/*!
 * @file vag.h
 * Reading and writing the streamed audio containers used by Jak 1, 2, 3 and X:
 *  - VAGDIR.AYB: the directory, maps an 8 character name to a location in the VAGWAD files.
 *  - VAGWAD.xxx: one file per language, containing the streams ("VAG files") back to back.
 *  - a VAG file: a 48 byte header followed by PS-ADPCM blocks. Stereo files store both channels in
 *    alternating 8 KB chunks, each channel starts with its own header.
 */

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "common/common_types.h"

namespace vag {

// Jak X uses the same formats as Jak 3.
enum class Game { Jak1, Jak2, Jak3 };

std::optional<Game> game_from_name(const std::string& name);
const char* game_name(Game game);

constexpr u32 HEADER_SIZE = 48;
constexpr u32 STEREO_CHUNK_SIZE = 0x2000;

/*!
 * Size of the unit that the directory uses for offsets into a WAD.
 */
u32 page_size(Game game);

/*!
 * The most entries that the game's in-memory copy of the directory can hold.
 */
u32 max_dir_entries(Game game);

struct DirEntry {
  std::string name;  // 8 characters, padded with spaces
  bool stereo = false;
  bool international = false;  // Jak 3: in the INT wad instead of the language ones
  u32 param = 0;               // Jak 3: unknown 4-bit field, preserved as-is
  u32 page = 0;                // offset in the WAD, in units of page_size()
};

struct Dir {
  std::vector<DirEntry> entries;
  u32 version = 0;  // Jak 3 only
};

std::string trim_name(const std::string& name);
std::string pad_name(const std::string& name);

/*!
 * Returns an error message if the name can't be stored in a directory for this game.
 */
std::optional<std::string> check_name(Game game, const std::string& name);

Dir parse_dir(Game game, const std::vector<u8>& data);

/*!
 * The file is padded with zeros to at least min_file_size, the original files are padded like this.
 */
std::vector<u8> write_dir(Game game, const Dir& dir, size_t min_file_size);

struct Header {
  u32 sample_rate = 0;
  // number of bytes after the header (times two for stereo, see entry_length)
  u32 size = 0;
};

/*!
 * Parse the header at the start of a VAG file. Handles both byte orders (VAGp / pGAV).
 */
std::optional<Header> parse_header(const u8* data, size_t len);

/*!
 * The number of bytes of a VAG file, including all padding.
 */
size_t entry_length(const Header& header, bool stereo);

/*!
 * Create a VAG file from samples. If right is empty, the result is mono.
 * If the right channel is not empty, it is padded with silence if it is shorter than the left.
 */
std::vector<u8> build_vag(Game game,
                          const std::string& name,
                          const std::vector<s16>& left,
                          const std::vector<s16>& right,
                          u32 sample_rate);

struct DecodedVag {
  std::vector<s16> left;
  std::vector<s16> right;
  u32 sample_rate = 0;
};

/*!
 * Decode a complete VAG file (as extracted by the unpacker).
 */
std::optional<DecodedVag> decode_vag(const u8* data, size_t len, bool stereo);

}  // namespace vag
