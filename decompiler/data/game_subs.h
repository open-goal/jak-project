#pragma once

#include <array>
#include <string>
#include <unordered_map>
#include <vector>

#include "common/util/FileUtil.h"
#include "common/util/FontUtils.h"

namespace decompiler {
struct ObjectFileData;

struct SpoolSubtitleMessage {
  enum class Kind { NIL = 0, STRING, IMAGE } kind;
  std::string text;

  s16 w, h;
  std::array<u32, 16> palette;
  std::vector<u8> data;
};

struct SpoolSubtitleRange {
  float start_frame;
  float end_frame;
  SpoolSubtitleMessage message[8];

  bool operator==(const SpoolSubtitleRange& other) const {
    if (start_frame != other.start_frame || end_frame != other.end_frame)
      return false;
    for (int i = 0; i < 8; ++i) {
      if (message[i].kind != other.message[i].kind)
        return false;
      if (message[i].kind == SpoolSubtitleMessage::Kind::IMAGE) {
        if (message[i].data != other.message[i].data ||
            message[i].palette != other.message[i].palette || message[i].w != other.message[i].w ||
            message[i].h != other.message[i].h)
          return false;
      } else {
        if (message[i].text != other.message[i].text)
          return false;
      }
    }
    return true;
  }
  bool operator!=(const SpoolSubtitleRange& other) const { return !(*this == other); }
};

std::vector<SpoolSubtitleRange> process_spool_subtitles(ObjectFileData& data,
                                                        GameTextVersion version);
std::string write_spool_subtitles(
    GameTextVersion version,
    const fs::path& image_out,
    const std::unordered_map<std::string, std::vector<SpoolSubtitleRange>>& data);
}  // namespace decompiler
