#pragma once

#include <vector>

#include "common/common_types.h"

class TextureConverter {
 public:
  TextureConverter();
  void upload(const u8* data, u32 dest, u32 size_vram_words);
  void upload_width(const u8* data, u32 dest, u32 width, u32 height);
  void download_rgba8888(u8* result,
                         u32 vram_addr,
                         u32 goal_tex_width,
                         u32 w,
                         u32 h,
                         u32 psm,
                         u32 clut_psm,
                         u32 clut_vram_addr,
                         u32 expected_size_bytes);

 private:
  std::vector<u8> m_vram;
};
