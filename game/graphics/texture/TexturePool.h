#pragma once

#include <array>
#include <memory>
#include <string>
#include "common/common_types.h"
#include "game/graphics/texture/TextureConverter.h"

struct TextureRecord {
  std::string page_name;
  std::string name;
  u8 mip_level;
  u16 w, h;
  std::vector<u8> data;
  u8 data_segment;
};

class TexturePool {
 public:
  void handle_upload_now(const u8* tpage, int mode, const u8* memory_base, u32 s7_ptr);

 private:
  TextureConverter m_tex_converter;

  // uses tex.dest[mip] indexing. (bytes / 256). Currently only sets the base of a texture.
  std::array<std::unique_ptr<TextureRecord>, 1024 * 1024 * 4 / 256> m_textures;
};
