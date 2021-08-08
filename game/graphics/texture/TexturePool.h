#pragma once

#include "common/common_types.h"
#include "game/graphics/texture/TextureConverter.h"

class TexturePool {
 public:
  void handle_upload_now(const u8* tpage, int mode, const u8* memory_base, u32 s7_ptr);

 private:
  TextureConverter m_tex_converter;
};
