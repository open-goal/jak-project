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
  u64 gpu_texture = 0;
  bool on_gpu = false;
};

struct TextureData {
  std::unique_ptr<TextureRecord> normal_texture;
  std::unique_ptr<TextureRecord> mt4hh_texture;
};

class TexturePool {
 public:
  void handle_upload_now(const u8* tpage, int mode, const u8* memory_base, u32 s7_ptr);
  void set_texture(u32 location, std::unique_ptr<TextureRecord>&& record);
  void set_mt4hh_texture(u32 location, std::unique_ptr<TextureRecord>&& record);
  void draw_debug_window();
  TextureRecord* lookup(u32 location) {
    if (m_textures.at(location).normal_texture) {
      return m_textures[location].normal_texture.get();
    } else {
      return nullptr;
    }
  }

  TextureRecord* lookup_mt4hh(u32 location) {
    if (m_textures.at(location).mt4hh_texture) {
      return m_textures[location].mt4hh_texture.get();
    } else {
      return nullptr;
    }
  }

  void upload_to_gpu(TextureRecord* rec);

  void relocate(u32 destination, u32 source, u32 format);

 private:
  void draw_debug_for_tex(const std::string& name, TextureRecord& tex);
  TextureConverter m_tex_converter;

  // uses tex.dest[mip] indexing. (bytes / 256). Currently only sets the base of a texture.
  std::array<TextureData, 1024 * 1024 * 4 / 256> m_textures;

  // textures that the game overwrote, but may be still allocated on the GPU.
  // TODO: free these periodically.
  std::vector<std::unique_ptr<TextureRecord>> m_garbage_textures;
};
