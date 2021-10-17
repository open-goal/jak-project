#pragma once

#include <array>
#include <memory>
#include <string>
#include "common/common_types.h"
#include "game/graphics/texture/TextureConverter.h"
#include "common/util/Serializer.h"

struct TextureRecord {
  std::string page_name;
  std::string name;
  u8 mip_level;
  u8 psm = -1;
  u8 cpsm = -1;
  u16 w, h;
  u8 data_segment;
  bool on_gpu = false;
  bool do_gc = true;  // if set, will be unloaded from GPU when another is upload on top
  bool only_on_gpu = false;
  std::vector<u8> data;
  u64 gpu_texture = 0;
  u32 dest = -1;

  u8 min_a_zero, max_a_zero, min_a_nonzero, max_a_nonzero;

  void unload_from_gpu();

  void serialize(Serializer& ser);
};

struct TextureData {
  std::shared_ptr<TextureRecord> normal_texture;
  std::shared_ptr<TextureRecord> mt4hh_texture;

  void serialize(Serializer& ser);
};

struct GoalTexture {
  s16 w;
  s16 h;
  u8 num_mips;
  u8 tex1_control;
  u8 psm;
  u8 mip_shift;
  u16 clutpsm;
  u16 dest[7];
  u16 clut_dest;
  u8 width[7];
  u32 name_ptr;
  u32 size;
  float uv_dist;
  u32 masks[3];

  s32 segment_of_mip(s32 mip) const {
    if (2 >= num_mips) {
      return num_mips - mip - 1;
    } else {
      return std::max(0, 2 - mip);
    }
  }
};

static_assert(sizeof(GoalTexture) == 60, "GoalTexture size");
static_assert(offsetof(GoalTexture, clutpsm) == 8);
static_assert(offsetof(GoalTexture, clut_dest) == 24);

struct GoalTexturePage {
  struct Seg {
    u32 block_data_ptr;
    u32 size;
    u32 dest;
  };
  u32 file_info_ptr;
  u32 name_ptr;
  u32 id;
  s32 length;  // texture count
  u32 mip0_size;
  u32 size;
  Seg segment[3];
  u32 pad[16];
  // start of array.

  std::string print() const;

  bool try_copy_texture_description(GoalTexture* dest,
                                    int idx,
                                    const u8* memory_base,
                                    const u8* tpage,
                                    u32 s7_ptr) {
    u32 ptr;
    memcpy(&ptr, tpage + sizeof(GoalTexturePage) + 4 * idx, 4);
    if (ptr == s7_ptr) {
      return false;
    }
    memcpy(dest, memory_base + ptr, sizeof(GoalTexture));
    return true;
  }
};

class TexturePool {
 public:
  void handle_upload_now(const u8* tpage, int mode, const u8* memory_base, u32 s7_ptr);

  std::vector<std::shared_ptr<TextureRecord>> convert_textures(const u8* tpage,
                                                               int mode,
                                                               const u8* memory_base,
                                                               u32 s7_ptr);

  void set_texture(u32 location, std::shared_ptr<TextureRecord> record);
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

  TextureRecord* get_random_texture();

  void upload_to_gpu(TextureRecord* rec);

  void relocate(u32 destination, u32 source, u32 format);

  void remove_garbage_textures();
  void discard(std::shared_ptr<TextureRecord> tex);

  void serialize(Serializer& ser);

 private:
  void unload_all_textures();
  void draw_debug_for_tex(const std::string& name, TextureRecord& tex);
  TextureConverter m_tex_converter;

  // uses tex.dest[mip] indexing. (bytes / 256). Currently only sets the base of a texture.
  std::array<TextureData, 1024 * 1024 * 4 / 256> m_textures;

  // textures that the game overwrote, but may be still allocated on the GPU.
  // TODO: free these periodically.
  std::vector<std::shared_ptr<TextureRecord>> m_garbage_textures;

  char m_regex_input[256] = "";

  int m_most_recent_gc_count = 0;
  int m_most_recent_gc_count_gpu = 0;
};
