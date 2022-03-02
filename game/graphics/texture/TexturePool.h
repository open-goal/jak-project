#pragma once

#include <array>
#include <memory>
#include <string>
#include <mutex>
#include <optional>
#include <unordered_map>
#include "common/common_types.h"
#include "game/graphics/texture/TextureConverter.h"
#include "common/util/Serializer.h"

constexpr bool EXTRA_TEX_DEBUG = false;

struct GpuTexture {
  std::string page_name;
  std::string name;
  struct Tex {
    u64 gl = -1;
    const u8* data = nullptr;
  };

  const u8* get_data_ptr() const {
    if (is_placeholder) {
      return nullptr;
    } else {
      return gpu_textures.at(0).data;
    }
  }
  std::vector<Tex> gpu_textures;
  std::vector<u32> slots;
  std::vector<u32> mt4hh_slots;
  u32 combo_id = -1;
  u16 w, h;
  bool is_placeholder = false;
  bool is_common = false;
  u32 data_size() const { return 4 * w * h; }

  void remove_slot(u32 slot);
  void add_slot(u32 slot);
};

struct TextureInput {
  std::string page_name;
  std::string name;
  u64 gpu_texture = -1;
  bool common = false;
  u32 combo_id = -1;
  const u8* src_data;
  u16 w, h;
};

struct TextureReference {
  u64 gpu_texture = -1;
  GpuTexture* source = nullptr;  // todo, do we actually need this link?
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
  TexturePool();
  void handle_upload_now(const u8* tpage, int mode, const u8* memory_base, u32 s7_ptr);

  GpuTexture* give_texture(const TextureInput& in);
  GpuTexture* give_texture_and_load_to_vram(const TextureInput& in, u32 vram_slot);
  void unload_texture(const std::string& name, u64 id);
  std::optional<u64> lookup(u32 location) {
    auto& t = m_textures[location];
    if (t.source) {
      // TODO remove this before merging.
      if constexpr (EXTRA_TEX_DEBUG) {
        if (t.source->is_placeholder) {
          ASSERT(t.gpu_texture == m_placeholder_texture_id);
        } else {
          bool fnd = false;
          for (auto& tt : t.source->gpu_textures) {
            if (tt.gl == t.gpu_texture) {
              fnd = true;
              break;
            }
          }
          ASSERT(fnd);
        }
      }

      return t.gpu_texture;
    } else {
      return {};
    }
  }

  GpuTexture* lookup_gpu_texture(u32 location) { return m_textures[location].source; }
  std::optional<u64> lookup_mt4hh(u32 location);

  u64 get_placeholder_texture() { return m_placeholder_texture_id; }
  void draw_debug_window();
  void relocate(u32 destination, u32 source, u32 format);
  void draw_debug_for_tex(const std::string& name, GpuTexture* tex, u32 slot);
  const std::array<TextureReference, 1024 * 1024 * 4 / 256> all_textures() const {
    return m_textures;
  }
  void move_existing_to_vram(GpuTexture* tex, u32 slot_addr);

  std::mutex& mutex() { return m_mutex; }

 private:
  void refresh_links(GpuTexture& texture);
  GpuTexture* get_gpu_texture_for_slot(const std::string& name, u32 slot);

  char m_regex_input[256] = "";
  std::array<TextureReference, 1024 * 1024 * 4 / 256> m_textures;
  struct Mt4hhTexture {
    TextureReference ref;
    u32 slot;
  };
  std::vector<Mt4hhTexture> m_mt4hh_textures;

  std::vector<u32> m_placeholder_data;
  u64 m_placeholder_texture_id = 0;

  std::unordered_map<std::string, GpuTexture> m_loaded_textures;

  std::mutex m_mutex;
};