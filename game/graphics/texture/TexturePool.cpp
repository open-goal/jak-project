#include "TexturePool.h"

#include "third-party/fmt/core.h"
#include "common/util/assert.h"
#include "common/util/FileUtil.h"

////////////////////////////////
// Extraction of textures
//   Note: this is intended to be temporary until we have a better system.
//    this simply converts the PS2 format textures loaded by the game, then puts them into the PC
//    port texture pool.

const char empty_string[] = "";
const char* goal_string(u32 ptr, const u8* memory_base) {
  if (ptr == 0) {
    return empty_string;
  }
  return (const char*)(memory_base + ptr + 4);
}

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

  std::string print() const {
    return fmt::format("Tpage id {} textures {} seg0 {} {} seg1 {} {} seg2 {} {}\n", id, length,
                       segment[0].size, segment[0].dest, segment[1].size, segment[1].dest,
                       segment[2].size, segment[2].dest);
  }

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


/*!
 * Handle a GOAL texture-page object being uploaded to VRAM.
 * The strategy:
 * - upload the texture-age to a fake 4MB VRAM, like the GOAL code would have done.
 * - "download" each texture in a reasonable format for the PC Port (currently RGBA8888)
 * - add this to the PC pool.
 *
 * The textures are scrambled around in a confusing way.
 */
void TexturePool::handle_upload_now(const u8* tpage, int mode, const u8* memory_base, u32 s7_ptr) {
  fmt::print("[TexutrePool C++] Got upload now! {} {}\n", (const void*)tpage, mode);

  // extract the texture-page object. This is just a description of the page data.
  GoalTexturePage texture_page;
  memcpy(&texture_page, tpage, sizeof(GoalTexturePage));

  std::vector<u8> output_buffer;
  output_buffer.resize(1024 * 1024);

  u32 sizes[3] = {texture_page.segment[0].size, texture_page.segment[1].size,
                  texture_page.segment[2].size};
  if (mode == -1) {
    // I don't really understand what's going on here with the size.
    // the sizes given aren't the actual sizes in memory, so if you just use that, you get the
    // wrong answer. I solved this in the decompiler by using
    u32 size = ((sizes[0] + sizes[1] + sizes[2] + 255) / 256) * 256;
    m_tex_converter.upload(memory_base + texture_page.segment[0].block_data_ptr,
                           texture_page.segment[0].dest, size);
  } else {
    assert(false);
  }


  // loop over all texture in the tpage and download them.
  for (int tex_idx = 0; tex_idx < texture_page.length; tex_idx++) {
    GoalTexture tex;
    if (texture_page.try_copy_texture_description(&tex, tex_idx, memory_base, tpage, s7_ptr)) {

      // each texture may have multiple mip levels.
      for (int mip_idx = 0; mip_idx < tex.num_mips; mip_idx++) {
        s32 segment = tex.segment_of_mip(mip_idx);

        u32 ww = tex.w >> mip_idx;
        u32 hh = tex.h >> mip_idx;
        m_tex_converter.download_rgba8888(output_buffer.data(), tex.dest[mip_idx],
                                          tex.width[mip_idx], ww, hh, tex.psm, tex.clutpsm,
                                          tex.clut_dest);

        // Debug output.
        const char* tpage_name = goal_string(texture_page.name_ptr, memory_base);
        const char* tex_name = goal_string(tex.name_ptr, memory_base);
        file_util::create_dir_if_needed(
            file_util::get_file_path({"debug_out", "textures", tpage_name}));
        file_util::write_rgba_png(
            fmt::format(
                file_util::get_file_path({"debug_out", "textures", tpage_name, "{}-{}-{}.png"}),
                tex_idx, tex_name, mip_idx),
            output_buffer.data(), ww, hh);
      }

      // TODO the clut
    } else {
      fmt::print("[{}] #f ------------\n", tex_idx);
    }
  }
}