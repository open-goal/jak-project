#include <regex>

#include "TexturePool.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "common/util/assert.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/log/log.h"
#include "game/graphics/pipelines/opengl.h"

////////////////////////////////
// Extraction of textures
//   Note: this is intended to be temporary until we have a better system.
//    this simply converts the PS2 format textures loaded by the game, then puts them into the PC
//    port texture pool.

// constexpr bool dump_textures_to_file = false;

namespace {
const char empty_string[] = "";
const char* goal_string(u32 ptr, const u8* memory_base) {
  if (ptr == 0) {
    return empty_string;
  }
  return (const char*)(memory_base + ptr + 4);
}

}  // namespace

std::string GoalTexturePage::print() const {
  return fmt::format("Tpage id {} textures {} seg0 {} {} seg1 {} {} seg2 {} {}\n", id, length,
                     segment[0].size, segment[0].dest, segment[1].size, segment[1].dest,
                     segment[2].size, segment[2].dest);
}

std::vector<std::shared_ptr<TextureRecord>> TexturePool::convert_textures(const u8* tpage,
                                                                          int mode,
                                                                          const u8* memory_base,
                                                                          u32 s7_ptr) {
  Timer timer;
  std::vector<std::shared_ptr<TextureRecord>> result;

  bool dump_textures_to_file = false;
  // extract the texture-page object. This is just a description of the page data.
  GoalTexturePage texture_page;
  memcpy(&texture_page, tpage, sizeof(GoalTexturePage));

  bool has_segment[3] = {true, true, true};

  u32 sizes[3] = {texture_page.segment[0].size, texture_page.segment[1].size,
                  texture_page.segment[2].size};
  if (mode == -1) {
    // I don't really understand what's going on here with the size.
    // the sizes given aren't the actual sizes in memory, so if you just use that, you get the
    // wrong answer. I solved this in the decompiler by using the size of the actual data, but we
    // don't really have that here.
    u32 size = ((sizes[0] + sizes[1] + sizes[2] + 255) / 256) * 256;

    m_tex_converter.upload(memory_base + texture_page.segment[0].block_data_ptr,
                           texture_page.segment[0].dest, size);

  } else if (mode == 2) {
    //    dump_textures_to_file = true;
    has_segment[0] = false;
    has_segment[1] = false;
    u32 size = ((sizes[2] + 255) / 256) * 256;

    // dest is in 4-byte vram words
    m_tex_converter.upload(memory_base + texture_page.segment[2].block_data_ptr,
                           texture_page.segment[2].dest, size);
  } else if (mode == -2) {
    has_segment[2] = false;
    // I don't really understand what's going on here with the size.
    // the selector texture the hud page will be missing the clut unless I make this bigger.
    u32 size = ((sizes[0] + sizes[1] + 2047) / 256) * 256;
    m_tex_converter.upload(memory_base + texture_page.segment[0].block_data_ptr,
                           texture_page.segment[0].dest, size);
  } else {
    // no reason to skip this, other than
    lg::error("TexturePool skipping upload now with mode {}.", mode);
    return {};
  }

  // loop over all texture in the tpage and download them.
  for (int tex_idx = 0; tex_idx < texture_page.length; tex_idx++) {
    GoalTexture tex;
    if (texture_page.try_copy_texture_description(&tex, tex_idx, memory_base, tpage, s7_ptr)) {
      // each texture may have multiple mip levels.
      for (int mip_idx = 0; mip_idx < tex.num_mips; mip_idx++) {
        if (has_segment[tex.segment_of_mip(mip_idx)]) {
          u32 ww = tex.w >> mip_idx;
          u32 hh = tex.h >> mip_idx;
          u32 size_bytes = ww * hh * 4;

          auto texture_record = std::make_shared<TextureRecord>();
          texture_record->page_name = goal_string(texture_page.name_ptr, memory_base);
          texture_record->name = goal_string(tex.name_ptr, memory_base);
          texture_record->mip_level = mip_idx;
          texture_record->w = ww;
          texture_record->h = hh;
          texture_record->data_segment = tex.segment_of_mip(mip_idx);
          texture_record->data.resize(size_bytes);
          texture_record->psm = tex.psm;
          texture_record->dest = tex.dest[mip_idx];

          m_tex_converter.download_rgba8888(texture_record->data.data(), tex.dest[mip_idx],
                                            tex.width[mip_idx], ww, hh, tex.psm, tex.clutpsm,
                                            tex.clut_dest, size_bytes);
          if (texture_record->name == "selector" || texture_record->name == "next") {
            fmt::print("{}: {} {} {} {}\n", texture_record->name, tex.psm, tex.clutpsm,
                       tex.clut_dest * 256 / 4,
                       texture_page.segment[0].dest + ((sizes[0] + sizes[1] + 255) / 256) * 256);
          }

          // Debug output.
          if (dump_textures_to_file) {
            const char* tpage_name = goal_string(texture_page.name_ptr, memory_base);
            const char* tex_name = goal_string(tex.name_ptr, memory_base);
            file_util::create_dir_if_needed(
                file_util::get_file_path({"debug_out", "textures", tpage_name}));
            file_util::write_rgba_png(
                fmt::format(
                    file_util::get_file_path({"debug_out", "textures", tpage_name, "{}-{}-{}.png"}),
                    tex_idx, tex_name, mip_idx),
                texture_record->data.data(), ww, hh);
          }
          result.push_back(std::move(texture_record));
        }
      }
    } else {
      // texture was #f, skip it.
    }
  }

  fmt::print("upload now took {:.2f} ms\n", timer.getMs());
  return result;
}

/*!
 * Handle a GOAL texture-page object being uploaded to VRAM.
 * The strategy:
 * - upload the texture-age to a fake 4MB VRAM, like the GOAL code would have done.
 * - "download" each texture in a reasonable format for the PC Port (currently RGBA8888)
 * - add this to the PC pool.
 *
 * The textures are scrambled around in a confusing way.
 *
 * NOTE: the actual conversion is currently done here, but this might be too slow.
 * We could store textures in the right format to begin with, or spread the conversion out over
 * multiple frames.
 */
void TexturePool::handle_upload_now(const u8* tpage, int mode, const u8* memory_base, u32 s7_ptr) {
  auto textures = convert_textures(tpage, mode, memory_base, s7_ptr);
  for (auto& tex : textures) {
    set_texture(tex->dest, tex);
  }
}

/*!
 * Store a texture in the pool. Location is specified like TBP.
 */
void TexturePool::set_texture(u32 location, std::shared_ptr<TextureRecord> record) {
  if (record->psm == 44) {
    if (m_textures.at(location).mt4hh_texture) {
      if (record->do_gc && m_textures.at(location).mt4hh_texture != record) {
        m_garbage_textures.push_back(std::move(m_textures[location].mt4hh_texture));
      }
    }
    m_textures[location].mt4hh_texture = std::move(record);
  } else {
    if (m_textures.at(location).normal_texture) {
      if (record->do_gc && m_textures.at(location).normal_texture != record) {
        m_garbage_textures.push_back(std::move(m_textures[location].normal_texture));
      }
    }
    m_textures[location].normal_texture = std::move(record);
  }
}

/*!
 * Move a texture.
 */
void TexturePool::relocate(u32 destination, u32 source, u32 format) {
  auto& src = m_textures.at(source).normal_texture;
  assert(src);
  if (format == 44) {
    m_textures.at(destination).mt4hh_texture = std::move(src);
  } else {
    m_textures.at(destination).normal_texture = std::move(src);
  }
}

void TexturePool::draw_debug_window() {
  int id = 0;
  int total_vram_bytes = 0;
  int total_textures = 0;
  int total_displayed_textures = 0;
  int total_uploaded_textures = 0;
  ImGui::Text("GC %d on GPU %d", m_most_recent_gc_count, m_most_recent_gc_count_gpu);
  ImGui::InputText("texture search", m_regex_input, sizeof(m_regex_input));
  std::regex regex(m_regex_input[0] ? m_regex_input : ".*");

  for (auto& record : m_textures) {
    if (record.normal_texture) {
      total_textures++;
      auto& tex = *record.normal_texture;
      if (std::regex_search(tex.name, regex)) {
        ImGui::PushID(id++);
        draw_debug_for_tex(tex.name, tex);
        ImGui::PopID();
        total_displayed_textures++;
      }

      if (tex.on_gpu) {
        total_vram_bytes += tex.w * tex.h * 4;  // todo, if we support other formats
        total_uploaded_textures++;
      }
    }

    if (record.mt4hh_texture) {
      total_textures++;
      auto& tex = *record.mt4hh_texture;
      if (std::regex_search(tex.name, regex)) {
        ImGui::PushID(id++);
        draw_debug_for_tex(tex.name, tex);
        ImGui::PopID();
        total_displayed_textures++;
      }

      if (tex.on_gpu) {
        total_vram_bytes += tex.w * tex.h * 4;  // todo, if we support other formats
        total_uploaded_textures++;
      }
    }
  }
  ImGui::Text("Total Textures: %d Uploaded: %d Shown: %d VRAM: %.3f MB", total_textures,
              total_uploaded_textures, total_displayed_textures,
              (float)total_vram_bytes / (1024 * 1024));
}

void TexturePool::draw_debug_for_tex(const std::string& name, TextureRecord& tex) {
  if (ImGui::TreeNode(name.c_str())) {
    ImGui::Text("Page: %s Size: %d x %d mip %d On GPU? %d psm %d", tex.page_name.c_str(), tex.w,
                tex.h, tex.mip_level, tex.on_gpu, tex.psm);
    if (tex.on_gpu) {
      ImGui::Image((void*)tex.gpu_texture, ImVec2(tex.w, tex.h));
    } else {
      if (ImGui::Button("Upload to GPU!")) {
        upload_to_gpu(&tex);
      }
    }
    ImGui::TreePop();
    ImGui::Separator();
  }
}

void TexturePool::upload_to_gpu(TextureRecord* tex) {
  assert(!tex->on_gpu);
  GLuint tex_id;
  glGenTextures(1, &tex_id);
  tex->gpu_texture = tex_id;
  glBindTexture(GL_TEXTURE_2D, tex_id);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex->w, tex->h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               tex->data.data());
  glBindTexture(GL_TEXTURE_2D, 0);

  // we have to set these, imgui won't do it automatically
  glActiveTexture(GL_TEXTURE0);
  glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  tex->on_gpu = true;
}

void TexturePool::remove_garbage_textures() {
  m_most_recent_gc_count = m_garbage_textures.size();
  m_most_recent_gc_count_gpu = 0;

  for (auto& t : m_garbage_textures) {
    if (t->on_gpu) {
      m_most_recent_gc_count_gpu++;
      GLuint tex_id = t->gpu_texture;
      glBindTexture(GL_TEXTURE_2D, tex_id);
      glDeleteTextures(1, &tex_id);
    }
  }
  m_garbage_textures.clear();
}

void TexturePool::discard(std::shared_ptr<TextureRecord> tex) {
  assert(!tex->do_gc);
  m_garbage_textures.push_back(tex);
}