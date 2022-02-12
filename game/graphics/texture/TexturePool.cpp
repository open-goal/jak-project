#include <regex>

#include "TexturePool.h"

#include "third-party/fmt/core.h"
#include "third-party/imgui/imgui.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/log/log.h"
#include "game/graphics/pipelines/opengl.h"
#include "common/util/Assert.h"

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

void TextureRecord::serialize(Serializer& ser) {
  if (only_on_gpu) {
    ASSERT(on_gpu);
    if (ser.is_saving()) {
      // we should download the texture and save it.
      data.resize(w * h * 4);
      glBindTexture(GL_TEXTURE_2D, gpu_texture);
      glGetTexImage(GL_TEXTURE_2D, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV, data.data());
    }
  }

  ser.from_str(&page_name);
  ser.from_str(&name);
  ser.from_ptr(&mip_level);
  ser.from_ptr(&psm);
  ser.from_ptr(&cpsm);
  ser.from_ptr(&w);
  ser.from_ptr(&h);
  ser.from_ptr(&data_segment);
  ser.from_ptr(&on_gpu);
  ser.from_ptr(&do_gc);
  ser.from_ptr(&only_on_gpu);
  ser.from_ptr(&gpu_texture);
  ser.from_ptr(&dest);
  ser.from_pod_vector(&data);

  if (ser.is_loading()) {
    gpu_texture = -1;
  }
}

void TextureData::serialize(Serializer& ser) {
  if (ser.is_saving()) {
    if (normal_texture) {
      ser.save<u8>(1);  // has it.
      normal_texture->serialize(ser);
    } else {
      ser.save<u8>(0);
    }

    if (mt4hh_texture) {
      ser.save<u8>(1);  // has it.
      mt4hh_texture->serialize(ser);

    } else {
      ser.save<u8>(0);
    }
  } else {
    u8 has_normal = ser.load<u8>();
    if (has_normal) {
      normal_texture = std::make_shared<TextureRecord>();
      normal_texture->serialize(ser);
      normal_texture->on_gpu = false;
      normal_texture->do_gc = true;
    } else {
      normal_texture.reset();
    }

    u8 has_mt4 = ser.load<u8>();
    if (has_mt4) {
      mt4hh_texture = std::make_shared<TextureRecord>();
      mt4hh_texture->serialize(ser);
      mt4hh_texture->on_gpu = false;
      mt4hh_texture->do_gc = true;
    } else {
      mt4hh_texture.reset();
    }
  }
}

void TexturePool::serialize(Serializer& ser) {
  m_tex_converter.serialize(ser);

  if (ser.is_loading()) {
    remove_garbage_textures();
    unload_all_textures();
  }
  for (auto& tex : m_textures) {
    tex.serialize(ser);
  }
}

void TexturePool::unload_all_textures() {
  for (auto& tex : m_textures) {
    if (tex.normal_texture && tex.normal_texture->on_gpu) {
      tex.normal_texture->unload_from_gpu();
    }

    if (tex.mt4hh_texture && tex.mt4hh_texture->on_gpu) {
      tex.mt4hh_texture->unload_from_gpu();
    }
  }
}

void TextureRecord::unload_from_gpu() {
  ASSERT(on_gpu);
  GLuint tex_id = gpu_texture;
  glBindTexture(GL_TEXTURE_2D, tex_id);
  glDeleteTextures(1, &tex_id);
  on_gpu = false;
  gpu_texture = -1;
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
    u32 size = ((sizes[0] + sizes[1] + sizes[2] + 2047) / 256) * 256;

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
  } else if (mode == 0) {
    has_segment[1] = false;
    has_segment[2] = false;
    u32 size = ((sizes[0] + 255) / 256) * 256;

    // dest is in 4-byte vram words
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
          texture_record->cpsm = tex.clutpsm;
          texture_record->dest = tex.dest[mip_idx];

          m_tex_converter.download_rgba8888(texture_record->data.data(), tex.dest[mip_idx],
                                            tex.width[mip_idx], ww, hh, tex.psm, tex.clutpsm,
                                            tex.clut_dest, size_bytes);

          u8 max_a_zero = 0;
          u8 min_a_zero = 255;
          u8 max_a_nonzero = 0;
          u8 min_a_nonzero = 255;
          for (u32 i = 0; i < ww * hh; i++) {
            u8 r = texture_record->data[i * 4 + 0];
            u8 g = texture_record->data[i * 4 + 1];
            u8 b = texture_record->data[i * 4 + 2];
            u8 a = texture_record->data[i * 4 + 3];
            if (r || g || b) {
              max_a_nonzero = std::max(max_a_nonzero, a);
              min_a_nonzero = std::min(min_a_nonzero, a);
            } else {
              max_a_zero = std::max(max_a_zero, a);
              min_a_zero = std::min(min_a_zero, a);
            }
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
                texture_record->data.data(), ww, hh, false);
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
        // fmt::print("replace add to garbage list {}\n", m_garbage_textures.back()->name);
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
  ASSERT(src);
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
  if (tex.on_gpu) {
    ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(0.3, 0.8, 0.3, 1.0));
  }
  if (ImGui::TreeNode(name.c_str())) {
    ImGui::Text("P: %s sz: %d x %d mip %d GPU? %d psm %d cpsm %d dest %d", tex.page_name.c_str(),
                tex.w, tex.h, tex.mip_level, tex.on_gpu, tex.psm, tex.cpsm, tex.dest);
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
  if (tex.on_gpu) {
    ImGui::PopStyleColor();
  }
}

void TexturePool::upload_to_gpu(TextureRecord* tex) {
  ASSERT(!tex->on_gpu);
  GLuint tex_id;
  glGenTextures(1, &tex_id);
  tex->gpu_texture = tex_id;
  GLint old_tex;
  glGetIntegerv(GL_ACTIVE_TEXTURE, &old_tex);
  glActiveTexture(GL_TEXTURE0);

  glBindTexture(GL_TEXTURE_2D, tex_id);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex->w, tex->h, 0, GL_RGBA, GL_UNSIGNED_INT_8_8_8_8_REV,
               tex->data.data());
  glBindTexture(GL_TEXTURE_2D, 0);

  // we have to set these, imgui won't do it automatically

  glBindTexture(GL_TEXTURE_2D, tex->gpu_texture);
  glGenerateMipmap(GL_TEXTURE_2D);

  float aniso = 0.0f;
  glGetFloatv(GL_MAX_TEXTURE_MAX_ANISOTROPY, &aniso);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY, aniso);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

  glActiveTexture(old_tex);
  tex->on_gpu = true;
}

void TexturePool::remove_garbage_textures() {
  m_most_recent_gc_count = m_garbage_textures.size();
  m_most_recent_gc_count_gpu = 0;

  for (auto& t : m_garbage_textures) {
    if (t->on_gpu) {
      m_most_recent_gc_count_gpu++;
      fmt::print("GC {}\n", t->name);
      t->unload_from_gpu();
    }
  }
  m_garbage_textures.clear();
}

void TexturePool::discard(std::shared_ptr<TextureRecord> tex) {
  ASSERT(!tex->do_gc);
  fmt::print("discard {}\n", tex->name);
  m_garbage_textures.push_back(tex);
}

TextureRecord* TexturePool::get_random_texture() {
  u32 idx = 8;
  for (u32 i = 0; i < m_textures.size(); i++) {
    if (m_textures.at((i + idx) % m_textures.size()).normal_texture) {
      return m_textures.at((i + idx) % m_textures.size()).normal_texture.get();
    }
  }
  return nullptr;
}
