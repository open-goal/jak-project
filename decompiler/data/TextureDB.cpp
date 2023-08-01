#include "TextureDB.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "third-party/fmt/core.h"
#define STBI_WINDOWS_UTF8
#include "third-party/stb_image/stb_image.h"

namespace decompiler {

void TextureDB::add_texture(u32 tpage,
                            u32 texid,
                            const std::vector<u32>& data,
                            u16 w,
                            u16 h,
                            const std::string& tex_name,
                            const std::string& tpage_name,
                            const std::vector<std::string>& level_names,
                            u32 num_mips,
                            u32 dest) {
  auto existing_tpage_name = tpage_names.find(tpage);
  if (existing_tpage_name == tpage_names.end()) {
    tpage_names[tpage] = tpage_name;
  } else {
    ASSERT(existing_tpage_name->second == tpage_name);
  }

  u32 combo_id = (tpage << 16) | texid;
  auto existing_tex = textures.find(combo_id);
  if (existing_tex != textures.end()) {
    ASSERT(existing_tex->second.name == tex_name);
    ASSERT(existing_tex->second.w == w);
    ASSERT(existing_tex->second.h == h);
    ASSERT(existing_tex->second.rgba_bytes == data);
    ASSERT(existing_tex->second.page == tpage);
    ASSERT(existing_tex->second.num_mips == num_mips);
    ASSERT(existing_tex->second.dest == dest);
  } else {
    auto& new_tex = textures[combo_id];
    new_tex.rgba_bytes = data;
    new_tex.name = tex_name;
    new_tex.w = w;
    new_tex.h = h;
    new_tex.page = tpage;
    new_tex.num_mips = num_mips;
    new_tex.dest = dest;
  }
  for (const auto& level_name : level_names) {
    texture_ids_per_level[level_name].insert(combo_id);
  }
}

void TextureDB::add_index_texture(u32 tpage,
                                  u32 texid,
                                  const std::vector<u8>& index_data,
                                  const std::array<math::Vector4<u8>, 256>& clut,
                                  u16 w,
                                  u16 h,
                                  const std::string& tex_name,
                                  const std::string& tpage_name,
                                  const std::vector<std::string>& level_names) {
  auto existing_tpage_name = tpage_names.find(tpage);
  if (existing_tpage_name == tpage_names.end()) {
    tpage_names[tpage] = tpage_name;
  } else {
    ASSERT(existing_tpage_name->second == tpage_name);
  }

  u32 combo_id = (tpage << 16) | texid;
  auto existing_tex = index_textures_by_combo_id.find(combo_id);
  if (existing_tex != index_textures_by_combo_id.end()) {
    ASSERT(existing_tex->second.name == tex_name);
    ASSERT(existing_tex->second.w == w);
    ASSERT(existing_tex->second.h == h);
    ASSERT(existing_tex->second.index_data == index_data);
    ASSERT(existing_tex->second.combo_id == combo_id);
    ASSERT(existing_tex->second.color_table == clut);
    ASSERT(existing_tex->second.tpage_name == tpage_name);
    for (auto& ln : level_names) {
      existing_tex->second.level_names.push_back(ln);
    }
  } else {
    auto& new_tex = index_textures_by_combo_id[combo_id];
    new_tex.index_data = index_data;
    new_tex.color_table = clut;
    new_tex.name = tex_name;
    new_tex.w = w;
    new_tex.h = h;
    new_tex.tpage_name = tpage_name;
    new_tex.combo_id = combo_id;
    new_tex.level_names = level_names;
  }
}

void TextureDB::replace_textures(const fs::path& path) {
  fs::path base_path(path);
  for (auto& tex : textures) {
    fs::path full_path = base_path / tpage_names.at(tex.second.page) / (tex.second.name + ".png");
    if (fs::exists(full_path)) {
      lg::info("Replacing {}", full_path.string().c_str());
      int w, h;
      auto data = stbi_load(full_path.string().c_str(), &w, &h, 0, 4);  // rgba channels
      if (!data) {
        lg::warn("failed to load PNG file: {}", full_path.string().c_str());
        continue;
      }
      tex.second.rgba_bytes.resize(w * h);
      memcpy(tex.second.rgba_bytes.data(), data, w * h * 4);
      tex.second.w = w;
      tex.second.h = h;
      stbi_image_free(data);
    }
  }
}

/*!
 * Generate a table of offsets
 */
std::string TextureDB::generate_texture_dest_adjustment_table() const {
  // group textures by page
  std::map<u32, std::vector<u32>> textures_by_page;
  for (const auto& [texture_id, texture] : textures) {
    textures_by_page[texture.page].push_back(texture_id);
  }

  std::string result = "{\n";
  // loop over pages (this overlap trick only applies within a page)
  for (const auto& [tpage, texture_ids_in_page] : textures_by_page) {
    // organize by tbp offset
    std::map<u32, std::vector<u32>> textures_by_tbp_offset;
    std::set<u32> all_used_tbp_offsets;
    u32 max_tbp_offset = 0;
    for (auto tid : texture_ids_in_page) {
      u32 tbp = textures.at(tid).dest;
      textures_by_tbp_offset[tbp].push_back(tid);
      all_used_tbp_offsets.insert(tbp);
      max_tbp_offset = std::max(max_tbp_offset, tbp);
    }

    // find tbp's with overlaps:
    bool needs_remap = false;
    for (const auto& [tbp, tex_ids] : textures_by_tbp_offset) {
      if (tex_ids.size() > 1) {
        needs_remap = true;
        break;
      }
    }

    if (needs_remap) {
      result += fmt::format("{{{},{{\n", tpage);
      for (const auto& [tbp, tex_ids] : textures_by_tbp_offset) {
        if (tex_ids.size() > 1) {
          int offset = 1;
          for (size_t id_id = 1; id_id < tex_ids.size(); id_id++) {
            auto id = tex_ids[id_id];

            bool ok = false;
            int tries = 50;
            // make sure we don't overlap again.
            while (!ok && tries > 0) {
              tries--;
              if (!all_used_tbp_offsets.count(tbp + offset)) {
                ok = true;
                break;
              }
              offset++;
            }

            ASSERT(ok);
            all_used_tbp_offsets.insert(tbp + offset);
            result += fmt::format("{{{}, {}}},", id & 0xffff, offset);
            offset++;
          }
        }
      }
      result.pop_back();
      result += "}},\n";
    }
  }
  result += "}\n";
  return result;
}
}  // namespace decompiler
