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
    for (auto tid : texture_ids_in_page) {
      textures_by_tbp_offset[textures.at(tid).dest].push_back(tid);
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
          int offset = 0;
          for (auto id : tex_ids) {
            result += fmt::format("{{{}, {}}},", id & 0xffff, offset++);
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
