#include "TextureDB.h"

#include "third-party/fmt/core.h"
#include "common/util/Assert.h"
#include "third-party/stb_image/stb_image.h"
#include <filesystem>

namespace decompiler {

void TextureDB::add_texture(u32 tpage,
                            u32 texid,
                            const std::vector<u32>& data,
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
  auto existing_tex = textures.find(combo_id);
  if (existing_tex != textures.end()) {
    ASSERT(existing_tex->second.name == tex_name);
    ASSERT(existing_tex->second.w == w);
    ASSERT(existing_tex->second.h == h);
    ASSERT(existing_tex->second.rgba_bytes == data);
    ASSERT(existing_tex->second.page == tpage);
  } else {
    auto& new_tex = textures[combo_id];
    new_tex.rgba_bytes = data;
    new_tex.name = tex_name;
    new_tex.w = w;
    new_tex.h = h;
    new_tex.page = tpage;
  }
  for (const auto& level_name : level_names) {
    texture_ids_per_level[level_name].insert(combo_id);
  }
}

void TextureDB::replace_textures(const std::string& path) {
  std::filesystem::path base_path(path);
  for (auto& tex : textures) {
    std::filesystem::path full_path =
        base_path / tpage_names.at(tex.second.page) / (tex.second.name + ".png");
    if (std::filesystem::exists(full_path)) {
      fmt::print("Replacing {}\n", full_path.string().c_str());
      int w, h;
      auto data = stbi_load(full_path.string().c_str(), &w, &h, 0, 4);  // rgba channels
      if (!data) {
        fmt::print("failed to load PNG file: {}\n", full_path.string().c_str());
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
}  // namespace decompiler
