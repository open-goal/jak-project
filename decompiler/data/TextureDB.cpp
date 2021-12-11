#include "TextureDB.h"

#include "common/util/assert.h"
#include "third-party/fmt/core.h"

namespace decompiler {

void TextureDB::add_texture(u32 tpage,
                            u32 texid,
                            const std::vector<u32>& data,
                            u16 w,
                            u16 h,
                            const std::string& tex_name,
                            const std::string& tpage_name) {
  auto existing_tpage_name = tpage_names.find(tpage);
  if (existing_tpage_name == tpage_names.end()) {
    tpage_names[tpage] = tpage_name;
  } else {
    assert(existing_tpage_name->second == tpage_name);
  }

  u32 combo_id = (tpage << 16) | texid;
  auto existing_tex = textures.find(combo_id);
  if (existing_tex != textures.end()) {
    assert(existing_tex->second.name == tex_name);
    assert(existing_tex->second.w == w);
    assert(existing_tex->second.h == h);
    assert(existing_tex->second.rgba_bytes == data);
    assert(existing_tex->second.page == tpage);
  } else {
    auto& new_tex = textures[combo_id];
    new_tex.rgba_bytes = data;
    new_tex.name = tex_name;
    new_tex.w = w;
    new_tex.h = h;
    new_tex.page = tpage;
  }
}

}  // namespace decompiler