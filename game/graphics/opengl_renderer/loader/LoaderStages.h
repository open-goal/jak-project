#pragma once

#include "game/graphics/opengl_renderer/loader/common.h"

std::vector<std::unique_ptr<LoaderStage>> make_loader_stages();
u64 add_texture(TexturePool& pool, const tfrag3::Texture& tex, bool is_common);

class MercLoaderStage : public LoaderStage {
 public:
  MercLoaderStage();
  bool run(Timer& timer, LoaderInput& data) override;
  void reset() override;

 private:
  bool m_done = false;
  bool m_opengl = false;
  bool m_vtx_uploaded = false;
  u32 m_idx = 0;
};