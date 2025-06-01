#pragma once

#include "common/log/log.h"
#include "common/util/gltf_util.h"

namespace decompiler {

gltf_util::MercSwapData load_replacement_merc_model(
    tfrag3::MercModel& mdl,
    u32 current_idx_count,
    u32 current_vtx_count,
    u32 current_tex_count,
    const std::string& path,
    const std::vector<tfrag3::MercVertex>& old_verts,
    bool custom_mdl);

gltf_util::MercSwapData load_custom_merc_model(const std::string& name,
                                               u32 current_idx_count,
                                               u32 current_vtx_count,
                                               u32 current_tex_count,
                                               const std::string& path,
                                               const std::vector<tfrag3::MercVertex>& old_verts,
                                               bool custom_mdl);
}  // namespace decompiler