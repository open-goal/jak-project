#pragma once
#include "common/common_types.h"
#include "common/math/Vector.h"

namespace level_tools {

// levels may remap textures if they provide one that should be shared
struct TextureRemap {
  u32 original_texid;
  u32 new_texid;
};

struct Joint {
  std::string name;
  int parent_idx = -1;  // -1 for magic ROOT joint.
  math::Matrix4f bind_pose_T_w;
};

/*!
 * Data extracted from art groups that is not needed for .FR3, but is potentially needed for other
 * stuff (skeleton export).
 */
struct ArtData {
  std::string art_group_name;
  std::string art_name;
  std::vector<Joint> joint_group;
};

}  // namespace level_tools