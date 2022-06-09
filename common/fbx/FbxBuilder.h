#pragma once

#include "common/fbx/FBX.h"
#include "common/math/Vector.h"

namespace fbx {

class FbxBuilder {
 public:
  FbxBuilder();

  u64 add_tri_mesh_geom(const std::string& name,
                        std::vector<math::Vector3f>& vtx_positions,
                        std::vector<uint32_t>& vtx_indices);
  void add_instance_of_geom(u64 geom_id);

  void write(const std::string& dest);

 private:
  u64 m_next_id = 1;  // root = 0
  std::vector<Node> m_geometry_nodes;
  std::vector<Node> m_model_nodes;
  std::vector<Node> m_connections_nodes;
};

}  // namespace fbx