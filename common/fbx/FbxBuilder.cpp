#include "FbxBuilder.h"
#include "common/util/FileUtil.h"

namespace fbx {

std::string make_name_class_string(const std::string& name, const std::string& klass) {
  std::string result = name;
  result.push_back(0);
  result.push_back(1);
  result.append(klass);
  return result;
}

FbxBuilder::FbxBuilder() {}

u64 FbxBuilder::add_tri_mesh_geom(const std::string& name,
                                  std::vector<math::Vector3f>& vtx_positions,
                                  std::vector<uint32_t>& vtx_indices) {
  u64 geom_id = m_next_id++;
  auto& node = m_geometry_nodes.emplace_back("Geometry");
  node.properties.emplace_back((s64)geom_id);
  node.properties.emplace_back(make_name_class_string(name, "Geometry"));
  node.properties.emplace_back("Mesh");
  {
    auto& verts = node.add_node("Vertices");
    verts.properties.emplace_back(&vtx_positions[0].x(), vtx_positions.size() * 3);
  }
  std::vector<int32_t> indices_fbx;
  for (uint32_t i = 0; i < vtx_indices.size(); i += 3) {
    indices_fbx.push_back(vtx_indices[i]);
    indices_fbx.push_back(vtx_indices[i + 1]);
    indices_fbx.push_back(vtx_indices[i + 2] ^ 0xffffffff);
  }
  node.add_node("PolygonVertexIndex").properties.emplace_back(indices_fbx);

  return geom_id;
}

void FbxBuilder::add_instance_of_geom(u64 geom_id) {
  u64 model_id = m_next_id++;
  auto& model = m_model_nodes.emplace_back("Model");
  model.properties.emplace_back((s64)model_id);
  model.properties.emplace_back(make_name_class_string("unknown", "Model"));
  model.properties.emplace_back("unknown");

  {
    auto& c = m_connections_nodes.emplace_back("Connect");
    c.properties.emplace_back("OO");
    c.properties.emplace_back((s64)geom_id);
    c.properties.emplace_back((s64)model_id);
  }

  {
    auto& c = m_connections_nodes.emplace_back("Connect");
    c.properties.emplace_back("OO");
    c.properties.emplace_back((s64)model_id);
    c.properties.emplace_back((s64)0);
  }
}

void FbxBuilder::write(const std::string& dest) {
  FbxRoot root;
  root.add_node("Creator").properties.emplace_back("OpenGOAL");
  auto& global_settings = root.add_node("GlobalSettings").children.emplace_back("Properties70");
  auto& objects = root.add_node("Objects");
  for (auto& k : m_geometry_nodes) {
    objects.children.push_back(k);
  }
  for (auto& k : m_model_nodes) {
    objects.children.push_back(k);
  }

  auto& connections = root.add_node("Connections");
  for (auto& k : m_connections_nodes) {
    connections.children.push_back(k);
  }

  Serializer ser;
  root.serialize(ser);
  auto result = ser.get_save_result();
  file_util::write_binary_file(dest, result.first, result.second);
}
}  // namespace fbx