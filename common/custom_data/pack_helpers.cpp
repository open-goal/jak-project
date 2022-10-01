#include "pack_helpers.h"

#include <map>

#include "common/log/log.h"

constexpr float kClusterSize = 4096 * 40;  // 100 in-game meters
constexpr float kMasterOffset = 12000 * 4096;

std::pair<u64, u16> position_to_cluster_and_offset(float in) {
  in += kMasterOffset;
  if (in < 0) {
    lg::print("negative: {}\n", in);
  }
  ASSERT(in >= 0);
  int cluster_cell = (in / kClusterSize);
  float leftover = in - (cluster_cell * kClusterSize);
  u16 offset = (leftover / kClusterSize) * float(UINT16_MAX);

  float recovered = ((float)cluster_cell + ((float)offset / UINT16_MAX)) * kClusterSize;
  float diff = std::fabs(recovered - in);
  ASSERT(diff < 7);
  ASSERT(cluster_cell >= 0);
  ASSERT(cluster_cell < UINT16_MAX);
  return {cluster_cell, offset};
}

void pack_tfrag_vertices(tfrag3::PackedTfragVertices* result,
                         const std::vector<tfrag3::PreloadedVertex>& vertices) {
  u32 next_cluster_idx = 0;
  std::map<u64, u32> clusters;

  for (auto& vtx : vertices) {
    auto x = position_to_cluster_and_offset(vtx.x);
    auto y = position_to_cluster_and_offset(vtx.y);
    auto z = position_to_cluster_and_offset(vtx.z);
    u64 cluster_id = 0;
    cluster_id |= x.first;
    cluster_id |= (y.first << 16);
    cluster_id |= (z.first << 32);

    auto cluster_it = clusters.find(cluster_id);
    u32 my_cluster_idx = 0;
    if (cluster_it == clusters.end()) {
      // first in cluster
      clusters[cluster_id] = next_cluster_idx;
      my_cluster_idx = next_cluster_idx;
      next_cluster_idx++;
    } else {
      my_cluster_idx = cluster_it->second;
    }

    tfrag3::PackedTfragVertices::Vertex out_vtx;
    out_vtx.xoff = x.second;
    out_vtx.yoff = y.second;
    out_vtx.zoff = z.second;
    out_vtx.cluster_idx = my_cluster_idx;
    // TODO check these
    out_vtx.s = vtx.s * 1024;
    out_vtx.t = vtx.t * 1024;
    out_vtx.color_index = vtx.color_index;
    result->vertices.push_back(out_vtx);
  }

  result->cluster_origins.resize(next_cluster_idx);
  for (auto& cluster : clusters) {
    auto& res = result->cluster_origins[cluster.second];
    res.x() = (u16)cluster.first;
    res.y() = (u16)(cluster.first >> 16);
    res.z() = (u16)(cluster.first >> 32);
  }

  ASSERT(next_cluster_idx < UINT16_MAX);
}
