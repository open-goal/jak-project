#pragma once

#include "decompiler/level_extractor/BspHeader.h"
#include "common/math/Vector.h"

namespace decompiler {

// the different "kinds" of tfrag. The actual renderers are almost identical and the only different
// is in GS setup (alpha blending) and in how the closest object is used.
enum class TFragmentKind { NORMAL, TRANS, DIRT, ICE, LOWRES, LOWRES_TRANS, INVALID };

// There is a tree of bspheres. Each has at most 8 children. All leaves are at the same depth and
// are tfrags.  Each node is identified by a u16.  They are sorted by depth.
struct DrawVisNode {
  math::Vector<float, 4> bsphere; // the bounding sphere, in meters (4096 = 1 game meter). w = rad
  u16 child_id = 0xffff; // the ID of our first child.
  u8 num_kids = 0xff; // number of children. The children are consecutive in memory
  u8 flags = 0; // flags.  If 1, we have a DrawVisNode child, otherwise a Tfrag.
};

// This is the actual tree data, minus the tfrags themselves.
struct VisNodeTree {
  std::vector<DrawVisNode> vis_nodes;
  u16 first_child_node = 0;
  u16 last_child_node = 0;
};

// The final result
struct ExtractedTFragmentTree {
  TFragmentKind kind = TFragmentKind::INVALID;
  VisNodeTree vis_nodes;

  u16 num_tfrags = 0;
  u16 tfrag_base_idx = 0;
};

ExtractedTFragmentTree extract_tfrag(const level_tools::DrawableTreeTfrag* tree);

}  // namespace decompiler