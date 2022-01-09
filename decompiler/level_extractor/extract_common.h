#pragma once

#include "decompiler/level_extractor/BspHeader.h"
#include "common/math/Vector.h"
#include "common/custom_data/Tfrag3Data.h"
#include "decompiler/data/TextureDB.h"

namespace decompiler {

/// <summary>
/// Get the index of the first draw node in an array. Works for node or tfrag.
/// </summary>
/// <param name="array"></param>
/// <returns></returns>
u16 get_first_idx(const level_tools::DrawableInlineArray* array) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);
  if (as_tie_instances) {
    return as_tie_instances->instances.at(0).id;
  } else if (as_nodes) {
    return as_nodes->draw_nodes.at(0).id;
  } else {
    assert(false);
  }
}

/// <summary>
/// Verify node indices follow the patterns we expect. Takes start as the expected first, writes the
/// end.
/// </summary>
/// <param name="array"></param>
/// <param name="start"></param>
/// <param name="end"></param>
/// <returns></returns>
bool verify_node_indices_from_array(const level_tools::DrawableInlineArray* array,
                                    u16 start,
                                    u16* end) {
  auto as_tie_instances = dynamic_cast<const level_tools::DrawableInlineArrayInstanceTie*>(array);
  auto as_nodes = dynamic_cast<const level_tools::DrawableInlineArrayNode*>(array);

  if (as_tie_instances) {
    for (auto& elt : as_tie_instances->instances) {
      if (elt.id != start) {
        fmt::print("bad inst: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else if (as_nodes) {
    for (auto& elt : as_nodes->draw_nodes) {
      if (elt.id != start) {
        fmt::print("bad node: exp {} got {}\n", start, elt.id);
        return false;
      }
      start++;
    }
    *end = start;
    return true;
  } else {
    fmt::print("bad node array type: {}\n", array->my_type());
    return false;
  }
}
}  // namespace decompiler
