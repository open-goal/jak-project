#pragma once

#include <vector>

#include "common/common_types.h"
#include "common/math/Vector.h"

/*!
 * Fix-up tfrag/tie format mesh to a best-guess unstripped mesh with proper triangle orientation.
 * The input is the tie/tfrag index list, using UINT32_MAX as primitive restart.
 * The output is an unstripped index list (groups of three indices, one for each triangle)
 * and a map from each element in the original index buffer to the new one.
 * Ex: unstripped[old_to_new_start[i]] is the start of prim stripped_indices[i].
 *
 * This depends on specific behavior of the original tfrag/tie renderers, and how
 * extract_tie/extract_tfrag work (basically keeping the order of vertices exactly the same as in VU
 * memory).
 *
 * The stripping logic of shrub/merc/generic models appears to be different, and this likely won't
 * work.
 */
void fixup_and_unstrip_tfrag_tie(const std::vector<u32>& stripped_indices,
                                 const std::vector<math::Vector3f>& positions,
                                 std::vector<u32>& unstripped,
                                 std::vector<u32>& old_to_new_start);