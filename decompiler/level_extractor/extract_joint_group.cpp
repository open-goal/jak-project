#include "extract_joint_group.h"

#include "common/math/geometry.h"

#include "decompiler/util/goal_data_reader.h"

namespace decompiler {

void extract_joint_group(const ObjectFileData& ag_data,
                         const DecompilerTypeSystem& dts,
                         GameVersion /*version*/,
                         std::map<std::string, level_tools::ArtData>& out) {
  auto locations = find_objects_with_type(ag_data.linked_data, "art-joint-geo");
  for (auto loc : locations) {
    TypedRef ref(Ref{&ag_data.linked_data, 0, loc * 4}, dts.ts.lookup_type("art-joint-geo"));
    auto name = read_string_field(ref, "name", dts, false);
    auto& data = out[name];
    data.art_name = name;
    data.art_group_name = ag_data.name_in_dgo;
    ASSERT(data.joint_group.empty());
    const int length = read_plain_data_field<int32_t>(ref, "length", dts);
    Ref iter = get_field_ref(ref, "data", dts);
    std::map<int, int> offset_to_joint;
    for (int i = 0; i < length; i++) {
      auto& njoint = data.joint_group.emplace_back();
      Ref joint = deref_label(iter);
      joint.byte_offset -= 4;
      bool inserted = offset_to_joint.insert({joint.byte_offset, i}).second;
      ASSERT(inserted);
      TypedRef tjoint = typed_ref_from_basic(joint, dts);
      ASSERT(tjoint.type->get_name() == "joint");
      ASSERT(read_plain_data_field<int32_t>(tjoint, "number", dts) == i);
      njoint.name = read_string_field(tjoint, "name", dts, true);
      memcpy_from_plain_data((u8*)njoint.bind_pose_T_w.data(),
                             get_field_ref(tjoint, "bind-pose", dts), 4 * 4 * sizeof(float));
      if (get_word_kind_for_field(tjoint, "parent", dts) == LinkedWord::PTR) {
        auto pjoint = deref_label(get_field_ref(tjoint, "parent", dts));
        njoint.parent_idx = offset_to_joint.at(pjoint.byte_offset - 4);
      } else {
        ASSERT(i == 0 || i == 1);
      }
      iter.byte_offset += 4;
    }
  }
}
}  // namespace decompiler