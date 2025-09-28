#pragma once

#include "goalc/build_level/common/Entity.h"

namespace jak2 {
/*
 *  (trans vector :inline :offset-assert 32)
 *  (aid uint32 :offset-assert 48)
 *  (kill-mask task-mask :offset-assert 52)
 *  (etype type :offset-assert 56) ;; probably type
 *  (task game-task :offset-assert 60)
 *  (vis-id uint16 :offset-assert 62)
 *  (quat quaternion :inline :offset-assert 64)
 */
struct EntityActor {
  ResLump res_lump;
  math::Vector4f trans;  // w = 1 here
  u32 aid = 0;
  u32 kill_mask = 0;
  std::string etype;
  u32 game_task = 0;
  u32 vis_id = 0;
  math::Vector4f quat;

  math::Vector4f bsphere;

  size_t generate(DataObjectGenerator& gen) const;
};

size_t generate_inline_array_actors(DataObjectGenerator& gen,
                                    const std::vector<EntityActor>& actors);

void add_actors_from_json(const nlohmann::json& json,
                          std::vector<EntityActor>& actor_list,
                          u32 base_aid,
                          decompiler::DecompilerTypeSystem& dts);
}  // namespace jak2