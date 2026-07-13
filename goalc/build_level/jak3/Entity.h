#pragma once

#include "goalc/build_level/common/Entity.h"

namespace jak3 {
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
  std::string name;
  ResLump res_lump;
  math::Vector4f trans;  // w = 1 here
  u32 aid = 0;
  u32 kill_mask = 0;
  std::string etype;
  u32 game_task = 0;
  u32 vis_id = 0;
  math::Vector4f quat;

  math::Vector4f bsphere;

  size_t slot;

  size_t generate(DataObjectGenerator& gen) const;
};

struct ActorGroup {
  u64 id;
  std::vector<const EntityActor*> actors;
  size_t slot;
};

size_t generate_inline_array_actors(DataObjectGenerator& gen, std::vector<EntityActor>& actors);
size_t generate_actor_group_array(DataObjectGenerator& gen, std::vector<ActorGroup>& actor_groups);

void add_actors_from_json(const nlohmann::json& json,
                          std::vector<EntityActor>& actor_list,
                          u32 base_aid,
                          decompiler::DecompilerTypeSystem& dts);
void add_actor_groups_from_json(const nlohmann::json& json,
                                const std::vector<EntityActor>& actors,
                                std::vector<ActorGroup>& actor_groups,
                                u32 base_id);
}  // namespace jak3