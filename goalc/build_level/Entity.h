#pragma once

#include "goalc/build_level/ResLump.h"

#include "third-party/json.hpp"

/*
 *     (trans vector :inline :offset-assert 32)
    (aid uint32 :offset-assert 48)
 *     (nav-mesh nav-mesh :offset-assert 52)
    (etype type :offset-assert 56) ;; probably type
    (task game-task :offset-assert 60)
    (vis-id uint16 :offset-assert 62)
    (vis-id-signed int16 :offset 62) ;; added
    (quat quaternion :inline :offset-assert 64)
 */
struct EntityActor {
  ResLump res_lump;
  u32 aid = 0;
  math::Vector4f trans;  // w = 1 here
  std::string etype;
  u32 game_task = 0;
  u32 vis_id = 0;
  math::Vector4f quat;

  math::Vector4f bsphere;

  size_t generate(DataObjectGenerator& gen) const;
};

struct EntityAmbient {
  ResLump res_lump;
  u32 aid = 0;
  math::Vector4f trans;
  u32 vis_id = 0;
  math::Vector4f bsphere;
  size_t generate(DataObjectGenerator& gen) const;
};

size_t generate_inline_array_actors(DataObjectGenerator& gen,
                                    const std::vector<EntityActor>& actors);

size_t generate_inline_array_ambients(DataObjectGenerator& gen,
                                      const std::vector<EntityAmbient>& ambients);
void add_ambients_from_json(const nlohmann::json& json,
                            std::vector<EntityAmbient>& ambient_list,
                            u32 base_aid);

void add_actors_from_json(const nlohmann::json& json,
                          std::vector<EntityActor>& actor_list,
                          u32 base_aid);