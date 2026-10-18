#include "Entity.h"

namespace jak3 {
size_t EntityActor::generate(DataObjectGenerator& gen) const {
  size_t result = res_lump.generate_header(gen, "entity-actor");
  for (int i = 0; i < 4; i++) {
    gen.add_word_float(trans[i]);
  }
  gen.add_word(aid);
  gen.add_word(kill_mask);
  gen.add_type_tag(etype);

  ASSERT(game_task < UINT16_MAX);
  ASSERT(vis_id < UINT16_MAX);
  u32 packed = (game_task) | (vis_id << 16);
  gen.add_word(packed);

  for (int i = 0; i < 4; i++) {
    gen.add_word_float(quat[i]);
  }

  res_lump.generate_tag_list_and_data(gen, result);
  return result;
}

size_t generate_drawable_actor(DataObjectGenerator& gen,
                               const EntityActor& actor,
                               size_t actor_loc) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-actor");  // 0
  size_t result = gen.current_offset_bytes();
  gen.add_word(actor.vis_id);                         // 4
  gen.link_word_to_byte(gen.add_word(0), actor_loc);  // 8
  gen.add_word(0);                                    // 12
  for (int i = 0; i < 4; i++) {
    gen.add_word_float(actor.bsphere[i]);  // 16, 20, 24, 28
  }

  return result;
}

size_t generate_inline_array_actors(DataObjectGenerator& gen, std::vector<EntityActor>& actors) {
  std::vector<size_t> actor_locs;
  for (auto& actor : actors) {
    actor_locs.push_back(actor.generate(gen));
  }

  gen.align_to_basic();
  gen.add_type_tag("drawable-inline-array-actor");  // 0
  size_t result = gen.current_offset_bytes();
  ASSERT(actors.size() < UINT16_MAX);
  gen.add_word(actors.size() << 16);  // 4
  gen.add_word(0);
  gen.add_word(0);

  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);

  ASSERT((gen.current_offset_bytes() % 16) == 0);

  for (size_t i = 0; i < actors.size(); i++) {
    actors[i].slot = actor_locs[i];
    generate_drawable_actor(gen, actors[i], actor_locs[i]);
  }
  return result;
}

void add_actors_from_json(const nlohmann::json& json,
                          std::vector<EntityActor>& actor_list,
                          u32 base_aid,
                          decompiler::DecompilerTypeSystem& dts) {
  for (const auto& actor_json : json) {
    auto& actor = actor_list.emplace_back();
    actor.aid = actor_json.value("aid", base_aid + actor_list.size());
    actor.trans = vectorm3_from_json(actor_json.at("trans"));
    actor.etype = actor_json.at("etype").get<std::string>();
    if (actor_json.contains("kill_mask") && actor_json.at("kill_mask").is_string()) {
      actor.kill_mask = get_enum_val(actor_json.at("kill_mask").get<std::string>(), dts);
    } else {
      actor.kill_mask = actor_json.value("kill_mask", 0);
    }
    if (actor_json.contains("game_task") && actor_json.at("game_task").is_string()) {
      actor.game_task = get_enum_val(actor_json.at("game_task").get<std::string>(), dts);
    } else {
      actor.game_task = actor_json.value("game_task", 0);
    }
    actor.vis_id = actor_json.value("vis_id", 0);
    actor.quat = math::Vector4f(0, 0, 0, 1);
    if (actor_json.find("quat") != actor_json.end()) {
      actor.quat = vector_from_json(actor_json.at("quat"));
    }
    actor.bsphere = vectorm4_from_json(actor_json.at("bsphere"));

    if (actor_json.find("lump") != actor_json.end()) {
      for (auto& [key, value] : actor_json.at("lump").items()) {
        if (value.is_string()) {
          std::string value_string = value.get<std::string>();
          if (key == "name") {
            actor.name = value_string;
          }
          if (!value_string.empty() && value_string[0] == '\'') {
            actor.res_lump.add_res(
                std::make_unique<ResSymbol>(key, value_string.substr(1), -1000000000.0000));
          } else {
            actor.res_lump.add_res(
                std::make_unique<ResString>(key, value_string, -1000000000.0000));
          }
          continue;
        }

        if (value.is_array()) {
          actor.res_lump.add_res(res_from_json_array(key, value, dts));
        }
      }
    }
    actor.res_lump.sort_res();
  }
}

size_t generate_actor_group(DataObjectGenerator& gen, const ActorGroup& group) {
  gen.align_to_basic();
  gen.add_type_tag("actor-group");
  size_t result = gen.current_offset_bytes();
  auto length = group.actors.size();
  gen.add_word(length);
  gen.add_word(length);
  gen.add_type_tag("entity-actor");
  for (auto& actor : group.actors) {
    gen.link_word_to_byte(gen.add_word(0), actor->slot);
    gen.add_word(actor->aid);
  }
  gen.align(4);
  return result;
}

size_t generate_actor_group_array(DataObjectGenerator& gen, std::vector<ActorGroup>& actor_groups) {
  gen.align_to_basic();
  gen.add_type_tag("array");
  size_t result = gen.current_offset_bytes();
  auto length = actor_groups.size();
  gen.add_word(length);
  gen.add_word(length);
  gen.add_type_tag("actor-group");
  std::vector<size_t> group_slots;
  for (auto& group : actor_groups) {
    group_slots.push_back(gen.add_word(0));
  }
  gen.align(4);
  for (int i = 0; i < group_slots.size(); i++) {
    actor_groups.at(i).slot = generate_actor_group(gen, actor_groups.at(i));
    gen.link_word_to_byte(group_slots.at(i), actor_groups.at(i).slot);
  }

  return result;
}

void add_actor_groups_from_json(const nlohmann::json& json,
                                const std::vector<EntityActor>& actors,
                                std::vector<ActorGroup>& actor_groups,
                                u32 base_id) {
  for (const auto& actor_group : json) {
    auto& group = actor_groups.emplace_back();
    group.id = actor_group.value("id", base_id + actor_groups.size());
    auto entity_list = actor_group.value<std::vector<std::string>>("entities", {});
    for (auto& ent_name : entity_list) {
      auto ent = std::ranges::find_if(
          actors, [ent_name](const EntityActor& actor) { return actor.name == ent_name; });
      if (ent != actors.end()) {
        group.actors.push_back(&*ent);
      } else {
        ASSERT_MSG(false, fmt::format("Failed to find actor \"{}\", declared in actor group id {}.",
                                      ent_name, group.id));
      }
    }
  }
}
}  // namespace jak3
