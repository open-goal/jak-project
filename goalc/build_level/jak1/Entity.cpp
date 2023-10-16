#include "Entity.h"

namespace jak1 {
size_t EntityActor::generate(DataObjectGenerator& gen) const {
  size_t result = res_lump.generate_header(gen, "entity-actor");
  for (int i = 0; i < 4; i++) {
    gen.add_word_float(trans[i]);
  }
  gen.add_word(aid);
  gen.add_word(0);  // nav mesh
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

size_t generate_inline_array_actors(DataObjectGenerator& gen,
                                    const std::vector<EntityActor>& actors) {
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
    generate_drawable_actor(gen, actors[i], actor_locs[i]);
  }
  return result;
}

void add_actors_from_json(const nlohmann::json& json,
                          std::vector<EntityActor>& actor_list,
                          u32 base_aid) {
  for (const auto& actor_json : json) {
    auto& actor = actor_list.emplace_back();
    actor.aid = actor_json.value("aid", base_aid + actor_list.size());
    actor.trans = vectorm3_from_json(actor_json.at("trans"));
    actor.etype = actor_json.at("etype").get<std::string>();
    actor.game_task = actor_json.value("game_task", 0);
    actor.vis_id = actor_json.value("vis_id", 0);
    actor.quat = math::Vector4f(0, 0, 0, 1);
    if (actor_json.find("quat") != actor_json.end()) {
      actor.quat = vector_from_json(actor_json.at("quat"));
    }
    actor.bsphere = vectorm4_from_json(actor_json.at("bsphere"));

    if (actor_json.find("lump") != actor_json.end()) {
      for (auto [key, value] : actor_json.at("lump").items()) {
        if (value.is_string()) {
          std::string value_string = value.get<std::string>();
          if (value_string.size() > 0 && value_string[0] == '\'') {
            actor.res_lump.add_res(
                std::make_unique<ResSymbol>(key, value_string.substr(1), -1000000000.0000));
          } else {
            actor.res_lump.add_res(
                std::make_unique<ResString>(key, value_string, -1000000000.0000));
          }
          continue;
        }

        if (value.is_array()) {
          actor.res_lump.add_res(res_from_json_array(key, value));
        }
      }
    }
    actor.res_lump.sort_res();
  }
}

size_t EntityAmbient::generate(DataObjectGenerator& gen) const {
  size_t result = res_lump.generate_header(gen, "entity-ambient");
  for (size_t i = 0; i < 4; i++) {
    gen.add_word_float(trans[i]);
  }
  ASSERT(vis_id < UINT16_MAX);
  gen.add_word(aid);
  gen.add_word(vis_id);
  res_lump.generate_tag_list_and_data(gen, result);
  return result;
}

size_t generate_drawable_ambient(DataObjectGenerator& gen,
                                 const EntityAmbient& ambient,
                                 size_t ambient_loc) {
  gen.align_to_basic();
  gen.add_type_tag("drawable-ambient");  // 0
  size_t result = gen.current_offset_bytes();
  gen.add_word(ambient.vis_id);                         // 4
  gen.link_word_to_byte(gen.add_word(0), ambient_loc);  // 8
  gen.add_word(0);                                      // 12
  for (int i = 0; i < 4; i++) {
    gen.add_word_float(ambient.bsphere[i]);  // 16, 20, 24, 28
  }
  return result;
}

size_t generate_inline_array_ambients(DataObjectGenerator& gen,
                                      const std::vector<EntityAmbient>& ambients) {
  std::vector<size_t> ambient_locs;
  for (auto& ambient : ambients) {
    ambient_locs.push_back(ambient.generate(gen));
  }
  gen.align_to_basic();
  gen.add_type_tag("drawable-inline-array-ambient");  // 0
  size_t result = gen.current_offset_bytes();
  ASSERT(ambients.size() < UINT16_MAX);
  gen.add_word(ambients.size() << 16);  // 4
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  gen.add_word(0);
  ASSERT((gen.current_offset_bytes() % 16) == 0);
  for (size_t i = 0; i < ambients.size(); i++) {
    generate_drawable_ambient(gen, ambients[i], ambient_locs[i]);
  }
  return result;
}

void add_ambients_from_json(const nlohmann::json& json,
                            std::vector<EntityAmbient>& ambient_list,
                            u32 base_aid) {
  for (const auto& ambient_json : json) {
    auto& ambient = ambient_list.emplace_back();
    ambient.aid = ambient_json.value("aid", base_aid + ambient_list.size());
    ambient.vis_id = ambient_json.value("vis_id", 0);
    ambient.trans = vectorm4_from_json(ambient_json.at("trans"));
    ambient.bsphere = vectorm4_from_json(ambient_json.at("bsphere"));
    if (ambient_json.find("lump") != ambient_json.end()) {
      for (auto [key, value] : ambient_json.at("lump").items()) {
        if (value.is_string()) {
          std::string value_string = value.get<std::string>();
          if (value_string.size() > 0 && value_string[0] == '\'') {
            ambient.res_lump.add_res(
                std::make_unique<ResSymbol>(key, value_string.substr(1), -1000000000.0000));
          } else {
            ambient.res_lump.add_res(
                std::make_unique<ResString>(key, value_string, -1000000000.0000));
          }
          continue;
        }
        if (value.is_array()) {
          ambient.res_lump.add_res(res_from_json_array(key, value));
        }
      }
    }
    ambient.res_lump.sort_res();
  }
}
}  // namespace jak1
