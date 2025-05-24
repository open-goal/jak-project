#include "extract_actors.h"

#include "common/goos/PrettyPrinter2.h"

#include "fmt/core.h"
#include "third-party/json.hpp"

namespace decompiler {

/*
 * {
"trans": [-21.6238, 20.0496, 17.1191], // translation
"etype": "fuel-cell",  // actor type
"game_task": 0, // associated game task (for powercells, etc)
"quat" : [0, 0, 0, 1], // quaternion
"bsphere": [-21.6238, 19.3496, 17.1191, 10], // bounding sphere
"lump": {
  "name":"test-fuel-cell"
}
},

 */

namespace {
nlohmann::json vectorm_json(const level_tools::Vector& v) {
  nlohmann::json result;
  for (int i = 0; i < 4; i++) {
    result.push_back(v.data[i] / 4096.f);
  }
  return result;
}

nlohmann::json vector_json(const level_tools::Vector& v) {
  nlohmann::json result;
  for (int i = 0; i < 4; i++) {
    result.push_back(v.data[i]);
  }
  return result;
}

nlohmann::json vector_json(const float* data) {
  nlohmann::json result;
  for (int i = 0; i < 4; i++) {
    result.push_back(data[i]);
  }
  return result;
}

nlohmann::json strings_json(const std::vector<std::string>& data, bool prefix_quote) {
  if (data.size() == 1) {
    if (prefix_quote) {
      return fmt::format("'{}", data[0]);
    } else {
      return data[0];
    }
  } else {
    nlohmann::json result;
    for (const auto& str : data) {
      if (prefix_quote) {
        result.push_back(fmt::format("'{}", str));
      } else {
        result.push_back(str);
      }
    }
    return result;
  }
}

template <typename T>
nlohmann::json value_json(const std::vector<u8>& data, int count) {
  ASSERT(count * sizeof(T) == data.size());
  if (count == 1) {
    T v;
    memcpy(&v, data.data(), sizeof(T));
    return v;
  } else {
    nlohmann::json ret;
    T v;
    for (int i = 0; i < count; i++) {
      memcpy(&v, data.data() + i * sizeof(T), sizeof(T));
      ret.push_back(v);
    }
    return ret;
  }
}
}  // namespace

std::string extract_actors_to_json(const level_tools::DrawableInlineArrayActor& actors) {
  nlohmann::json json;

  for (const auto& dactor : actors.drawable_actors) {
    const auto& actor = dactor.actor;
    auto& json_actor = json.emplace_back();
    json_actor["bsphere"] = vectorm_json(dactor.bsphere);
    // drawable ID?

    json_actor["trans"] = vectorm_json(actor.trans);
    json_actor["aid"] = actor.aid;  // aid
    // nav mesh
    json_actor["etype"] = actor.etype;
    json_actor["game_task"] = actor.task;
    // vis ID?
    json_actor["quat"] = vector_json(actor.quat);
    auto& json_lump = json_actor["lump"];
    for (const auto& res : actor.res_list) {
      if (res.elt_type == "string") {
        json_lump[res.name] = strings_json(res.strings, false);
      } else if (res.elt_type == "symbol") {
        json_lump[res.name] = strings_json(res.strings, true);
      } else if (res.elt_type == "type") {
        // TODO: confusion with symbols
        json_lump[res.name] = strings_json(res.strings, true);
      } else if (res.elt_type == "vector") {
        const float* data = (const float*)res.inlined_storage.data();
        if (res.count == 1) {
          json_lump[res.name] = vector_json(data);
        } else {
          for (int i = 0; i < res.count; i++) {
            json_lump[res.name].push_back(vector_json(data + 4 * i));
          }
        }
      } else if (res.elt_type == "pair") {
        json_lump[res.name] = pretty_print::to_string(res.script);
      } else if (res.elt_type == "float") {
        json_lump[res.name] = value_json<float>(res.inlined_storage, res.count);
      } else if (res.elt_type == "int32") {
        json_lump[res.name] = value_json<int32_t>(res.inlined_storage, res.count);
      } else if (res.elt_type == "int16") {
        json_lump[res.name] = value_json<int16_t>(res.inlined_storage, res.count);
      } else if (res.elt_type == "int8") {
        json_lump[res.name] = value_json<int8_t>(res.inlined_storage, res.count);
      } else if (res.elt_type == "uint32") {
        json_lump[res.name] = value_json<uint32_t>(res.inlined_storage, res.count);
      } else if (res.elt_type == "uint16") {
        json_lump[res.name] = value_json<uint16_t>(res.inlined_storage, res.count);
      } else if (res.elt_type == "uint8") {
        json_lump[res.name] = value_json<uint8_t>(res.inlined_storage, res.count);
      } else if (res.elt_type == "actor-group") {
        // not supported.
      } else {
        ASSERT_NOT_REACHED();
      }
    }
  }

  return json.dump(2);
}

}  // namespace decompiler
