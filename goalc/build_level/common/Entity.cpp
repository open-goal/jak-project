#include "Entity.h"

math::Vector4f vectorm3_from_json(const nlohmann::json& json) {
  ASSERT(json.size() == 3);
  math::Vector4f result;
  for (int i = 0; i < 3; i++) {
    result[i] = json[i].get<float>() * METER_LENGTH;
  }
  result[3] = 1.f;
  return result;
}

math::Vector4f vectorm4_from_json(const nlohmann::json& json) {
  ASSERT(json.size() == 4);
  math::Vector4f result;
  for (int i = 0; i < 4; i++) {
    result[i] = json[i].get<float>() * METER_LENGTH;
  }
  return result;
}

math::Vector4f movie_pos_from_json(const nlohmann::json& json) {
  ASSERT(json.size() == 4);
  math::Vector4f result;
  for (int i = 0; i < 3; i++) {
    result[i] = json[i].get<float>() * METER_LENGTH;
  }
  result[3] = json[3].get<float>() * DEGREES_LENGTH;
  return result;
}

math::Vector4f vector_from_json(const nlohmann::json& json) {
  ASSERT(json.size() == 4);
  math::Vector4f result;
  for (int i = 0; i < 4; i++) {
    result[i] = json[i].get<float>();
  }
  return result;
}

math::Vector4f vector_vol_from_json(const nlohmann::json& json) {
  ASSERT(json.size() == 4);
  math::Vector4f result;
  for (int i = 0; i < 3; i++) {
    result[i] = json[i].get<float>();
  }
  result[3] = json[3].get<float>() * METER_LENGTH;
  return result;
}

u64 parse_enum(EnumType* e, goos::Object& rest) {
  if (e->is_bitfield()) {
    u64 value = 0;
    for_each_in_list(rest, [&](const goos::Object& o) {
      auto kv = e->entries().find(o.as_symbol().name_ptr);
      ASSERT_MSG(kv != e->entries().end(),
                 fmt::format("The value {} was not found in enum.", o.print()));
      value |= ((u64)1 << (u64)kv->second);
    });
    return value;
  } else {
    u64 value = 0;
    bool got = false;
    for_each_in_list(rest, [&](const goos::Object& o) {
      ASSERT_MSG(!got, "Invalid enum lookup.");
      auto kv = e->entries().find(o.as_symbol().name_ptr);
      ASSERT_MSG(kv != e->entries().end(),
                 fmt::format("The value {} was not found in enum.", o.print()));
      value = kv->second;
      got = true;
    });
    ASSERT_MSG(got, "Invalid enum lookup.");
    return value;
  }
}

u64 get_enum_val(const std::string& val, decompiler::DecompilerTypeSystem& dts) {
  auto& reader = pretty_print::get_pretty_printer_reader();
  auto value = reader.read_from_string(val).as_pair()->cdr.as_pair()->car;
  auto type = value.as_pair()->car.as_symbol().name_ptr;
  auto rest = value.as_pair()->cdr;
  auto enum_def = dts.ts.try_enum_lookup(type);
  ASSERT_MSG(enum_def, fmt::format("Enum {} was not found.", type));
  return parse_enum(enum_def, rest);
}

u64 get_enum_or_int(const nlohmann::json& value, decompiler::DecompilerTypeSystem& dts) {
  if (value.is_string()) {
    return get_enum_val(value.get<std::string>(), dts);
  } else {
    ASSERT(value.is_number());
    return value.get<int>();
  }
}
template <typename T>
std::vector<T> enum_from_json(const nlohmann::json& json, decompiler::DecompilerTypeSystem& dts) {
  std::vector<T> result;
  for (const auto& entry : json) {
    result.push_back(static_cast<T>(get_enum_val(entry.get<std::string>(), dts)));
  }
  return result;
}

static std::unordered_map<std::string,
                          std::function<std::unique_ptr<Res>(const std::string&,
                                                             const nlohmann::json&,
                                                             decompiler::DecompilerTypeSystem&)>>
    lump_map = {
        // integers
        {"int32",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<s32> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<int>());
           }
           return std::make_unique<ResInt32>(name, data, -1000000000.0000);
         }},
        {"uint32",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<u32> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<u32>());
           }
           return std::make_unique<ResUint32>(name, data, -1000000000.0000);
         }},
        {"enum-int32",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           std::vector<s32> data;
           for (size_t i = 1; i < json.size(); i++) {
             data = enum_from_json<s32>(json[i], dts);
           }
           return std::make_unique<ResInt32>(name, data, -1000000000.0000);
         }},
        {"enum-uint32",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           std::vector<u32> data;
           for (size_t i = 1; i < json.size(); i++) {
             data = enum_from_json<u32>(json[i], dts);
           }
           return std::make_unique<ResUint32>(name, data, -1000000000.0000);
         }},
        // special lumps
        {"eco-info",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           std::vector<s32> data;
           // pickup-type
           data.push_back(static_cast<s32>(get_enum_val(json[1].get<std::string>(), dts)));
           // amount
           data.push_back(static_cast<s32>(get_enum_or_int(json[2], dts)));
           return std::make_unique<ResInt32>(name, data, -1000000000.0000);
         }},
        {"cell-info",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           std::vector<s32> data;
           // (pickup-type fuel-cell)
           data.push_back(6);
           data.push_back(static_cast<s32>(get_enum_or_int(json[1], dts)));
           return std::make_unique<ResInt32>(name, data, -1000000000.0000);
         }},
        {"buzzer-info",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           std::vector<s32> data;
           // (pickup-type buzzer)
           data.push_back(8);
           auto task = static_cast<s32>(get_enum_val(json[1].get<std::string>(), dts));
           auto buzzer = json[2].get<int>();
           data.push_back(task + (buzzer * (1 << 16)));
           return std::make_unique<ResInt32>(name, data, -1000000000.0000);
         }},
        {"water-height",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           std::vector<float> data;
           // water-height
           data.push_back(json[1].get<float>() * METER_LENGTH);
           // wade-height
           data.push_back(json[2].get<float>() * METER_LENGTH);
           // swim-height
           data.push_back(json[3].get<float>() * METER_LENGTH);
           // water-flags
           data.push_back(static_cast<float>(get_enum_val(json[4].get<std::string>(), dts)));
           // bottom-height
           if (json.size() >= 6) {
             data.push_back(json[5].get<float>() * METER_LENGTH);
           }
           return std::make_unique<ResFloat>(name, data, -1000000000.0000);
         }},
        {"symbol",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<std::string> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<std::string>());
           }
           return std::make_unique<ResSymbol>(name, data, -1000000000.0000);
         }},
        {"type",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<std::string> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<std::string>());
           }
           return std::make_unique<ResType>(name, data, -1000000000.0000);
         }},
        {"string",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<std::string> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<std::string>());
           }
           return std::make_unique<ResString>(name, data, -1000000000.0000);
         }},
        // vectors
        {"vector",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<math::Vector4f> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(vector_from_json(json[i]));
           }
           return std::make_unique<ResVector>(name, data, -1000000000.0000);
         }},
        {"vector4m",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<math::Vector4f> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(vectorm4_from_json(json[i]));
           }
           return std::make_unique<ResVector>(name, data, -1000000000.0000);
         }},
        {"vector3m",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<math::Vector4f> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(vectorm3_from_json(json[i]));
           }
           return std::make_unique<ResVector>(name, data, -1000000000.0000);
         }},
        {"movie-pos",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<math::Vector4f> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(movie_pos_from_json(json[i]));
           }
           return std::make_unique<ResVector>(name, data, -1000000000.0000);
         }},
        {"vector-vol",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<math::Vector4f> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(vector_vol_from_json(json[i]));
           }
           return std::make_unique<ResVector>(name, data, -1000000000.0000);
         }},
        // floats
        {"float",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<float> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<float>());
           }
           return std::make_unique<ResFloat>(name, data, -1000000000.0000);
         }},
        {"meters",
         [](const std::string& name,
            const nlohmann::json& json,
            decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<float> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<float>() * METER_LENGTH);
           }
           return std::make_unique<ResFloat>(name, data, -1000000000.0000);
         }},
        {"degrees", [](const std::string& name,
                       const nlohmann::json& json,
                       decompiler::DecompilerTypeSystem& dts) {
           (void)dts;
           std::vector<float> data;
           for (size_t i = 1; i < json.size(); i++) {
             data.push_back(json[i].get<float>() * DEGREES_LENGTH);
           }
           return std::make_unique<ResFloat>(name, data, -1000000000.0000);
         }}};

std::unique_ptr<Res> res_from_json_array(const std::string& name,
                                         const nlohmann::json& json_array,
                                         decompiler::DecompilerTypeSystem& dts) {
  if (json_array.empty()) {
    throw std::runtime_error(fmt::format("json for {} lump was empty", name));
  }
  auto& lump = json_array[0];
  if (lump.type() != nlohmann::detail::value_t::string) {
    throw std::runtime_error(
        fmt::format("first entry of lump \"{}\" has json type {}, but should be string", name,
                    lump.type_name()));
  }
  auto array_type = lump.get<std::string>();
  if (lump_map.find(array_type) != lump_map.end()) {
    return lump_map[array_type](name, json_array, dts);
  } else {
    throw std::runtime_error(
        fmt::format("unsupported array type for lump {}: {}\n", name, array_type));
  }
}