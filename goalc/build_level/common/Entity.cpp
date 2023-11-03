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

std::unique_ptr<Res> res_from_json_array(const std::string& name,
                                         const nlohmann::json& json_array) {
  ASSERT(json_array.size() > 0);
  std::string array_type = json_array[0].get<std::string>();
  if (array_type == "int32") {
    std::vector<s32> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(json_array[i].get<int>());
    }
    return std::make_unique<ResInt32>(name, data, -1000000000.0000);
  } else if (array_type == "uint32") {
    std::vector<u32> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(json_array[i].get<u32>());
    }
    return std::make_unique<ResUint32>(name, data, -1000000000.0000);
  } else if (array_type == "vector") {
    std::vector<math::Vector4f> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(vector_from_json(json_array[i]));
    }
    return std::make_unique<ResVector>(name, data, -1000000000.0000);
  } else if (array_type == "vector4m") {
    std::vector<math::Vector4f> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(vectorm4_from_json(json_array[i]));
    }
    return std::make_unique<ResVector>(name, data, -1000000000.0000);
  } else if (array_type == "movie-pos") {
    std::vector<math::Vector4f> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(movie_pos_from_json(json_array[i]));
    }
    return std::make_unique<ResVector>(name, data, -1000000000.0000);
  } else if (array_type == "float") {
    std::vector<float> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(json_array[i].get<float>());
    }
    return std::make_unique<ResFloat>(name, data, -1000000000.0000);
  } else if (array_type == "meters") {
    std::vector<float> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(json_array[i].get<float>() * METER_LENGTH);
    }
    return std::make_unique<ResFloat>(name, data, -1000000000.0000);
  } else if (array_type == "degrees") {
    std::vector<float> data;
    for (size_t i = 1; i < json_array.size(); i++) {
      data.push_back(json_array[i].get<float>() * DEGREES_LENGTH);
    }
    return std::make_unique<ResFloat>(name, data, -1000000000.0000);
  } else {
    ASSERT_MSG(false, fmt::format("unsupported array type: {}\n", array_type));
  }
}