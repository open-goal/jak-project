#pragma once

#include "common/goal_constants.h"
#include "common/goos/ParseHelpers.h"
#include "common/goos/Printer.h"
#include "common/util/Assert.h"

#include "decompiler/util/DecompilerTypeSystem.h"
#include "goalc/build_level/common/ResLump.h"
#include "goalc/data_compiler/DataObjectGenerator.h"

#include "third-party/json.hpp"

math::Vector4f vectorm3_from_json(const nlohmann::json& json);
math::Vector4f vectorm4_from_json(const nlohmann::json& json);
math::Vector4f vector_from_json(const nlohmann::json& json);
u64 get_enum_val(const std::string& val, decompiler::DecompilerTypeSystem& dts);
std::unique_ptr<Res> res_from_json_array(const std::string& name,
                                         const nlohmann::json& json_array,
                                         decompiler::DecompilerTypeSystem& dts);