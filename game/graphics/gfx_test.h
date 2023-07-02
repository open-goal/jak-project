#pragma once

#include <string>

#include "common/util/json_util.h"

namespace tests {
struct GPUTestOutput {
  bool success;
  std::string error;
  std::string errorCause;
};
void to_json(json& j, const GPUTestOutput& obj);

GPUTestOutput run_gpu_test(const std::string& test_type);
}  // namespace tests
