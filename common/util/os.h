#pragma once

#include <cstddef>
#include <string>

// Note: these are not implemented on windows and will return zero.
size_t get_peak_rss();
void setup_cpu_info();

struct CpuInfo {
  bool initialized = false;
  bool has_avx = false;
  bool has_avx2 = false;

  std::string brand;
  std::string model;
};

CpuInfo& get_cpu_info();
