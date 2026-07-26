#include "os.h"

#include <fstream>

#include "common/common_types.h"
#include "common/log/log.h"
#include "common/util/string_util.h"

#ifdef __APPLE__
#include <stdio.h>

#include <sys/sysctl.h>
#include <sys/types.h>
#endif

#ifdef _WIN32
// clang-format off
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>
#include <psapi.h>
// clang-format on
size_t get_peak_rss() {
  HANDLE hProcess = GetCurrentProcess();
  PROCESS_MEMORY_COUNTERS pmc;
  if (GetProcessMemoryInfo(hProcess, &pmc, sizeof(pmc))) {
    return pmc.PeakWorkingSetSize;
  } else {
    return 0;
  }
}
#else
#include <sys/resource.h>
size_t get_peak_rss() {
  rusage x;
  getrusage(RUSAGE_SELF, &x);
  return x.ru_maxrss * 1024;
}
#endif

#ifdef _WIN32
// windows has a __cpuid
#include <intrin.h>
#elif __x86_64__
// using int to be compatible with msvc's intrinsic
void __cpuidex(int result[4], int eax, int ecx) {
  asm("cpuid\n\t"
      : "=a"(result[0]), "=b"(result[1]), "=c"(result[2]), "=d"(result[3])
      : "0"(eax), "2"(ecx));
}
#endif

void setup_cpu_info_windows(CpuInfo& info) {
#if defined(_M_X64) || defined(_M_IX86)
  // Vendor
  {
    int result[4];
    __cpuidex(result, 0, 0);

    for (int r : {1, 3, 2}) {
      int reg = result[r];
      for (int i = 0; i < 4; i++) {
        info.vendor.push_back(reg & 0xff);
        reg >>= 8;
      }
    }
  }
  // Brand string
  for (int leaf = 0x80000002; leaf <= 0x80000004; leaf++) {
    int result[4];
    __cpuidex(result, leaf, 0);

    for (int reg : result) {
      for (int i = 0; i < 4; i++) {
        info.model.push_back(reg & 0xff);
        reg >>= 8;
      }
    }
  }
  {
    int result[4];
    __cpuidex(result, 1, 0);
    info.has_avx = result[2] & (1 << 28);
  }
  {
    int result[4];
    __cpuidex(result, 7, 0);
    info.has_avx2 = result[1] & (1 << 5);
  }
#elif defined(_M_ARM64)
  info.vendor = "ARM";
  HKEY key;
  if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, "HARDWARE\\DESCRIPTION\\System\\CentralProcessor\\0", 0,
                    KEY_READ, &key) == ERROR_SUCCESS) {
    char buf[256];
    DWORD size = sizeof(buf);
    if (RegQueryValueExA(key, "ProcessorNameString", nullptr, nullptr, reinterpret_cast<BYTE*>(buf),
                         &size) == ERROR_SUCCESS) {
      info.model = buf;
    }
    RegCloseKey(key);
  }
  info.has_neon = IsProcessorFeaturePresent(PF_ARM_NEON_INSTRUCTIONS_AVAILABLE);
#endif
}

void setup_cpu_info_linux(CpuInfo& info) {
  std::ifstream cpuinfo("/proc/cpuinfo");
  std::string line;
  while (std::getline(cpuinfo, line)) {
    auto colon = line.find(':');
    if (colon == std::string::npos) {
      continue;
    }
    std::string key = line.substr(0, colon);
    std::string value = line.substr(colon + 1);
    while (!value.empty() && value.front() == ' ') {
      value.erase(value.begin());
    }
    if (info.brand.empty() && key == "vendor_id") {
      info.brand = value;
    }
    if (info.model.empty() && (key == "model name" || key == "Processor")) {
      info.model = value;
    }
  }
#if defined(__aarch64__)
  info.brand = "ARM";
#ifdef HWCAP_ASIMD
  info.has_neon = getauxval(AT_HWCAP) & HWCAP_ASIMD;
#endif
#endif

#if defined(__x86_64__)
  int result[4];
  __cpuidex(result, 1, 0);
  info.has_avx = result[2] & (1 << 28);
  __cpuidex(result, 7, 0);
  info.has_avx2 = result[1] & (1 << 5);
#endif
}

void setup_cpu_info_macos(CpuInfo& info) {
#if defined(__x86_64__)
  int result[4];
  __cpuidex(result, 0, 0);
  for (int r : {1, 3, 2}) {
    int reg = result[r];
    for (int i = 0; i < 4; i++) {
      info.vendor.push_back(reg & 0xff);
      reg >>= 8;
    }
  }
  for (int leaf = 0x80000002; leaf <= 0x80000004; leaf++) {
    __cpuidex(result, leaf, 0);
    for (int reg : result) {
      for (int i = 0; i < 4; i++) {
        info.model.push_back(reg & 0xff);
        reg >>= 8;
      }
    }
  }
  __cpuidex(result, 1, 0);
  info.has_avx = result[2] & (1 << 28);
  __cpuidex(result, 7, 0);
  info.has_avx2 = result[1] & (1 << 5);
#elif defined(__aarch64__) || defined(__arm64__)
  info.brand = "Apple";
  char buf[128];
  size_t len = sizeof(buf);
  if (sysctlbyname("hw.model", buf, &len, nullptr, 0) == 0) {
    info.model = buf;
  }
  info.has_neon = true;
#endif
}

CpuInfo gCpuInfo;

void setup_cpu_info() {
  if (gCpuInfo.initialized) {
    return;
  }

#if defined(_WIN32)
  setup_cpu_info_windows(gCpuInfo);
#elif defined(__APPLE__)
  setup_cpu_info_macos(gCpuInfo);
#elif defined(__linux__)
  setup_cpu_info_linux(gCpuInfo);
#else
  gCpuInfo.brand = "Unknown Brand";
  gCpuInfo.model = "Unknown Model";
#endif

  printf("-------- CPU Information --------\n");
  printf(" Brand: %s\n", gCpuInfo.brand.c_str());
  printf(" Model: %s\n", gCpuInfo.model.c_str());
  printf(" AVX  : %s\n", gCpuInfo.has_avx ? "true" : "false");
  printf(" AVX2 : %s\n", gCpuInfo.has_avx2 ? "true" : "false");
  printf(" NEON : %s\n", gCpuInfo.has_neon ? "true" : "false");
  fflush(stdout);

  gCpuInfo.initialized = true;
}

CpuInfo& get_cpu_info() {
  return gCpuInfo;
}

std::optional<double> get_macos_major_version() {
#ifndef __APPLE__
  return {};
#else
  char buffer[128];
  size_t bufferlen = 128;
  auto ok = sysctlbyname("kern.osproductversion", &buffer, &bufferlen, NULL, 0);
  if (ok != 0) {
    lg::warn("Unable to check for `kern.osproductversion` to determine macOS version");
    return {};
  }
  try {
    std::string macos_major_version = buffer;
    if (str_util::contains(buffer, ".")) {
      macos_major_version = str_util::split_string(macos_major_version, ".")[0];
    }
    return std::stod(macos_major_version);
  } catch (std::exception& e) {
    lg::error("Error occured when attempting to convert sysctl value {} to number", buffer);
    return {};
  }
#endif
}