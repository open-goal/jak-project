#include "os.h"

#include "common/common_types.h"
#include "common/log/log.h"

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
#else
// for now, just return 0's.
void __cpuidex(int result[4], int eax, int ecx) {
  lg::warn("cpuid not implemented on this platform");
  for (int i = 0; i < 4; i++) {
    result[i] = 0;
  }
}
#endif

CpuInfo gCpuInfo;

void setup_cpu_info() {
  if (gCpuInfo.initialized) {
    return;
  }

  // as a test, get the brand and model
  for (u32 i = 0x80000002; i <= 0x80000004; i++) {
    int result[4];
    __cpuidex(result, i, 0);
    for (auto reg : result) {
      for (int c = 0; c < 4; c++) {
        gCpuInfo.model.push_back(reg);
        reg >>= 8;
      }
    }
  }

  {
    int result[4];
    __cpuidex(result, 0, 0);
    for (auto r : {1, 3, 2}) {
      for (int c = 0; c < 4; c++) {
        gCpuInfo.brand.push_back(result[r]);
        result[r] >>= 8;
      }
    }
  }

  // check for AVX2
  {
    int result[4];
    __cpuidex(result, 7, 0);
    gCpuInfo.has_avx2 = result[1] & (1 << 5);
  }

  {
    int result[4];
    __cpuidex(result, 1, 0);
    gCpuInfo.has_avx = result[2] & (1 << 28);
  }

  printf("-------- CPU Information --------\n");
  printf(" Brand: %s\n", gCpuInfo.brand.c_str());
  printf(" Model: %s\n", gCpuInfo.model.c_str());
  printf(" AVX  : %s\n", gCpuInfo.has_avx ? "true" : "false");
  printf(" AVX2 : %s\n", gCpuInfo.has_avx2 ? "true" : "false");
  fflush(stdout);

  gCpuInfo.initialized = true;
}

CpuInfo& get_cpu_info() {
  return gCpuInfo;
}

std::optional<double> get_macos_version() {
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
    return std::stod(buffer);
  } catch (std::exception& e) {
    lg::error("Error occured when attempting to convert sysctl value {} to number", buffer);
    return {};
  }
#endif
}