#ifndef OVERLORD_H_
#define OVERLORD_H_

#include <cstddef>

namespace jak3 {
enum class EIsoStatus { Unk };

int start_overlord_wrapper(int argc, const char* const* argv, bool* signal);
void Panic();
char* strncpyz(char* dst, const char* src, size_t sz);

void InitSound();

int VBlank_Initialize();
}  // namespace jak3

#endif
