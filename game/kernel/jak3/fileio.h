#pragma once

namespace jak3 {
char* MakeFileName(int type, const char* name, int new_string);
char* DecodeFileName(const char* name);
}  // namespace jak3