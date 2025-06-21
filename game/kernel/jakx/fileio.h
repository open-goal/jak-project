#pragma once

namespace jakx {
char* MakeFileName(int type, const char* name, int new_string);
char* DecodeFileName(const char* name);
}  // namespace jakx