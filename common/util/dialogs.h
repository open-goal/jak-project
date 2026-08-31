#pragma once

#include <string>

#if !defined(__SWITCH__)
#include "third-party/libtinyfiledialogs/tinyfiledialogs.h"
#endif

namespace dialogs {
void create_error_message_dialog(const std::string& title, const std::string& message);
}
