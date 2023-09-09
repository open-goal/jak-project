#pragma once

#include <string>

#include "third-party/libtinyfiledialogs/tinyfiledialogs.h"

namespace dialogs {
void create_error_message_dialog(const std::string& title, const std::string& message);
}
