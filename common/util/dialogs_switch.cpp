#include "dialogs.h"

#include "common/log/log.h"

namespace dialogs {
// No native OS dialog on Switch (see dialogs.h) -- log the error instead of dropping it
// silently, since these are real critical-error reports (e.g. "couldn't create a GL context").
void create_error_message_dialog(const std::string& title, const std::string& message) {
  lg::error("[{}] {}", title, message);
}
}  // namespace dialogs
