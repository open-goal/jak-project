#include "dialogs.h"

namespace dialogs {
// char const * const aDialogType , /* "ok" "okcancel" "yesno" "yesnocancel" */
// char const * const aIconType , /* "info" "warning" "error" "question" */
// int const aDefaultButton ) /* 0 for cancel/no , 1 for ok/yes , 2 for no in yesnocancel */
void create_error_message_dialog(const std::string& title, const std::string& message) {
  tinyfd_messageBox(title.data(), message.data(), "ok", "error", 1);
}
}  // namespace dialogs
