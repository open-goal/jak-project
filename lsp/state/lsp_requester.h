#pragma once

#include <optional>
#include <string>

#include "common/util/json_util.h"

#include "lsp/transport/stdio.h"

class LSPRequester {
 public:
  void send_progress_create_request(const std::string& title,
                                    const std::string& message,
                                    const int percentage);
  void send_progress_update_request(const std::string& title,
                                    const std::string& message,
                                    const int percentage);
  void send_progress_finish_request(const std::string& title, const std::string& message);

 private:
  void send_request(const json& payload, const std::string& method);
  void send_notification(const json& payload, const std::string& method);
};
