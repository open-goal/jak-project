#include "lsp_requester.h"

#include <iostream>

#include "common/log/log.h"
#include "common/util/string_util.h"

#include "lsp/protocol/progress_report.h"

void LSPRequester::send_request(const json& params, const std::string& method) {
  json req;
  req["id"] = str_util::uuid();
  req["method"] = method;
  req["params"] = params;
  req["jsonrpc"] = "2.0";

  std::string request;
  request.append("Content-Length: " + std::to_string(req.dump().size()) + "\r\n");
  request.append("Content-Type: application/vscode-jsonrpc;charset=utf-8\r\n");
  request.append("\r\n");
  request += req.dump();

  // Send requests immediately, as they may be done during the handling of a client request
  lg::info("Sending Request {}", method);
  std::cout << request.c_str() << std::flush;
}

void LSPRequester::send_notification(const json& params, const std::string& method) {
  json notification;
  notification["method"] = method;
  notification["params"] = params;
  notification["jsonrpc"] = "2.0";

  std::string request;
  request.append("Content-Length: " + std::to_string(notification.dump().size()) + "\r\n");
  request.append("Content-Type: application/vscode-jsonrpc;charset=utf-8\r\n");
  request.append("\r\n");
  request += notification.dump();

  // Send requests immediately, as they may be done during the handling of a client request
  lg::info("Sending Notification {}", method);
  std::cout << request.c_str() << std::flush;
}

void LSPRequester::send_progress_create_request(const std::string& title,
                                                const std::string& message,
                                                const int percentage) {
  const std::string token = fmt::format("opengoal/{}", title);
  LSPSpec::WorkDoneProgressCreateParams createRequest;
  createRequest.token = token;
  send_request(createRequest, "window/workDoneProgress/create");
  LSPSpec::WorkDoneProgressBegin beginPayload;
  beginPayload.title = title;
  beginPayload.cancellable = false;  // TODO - maybe one day
  beginPayload.message = message;
  if (percentage > 0) {
    beginPayload.percentage = percentage;
  }
  LSPSpec::ProgressNotificationPayload notification;
  notification.token = token;
  notification.beginValue = beginPayload;
  send_notification(notification, "$/progress");
}

void LSPRequester::send_progress_update_request(const std::string& title,
                                                const std::string& message,
                                                const int percentage) {
  const std::string token = fmt::format("opengoal/{}", title);
  LSPSpec::WorkDoneProgressReport reportPayload;
  reportPayload.cancellable = false;  // TODO - maybe one day
  reportPayload.message = message;
  if (percentage > 0) {
    reportPayload.percentage = percentage;
  }
  LSPSpec::ProgressNotificationPayload notification;
  notification.token = token;
  notification.reportValue = reportPayload;
  send_notification(notification, "$/progress");
}

void LSPRequester::send_progress_finish_request(const std::string& title,
                                                const std::string& message) {
  const std::string token = fmt::format("opengoal/{}", title);
  LSPSpec::WorkDoneProgressEnd endPayload;
  endPayload.message = message;
  LSPSpec::ProgressNotificationPayload notification;
  notification.token = token;
  notification.endValue = endPayload;
  send_notification(notification, "$/progress");
}
