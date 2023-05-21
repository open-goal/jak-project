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

void LSPRequester::send_progress_create_request(const std::string& token,
                                                const std::string& title) {
  LSPSpec::WorkDoneProgressCreateParams params;
  params.token = token;
  send_request(params, "window/workDoneProgress/create");
  LSPSpec::ProgressPayloadBegin beginPayload;
  beginPayload.title = title;
  LSPSpec::ProgressParamsBegin beginParams;
  beginParams.token = token;
  beginParams.value = beginPayload;
  send_notification(beginParams, "$/progress");
}

void LSPRequester::send_progress_update_request(const std::string& token,
                                                const std::string& message) {
  LSPSpec::ProgressPayloadReport reportPayload;
  reportPayload.message = message;
  LSPSpec::ProgressParamsReport reportParams;
  reportParams.token = token;
  reportParams.value = reportPayload;
  send_notification(reportParams, "$/progress");
}

void LSPRequester::send_progress_finish_request(const std::string& token,
                                                const std::string& message) {
  LSPSpec::ProgressPayloadEnd endPayload;
  endPayload.message = message;
  LSPSpec::ProgressParamsEnd endParams;
  endParams.token = token;
  endParams.value = endPayload;
  send_notification(endParams, "$/progress");
}
