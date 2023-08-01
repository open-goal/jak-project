#pragma once

#include "common_types.h"

namespace LSPSpec {
struct WorkDoneProgressCreateParams {
  // The token to be used to report progress. (can also be an integer)
  std::string token;
};

void to_json(json& j, const WorkDoneProgressCreateParams& obj);
void from_json(const json& j, WorkDoneProgressCreateParams& obj);

struct ProgressPayloadBegin {
  std::string kind = "begin";
  // Mandatory title of the progress operation. Used to briefly inform about
  // the kind of operation being performed.
  //
  // Examples: "Indexing" or "Linking dependencies".
  std::string title;
  // Controls if a cancel button should show to allow the user to cancel the
  // long running operation. Clients that don't support cancellation are
  // allowed to ignore the setting.
  bool cancellable = false;
  // Optional, more detailed associated progress message. Contains
  // complementary information to the `title`.
  //
  // Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
  // If unset, the previous progress message (if any) is still valid.
  std::optional<std::string> message;
  // Optional progress percentage to display (value 100 is considered 100%).
  // If not provided infinite progress is assumed and clients are allowed
  // to ignore the `percentage` value in subsequent in report notifications.
  //
  // The value should be steadily rising. Clients are free to ignore values
  // that are not following this rule. The value range is [0, 100]
  std::optional<uint32_t> percentage;
};
void to_json(json& j, const ProgressPayloadBegin& obj);
void from_json(const json& j, ProgressPayloadBegin& obj);

struct ProgressParamsBegin {
  // The progress token provided by the client or server.
  std::string token;
  // Payload
  ProgressPayloadBegin value;
};

void to_json(json& j, const ProgressParamsBegin& obj);
void from_json(const json& j, ProgressParamsBegin& obj);

struct ProgressPayloadReport {
  std::string kind = "report";
  // Controls enablement state of a cancel button. This property is only valid
  // if a cancel button got requested in the `WorkDoneProgressBegin` payload.
  //
  // Clients that don't support cancellation or don't support control the
  // button's enablement state are allowed to ignore the setting.
  bool cancellable = false;
  // Optional, more detailed associated progress message. Contains
  // complementary information to the `title`.
  //
  // Examples: "3/25 files", "project/src/module2", "node_modules/some_dep".
  // If unset, the previous progress message (if any) is still valid.
  std::optional<std::string> message;
  // Optional progress percentage to display (value 100 is considered 100%).
  // If not provided infinite progress is assumed and clients are allowed
  // to ignore the `percentage` value in subsequent in report notifications.
  //
  // The value should be steadily rising. Clients are free to ignore values
  // that are not following this rule. The value range is [0, 100]
  std::optional<uint32_t> percentage;
};
void to_json(json& j, const ProgressPayloadReport& obj);
void from_json(const json& j, ProgressPayloadReport& obj);

struct ProgressParamsReport {
  // The progress token provided by the client or server.
  std::string token;
  // Payload
  ProgressPayloadReport value;
};

void to_json(json& j, const ProgressParamsReport& obj);
void from_json(const json& j, ProgressParamsReport& obj);

struct ProgressPayloadEnd {
  std::string kind = "end";
  // Optional, a final message indicating to for example indicate the outcome
  // of the operation.
  std::optional<std::string> message;
};
void to_json(json& j, const ProgressPayloadEnd& obj);
void from_json(const json& j, ProgressPayloadEnd& obj);

struct ProgressParamsEnd {
  // The progress token provided by the client or server.
  std::string token;
  // Payload
  ProgressPayloadEnd value;
};

void to_json(json& j, const ProgressParamsEnd& obj);
void from_json(const json& j, ProgressParamsEnd& obj);
}  // namespace LSPSpec
