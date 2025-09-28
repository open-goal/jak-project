#include "progress_report.h"

void LSPSpec::to_json(json& j, const WorkDoneProgressCreateParams& obj) {
  json_serialize(token);
}

void LSPSpec::from_json(const json& j, WorkDoneProgressCreateParams& obj) {
  json_deserialize_if_exists(token);
}

void LSPSpec::to_json(json& j, const WorkDoneProgressBegin& obj) {
  json_serialize(kind);
  json_serialize(title);
  json_serialize(cancellable);
  json_serialize_optional(message);
  json_serialize_optional(percentage);
}

void LSPSpec::from_json(const json& j, WorkDoneProgressBegin& obj) {
  json_deserialize_if_exists(kind);
  json_deserialize_if_exists(title);
  json_deserialize_if_exists(cancellable);
  json_deserialize_optional_if_exists(message);
  json_deserialize_optional_if_exists(percentage);
}

void LSPSpec::to_json(json& j, const WorkDoneProgressReport& obj) {
  json_serialize(kind);
  json_serialize(cancellable);
  json_serialize_optional(message);
  json_serialize_optional(percentage);
}

void LSPSpec::from_json(const json& j, WorkDoneProgressReport& obj) {
  json_deserialize_if_exists(kind);
  json_deserialize_if_exists(cancellable);
  json_deserialize_optional_if_exists(message);
  json_deserialize_optional_if_exists(percentage);
}

void LSPSpec::to_json(json& j, const WorkDoneProgressEnd& obj) {
  json_serialize(kind);
  json_serialize_optional(message);
}

void LSPSpec::from_json(const json& j, WorkDoneProgressEnd& obj) {
  json_deserialize_if_exists(kind);
  json_deserialize_optional_if_exists(message);
}

void LSPSpec::to_json(json& j, const ProgressNotificationPayload& obj) {
  json_serialize(token);
  if (obj.beginValue) {
    j["value"] = obj.beginValue.value();
  } else if (obj.reportValue) {
    j["value"] = obj.reportValue.value();
  } else {
    j["value"] = obj.endValue.value();
  }
}

void LSPSpec::from_json(const json& j, ProgressNotificationPayload& obj) {
  json_deserialize_if_exists(token);
  // TODO - not needed, but if so -- deserialize 'value', it's possible to figure out which is the
  // right one
}
