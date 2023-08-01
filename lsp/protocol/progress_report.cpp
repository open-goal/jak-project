#include "progress_report.h"

void LSPSpec::to_json(json& j, const WorkDoneProgressCreateParams& obj) {
  json_serialize(token);
}

void LSPSpec::from_json(const json& j, WorkDoneProgressCreateParams& obj) {
  json_deserialize_if_exists(token);
}

void LSPSpec::to_json(json& j, const ProgressPayloadBegin& obj) {
  json_serialize(kind);
  json_serialize(title);
  json_serialize(cancellable);
  json_serialize_optional(message);
  json_serialize_optional(percentage);
}

void LSPSpec::from_json(const json& j, ProgressPayloadBegin& obj) {
  json_deserialize_if_exists(kind);
  json_deserialize_if_exists(title);
  json_deserialize_if_exists(cancellable);
  json_deserialize_optional_if_exists(message);
  json_deserialize_optional_if_exists(percentage);
}

void LSPSpec::to_json(json& j, const ProgressParamsBegin& obj) {
  json_serialize(token);
  json_serialize(value);
}

void LSPSpec::from_json(const json& j, ProgressParamsBegin& obj) {
  json_deserialize_if_exists(token);
  json_deserialize_if_exists(value);
}

void LSPSpec::to_json(json& j, const ProgressPayloadReport& obj) {
  json_serialize(kind);
  json_serialize(cancellable);
  json_serialize_optional(message);
  json_serialize_optional(percentage);
}

void LSPSpec::from_json(const json& j, ProgressPayloadReport& obj) {
  json_deserialize_if_exists(kind);
  json_deserialize_if_exists(cancellable);
  json_deserialize_optional_if_exists(message);
  json_deserialize_optional_if_exists(percentage);
}

void LSPSpec::to_json(json& j, const ProgressParamsReport& obj) {
  json_serialize(token);
  json_serialize(value);
}

void LSPSpec::from_json(const json& j, ProgressParamsReport& obj) {
  json_deserialize_if_exists(token);
  json_deserialize_if_exists(value);
}

void LSPSpec::to_json(json& j, const ProgressPayloadEnd& obj) {
  json_serialize(kind);
  json_serialize_optional(message);
}

void LSPSpec::from_json(const json& j, ProgressPayloadEnd& obj) {
  json_deserialize_if_exists(kind);
  json_deserialize_optional_if_exists(message);
}

void LSPSpec::to_json(json& j, const ProgressParamsEnd& obj) {
  json_serialize(token);
  json_serialize(value);
}

void LSPSpec::from_json(const json& j, ProgressParamsEnd& obj) {
  json_deserialize_if_exists(token);
  json_deserialize_if_exists(value);
}
