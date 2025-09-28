#include "completion.h"

void LSPSpec::to_json(json& j, const CompletionParams& obj) {
  json_serialize(textDocument);
  json_serialize(position);
}

void LSPSpec::from_json(const json& j, CompletionParams& obj) {
  json_deserialize_if_exists(textDocument);
  json_deserialize_if_exists(position);
}

void LSPSpec::to_json(json& j, const CompletionItemLabelDetails& obj) {
  json_serialize_optional(detail);
  json_serialize_optional(description);
}

void LSPSpec::from_json(const json& j, CompletionItemLabelDetails& obj) {
  json_deserialize_optional_if_exists(detail);
  json_deserialize_optional_if_exists(description);
}

void LSPSpec::to_json(json& j, const CompletionItem& obj) {
  json_serialize(label);
  json_serialize_optional(labelDetails);
  json_serialize_optional(kind);
  json_serialize_optional(tags);
  json_serialize_optional(detail);
  json_serialize_optional(documentation);
  json_serialize_optional(preselect);
  json_serialize_optional(sortText);
  json_serialize_optional(filterText);
  json_serialize_optional(insertText);
  json_serialize_optional(textEdit);
  json_serialize_optional(textEditText);
  json_serialize_optional(additionalTextEdits);
  json_serialize_optional(commitCharacters);
}

void LSPSpec::from_json(const json& j, CompletionItem& obj) {
  json_deserialize_if_exists(label);
  json_deserialize_optional_if_exists(labelDetails);
  json_deserialize_optional_if_exists(kind);
  json_deserialize_optional_if_exists(tags);
  json_deserialize_optional_if_exists(detail);
  json_deserialize_optional_if_exists(documentation);
  json_deserialize_optional_if_exists(preselect);
  json_deserialize_optional_if_exists(sortText);
  json_deserialize_optional_if_exists(filterText);
  json_deserialize_optional_if_exists(insertText);
  json_deserialize_optional_if_exists(textEdit);
  json_deserialize_optional_if_exists(textEditText);
  json_deserialize_optional_if_exists(additionalTextEdits);
  json_deserialize_optional_if_exists(commitCharacters);
}

void LSPSpec::to_json(json& j, const CompletionList& obj) {
  json_serialize(isIncomplete);
  json_serialize(items);
}

void LSPSpec::from_json(const json& j, CompletionList& obj) {
  json_deserialize_if_exists(isIncomplete);
  json_deserialize_if_exists(items);
}
