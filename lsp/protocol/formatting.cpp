#include "formatting.h"

void LSPSpec::to_json(json& j, const FormattingOptions& obj) {
  json_serialize(tabSize);
  json_serialize(insertSpaces);
  json_serialize_optional(trimTrailingWhitespace);
  json_serialize_optional(insertFinalNewLine);
  json_serialize_optional(trimFinalNewLines);
}

void LSPSpec::from_json(const json& j, FormattingOptions& obj) {
  json_deserialize_if_exists(tabSize);
  json_deserialize_if_exists(insertSpaces);
  json_deserialize_optional_if_exists(trimTrailingWhitespace);
  json_deserialize_optional_if_exists(insertFinalNewLine);
  json_deserialize_optional_if_exists(trimFinalNewLines);
}

void LSPSpec::to_json(json& j, const DocumentFormattingParams& obj) {
  json_serialize(textDocument);
  json_serialize(options);
}

void LSPSpec::from_json(const json& j, DocumentFormattingParams& obj) {
  json_deserialize_if_exists(textDocument);
  json_deserialize_if_exists(options);
}
