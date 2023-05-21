#include "document_color.h"

void LSPSpec::to_json(json& j, const DocumentColorParams& obj) {
  json_serialize(textDocument);
}

void LSPSpec::from_json(const json& j, DocumentColorParams& obj) {
  json_deserialize_if_exists(textDocument);
}

void LSPSpec::to_json(json& j, const ColorInformation& obj) {
  json_serialize(range);
  json_serialize(color);
}

void LSPSpec::from_json(const json& j, ColorInformation& obj) {
  json_deserialize_if_exists(range);
  json_deserialize_if_exists(color);
}
