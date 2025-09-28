#include "type_hierarchy.h"

#include "common/util/json_util.h"

// TODO - there's gotta be a way to share json serialization/deserialization
// figure it out _soon_
void LSPSpec::to_json(json& j, const TypeHierarchyPrepareParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}, {"position", obj.m_position}};
}

void LSPSpec::from_json(const json& j, TypeHierarchyPrepareParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
  j.at("position").get_to(obj.m_position);
}

void LSPSpec::to_json(json& j, const TypeHierarchyItem& obj) {
  json_serialize(name);
  json_serialize(kind);
  json_serialize_optional(tags);
  json_serialize_optional(detail);
  json_serialize(uri);
  json_serialize(range);
  json_serialize(selectionRange);
}

void LSPSpec::from_json(const json& j, TypeHierarchyItem& obj) {
  json_deserialize_if_exists(name);
  json_deserialize_if_exists(kind);
  json_deserialize_optional_if_exists(tags);
  json_deserialize_optional_if_exists(detail);
  json_deserialize_if_exists(uri);
  json_deserialize_if_exists(range);
  json_deserialize_if_exists(selectionRange);
}

void LSPSpec::to_json(json& j, const TypeHierarchySupertypesParams& obj) {
  json_serialize(item);
}

void LSPSpec::from_json(const json& j, TypeHierarchySupertypesParams& obj) {
  json_deserialize_if_exists(item);
}

void LSPSpec::to_json(json& j, const TypeHierarchySubtypesParams& obj) {
  json_serialize(item);
}

void LSPSpec::from_json(const json& j, TypeHierarchySubtypesParams& obj) {
  json_deserialize_if_exists(item);
}
