#include "common_types.h"

void LSPSpec::to_json(json& j, const Position& obj) {
  j = json{{"line", obj.m_line}, {"character", obj.m_character}};
}

void LSPSpec::from_json(const json& j, Position& obj) {
  j.at("line").get_to(obj.m_line);
  j.at("character").get_to(obj.m_character);
}

LSPSpec::Range::Range(Position start, Position end) : m_start(start), m_end(end) {}

LSPSpec::Range::Range(uint32_t line, uint32_t character)
    : m_start({line, character}), m_end({line, character}) {}

void LSPSpec::to_json(json& j, const Range& obj) {
  // TODO - not sure if this works yet, but nice if it does!
  j = json{{"start", obj.m_start}, {"end", obj.m_end}};
}

void LSPSpec::from_json(const json& j, Range& obj) {
  obj.m_start = j.at("start").get<Position>();
  obj.m_end = j.at("end").get<Position>();
}

void LSPSpec::to_json(json& j, const TextDocumentItem& obj) {
  j = json{{"uri", obj.m_uri},
           {"languageId", obj.m_languageId},
           {"version", obj.m_version},
           {"text", obj.m_text}};
}

void LSPSpec::from_json(const json& j, TextDocumentItem& obj) {
  j.at("uri").get_to(obj.m_uri);
  j.at("languageId").get_to(obj.m_languageId);
  j.at("version").get_to(obj.m_version);
  j.at("text").get_to(obj.m_text);
}

void LSPSpec::to_json(json& j, const TextDocumentIdentifier& obj) {
  j = json{{"uri", obj.m_uri}};
}

void LSPSpec::from_json(const json& j, TextDocumentIdentifier& obj) {
  j.at("uri").get_to(obj.m_uri);
}

void LSPSpec::to_json(json& j, const VersionedTextDocumentIdentifier& obj) {
  j = json{{"uri", obj.m_uri}, {"version", obj.m_version}};
}

void LSPSpec::from_json(const json& j, VersionedTextDocumentIdentifier& obj) {
  j.at("uri").get_to(obj.m_uri);
  j.at("version").get_to(obj.m_version);
}

void LSPSpec::to_json(json& j, const Location& obj) {
  j = json{{"uri", obj.m_uri}, {"range", obj.m_range}};
}

void LSPSpec::from_json(const json& j, Location& obj) {
  j.at("uri").get_to(obj.m_uri);
  j.at("range").get_to(obj.m_range);
}

void LSPSpec::to_json(json& j, const TextDocumentPositionParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}, {"position", obj.m_position}};
}

void LSPSpec::from_json(const json& j, TextDocumentPositionParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
  j.at("position").get_to(obj.m_position);
}

void LSPSpec::to_json(json& j, const MarkupContent& obj) {
  j = json{{"kind", obj.m_kind}, {"value", obj.m_value}};
}

void LSPSpec::from_json(const json& j, MarkupContent& obj) {
  j.at("kind").get_to(obj.m_kind);
  j.at("value").get_to(obj.m_value);
}

void LSPSpec::to_json(json& j, const Color& obj) {
  json_serialize(red);
  json_serialize(green);
  json_serialize(blue);
  json_serialize(alpha);
}

void LSPSpec::from_json(const json& j, Color& obj) {
  json_deserialize_if_exists(red);
  json_deserialize_if_exists(green);
  json_deserialize_if_exists(blue);
  json_deserialize_if_exists(alpha);
}

void LSPSpec::to_json(json& j, const TextEdit& obj) {
  json_serialize(range);
  json_serialize(newText);
}

void LSPSpec::from_json(const json& j, TextEdit& obj) {
  json_deserialize_if_exists(range);
  json_deserialize_if_exists(newText);
}
