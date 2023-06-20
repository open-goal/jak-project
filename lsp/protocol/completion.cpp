#include "completion.h"

void LSPSpec::to_json(json& j, const CompletionParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}, {"position", obj.m_position}};
}

void LSPSpec::from_json(const json& j, CompletionParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
  j.at("position").get_to(obj.m_position);
}

void LSPSpec::to_json(json& j, const CompletionList& obj) {}

void LSPSpec::from_json(const json& j, CompletionList& obj) {}
