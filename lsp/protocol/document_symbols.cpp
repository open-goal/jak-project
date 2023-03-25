#include "document_symbols.h"

void LSPSpec::to_json(json& j, const DocumentSymbol& obj) {
  j = json{{"name", obj.m_name},
           {"kind", obj.m_kind},
           {"range", obj.m_range},
           {"selectionRange", obj.m_selectionRange}};
  if (obj.m_detail) {
    j["detail"] = obj.m_detail.value();
  }
  if (obj.m_tags) {
    j["tags"] = obj.m_tags.value();
  }
  if (obj.m_children) {
    j["children"] = obj.m_children.value();
  }
}

void LSPSpec::from_json(const json& j, DocumentSymbol& obj) {
  j.at("name").get_to(obj.m_name);
  j.at("kind").get_to(obj.m_kind);
  j.at("range").get_to(obj.m_range);
  j.at("selectionRange").get_to(obj.m_selectionRange);
  if (j.contains("detail")) {
    obj.m_detail = std::make_optional(j.at("detail").get<std::string>());
  }
  if (j.contains("tags")) {
    obj.m_tags = std::make_optional(j.at("tags").get<std::vector<SymbolTag>>());
  }
  if (j.contains("children")) {
    obj.m_children = std::make_optional(j.at("children").get<std::vector<DocumentSymbol>>());
  }
}

void LSPSpec::to_json(json& j, const DocumentSymbolParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}};
}

void LSPSpec::from_json(const json& j, DocumentSymbolParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
}
