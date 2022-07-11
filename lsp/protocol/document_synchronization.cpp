#include "document_synchronization.h"

void LSPSpec::to_json(json& j, const DidOpenTextDocumentParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}};
}

void LSPSpec::from_json(const json& j, DidOpenTextDocumentParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
}
