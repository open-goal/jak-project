#include "document_synchronization.h"

void LSPSpec::to_json(json& j, const DidOpenTextDocumentParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}};
}

void LSPSpec::from_json(const json& j, DidOpenTextDocumentParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
}

void LSPSpec::to_json(json& j, const TextDocumentContentChangeEvent& obj) {
  j = json{{"text", obj.m_text}};
}

void LSPSpec::from_json(const json& j, TextDocumentContentChangeEvent& obj) {
  j.at("text").get_to(obj.m_text);
}

void LSPSpec::to_json(json& j, const DidChangeTextDocumentParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}, {"contentChanges", obj.m_contentChanges}};
}

void LSPSpec::from_json(const json& j, DidChangeTextDocumentParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
  j.at("contentChanges").get_to(obj.m_contentChanges);
}

void LSPSpec::to_json(json& j, const DidCloseTextDocumentParams& obj) {
  j = json{{"textDocument", obj.m_textDocument}};
}

void LSPSpec::from_json(const json& j, DidCloseTextDocumentParams& obj) {
  j.at("textDocument").get_to(obj.m_textDocument);
}
