#pragma once

#include "common_types.h"

namespace LSPSpec {
struct DidOpenTextDocumentParams {
  TextDocumentItem m_textDocument;
};

void to_json(json& j, const DidOpenTextDocumentParams& obj);
void from_json(const json& j, DidOpenTextDocumentParams& obj);

struct TextDocumentContentChangeEvent {
  std::string m_text;
};

void to_json(json& j, const TextDocumentContentChangeEvent& obj);
void from_json(const json& j, TextDocumentContentChangeEvent& obj);

struct DidChangeTextDocumentParams {
  VersionedTextDocumentIdentifier m_textDocument;
  std::vector<TextDocumentContentChangeEvent> m_contentChanges;
};

void to_json(json& j, const DidChangeTextDocumentParams& obj);
void from_json(const json& j, DidChangeTextDocumentParams& obj);

struct DidCloseTextDocumentParams {
  TextDocumentIdentifier m_textDocument;
};

void to_json(json& j, const DidCloseTextDocumentParams& obj);
void from_json(const json& j, DidCloseTextDocumentParams& obj);

}  // namespace LSPSpec
