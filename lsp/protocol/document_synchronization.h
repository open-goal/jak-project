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

enum class TextDocumentSaveReason {
  // Manually triggered, e.g. by the user pressing save, by starting debugging, or by an API call.
  Manual = 1,
  // Automatic after a delay.
  AfterDelay = 2,
  // When the editor lost focus.
  FocusOut = 3,
};

// The parameters send in a will save text document notification.
struct WillSaveTextDocumentParams {
  // The document that will be saved.
  TextDocumentIdentifier textDocument;
  // The 'TextDocumentSaveReason'.
  TextDocumentSaveReason reason;
};

void to_json(json& j, const WillSaveTextDocumentParams& obj);
void from_json(const json& j, WillSaveTextDocumentParams& obj);

}  // namespace LSPSpec
