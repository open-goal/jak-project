#pragma once

#include "common_types.h"

// TODO - not fully implemented!

namespace LSPSpec {

/// @brief How a completion was triggered
enum class CompletionTriggerKind {
  /// Completion was triggered by typing an identifier (24x7 code complete), manual invocation (e.g
  /// Ctrl+Space) or via API.
  Invoked = 1,
  /// Completion was triggered by a trigger character specified by the `triggerCharacters`
  /// properties of the `CompletionRegistrationOptions`.
  TriggerCharacter = 2,
  /// Completion was re-triggered as the current completion list is incomplete.
  TriggerForIncompleteCompletions = 3,
};

// TODO - look into inheriting structs?
struct CompletionParams {
  /// @brief The text document.
  TextDocumentIdentifier m_textDocument;
  /// @brief The position inside the text document.
  Position m_position;
};

void to_json(json& j, const CompletionParams& obj);
void from_json(const json& j, CompletionParams& obj);

}  // namespace LSPSpec
