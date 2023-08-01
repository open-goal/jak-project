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

/// @brief Additional details for a completion item label.
struct CompletionItemLabelDetails {
  /// An optional string which is rendered less prominently directly after {@link
  /// CompletionItem.label label}, without any spacing. Should be used for function signatures or
  /// type annotations.
  std::optional<std::string> detail;
  /// An optional string which is rendered less prominently after {@link
  /// CompletionItemLabelDetails.detail}. Should be used for fully qualified names or file path.
  std::optional<std::string> description;
};

/// @brief The kind of a completion entry.
enum class CompletionItemKind {
  Text = 1,
  Method = 2,
  Function = 3,
  Constructor = 4,
  Field = 5,
  Variable = 6,
  Class = 7,
  Interface = 8,
  Module = 9,
  Property = 10,
  Unit = 11,
  Value = 12,
  Enum = 13,
  Keyword = 14,
  Snippet = 15,
  Color = 16,
  File = 17,
  Reference = 18,
  Folder = 19,
  EnumMember = 20,
  Constant = 21,
  Struct = 22,
  Event = 23,
  Operator = 24,
  TypeParameter = 25
};

/// Completion item tags are extra annotations that tweak the rendering of a completion item.
enum class CompletionItemTag {
  /// Render a completion as obsolete, usually using a strike-out.
  Deprecated = 1
};

struct CompletionItem {
  /// The label of this completion item.
  ///
  /// The label property is also by default the text that is inserted when selecting this
  /// completion.
  ///
  /// If label details are provided the label itself should be an unqualified name of the completion
  /// item.
  std::string label;
  /// Additional details for the label
  std::optional<CompletionItemLabelDetails> labelDetails;
  /// The kind of this completion item. Based of the kind an icon is chosen by the editor. The
  /// standardized set of available values is defined in `CompletionItemKind`.
  std::optional<CompletionItemKind> kind;
  /// Tags for this completion item.
  std::optional<std::vector<CompletionItemTag>> tags;
  /// A human-readable string with additional information about this item, like type or symbol
  /// information.
  std::optional<std::string> detail;
  /// A human-readable string that represents a doc-comment.
  std::optional<std::string> documentation;
  // NOTE - skipped deprecated
  /// Select this item when showing.
  ///
  /// *Note* that only one completion item can be selected and that the tool / client decides which
  /// item that is. The rule is that the *first* item of those that match best is selected.
  std::optional<bool> preselect;
  /// A string that should be used when comparing this item with other items. When omitted the label
  /// is used as the sort text for this item.
  std::optional<std::string> sortText;
  /// A string that should be used when filtering a set of completion items. When omitted the label
  /// is used as the  filter text for this item.
  std::optional<std::string> filterText;
  // TODO - a lot of other fields...
};

struct CompletionList {
  /// This list is not complete. Further typing should result in recomputing this list.
  ///
  /// Recomputed lists have all their items replaced (not appended) in the incomplete completion
  /// sessions.
  bool m_isIncomplete;
  /// The completion items.
  std::vector<CompletionItem> m_items;
};

void to_json(json& j, const CompletionList& obj);
void from_json(const json& j, CompletionList& obj);

}  // namespace LSPSpec
