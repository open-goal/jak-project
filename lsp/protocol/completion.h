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
  /// The text document.
  TextDocumentIdentifier textDocument;
  /// The position inside the text document.
  Position position;
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

void to_json(json& j, const CompletionItemLabelDetails& obj);
void from_json(const json& j, CompletionItemLabelDetails& obj);

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
  /// TODO - can also be MarkupContent
  std::optional<std::string> documentation;
  // NOTE - skipped deprecated (because it's deprecated!)
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
  /// A string that should be inserted into a document when selecting
  /// this completion. When omitted the label is used as the insert text
  /// for this item.
  ///
  /// The `insertText` is subject to interpretation by the client side.
  /// Some tools might not take the string literally. For example
  /// VS Code when code complete is requested in this example
  /// `con<cursor position>` and a completion item with an `insertText` of
  /// `console` is provided it will only insert `sole`. Therefore it is
  /// recommended to use `textEdit` instead since it avoids additional client
  /// side interpretation.
  std::optional<std::string> insertText;
  /// The format of the insert text. The format applies to both the
  /// `insertText` property and the `newText` property of a provided
  /// `textEdit`. If omitted defaults to `InsertTextFormat.PlainText`.
  ///
  /// Please note that the insertTextFormat doesn't apply to
  /// `additionalTextEdits`.
  // TODO - std::optional<InsertTextFormat> insertTextFormat;
  /// How whitespace and indentation is handled during completion
  /// item insertion. If not provided the client's default value depends on
  /// the `textDocument.completion.insertTextMode` client capability.
  ///
  /// @since 3.16.0
  /// @since 3.17.0 - support for `textDocument.completion.insertTextMode`
  // TODO - std::optional<InsertTextMode> insertTextMode;
  /// An edit which is applied to a document when selecting this completion.
  /// When an edit is provided the value of `insertText` is ignored.
  ///
  /// *Note:* The range of the edit must be a single line range and it must
  /// contain the position at which completion has been requested.
  ///
  /// Most editors support two different operations when accepting a completion
  /// item. One is to insert a completion text and the other is to replace an
  /// existing text with a completion text. Since this can usually not be
  /// predetermined by a server it can report both ranges. Clients need to
  /// signal support for `InsertReplaceEdit`s via the
  /// `textDocument.completion.completionItem.insertReplaceSupport` client
  /// capability property.
  ///
  /// *Note 1:* The text edit's range as well as both ranges from an insert
  /// replace edit must be a [single line] and they must contain the position
  /// at which completion has been requested.
  /// *Note 2:* If an `InsertReplaceEdit` is returned the edit's insert range
  /// must be a prefix of the edit's replace range, that means it must be
  /// contained and starting at the same position.
  ///
  /// @since 3.16.0 additional type `InsertReplaceEdit`
  /// TODO - can also be InsertReplaceEdit
  std::optional<TextEdit> textEdit;
  /// The edit text used if the completion item is part of a CompletionList and
  /// CompletionList defines an item default for the text edit range.
  ///
  /// Clients will only honor this property if they opt into completion list
  /// item defaults using the capability `completionList.itemDefaults`.
  ///
  /// If not provided and a list's default range is provided the label
  /// property is used as a text.
  ///
  /// @since 3.17.0
  std::optional<std::string> textEditText;
  /// An optional array of additional text edits that are applied when
  /// selecting this completion. Edits must not overlap (including the same
  /// insert position) with the main edit nor with themselves.
  ///
  /// Additional text edits should be used to change text unrelated to the
  /// current cursor position (for example adding an import statement at the
  /// top of the file if the completion item will insert an unqualified type).
  std::optional<std::vector<TextEdit>> additionalTextEdits;
  /// An optional set of characters that when pressed while this completion is
  /// active will accept it first and then type that character. *Note* that all
  /// commit characters should have `length=1` and that superfluous characters
  /// will be ignored.
  std::optional<std::vector<std::string>> commitCharacters;
  /// An optional command that is executed *after* inserting this completion.
  /// *Note* that additional modifications to the current document should be
  /// described with the additionalTextEdits-property.
  // TODO - std::optional<Command> command;
  /// A data entry field that is preserved on a completion item between
  /// a completion and a completion resolve request.
  // TODO - LSPAny for data
};

void to_json(json& j, const CompletionItem& obj);
void from_json(const json& j, CompletionItem& obj);

// Represents a collection of [completion items](#CompletionItem) to be
// presented in the editor.
struct CompletionList {
  /// This list is not complete. Further typing should result in recomputing
  /// this list.
  ///
  /// Recomputed lists have all their items replaced (not appended) in the
  /// incomplete completion sessions.
  bool isIncomplete;
  // TODO - do itemDefaults
  /// The completion items.
  std::vector<CompletionItem> items;
};

void to_json(json& j, const CompletionList& obj);
void from_json(const json& j, CompletionList& obj);

}  // namespace LSPSpec
