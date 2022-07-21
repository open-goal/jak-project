#pragma once

#include <optional>

#include "common_types.h"

#include "third-party/json.hpp"

namespace LSPSpec {
enum class SymbolKind {
  File = 1,
  Module = 2,
  Namespace = 3,
  Package = 4,
  Class = 5,
  Method = 6,
  Property = 7,
  Field = 8,
  Constructor = 9,
  Enum = 10,
  Interface = 11,
  Function = 12,
  Variable = 13,
  Constant = 14,
  String = 15,
  Number = 16,
  Boolean = 17,
  Array = 18,
  Object = 19,
  Key = 20,
  Null = 21,
  EnumMember = 22,
  Struct = 23,
  Event = 24,
  Operator = 25,
  TypeParameter = 26
};

/// @brief Symbol tags are extra annotations that tweak the rendering of a symbol.
/// @since 3.16
enum class SymbolTag {
  /// @brief Render a symbol as obsolete, usually using a strike-out.
  Deprecated = 1
};

struct DocumentSymbol {
  std::string m_name;
  std::optional<std::string> m_detail;
  SymbolKind m_kind;
  std::optional<std::vector<SymbolTag>> m_tags;
  /// @brief The range enclosing this symbol not including leading/trailing whitespace
  /// but everything else like comments. This information is typically used to determine
  /// if the clients cursor is inside the symbol to reveal in the symbol in the UI.
  Range m_range;
  /// @brief The range that should be selected and revealed when this symbol is being
  /// picked, e.g. the name of a function. Must be contained by the `range`.
  Range m_selectionRange;
  /// @brief Children of this symbol, e.g. properties of a class.
  std::optional<std::vector<DocumentSymbol>> m_children;
};

void to_json(json& j, const DocumentSymbol& obj);
void from_json(const json& j, DocumentSymbol& obj);

struct DocumentSymbolParams {
  TextDocumentIdentifier m_textDocument;
};

void to_json(json& j, const DocumentSymbolParams& obj);
void from_json(const json& j, DocumentSymbolParams& obj);

}  // namespace LSPSpec
