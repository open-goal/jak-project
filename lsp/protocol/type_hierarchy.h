#pragma once

#include "common_types.h"

#include "lsp/protocol/document_symbols.h"

namespace LSPSpec {

struct TypeHierarchyPrepareParams : TextDocumentPositionParams {};

void to_json(json& j, const TypeHierarchyPrepareParams& obj);
void from_json(const json& j, TypeHierarchyPrepareParams& obj);

struct TypeHierarchyItem {
  /// The name of this item.
  std::string name;
  /// The kind of this item.
  SymbolKind kind;
  /// Tags for this item.
  std::optional<std::vector<SymbolTag>> tags;
  /// More detail for this item, e.g. the signature of a function.
  std::optional<std::string> detail;
  /// The resource identifier of this item.
  DocumentUri uri;
  /// The range enclosing this symbol not including leading/trailing whitespace
  /// but everything else, e.g. comments and code.
  Range range;
  /// The range that should be selected and revealed when this symbol is being
  /// picked, e.g. the name of a function. Must be contained by the
  /// `range` of this
  Range selectionRange;
  /// A data entry field that is preserved between a type hierarchy prepare and
  /// supertypes or subtypes requests. It could also be used to identify the
  /// type hierarchy in the server, helping improve the performance on
  /// resolving supertypes and subtypes.
  // ANY data;
};

void to_json(json& j, const TypeHierarchyItem& obj);
void from_json(const json& j, TypeHierarchyItem& obj);

struct TypeHierarchySupertypesParams {
  TypeHierarchyItem item;
};

void to_json(json& j, const TypeHierarchySupertypesParams& obj);
void from_json(const json& j, TypeHierarchySupertypesParams& obj);

struct TypeHierarchySubtypesParams {
  TypeHierarchyItem item;
};

void to_json(json& j, const TypeHierarchySubtypesParams& obj);
void from_json(const json& j, TypeHierarchySubtypesParams& obj);

}  // namespace LSPSpec
