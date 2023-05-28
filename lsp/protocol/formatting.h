#pragma once

#include "common_types.h"

namespace LSPSpec {

// Value-object describing what options formatting should use.
struct FormattingOptions {
  // Size of a tab in spaces.
  uint32_t tabSize;
  // Prefer spaces over tabs.
  bool insertSpaces;
  // Trim trailing whitespace on a line.
  //
  // @since 3.15.0
  std::optional<bool> trimTrailingWhitespace;
  // Insert a newline character at the end of the file if one does not exist.
  //
  // @since 3.15.0
  std::optional<bool> insertFinalNewLine;
  // Trim all newlines after the final newline at the end of the file.
  //
  // @since 3.15.0
  std::optional<bool> trimFinalNewLines;
  // NOTE - omitting dynamic properties, not standardized anyway
  // Signature for further properties.
};

void to_json(json& j, const FormattingOptions& obj);
void from_json(const json& j, FormattingOptions& obj);

struct DocumentFormattingParams {
  // The document to format.
  TextDocumentIdentifier textDocument;
  // The format options.
  FormattingOptions options;
};

void to_json(json& j, const DocumentFormattingParams& obj);
void from_json(const json& j, DocumentFormattingParams& obj);

}  // namespace LSPSpec
