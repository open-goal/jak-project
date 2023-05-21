#pragma once

#include "common_types.h"

namespace LSPSpec {
struct DocumentColorParams {
  // The text document.
  TextDocumentIdentifier textDocument;
};

void to_json(json& j, const DocumentColorParams& obj);
void from_json(const json& j, DocumentColorParams& obj);

struct ColorInformation {
  // The range in the document where this color appears.
  Range range;
  // The actual color value for this color range.
  Color color;
};

void to_json(json& j, const ColorInformation& obj);
void from_json(const json& j, ColorInformation& obj);

}  // namespace LSPSpec
