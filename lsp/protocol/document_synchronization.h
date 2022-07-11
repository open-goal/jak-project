#pragma once

#include "common_types.h"

namespace LSPSpec {
struct DidOpenTextDocumentParams {
  TextDocumentItem m_textDocument;
};

void to_json(json& j, const DidOpenTextDocumentParams& obj);
void from_json(const json& j, DidOpenTextDocumentParams& obj);

}  // namespace LSPSpec
