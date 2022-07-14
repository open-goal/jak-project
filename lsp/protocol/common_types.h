#pragma once

#include <optional>
#include <stdint.h>
#include <string>
#include <vector>

#include "third-party/json.hpp"

using json = nlohmann::json;

namespace LSPSpec {
// TODO - eventually parse this
typedef std::string URI;
typedef std::string DocumentUri;

struct Position {
  /// @brief Line position in a document (zero-based).
  uint32_t m_line;
  /// @brief Character offset on a line in a document (zero-based). The meaning of this
  /// offset is determined by the negotiated `PositionEncodingKind`.
  ///
  /// If the character value is greater than the line length it defaults back
  /// to the line length.
  uint32_t m_character;
};
void to_json(json& j, const Position& obj);
void from_json(const json& j, Position& obj);

struct Range {
  Position m_start;
  Position m_end;
};
void to_json(json& j, const Range& obj);
void from_json(const json& j, Range& obj);

struct TextDocumentItem {
  DocumentUri m_uri;
  std::string m_languageId;  // ie. opengoal-ir
  int32_t m_version;
  std::string m_text;
};

void to_json(json& j, const TextDocumentItem& obj);
void from_json(const json& j, TextDocumentItem& obj);

struct TextDocumentIdentifier {
  DocumentUri m_uri;
};

void to_json(json& j, const TextDocumentIdentifier& obj);
void from_json(const json& j, TextDocumentIdentifier& obj);

struct VersionedTextDocumentIdentifier {
  DocumentUri m_uri;
  /// @brief The version number of this document.
  /// The version number of a document will increase after each change, including undo/redo. The
  /// number doesn't need to be consecutive.
  int32_t m_version;
};

void to_json(json& j, const VersionedTextDocumentIdentifier& obj);
void from_json(const json& j, VersionedTextDocumentIdentifier& obj);

struct Location {
  DocumentUri m_uri;
  Range m_range;
};
void to_json(json& j, const Location& obj);
void from_json(const json& j, Location& obj);

struct TextDocumentPositionParams {
  /// @brief The text document.
  TextDocumentIdentifier m_textDocument;
  /// @brief The position inside the text document.
  Position m_position;
};

void to_json(json& j, const TextDocumentPositionParams& obj);
void from_json(const json& j, TextDocumentPositionParams& obj);
}  // namespace LSPSpec
