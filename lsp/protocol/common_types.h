#pragma once

#include <optional>
#include <stdint.h>
#include <string>
#include <vector>

#include "common/util/json_util.h"

namespace LSPSpec {

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

  Range(){};
  Range(Position start, Position end);
  // point constructor
  Range(uint32_t line, uint32_t character);
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

// A `MarkupContent` literal represents a string value which content is
// interpreted base on its kind flag. Currently the protocol supports
// `plaintext` and `markdown` as markup kinds.
//
// If the kind is `markdown` then the value can contain fenced code blocks like
// in GitHub issues.
//
// Here is an example how such a string can be constructed using
// JavaScript / TypeScript:
// ```typescript
// let markdown: MarkdownContent = {
// 	kind: MarkupKind.Markdown,
// 	value: [
// 		'# Header',
// 		'Some text',
// 		'```typescript',
// 		'someCode();',
// 		'```'
// 	].join('\n')
// };
// ```
//
// *Please Note* that clients might sanitize the return markdown. A client could
// decide to remove HTML from the markdown to avoid script execution.
struct MarkupContent {
  std::string m_kind;  // Actually a MarkupKind which is either 'plaintext' or 'markdown'
  std::string m_value;
};

void to_json(json& j, const MarkupContent& obj);
void from_json(const json& j, MarkupContent& obj);

// Represents a color in RGBA space.
struct Color {
  // The red component of this color in the range [0-1].
  float red;
  // The green component of this color in the range [0-1].
  float green;
  // The blue component of this color in the range [0-1].
  float blue;
  // The alpha component of this color in the range [0-1].
  float alpha;
};
void to_json(json& j, const Color& obj);
void from_json(const json& j, Color& obj);

struct TextEdit {
  // The range of the text document to be manipulated. To insert
  // text into a document create a range where start === end.
  Range range;
  // The string to be inserted. For delete operations use an
  // empty string.
  std::string newText;
};
void to_json(json& j, const TextEdit& obj);
void from_json(const json& j, TextEdit& obj);
}  // namespace LSPSpec
