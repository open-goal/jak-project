#pragma once

#include "common_types.h"

namespace LSPSpec {
/**
 * A `MarkupContent` literal represents a string value which content is
 * interpreted base on its kind flag. Currently the protocol supports
 * `plaintext` and `markdown` as markup kinds.
 *
 * If the kind is `markdown` then the value can contain fenced code blocks like
 * in GitHub issues.
 *
 * Here is an example how such a string can be constructed using
 * JavaScript / TypeScript:
 * ```typescript
 * let markdown: MarkdownContent = {
 * 	kind: MarkupKind.Markdown,
 * 	value: [
 * 		'# Header',
 * 		'Some text',
 * 		'```typescript',
 * 		'someCode();',
 * 		'```'
 * 	].join('\n')
 * };
 * ```
 *
 * *Please Note* that clients might sanitize the return markdown. A client could
 * decide to remove HTML from the markdown to avoid script execution.
 */
struct MarkupContent {
  std::string m_kind;  // Actually a MarkupKind which is either 'plaintext' or 'markdown'
  std::string m_value;
};

void to_json(json& j, const MarkupContent& obj);
void from_json(const json& j, MarkupContent& obj);

struct Hover {
  /// @brief The hover's content
  MarkupContent m_contents;
  /// @brief An optional range is a range inside a text document that is used to visualize a hover,
  /// e.g. by changing the background color.
  std::optional<Range> m_range;
};

void to_json(json& j, const Hover& obj);
void from_json(const json& j, Hover& obj);

}  // namespace LSPSpec
