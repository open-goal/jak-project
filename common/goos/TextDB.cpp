/*!
 * @file TextDB.cpp
 * The Text Database for storing source code text.
 * This allows us to ask for things like "where did this form come from?"
 * and be able to print the file name and offset into the file, as well as the line.
 *
 * The purpose is to be able to have an error message like:
 *
 * Error on (+ a b): a has invalid type (string)
 * From my-file.gc, line 25:
 *   (+ 1 (+ a b)) ; compute the sum
 */

#include "TextDB.h"

#include "common/util/FileUtil.h"

#include "third-party/fmt/core.h"

namespace goos {

/*!
 * Initialize with the given string
 */
SourceText::SourceText(std::string r) : m_text(std::move(r)) {
  // find line breaks
  build_offsets();
}

/*!
 * Update line break data. Should be called any time the text is updated.
 */
void SourceText::build_offsets() {
  m_offset_by_line.clear();
  m_offset_by_line.push_back(0);
  for (uint32_t i = 0; i < m_text.size(); i++) {
    if (m_text[i] == '\n') {
      m_offset_by_line.push_back(i);
    }
  }
  m_offset_by_line.push_back(m_text.size());
}

/*!
 * Get the text of the line containing the character at position "offset" from this source.
 */
std::string SourceText::get_line_containing_offset(int offset) {
  auto range = get_containing_line(offset);
  int start_offset = range.first ? 1 : 0;
  return m_text.substr(range.first + start_offset,
                       std::max(0, range.second - range.first - start_offset));
}

/*!
 * Get the index of the line containing the character at position "offset".
 * O(n_lines) crappy implementation.
 * Error if not found.
 */
int SourceText::get_line_idx(int offset) {
  for (uint32_t line = 0; line < m_offset_by_line.size() - 1; line++) {
    if (offset >= m_offset_by_line[line] && offset <= m_offset_by_line[line + 1]) {
      return line;
    }
  }
  throw std::runtime_error("Unable to get line index for character at position " +
                           std::to_string(offset));
}

int SourceText::get_offset_of_line(int line_idx) {
  return m_offset_by_line.at(line_idx);
}

/*!
 * Gets the [start, end) character offset of the line containing the given offset.
 */
std::pair<int, int> SourceText::get_containing_line(int offset) {
  for (uint32_t line = 0; line < m_offset_by_line.size() - 1; line++) {
    if (offset >= m_offset_by_line[line] && offset <= m_offset_by_line[line + 1]) {
      return std::make_pair(m_offset_by_line[line], m_offset_by_line[line + 1]);
    }
  }
  return std::make_pair(0, (int)m_text.size());
}

/*!
 * Read text from a file.
 */
FileText::FileText(const std::string& filename, const std::string& description_name)
    : m_filename(filename), m_desc_name(description_name) {
  m_text = file_util::read_text_file(m_filename);
  build_offsets();
}

/*!
 * Inform the TextDB about a source of text.
 */
void TextDb::insert(const std::shared_ptr<SourceText>& frag) {
  m_fragments.push_back(frag);
}

/*!
 * Link the GOOS object o to the offset into the given text fragment.
 * The object _must_ be a pair or empty list.
 */
void TextDb::link(const Object& o, std::shared_ptr<SourceText> frag, int offset) {
  if (o.is_empty_list())
    return;
  ASSERT(o.is_pair());
  TextRef ref;
  ref.offset = offset;
  ref.frag = std::move(frag);
  m_map[o.heap_obj] = ref;
}

/*!
 * Given an object, get a string representing where it's from. Or "?" if we can't find it.
 */
std::string TextDb::get_info_for(const Object& o, bool* terminate_compiler_error) const {
  if (o.is_pair()) {
    auto kv = m_map.find(o.heap_obj);
    if (kv != m_map.end()) {
      if (terminate_compiler_error) {
        *terminate_compiler_error = kv->second.frag->terminate_compiler_error();
      }
      return get_info_for(kv->second.frag, kv->second.offset);
    } else {
      if (terminate_compiler_error) {
        *terminate_compiler_error = false;
      }
      return "?\n";
    }
  } else {
    if (terminate_compiler_error) {
      *terminate_compiler_error = false;
    }
    return "?\n";
  }
}

std::optional<TextDb::ShortInfo> TextDb::get_short_info_for(const Object& o) const {
  if (o.is_pair()) {
    auto kv = m_map.find(o.heap_obj);
    if (kv != m_map.end()) {
      return get_short_info_for(kv->second.frag, kv->second.offset);
    } else {
      return {};
    }
  } else {
    return {};
  }
}

/*!
 * Given a source text and an offset, print a description of where it is.
 */
std::string TextDb::get_info_for(const std::shared_ptr<SourceText>& frag, int offset) const {
  int line_idx = frag->get_line_idx(offset);
  std::string result = frag->get_description() + ":" + std::to_string(line_idx + 1) + "\n";
  result += frag->get_line_containing_offset(offset) + "\n";
  int offset_in_line = std::max(offset - frag->get_offset_of_line(line_idx), 1) - 1;

  std::string pointer(offset_in_line, ' ');
  pointer += "^\n";
  return result + pointer;
}

std::optional<TextDb::ShortInfo> TextDb::get_short_info_for(const std::shared_ptr<SourceText>& frag,
                                                            int offset) const {
  int line_idx = frag->get_line_idx(offset);
  int offset_in_line = std::max(offset - frag->get_offset_of_line(line_idx), 1) - 1;
  ShortInfo info_result;
  info_result.filename = frag->get_description();
  info_result.line_idx_to_display = line_idx;
  info_result.pos_in_line = offset_in_line;
  return std::make_optional(info_result);
}

std::optional<TextDb::ShortInfo> TextDb::try_get_short_info(
    const std::shared_ptr<goos::HeapObject>& heap_obj) const {
  auto it = m_map.find(heap_obj);
  if (it != m_map.end()) {
    auto& frag = it->second.frag;
    // shorten the string
    std::string name = frag->get_description();
    size_t start = 0;
    for (size_t i = 0; i < name.size(); i++) {
      if (name[i] == '/' || name[i] == '\\') {
        start = i + 1;
      }
    }
    if (start < name.size()) {
      name = name.substr(start);
    }

    ShortInfo result;
    result.filename = name;

    int line_idx = frag->get_line_idx(it->second.offset);
    result.line_idx_to_display = line_idx + 1;

    int offset_of_line = frag->get_offset_of_line(line_idx);
    int offset_of_next_line = frag->get_offset_of_line(line_idx + 1);

    int line_length = offset_of_next_line - offset_of_line;

    int start_offset_in_line = it->second.offset - offset_of_line - 1;
    result.pos_in_line = std::max(start_offset_in_line, 0);
    result.line_text = std::string(frag->get_text() + offset_of_line + 1, line_length - 1);
    return result;
  }
  return {};
}

std::optional<TextDb::ShortInfo> TextDb::try_get_short_info(const Object& o) const {
  if (o.is_pair()) {
    return try_get_short_info(o.heap_obj);
  }

  return {};
}

bool TextDb::has_info(const Object& o) const {
  return o.is_pair() && (m_map.find(o.heap_obj) != m_map.end());
}

/*!
 * Make child have the same location in the source as parent.  For example, if parent generates
 * code that we want to be associated with the parent's location in source.
 *
 * Note: this only has an effect if both parent and child are pair/list. Otherwise it does nothing.
 */
void TextDb::inherit_info(const Object& parent, const Object& child) {
  if (parent.is_pair() && child.is_pair()) {
    auto parent_kv = m_map.find(parent.heap_obj);
    if (parent_kv != m_map.end()) {
      std::vector<const Object*> children = {&child};
      // mark all forms as children. This will help with error messages in macros, and makes
      // (add-macro-to-autocomplete) work properly.
      while (!children.empty()) {
        auto top = children.back();
        children.pop_back();
        if (m_map.insert({top->heap_obj, parent_kv->second}).second) {
          if (top->as_pair()->car.is_pair()) {
            children.push_back(&top->as_pair()->car);
          }
          if (top->as_pair()->cdr.is_pair()) {
            children.push_back(&top->as_pair()->cdr);
          }
        }
      }
    }
  }
}

void TextDb::clear_info() {
  m_map.clear();
  m_fragments.clear();
}
}  // namespace goos
