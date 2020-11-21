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

#include "common/util/FileUtil.h"

#include "TextDB.h"

namespace goos {

/*!
 * Initialize with the given string
 */
SourceText::SourceText(std::string r) : text(std::move(r)) {
  // find line breaks
  build_offsets();
}

/*!
 * Update line break data. Should be called any time the text is updated.
 */
void SourceText::build_offsets() {
  offset_by_line.clear();
  offset_by_line.push_back(0);
  for (uint32_t i = 0; i < text.size(); i++) {
    if (text[i] == '\n') {
      offset_by_line.push_back(i);
    }
  }
  offset_by_line.push_back(text.size());
}

/*!
 * Get the text of the line containing the character at position "offset" from this source.
 */
std::string SourceText::get_line_containing_offset(int offset) {
  auto range = get_containing_line(offset);
  int start_offset = range.first ? 1 : 0;
  return text.substr(range.first + start_offset,
                     std::max(0, range.second - range.first - start_offset));
}

/*!
 * Get the index of the line containing the character at position "offset".
 * O(n_lines) crappy implementation.
 * Error if not found.
 */
int SourceText::get_line_idx(int offset) {
  for (uint32_t line = 0; line < offset_by_line.size() - 1; line++) {
    if (offset >= offset_by_line[line] && offset <= offset_by_line[line + 1]) {
      return line;
    }
  }
  throw std::runtime_error("Unable to get line index for character at position " +
                           std::to_string(offset));
}

/*!
 * Gets the [start, end) character offset of the line containing the given offset.
 */
std::pair<int, int> SourceText::get_containing_line(int offset) {
  for (uint32_t line = 0; line < offset_by_line.size() - 1; line++) {
    if (offset >= offset_by_line[line] && offset <= offset_by_line[line + 1]) {
      return std::make_pair(offset_by_line[line], offset_by_line[line + 1]);
    }
  }
  return std::make_pair(0, text.size());
}

/*!
 * Read text from a file.
 */
FileText::FileText(std::string filename_) : filename(std::move(filename_)) {
  text = file_util::read_text_file(filename);
  build_offsets();
}

/*!
 * Inform the TextDB about a source of text.
 */
void TextDb::insert(const std::shared_ptr<SourceText>& frag) {
  fragments.push_back(frag);
}

/*!
 * Link the GOOS object o to the offset into the given text fragment.
 * The object _must_ be a pair or empty list.
 */
void TextDb::link(const Object& o, std::shared_ptr<SourceText> frag, int offset) {
  if (o.is_empty_list())
    return;
  assert(o.is_pair());
  TextRef ref;
  ref.offset = offset;
  ref.frag = std::move(frag);
  map[o.heap_obj] = ref;
}

/*!
 * Given an object, get a string representing where it's from. Or "?" if we can't find it.
 */
std::string TextDb::get_info_for(const Object& o) {
  if (o.is_pair()) {
    auto kv = map.find(o.heap_obj);
    if (kv != map.end()) {
      return get_info_for(kv->second.frag, kv->second.offset);
    } else {
      return "?";
    }
  } else {
    return "?";
  }
}

/*!
 * Given a source text and an offset, print a description of where it is.
 */
std::string TextDb::get_info_for(const std::shared_ptr<SourceText>& frag, int offset) {
  std::string result = "text from " + frag->get_description() +
                       ", line: " + std::to_string(frag->get_line_idx(offset) + 1) + "\n";
  result += frag->get_line_containing_offset(offset) + "\n";
  return result;
}
}  // namespace goos
