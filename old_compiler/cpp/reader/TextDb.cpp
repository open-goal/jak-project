#include "goos/Object.h"
#include "TextDb.h"

void TextDb::insert(std::shared_ptr<ITextFragment> frag) {
  fragments.push_back(frag);
}

void TextDb::link(Object o, std::shared_ptr<ITextFragment> frag, int offset) {
  if (o.type == EMPTY_LIST)
    return;
  assert(o.type == PAIR);
  TextRef ref;
  ref.offset = offset;
  ref.frag = frag;
  map[o.alloc] = ref;
}

std::string TextDb::get_info_for(Object o) {
  if (o.type == PAIR) {
    auto kv = map.find(o.alloc);
    if (kv != map.end()) {
      // todo, get actual line stuff.
      return get_info_for(kv->second.frag, kv->second.offset);
    } else {
      //      return "object from untracked source:\n" + o.inspect() + "\n";
      return "?";
    }
  } else {
    return "?";
    //    return "object from untracked source:\n" + o.inspect() + "\n";
  }
}

std::string TextDb::get_info_for(std::shared_ptr<ITextFragment> frag, int offset) {
  std::string result = "text from " + frag->get_description() +
                       ", line: " + std::to_string(frag->get_line_idx(offset) + 1) + "\n";
  result += frag->get_line_containing_offset(offset) + "\n";
  return result;
}

ITextFragment::ITextFragment(const std::string& r) : text(r) {
  build_offsets();
}

void ITextFragment::build_offsets() {
  offset_by_line.push_back(0);
  for (uint32_t i = 0; i < text.size(); i++) {
    if (text[i] == '\n') {
      offset_by_line.push_back(i);
    }
  }
}

std::pair<int, int> ITextFragment::get_containing_line(int offset) {
  for (uint32_t line = 0; line < offset_by_line.size() - 1; line++) {
    if (offset >= offset_by_line[line] && offset <= offset_by_line[line + 1]) {
      return std::make_pair(offset_by_line[line], offset_by_line[line + 1]);
    }
  }
  return std::make_pair(0, text.size());
}

std::string ITextFragment::get_line_containing_offset(int offset) {
  auto range = get_containing_line(offset);
  return text.substr(range.first, range.second - range.first);
}

int ITextFragment::get_line_idx(int offset) {
  for (uint32_t line = 0; line < offset_by_line.size() - 1; line++) {
    if (offset >= offset_by_line[line] && offset <= offset_by_line[line + 1]) {
      return line;
    }
  }
  return -1;
}