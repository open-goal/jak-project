#ifndef COMPILER_TEXTDB_H
#define COMPILER_TEXTDB_H

#include <string>
#include <vector>
#include <memory>
#include <unordered_map>
#include <fstream>

#include "goos/Object.h"

class ITextFragment {
 public:
  ITextFragment(const std::string& r);
  ITextFragment() = default;
  virtual const char* get_text() = 0;
  virtual int get_size() = 0;
  virtual std::string get_description() = 0;
  virtual ~ITextFragment(){};
  std::string get_line_containing_offset(int offset);
  int get_line_idx(int offset);

 protected:
  std::string text;
  std::vector<int> offset_by_line;
  void build_offsets();
  std::pair<int, int> get_containing_line(int offset);
};

class ReplText : public ITextFragment {
 public:
  ReplText(const std::string& text_) : ITextFragment(text_) {}
  const char* get_text() { return text.c_str(); }
  int get_size() { return text.size(); }
  std::string get_description() { return "REPL"; }
  ~ReplText() = default;
};

class ProgramString : public ITextFragment {
 public:
  ProgramString(const std::string& text_) : ITextFragment(text_) {}
  const char* get_text() { return text.c_str(); }
  int get_size() { return text.size(); }
  std::string get_description() { return "Program string"; }
  ~ProgramString() = default;
};

class FileText : public ITextFragment {
 public:
  FileText(const std::string& filename_) : filename(filename_) {
    std::ifstream file(filename);
    if (file.fail()) {
      printf("Unable to open file %s\n", filename.c_str());
      throw std::runtime_error("File can't be opened\n");
    }

    file.seekg(0, std::ios::end);
    text.reserve((unsigned long)file.tellg());
    file.seekg(0, std::ios::beg);
    text.assign((std::istreambuf_iterator<char>(file)), std::istreambuf_iterator<char>());
    build_offsets();
  }

  const char* get_text() { return text.c_str(); }
  int get_size() { return text.size(); }
  std::string get_description() { return filename; }
  ~FileText() = default;

 private:
  std::string filename;
};

struct TextRef {
  int offset;
  std::shared_ptr<ITextFragment> frag;
};

class TextDb {
 public:
  void insert(std::shared_ptr<ITextFragment> frag);
  void link(Object o, std::shared_ptr<ITextFragment> frag, int offset);
  std::string get_info_for(Object o);
  std::string get_info_for(std::shared_ptr<ITextFragment> frag, int offset);

 private:
  std::vector<std::shared_ptr<ITextFragment>> fragments;
  std::unordered_map<std::shared_ptr<AllocObject>, TextRef> map;
};

#endif  // COMPILER_TEXTDB_H
