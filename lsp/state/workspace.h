#pragma once

#include <map>
#include <string>
#include <utility>
#include "common/util/FileUtil.h"


// TODO - will need ideas to support multiple languages in one LSP
// - perhaps a separate workspace per language?  or some sort of directory
// - when you get the file contents, it comes with language identifier so this isn;t _that_ hard to do

class Workspace {
 public:
  Workspace();
  virtual ~Workspace();

  bool is_initialized();
  void set_initialized(bool new_value);

  //std::map<std::string, std::string>& documents();
  /*void add_document(std::string key, std::string text);
  bool remove_document(std::string key);
  bool change_document(std::string key, std::string text);*/

 private:
  bool m_initialized = false;
  std::map<fs::path, std::string> m_document_content;
  std::map<fs::path, std::vector<int>> m_document_symbols;
  std::map<fs::path, std::string> m_all_types_files;
};
