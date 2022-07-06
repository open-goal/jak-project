#pragma once

#include <map>
#include <string>
#include <utility>

class Workspace {
 public:
  Workspace();
  virtual ~Workspace();

  bool is_initialized();
  void set_initialized(bool new_value);

  std::map<std::string, std::string>& documents();
  void add_document(std::string key, std::string text);
  bool remove_document(std::string key);
  bool change_document(std::string key, std::string text);

 private:
  bool m_initialized = false;
  std::map<std::string, std::string> m_documents;
};
