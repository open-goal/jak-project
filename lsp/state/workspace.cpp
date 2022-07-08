#include "workspace.h"

Workspace::Workspace(){};
Workspace::~Workspace(){};

bool Workspace::is_initialized() {
  return m_initialized;
};

void Workspace::set_initialized(bool new_value) {
  m_initialized = new_value;
};
//
//std::map<std::string, std::string>& Workspace::documents() {
//  return m_documents;
//};
//
//void Workspace::add_document(std::string key, std::string text) {
//  m_documents[key] = text;
//}
//
//bool Workspace::remove_document(std::string key) {
//  auto it = m_documents.find(key);
//  if (it != m_documents.end()) {
//    m_documents.erase(it);
//    return true;
//  }
//  return false;
//}
//
//bool Workspace::change_document(std::string key, std::string text) {
//  auto it = m_documents.find(key);
//  if (it != m_documents.end()) {
//    m_documents[key] = text;
//    return true;
//  }
//  return false;
//}
