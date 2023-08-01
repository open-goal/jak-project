#pragma once

#include <string>
#include <vector>

#include "common/util/Assert.h"

/*!
 * A simple prefix tree. It works similarly to a map, but also supports fast lookups by prefix with
 * the ability to lookup all entries with keys that begin with a given prefix.
 *
 * It owns the memory for the objects it stores.
 * Doing an insert will create a copy of your object.
 *
 * Other that deleting the whole thing, there is no support for removing a node.
 */
template <typename T>
class Trie {
 public:
  Trie() = default;
  Trie(const Trie&) = delete;
  Trie& operator=(const Trie&) = delete;

  // Insert an object, replacing an existing one if it exists
  void insert(const std::string& str, const T& obj);

  // Get the object at the string. Default construct a new one if none exists.
  T* operator[](const std::string& str);

  // Lookup an existing object. If none exists, return nullptr.
  T* lookup(const std::string& str) const;

  // return the number of entries.
  int size() const { return m_size; }

  // Get all objects starting with the given prefix.
  std::vector<T*> lookup_prefix(const std::string& str) const;

  // Get all nodes in the tree.
  std::vector<T*> get_all_nodes() const;
  ~Trie();

 private:
  static constexpr int CHAR_SIZE = 256;

  static int idx(char c) { return (u8)c; }

  struct Node {
    T* value = nullptr;
    Node* children[CHAR_SIZE] = {0};

    void delete_children() {
      if (value) {
        delete value;
        value = nullptr;
      }
      for (auto child : children) {
        if (child) {
          child->delete_children();
          delete child;
          child = nullptr;
        }
      }
    }

    /*!
     * Return true if a new object was inserted.
     */
    bool insert(const char* str, const T* obj) {
      if (!*str) {
        // we are the child!
        if (value) {
          delete value;
          value = new T(*obj);
          return false;  // didn't change the count.
        } else {
          value = new T(*obj);
          return true;  // did change the count
        }

      } else {
        // still more to go
        char first = *str;
        if (!children[idx(first)]) {
          children[idx(first)] = new Node();
        }
        return children[idx(first)]->insert(str + 1, obj);
      }
    }

    T* bracket_operator(const char* str, bool* inserted) {
      if (!*str) {
        // we are the child!
        if (!value) {
          value = new T();
          *inserted = true;
        } else {
          *inserted = false;
        }

        return value;
      } else {
        // still more to go
        char first = *str;
        if (!children[idx(first)]) {
          children[idx(first)] = new Node();
        }
        return children[idx(first)]->bracket_operator(str + 1, inserted);
      }
    }

    T* lookup(const char* str) const {
      if (!*str) {
        return value;
      }
      if (children[idx(*str)]) {
        return children[idx(*str)]->lookup(str + 1);
      }
      return nullptr;
    }

    void get_all_children(std::vector<T*>& result) const {
      if (value) {
        result.push_back(value);
      }
      for (auto child : children) {
        if (child) {
          child->get_all_children(result);
        }
      }
    }

    std::vector<T*> lookup_prefix(const char* str) const {
      if (!*str) {
        std::vector<T*> result;
        get_all_children(result);
        return result;
      } else {
        if (children[idx(*str)]) {
          return children[idx(*str)]->lookup_prefix(str + 1);
        } else {
          return {};
        }
      }
    }
  };

  Node m_root;
  int m_size = 0;
};

template <typename T>
Trie<T>::~Trie<T>() {
  m_root.delete_children();
  m_size = 0;
}

template <typename T>
void Trie<T>::insert(const std::string& str, const T& obj) {
  if (m_root.insert(str.c_str(), &obj)) {
    m_size++;
  }
}

template <typename T>
T* Trie<T>::lookup(const std::string& str) const {
  return m_root.lookup(str.c_str());
}

template <typename T>
T* Trie<T>::operator[](const std::string& str) {
  bool added = false;
  auto result = m_root.bracket_operator(str.c_str(), &added);
  if (added) {
    m_size++;
  }
  return result;
}

template <typename T>
std::vector<T*> Trie<T>::lookup_prefix(const std::string& str) const {
  return m_root.lookup_prefix(str.c_str());
}

template <typename T>
std::vector<T*> Trie<T>::get_all_nodes() const {
  std::vector<T*> result;
  m_root.get_all_children(result);
  return result;
}
