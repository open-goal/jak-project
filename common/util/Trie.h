#pragma once

#include <cassert>
#include <vector>
#include <string>

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
  T* lookup(const std::string& str);

  // Get all objects starting with the given prefix.
  std::vector<T*> lookup_prefix(const std::string& str);
  ~Trie();

 private:
  static constexpr int CHAR_START = ' ';
  static constexpr int CHAR_END = '~' + 1;
  static constexpr int CHAR_SIZE = CHAR_END - CHAR_START;

  static int idx(char c) {
    assert(c >= CHAR_START);
    assert(c < CHAR_END);
    return c - CHAR_START;
  }

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

    void insert(const char* str, const T* obj) {
      if (!*str) {
        // we are the child!
        if (value) {
          delete value;
        }
        value = new T(*obj);
      } else {
        // still more to go
        char first = *str;
        if (!children[idx(first)]) {
          children[idx(first)] = new Node();
        }
        children[idx(first)]->insert(str + 1, obj);
      }
    }

    T* operator[](const char* str) {
      if (!*str) {
        // we are the child!
        if (!value) {
          value = new T();
        }

        return value;
      } else {
        // still more to go
        char first = *str;
        if (!children[idx(first)]) {
          children[idx(first)] = new Node();
        }
        return children[idx(first)]->operator[](str + 1);
      }
    }

    T* lookup(const char* str) {
      if (!*str) {
        return value;
      }
      if (children[idx(*str)]) {
        return children[idx(*str)]->lookup(str + 1);
      }
      return nullptr;
    }

    void get_all_children(std::vector<T*>& result) {
      if (value) {
        result.push_back(value);
      }
      for (auto child : children) {
        if (child) {
          child->get_all_children(result);
        }
      }
    }

    std::vector<T*> lookup_prefix(const char* str) {
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
};

template <typename T>
Trie<T>::~Trie<T>() {
  m_root.delete_children();
}

template <typename T>
void Trie<T>::insert(const std::string& str, const T& obj) {
  m_root.insert(str.c_str(), &obj);
}

template <typename T>
T* Trie<T>::lookup(const std::string& str) {
  return m_root.lookup(str.c_str());
}

template <typename T>
T* Trie<T>::operator[](const std::string& str) {
  return m_root.operator[](str.c_str());
}

template <typename T>
std::vector<T*> Trie<T>::lookup_prefix(const std::string& str) {
  return m_root.lookup_prefix(str.c_str());
}