#pragma once

#include <array>
#include <memory>
#include <string>
#include <vector>

// A normal Trie does not allow for duplicate keys, however this one does
// It allows for insertion and removal
template <typename T>
class TrieWithDuplicates {
 private:
  struct TrieNode {
    std::array<std::unique_ptr<TrieNode>, 256> children;
    std::vector<std::unique_ptr<T>> elements;
  };

  std::unique_ptr<TrieNode> root = std::make_unique<TrieNode>();

 public:
  TrieWithDuplicates() {}

  T* insert(const std::string& key, const T& element) {
    std::unique_ptr<T> new_element = std::make_unique<T>(element);
    TrieNode* curr_node = root.get();
    for (const char character : key) {
      auto& child = curr_node->children[(uint8_t)character];
      if (!child) {
        child = std::make_unique<TrieNode>();
      }
      curr_node = child.get();
    }
    curr_node->elements.push_back(std::move(new_element));
    return curr_node->elements.back().get();
  }

  std::vector<T*> retrieve_with_prefix(const std::string& prefix, int max_count = -1) const {
    std::vector<T*> results;
    TrieNode* curr_node = root.get();
    for (const char character : prefix) {
      if (max_count >= 0 && (int)results.size() > max_count) {
        return results;
      }
      const auto& child = curr_node->children.at((uint8_t)character);
      if (child == nullptr) {
        return results;  // tree ends, nothing found with that prefix
      } else {
        curr_node = child.get();
      }
    }
    retrieve_elements(curr_node, results, max_count);
    return results;
  }

  std::vector<T*> retrieve_with_exact(const std::string& key) const {
    std::vector<T*> results;
    TrieNode* curr_node = root.get();
    for (const char character : key) {
      const auto& child = curr_node->children.at((uint8_t)character);
      if (child == nullptr) {
        return results;  // tree ends, nothing found with that key
      } else {
        curr_node = child.get();
      }
    }
    for (const auto& element : curr_node->elements) {
      results.push_back(element.get());
    }
    return results;
  }

  bool remove(const std::string& key, const T* to_be_removed) {
    TrieNode* curr_node = root.get();
    for (const char character : key) {
      const auto& child = curr_node->children.at((uint8_t)character);
      if (child == nullptr) {
        return false;  // tree ends, nothing found with that key
      } else {
        curr_node = child.get();
      }
    }
    // Since the trie holds duplicates, we can't delete on the key alone
    // now search to see which element is identical
    auto it = curr_node->elements.begin();
    while (it != curr_node->elements.end()) {
      if (it->get() == to_be_removed) {
        it = curr_node->elements.erase(it);
        return true;  // we can assume that the same ptr isn't stored twice.
      } else {
        ++it;
      }
    }
    return false;
  }

  // Return the total number of elements stored in the TrieMap
  int size() const {
    int count = 0;
    count_elements(root.get(), count);
    return count;
  }

  std::vector<T*> get_all_elements() const {
    std::vector<T*> results;
    get_all_elements_helper(root.get(), results);
    return results;
  }

 private:
  void retrieve_elements(const TrieNode* node, std::vector<T*>& results, int max_count = -1) const {
    for (const auto& element : node->elements) {
      if (max_count >= 0 && (int)results.size() > max_count) {
        return;
      }
      results.push_back(element.get());
    }
    for (const auto& child : node->children) {
      if (max_count >= 0 && (int)results.size() > max_count) {
        return;
      }
      if (child.get() != nullptr) {
        retrieve_elements(child.get(), results, max_count);
      }
    }
  }

  void count_elements(const TrieNode* node, int& count) const {
    count += node->elements.size();
    for (const auto& child : node->children) {
      if (child.get() != nullptr) {
        count_elements(child.get(), count);
      }
    }
  }

  void get_all_elements_helper(const TrieNode* node, std::vector<T*>& result) const {
    for (const auto& element : node->elements) {
      result.push_back(element.get());
    }
    for (const auto& child : node->children) {
      if (child.get() != nullptr) {
        get_all_elements_helper(child.get(), result);
      }
    }
  }
};
