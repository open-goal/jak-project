#pragma once

#include <algorithm>
#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

// TrieMap class
template <typename T>
class TrieMap {
 private:
  // TrieNode structure
  struct TrieNode {
    std::unordered_map<char, std::shared_ptr<TrieNode>> children;
    std::vector<std::shared_ptr<T>> elements;
  };

  std::shared_ptr<TrieNode> root;

 public:
  TrieMap() : root(std::make_shared<TrieNode>()) {}

  // Insert an element with a key into the TrieMap and return the inserted element
  std::shared_ptr<T> insert(const std::string& key, const T& element) {
    std::shared_ptr<T> shared_element = std::make_shared<T>(element);
    std::shared_ptr<TrieNode> node = root;
    for (char c : key) {
      if (node->children.find(c) == node->children.end()) {
        node->children[c] = std::make_shared<TrieNode>();
      }
      node = node->children[c];
    }
    // Store element at the leaf node
    node->elements.push_back(shared_element);
    return shared_element;
  }

  // Retrieve elements with a given prefix
  std::vector<std::shared_ptr<T>> retrieve_with_prefix(const std::string& prefix) const {
    std::vector<std::shared_ptr<T>> result;
    std::shared_ptr<TrieNode> node = root;
    // Traverse to the node representing the prefix
    for (char c : prefix) {
      if (node->children.find(c) == node->children.end()) {
        return result;  // No elements with the given prefix
      }
      node = node->children[c];
    }
    // Gather all elements stored at or below this node
    retrieve_elements(node, result);
    return result;
  }

  // Retrieve elements with an exact key match
  std::vector<std::shared_ptr<T>> retrieve_with_exact(const std::string& key) const {
    std::vector<std::shared_ptr<T>> result;
    std::shared_ptr<TrieNode> node = root;
    // Traverse to the node representing the key
    for (char c : key) {
      if (node->children.find(c) == node->children.end()) {
        return result;  // No elements with the given key
      }
      node = node->children[c];
    }
    // Return elements stored at this node
    return node->elements;
  }

  // Remove the specified element from the TrieMap
  void remove(const std::shared_ptr<T>& element) { remove_element(root, element); }

  // Return the total number of elements stored in the TrieMap
  int size() const {
    int count = 0;
    count_elements(root, count);
    return count;
  }

  // Return a vector containing shared pointers to all elements stored in the TrieMap
  std::vector<std::shared_ptr<T>> get_all_elements() const {
    std::vector<std::shared_ptr<T>> result;
    get_all_elements_helper(root, result);
    return result;
  }

 private:
  // Recursive function to retrieve elements stored at or below the given node
  void retrieve_elements(std::shared_ptr<TrieNode> node,
                         std::vector<std::shared_ptr<T>>& result) const {
    // Add elements stored at this node to the result
    for (const auto& element : node->elements) {
      result.push_back(element);
    }
    // Recursively traverse children
    for (const auto& child : node->children) {
      retrieve_elements(child.second, result);
    }
  }

  // Recursive function to remove the specified element from the TrieMap
  bool remove_element(std::shared_ptr<TrieNode> node, const std::shared_ptr<T>& element) {
    // Remove the element if it exists at this node
    auto& elements = node->elements;
    auto it = std::find(elements.begin(), elements.end(), element);
    if (it != elements.end()) {
      elements.erase(it);
      return true;
    }
    // Recursively search children
    for (auto& child : node->children) {
      if (remove_element(child.second, element)) {
        // Remove child node if it's empty after removal
        if (child.second->elements.empty() && child.second->children.empty()) {
          node->children.erase(child.first);
        }
        return true;
      }
    }
    return false;
  }

  // Recursive function to count elements stored at or below the given node
  void count_elements(std::shared_ptr<TrieNode> node, int& count) const {
    // Increment count by the number of elements stored at this node
    count += node->elements.size();
    // Recursively traverse children
    for (const auto& child : node->children) {
      count_elements(child.second, count);
    }
  }

  // Recursive helper function to collect all elements stored in the TrieMap
  void get_all_elements_helper(std::shared_ptr<TrieNode> node,
                               std::vector<std::shared_ptr<T>>& result) const {
    // Add elements stored at this node to the result
    for (const auto& element : node->elements) {
      result.push_back(element);
    }
    // Recursively traverse children
    for (const auto& child : node->children) {
      get_all_elements_helper(child.second, result);
    }
  }
};

// TrieMap<std::string> trie_map;
//
//// Insert elements
// std::shared_ptr<std::string> inserted_element_1 = trie_map.insert("apple", "A fruit");
// std::shared_ptr<std::string> inserted_element_2 = trie_map.insert("app", "An application");
// std::shared_ptr<std::string> inserted_element_3 = trie_map.insert("banana", "Another fruit");
// std::shared_ptr<std::string> inserted_element_4 = trie_map.insert("apple", "Another apple");
//
//// Remove an element
// trie_map.remove(inserted_element_1);
//
//// Retrieve elements with a prefix
// std::vector<std::shared_ptr<std::string>> prefix_results = trie_map.retrieve_with_prefix("app");
