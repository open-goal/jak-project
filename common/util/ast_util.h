#pragma once

#include <string>
#include <vector>

#include "tree_sitter/api.h"

namespace ast_util {
std::string get_source_code(const std::string& source, const TSNode& node);
void search_for_forms_that_begin_with(const std::string& source,
                                      const TSNode curr_node,
                                      const std::vector<std::string>& prefix,
                                      std::vector<TSNode>& results);

}  // namespace ast_util
