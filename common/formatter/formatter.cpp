#include "formatter.h"

#include "formatter_tree.h"

#include "common/log/log.h"
#include "common/util/FileUtil.h"
#include "common/util/string_util.h"

#include "tree_sitter/api.h"

#include "third-party/fmt/core.h"

// Declare the `tree_sitter_opengoal` function, which is
// implemented by the `tree-sitter-opengoal` library.
extern "C" {
extern const TSLanguage* tree_sitter_opengoal();
}

std::string apply_formatting(
    const FormatterTreeNode& curr_node,
    std::string output,
    std::optional<formatter_rules::config::FormFormattingConfig> form_element_config) {
  using namespace formatter_rules;
  if (!curr_node.token && curr_node.refs.empty()) {
    return output;
  }
  std::string curr_form = "";
  // Print the token
  if (curr_node.token) {
    curr_form += curr_node.token.value();
    return curr_form;
  }
  if (!curr_node.metadata.is_top_level) {
    curr_form += "(";
  }
  // Iterate the form

  bool inline_form = false;
  // Also check if the form should be constant-paired
  const bool constant_pair_form = constant_pairs::form_should_be_constant_paired(curr_node);
  if (!constant_pair_form) {
    // Determine if the form should be inlined or hung/flowed
    // TODO - this isn't entirely accurate, needs current cursor positioning (which is tricky
    // because recursion!)
    inline_form = indent::form_can_be_inlined(curr_form, curr_node);
  }
  const bool flowing = indent::should_form_flow(curr_node, inline_form);
  std::optional<formatter_rules::config::FormFormattingConfig> form_config;
  if (!curr_node.refs.empty() && curr_node.refs.at(0).token) {
    const auto& form_head = curr_node.refs.at(0).token;
    if (form_head && config::opengoal_form_config.find(form_head.value()) !=
                         config::opengoal_form_config.end()) {
      form_config = config::opengoal_form_config.at(form_head.value());
    }
  }
  // TODO - might want to make some kind of per-form config struct, simplify the passing around of
  // info below
  for (int i = 0; i < curr_node.refs.size(); i++) {
    const auto& ref = curr_node.refs.at(i);
    // Figure out if the element should be inlined or not
    bool inline_element = inline_form;
    if (indent::inline_form_element(curr_node, i)) {
      inline_element = indent::inline_form_element(curr_node, i).value();
    }
    // Append a newline if needed
    // TODO - cleanup / move
    bool is_binding_list = false;
    bool force_newline = false;
    bool override_force_flow = false;
    if (form_config) {
      force_newline = std::find(form_config->force_newline_at_indices.begin(),
                                form_config->force_newline_at_indices.end(),
                                i) != form_config->force_newline_at_indices.end();
      // Check if it's a small enough binding list, if so we don't force a newline if the element
      // can be inlined
      if (inline_element && i > 0 && form_config->bindings_at_index == i - 1 &&
          curr_node.refs.at(i - 1).refs.size() < form_config->allow_inlining_if_size_less_than) {
        force_newline = false;
        override_force_flow = true;
      }
      is_binding_list = form_config->bindings_at_index == i;
    }

    if (!curr_node.metadata.is_top_level &&
        (!inline_element || is_binding_list || force_newline ||
         (form_element_config && form_element_config->force_flow))) {
      indent::append_newline(curr_form, ref, curr_node, i, flowing, constant_pair_form,
                             (form_element_config && form_element_config->force_flow));
    }
    // TODO - indent the line (or don't)
    // Either print the element's token, or recursively format it as well
    if (ref.token) {
      // TODO depth hard-coded to 1, i think this can be removed, since
      // forms are always done bottom-top recursively, they always act
      // independently as if it was the shallowest depth
      if (!inline_element || force_newline) {
        indent::indent_line(curr_form, ref, curr_node, 1, i, flowing);
      }
      if (ref.metadata.node_type == "comment" && ref.metadata.is_inline) {
        curr_form += " " + ref.token.value();
      } else if (ref.metadata.node_type == "block_comment") {
        curr_form += comments::format_block_comment(ref.token.value());
      } else {
        curr_form += ref.token.value();
      }
      if (!curr_node.metadata.is_top_level) {
        curr_form += " ";
      }
    } else {
      // See if the item at this position has specific formatting
      std::optional<formatter_rules::config::FormFormattingConfig> config = {};
      std::string formatted_form;
      if (form_config && form_config->index_configs.find(i) != form_config->index_configs.end()) {
        formatted_form = apply_formatting(ref, "", *form_config->index_configs.at(i));
      } else {
        formatted_form = apply_formatting(ref, "", {});
      }
      // TODO - align inner lines only
      if (!curr_node.metadata.is_top_level) {
        indent::align_lines(
            formatted_form, ref, curr_node, constant_pair_form, flowing,
            (!override_force_flow && form_config && i >= form_config->start_flow_at_index),
            inline_element);
      }
      curr_form += formatted_form;
      if (!curr_node.metadata.is_top_level) {
        curr_form += " ";
      }
    }
    // Handle blank lines at the top level, skip if it's the final element
    blank_lines::separate_by_newline(curr_form, curr_node, ref, i);
  }
  if (!curr_node.metadata.is_top_level) {
    curr_form = str_util::rtrim(curr_form) + ")";
  }
  return curr_form;
}

std::optional<std::string> formatter::format_code(const std::string& source) {
  // Create a parser.
  std::shared_ptr<TSParser> parser(ts_parser_new(), TreeSitterParserDeleter());

  // Set the parser's language (JSON in this case).
  ts_parser_set_language(parser.get(), tree_sitter_opengoal());

  // Build a syntax tree based on source code stored in a string.
  std::shared_ptr<TSTree> tree(
      ts_parser_parse_string(parser.get(), NULL, source.c_str(), source.length()),
      TreeSitterTreeDeleter());

  // Get the root node of the syntax tree.
  TSNode root_node = ts_tree_root_node(tree.get());
  if (ts_node_is_null(root_node) || ts_node_has_error(root_node)) {
    return std::nullopt;
  }

  try {
    const auto formatting_tree = FormatterTree(source, root_node);
    std::string formatted_code = apply_formatting(formatting_tree.root, "", {});
    return formatted_code;
  } catch (std::exception& e) {
    lg::error("Unable to format code - {}", e.what());
  }

  return std::nullopt;
}
