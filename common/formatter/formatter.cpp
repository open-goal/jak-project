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

int hang_indentation_width(const FormatterTreeNode& curr_node) {
  if (curr_node.token || curr_node.refs.empty()) {
    return 0;
  }
  // Get the first element of the form
  const auto& first_elt = curr_node.refs.at(0);
  if (first_elt.token) {
    return first_elt.token->length() +
           2;  // +2 because the opening paren and then the following space
  }
  // Otherwise, continue nesting
  return 1 + hang_indentation_width(first_elt);
}

// TODO - compute length of each node and store it
void apply_formatting_config(
    FormatterTreeNode& curr_node,
    std::optional<std::shared_ptr<formatter_rules::config::FormFormattingConfig>>
        config_from_parent = {}) {
  using namespace formatter_rules;
  // node is empty, base-case
  if (curr_node.token || curr_node.refs.empty()) {
    return;
  }
  // first, check to see if this form already has a predefined formatting configuration
  // if it does, that simplifies things because there is only 1 way of formatting the form
  std::optional<formatter_rules::config::FormFormattingConfig> predefined_config;
  if (!config_from_parent && !curr_node.refs.empty() && curr_node.refs.at(0).token) {
    const auto& form_head = curr_node.refs.at(0).token;
    if (form_head && config::opengoal_form_config.find(form_head.value()) !=
                         config::opengoal_form_config.end()) {
      predefined_config = config::opengoal_form_config.at(form_head.value());
      curr_node.formatting_config = predefined_config.value();
    }
  } else if (config_from_parent) {
    predefined_config = *config_from_parent.value();
    curr_node.formatting_config = predefined_config.value();
  }
  // In order to keep things simple, as well as because its ineffectual in lisp code (you can only
  // enforce it so much without making things unreadable), line width will not matter for deciding
  // whether or not to hang or flow the form
  //
  // This means that a hang would ALWAYS win, because it's 1 less line break.  Therefore this
  // simplifies our approach there is no need to explore both braches to see which one would be
  // preferred.
  //
  // Instead, we either use the predefined configuration (obviously) or we do some checks for some
  // outlier conditions to see if things should be formatted differently
  //
  // Otherwise, we always default to a hang.
  //
  // NOTE - any modifications here to child elements could be superseeded later in the recursion
  // in order to maintain your sanity, only modify things here that _arent_ touched by default
  // configurations.  These are explicitly prepended with `parent_mutable_`
  if (!predefined_config) {
    if (curr_node.metadata.is_top_level) {
      curr_node.formatting_config.indentation_width = 0;
      curr_node.formatting_config.hang_forms = false;
    } else if (constant_list::is_constant_list(curr_node)) {
      // - Check if the form is a constant list (ie. a list of numbers)
      curr_node.formatting_config.indentation_width = 1;
      curr_node.formatting_config.hang_forms = false;
      curr_node.formatting_config.has_constant_pairs =
          constant_pairs::form_should_be_constant_paired(curr_node);
      // If applicable, iterate through the constant pairs, since we can potentially pair up
      // non-constant second elements in a pair (like a function call), there is the potential that
      // they need to spill to the next line and get indented in extra.  This is an exceptional
      // circumstance, we do NOT do this sort of thing when formatting normal forms (cond/case pairs
      // are another similar situation)
      if (curr_node.formatting_config.has_constant_pairs) {
        for (int i = 0; i < (int)curr_node.refs.size(); i++) {
          auto& child_ref = curr_node.refs.at(i);
          const auto type = child_ref.metadata.node_type;
          if (constant_types.find(type) == constant_types.end() &&
              constant_pairs::is_element_second_in_constant_pair(curr_node, child_ref, i)) {
            child_ref.formatting_config.parent_mutable_extra_indent = 2;
          }
        }
      }

    } else if (curr_node.formatting_config.hang_forms && curr_node.refs.size() > 1 &&
               curr_node.refs.at(1).metadata.is_comment) {
      // - Check if the second argument is a comment, it looks better if we flow instead
      curr_node.formatting_config.hang_forms = false;
    }
  }
  // If we are hanging, lets determine the indentation width since it is based on the form itself
  if (curr_node.formatting_config.hang_forms) {
    curr_node.formatting_config.indentation_width = hang_indentation_width(curr_node);
  }
  // iterate through the refs
  for (int i = 0; i < (int)curr_node.refs.size(); i++) {
    auto& ref = curr_node.refs.at(i);
    if (!ref.token) {
      // If the child has a pre-defined configuration at that index, we pass it along
      if (predefined_config &&
          predefined_config->index_configs.find(i) != predefined_config->index_configs.end()) {
        apply_formatting_config(ref, predefined_config->index_configs.at(i));
      } else {
        apply_formatting_config(ref);
      }
    }
  }
}

int get_total_form_inlined_width(const FormatterTreeNode& curr_node) {
  if (curr_node.token) {
    return curr_node.token->length();
  }
  int width = 1;
  for (const auto& ref : curr_node.refs) {
    width += get_total_form_inlined_width(ref);
  }
  return width + 1;
}

bool form_contains_comment(const FormatterTreeNode& curr_node) {
  if (curr_node.metadata.is_comment) {
    return true;
  }
  for (const auto& ref : curr_node.refs) {
    const auto contains_comment = form_contains_comment(ref);
    if (contains_comment) {
      return true;
    }
  }
  return false;
}

bool form_contains_node_that_prevents_inlining(const FormatterTreeNode& curr_node) {
  if (curr_node.formatting_config.should_prevent_inlining(curr_node.formatting_config,
                                                          curr_node.refs.size())) {
    return true;
  }
  for (const auto& ref : curr_node.refs) {
    const auto prevents_inlining = form_contains_node_that_prevents_inlining(ref);
    if (prevents_inlining) {
      return true;
    }
  }
  return false;
}

bool can_node_be_inlined(const FormatterTreeNode& curr_node, int cursor_pos) {
  using namespace formatter_rules;
  // First off, we cannot inline the top level
  if (curr_node.metadata.is_top_level) {
    return false;
  }
  // If the config explicitly prevents inlining, or it contains a sub-node that prevents inlining
  if (curr_node.formatting_config.prevent_inlining ||
      form_contains_node_that_prevents_inlining(curr_node)) {
    return false;
  }
  // nor can we inline something that contains a comment in the middle
  if (form_contains_comment(curr_node)) {
    return false;
  }
  // constant pairs are not inlined!
  if (curr_node.formatting_config.has_constant_pairs) {
    return false;
  }
  // If this is set in the config, then the form is intended to be partially inlined
  if (curr_node.formatting_config.inline_until_index != -1) {
    return false;
  }
  // let's see if we can inline the form all on one line to do that, we recursively explore
  // the form to find the total width
  int line_width = cursor_pos + get_total_form_inlined_width(curr_node);
  return line_width <= indent::line_width_target;  // TODO - comments
}

std::vector<std::string> apply_formatting(const FormatterTreeNode& curr_node,
                                          std::vector<std::string> output = {},
                                          int cursor_pos = 0) {
  using namespace formatter_rules;
  if (!curr_node.token && curr_node.refs.empty()) {
    return output;
  }

  // If its a token, just print the token and move on
  if (curr_node.token) {
    return {curr_node.token.value()};
  }

  bool inline_form = can_node_be_inlined(curr_node, cursor_pos);
  // TODO - also if the form is inlinable, we can skip all the complication below and just...inline
  // it!
  // TODO - should figure out the inlining here as well, instead of the bool above

  // Iterate the form, building up a list of the final lines but don't worry about indentation
  // at this stage.  Once the lines are finalized, it's easy to add the indentation later
  //
  // This means we may combine elements onto the same line in this step.
  std::vector<std::string> form_lines = {};
  for (int i = 0; i < (int)curr_node.refs.size(); i++) {
    const auto& ref = curr_node.refs.at(i);
    // Add new line entry
    if (ref.token) {
      // Cleanup block-comments
      std::string val = ref.token.value();
      if (ref.metadata.node_type == "block_comment") {
        // TODO - change this sanitization to return a list of lines instead of a single new-lined
        // line
        val = comments::format_block_comment(ref.token.value());
      }
      form_lines.push_back(val);
    } else {
      // If it's not a token, we have to recursively build up the form
      // TODO - add the cursor_pos here
      const auto& lines = apply_formatting(ref, {}, cursor_pos);
      for (int i = 0; i < (int)lines.size(); i++) {
        const auto& line = lines.at(i);
        form_lines.push_back(fmt::format(
            "{}{}", str_util::repeat(ref.formatting_config.parent_mutable_extra_indent, " "),
            line));
      }
    }
    // If we are hanging forms, combine the first two forms onto the same line
    if (i == (int)curr_node.refs.size() - 1 && form_lines.size() > 1 &&
        (curr_node.formatting_config.hang_forms ||
         curr_node.formatting_config.combine_first_two_lines)) {
      form_lines.at(0) += fmt::format(" {}", form_lines.at(1));
      form_lines.erase(form_lines.begin() + 1);
    } else if ((i + 1) < (int)curr_node.refs.size()) {
      const auto& next_ref = curr_node.refs.at(i + 1);
      // combine the next inline comment or constant pair
      if ((next_ref.metadata.node_type == "comment" && next_ref.metadata.is_inline) ||
          (curr_node.formatting_config.has_constant_pairs &&
           constant_pairs::is_element_second_in_constant_pair(curr_node, next_ref, i + 1))) {
        if (next_ref.token) {
          form_lines.at(form_lines.size() - 1) += fmt::format(" {}", next_ref.token.value());
          i++;
        } else if (can_node_be_inlined(next_ref, cursor_pos)) {
          const auto& lines = apply_formatting(next_ref, {}, cursor_pos);  // TODO - cursor pos
          for (const auto& line : lines) {
            form_lines.at(form_lines.size() - 1) += fmt::format(" {}", line);
          }
          i++;
        }
      }
    }
    // If we are at the top level, potential separate with a new line
    if (blank_lines::should_insert_blank_line(curr_node, ref, i)) {
      form_lines.at(form_lines.size() - 1) += "\n";
    }
  }

  // Consolidate any lines if the configuration requires it
  if (curr_node.formatting_config.inline_until_index != -1) {
    std::vector<std::string> new_form_lines = {};
    for (int i = 0; i < (int)form_lines.size(); i++) {
      if (i < curr_node.formatting_config.inline_until_index) {
        if (new_form_lines.empty()) {
          new_form_lines.push_back(form_lines.at(i));
        } else {
          new_form_lines.at(0) += fmt::format(" {}", form_lines.at(i));
        }
      } else {
        new_form_lines.push_back(form_lines.at(i));
      }
    }
    form_lines = new_form_lines;
  }

  // Apply necessary indentation to each line and add parens
  if (!curr_node.metadata.is_top_level) {
    std::string form_surround_start = "(";
    std::string form_surround_end = ")";
    form_lines[0] = fmt::format("{}{}", form_surround_start, form_lines[0]);
    form_lines[form_lines.size() - 1] =
        fmt::format("{}{}", form_lines[form_lines.size() - 1], form_surround_end);
  }
  std::string curr_form = "";
  if (curr_node.formatting_config.parent_mutable_extra_indent > 0) {
    curr_form += str_util::repeat(curr_node.formatting_config.parent_mutable_extra_indent, " ");
  }
  if (inline_form) {
    form_lines = {fmt::format("{}", fmt::join(form_lines, " "))};
  } else {
    for (int i = 0; i < (int)form_lines.size(); i++) {
      if (i > 0) {
        auto& line = form_lines.at(i);
        line = fmt::format("{}{}",
                           str_util::repeat(curr_node.formatting_config.indentation_width_for_index(
                                                curr_node.formatting_config, i),
                                            " "),
                           line);
      }
    }
  }
  return form_lines;
}

std::string join_formatted_lines(const std::vector<std::string> lines) {
  // TODO - respect original file line endings?
  return fmt::format("{}", fmt::join(lines, "\n"));
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
    // There are three phases of formatting
    // 1. Simplify the AST down to something that is easier to work on from a formatting perspective
    // this also gathers basic metadata that can be done at this stage, like if the token is a
    // comment or if the form is on the top-level
    auto formatting_tree = FormatterTree(source, root_node);
    // 2. Recursively iterate through this simplified FormatterTree and figure out what rules
    // need to be applied to produce an optimal result
    apply_formatting_config(formatting_tree.root);
    // 3. Use this updated FormatterTree to print out the final source-code, while doing so
    // we may deviate from the optimal result to produce something even more optimal by inlining
    // forms that can fit within the line width.
    const auto formatted_lines = apply_formatting(formatting_tree.root);
    // 4. Now we joint he lines together, it's easier when formatting to leave all lines independent
    // so adding indentation is easier
    const auto formatted_source = join_formatted_lines(formatted_lines);
    return formatted_source;
  } catch (std::exception& e) {
    lg::error("Unable to format code - {}", e.what());
  }

  return std::nullopt;
}
