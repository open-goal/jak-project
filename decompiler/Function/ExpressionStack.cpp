#include "third-party/fmt/core.h"
#include "ExpressionStack.h"

namespace decompiler {
std::string ExpressionStack::StackEntry::print(LinkedObjectFile& file) {
  return fmt::format("d: {} s: {} | {} <- {}", display, sequence_point,
                     destination.has_value() ? destination.value().to_charp() : "N/A",
                     source->print(file));
}

std::string ExpressionStack::print(LinkedObjectFile& file) {
  std::string result;
  for (auto& x : m_stack) {
    result += x.print(file);
    result += '\n';
  }
  return result;
}

void ExpressionStack::set(Register reg, std::shared_ptr<IR> value, bool sequence_point) {
  StackEntry entry;
  entry.display = true;  // by default, we should display everything!
  entry.sequence_point = sequence_point;
  entry.destination = reg;
  entry.source = std::move(value);
  m_stack.push_back(entry);
}

bool ExpressionStack::is_single_expression() {
  int count = 0;
  for (auto& e : m_stack) {
    if (e.display) {
      count++;
    }
  }
  return count == 1;
}

void ExpressionStack::add_no_set(std::shared_ptr<IR> value, bool sequence_point) {
  StackEntry entry;
  entry.display = true;
  entry.destination = std::nullopt;
  entry.source = value;
  entry.sequence_point = sequence_point;
  m_stack.push_back(entry);
}

/*!
 * "Remove" an entry from the stack. Cannot cross a sequence point.
 * Internally, the entry is still stored. It is just flagged with display=false.
 */
std::shared_ptr<IR> ExpressionStack::get(Register reg) {
  for (size_t i = m_stack.size(); i-- > 0;) {
    auto& entry = m_stack.at(i);
    if (entry.display) {
      if (entry.destination == reg) {
        entry.display = false;
        return entry.source;
      } else {
        // we didn't match
        if (entry.sequence_point) {
          // and it's a sequence point! can't look any more back than this.
          return std::make_shared<IR_Register>(reg, -1);
        }
      }
    }
  }
  return std::make_shared<IR_Register>(reg, -1);
}

/*!
 * Convert the stack into a sequence of compacted expressions.
 * This is final result of the expression compaction algorithm.
 */
std::vector<std::shared_ptr<IR>> ExpressionStack::get_result() {
  std::vector<std::shared_ptr<IR>> result;

  for (auto& e : m_stack) {
    if (!e.display) {
      continue;
    }
    if (e.destination.has_value()) {
      auto dst_reg = std::make_shared<IR_Register>(e.destination.value(), -1);
      auto op = std::make_shared<IR_Set>(IR_Set::EXPR, dst_reg, e.source);
      result.push_back(op);
    } else {
      result.push_back(e.source);
    }
  }

  return result;
}

bool ExpressionStack::display_stack_empty() {
  for (auto& e : m_stack) {
    if (e.display) {
      return false;
    }
  }
  return true;
}

ExpressionStack::StackEntry& ExpressionStack::get_display_stack_top() {
  for (size_t i = m_stack.size(); i-- > 0;) {
    auto& entry = m_stack.at(i);
    if (entry.display) {
      return entry;
    }
  }
  assert(false);
}
}  // namespace decompiler