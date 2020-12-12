#include "third-party/fmt/core.h"
#include "ExpressionStack.h"

std::string ExpressionStack::StackEntry::print(LinkedObjectFile& file) {
  return fmt::format("d: {} {} <- {}", display, destination.to_charp(), source->print(file));
}

std::string ExpressionStack::print(LinkedObjectFile& file) {
  std::string result;
  for (auto& x : m_stack) {
    result += x.print(file);
    result += '\n';
  }
  return result;
}

void ExpressionStack::set(Register reg, std::shared_ptr<IR> value) {
  StackEntry entry;
  entry.display = true;  // by default, we should display everything!
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

std::shared_ptr<IR> ExpressionStack::get(Register reg) {
  // see if the stack top is this register...
  if (!display_stack_empty()) {
    auto& top = get_display_stack_top();
    if (top.destination == reg) {
      // yep. We can compact!
      top.display = false;
      return top.source;
    }
  }
  return std::make_shared<IR_Register>(reg, -1);
}

std::vector<std::shared_ptr<IR>> ExpressionStack::get_result() {
  std::vector<std::shared_ptr<IR>> result;

  for (auto& e : m_stack) {
    if (!e.display) {
      continue;
    }
    auto dst_reg = std::make_shared<IR_Register>(e.destination, -1);
    auto op = std::make_shared<IR_Set>(IR_Set::EXPR, dst_reg, e.source);
    result.push_back(op);
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