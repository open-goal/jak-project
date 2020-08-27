#ifndef JAK_REGALLOCINSTR_H
#define JAK_REGALLOCINSTR_H

#include <string>
#include <vector>
#include <utility>
//#include "goal/GoalEnv.h"
#include "codegen/ColoringAssignment.h"

struct RegAllocInstr {
  //  std::vector<ColoringInput> clobber;
  // order of ops:
  // read, clobber, write,
  std::vector<ColoringAssignment> clobber;
  std::vector<ColoringAssignment> exclusive;
  std::vector<ColoringInput> write;
  std::vector<ColoringInput> read;
  std::vector<int> jumps;
  bool fallthrough = true;
  bool is_move = false;

  std::string print() {
    std::string result = "(";

    bool first = true;

    if (!write.empty()) {
      first = false;
      result += "(write";
      for (auto& i : write) {
        result += " " + i.print();
      }
      result += ")";
    }

    if (!read.empty()) {
      if (!first) {
        result += " ";
      }
      first = false;
      result += "(read";
      for (auto& i : read) {
        result += " " + i.print();
      }
      result += ")";
    }

    if (!clobber.empty()) {
      if (!first) {
        result += " ";
      }
      first = false;
      result += "(clobber";
      for (auto& i : clobber) {
        result += " " + i.print();
      }
      result += ")";
    }

    if (!jumps.empty()) {
      if (!first) {
        result += " ";
      }
      first = false;
      result += "(jumps";
      for (auto& i : jumps) {
        result += " " + std::to_string(i);
      }
      result += ")";
    }
    result += ")";
    return result;
  }

  bool reads(int id) {
    for (const auto& x : read) {
      if (x.id == id)
        return true;
    }
    return false;
  }

  bool writes(int id) {
    for (const auto& x : write) {
      if (x.id == id)
        return true;
    }
    return false;
  }
};

#endif  // JAK_REGALLOCINSTR_H
