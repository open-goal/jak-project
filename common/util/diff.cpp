// Copyright 2005, Google Inc.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     * Redistributions of source code must retain the above copyright
// notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above
// copyright notice, this list of conditions and the following disclaimer
// in the documentation and/or other materials provided with the
// distribution.
//     * Neither the name of Google Inc. nor the names of its
// contributors may be used to endorse or promote products derived from
// this software without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

//
// The Google C++ Testing and Mocking Framework (Google Test)

#include "diff.h"

#include <algorithm>
#include <list>
#include <map>
#include <ostream>
#include <sstream>
#include <vector>

namespace {
enum EditType { kMatch, kAdd, kRemove, kReplace };
std::vector<EditType> CalculateOptimalEdits(const std::vector<size_t>& left,
                                            const std::vector<size_t>& right) {
  std::vector<std::vector<double> > costs(left.size() + 1, std::vector<double>(right.size() + 1));
  std::vector<std::vector<EditType> > best_move(left.size() + 1,
                                                std::vector<EditType>(right.size() + 1));

  // Populate for empty right.
  for (size_t l_i = 0; l_i < costs.size(); ++l_i) {
    costs[l_i][0] = static_cast<double>(l_i);
    best_move[l_i][0] = kRemove;
  }
  // Populate for empty left.
  for (size_t r_i = 1; r_i < costs[0].size(); ++r_i) {
    costs[0][r_i] = static_cast<double>(r_i);
    best_move[0][r_i] = kAdd;
  }

  for (size_t l_i = 0; l_i < left.size(); ++l_i) {
    for (size_t r_i = 0; r_i < right.size(); ++r_i) {
      if (left[l_i] == right[r_i]) {
        // Found a match. Consume it.
        costs[l_i + 1][r_i + 1] = costs[l_i][r_i];
        best_move[l_i + 1][r_i + 1] = kMatch;
        continue;
      }

      const double add = costs[l_i + 1][r_i];
      const double remove = costs[l_i][r_i + 1];
      const double replace = costs[l_i][r_i];
      if (add < remove && add < replace) {
        costs[l_i + 1][r_i + 1] = add + 1;
        best_move[l_i + 1][r_i + 1] = kAdd;
      } else if (remove < add && remove < replace) {
        costs[l_i + 1][r_i + 1] = remove + 1;
        best_move[l_i + 1][r_i + 1] = kRemove;
      } else {
        // We make replace a little more expensive than add/remove to lower
        // their priority.
        costs[l_i + 1][r_i + 1] = replace + 1.00001;
        best_move[l_i + 1][r_i + 1] = kReplace;
      }
    }
  }

  // Reconstruct the best path. We do it in reverse order.
  std::vector<EditType> best_path;
  for (size_t l_i = left.size(), r_i = right.size(); l_i > 0 || r_i > 0;) {
    EditType move = best_move[l_i][r_i];
    best_path.push_back(move);
    l_i -= move != kAdd;
    r_i -= move != kRemove;
  }
  std::reverse(best_path.begin(), best_path.end());
  return best_path;
}

// Helper class to convert string into ids with deduplication.
class InternalStrings {
 public:
  size_t GetId(const std::string& str) {
    IdMap::iterator it = ids_.find(str);
    if (it != ids_.end())
      return it->second;
    size_t id = ids_.size();
    return ids_[str] = id;
  }

 private:
  typedef std::map<std::string, size_t> IdMap;
  IdMap ids_;
};

std::vector<EditType> CalculateOptimalEdits(const std::vector<std::string>& left,
                                            const std::vector<std::string>& right) {
  std::vector<size_t> left_ids, right_ids;
  {
    InternalStrings intern_table;
    for (size_t i = 0; i < left.size(); ++i) {
      left_ids.push_back(intern_table.GetId(left[i]));
    }
    for (size_t i = 0; i < right.size(); ++i) {
      right_ids.push_back(intern_table.GetId(right[i]));
    }
  }
  return CalculateOptimalEdits(left_ids, right_ids);
}

constexpr char RESET_COLOR[] = "\x1B[0m";
constexpr char RED_COLOR[] = "\x1B[31m";
constexpr char GREEN_COLOR[] = "\x1B[32m";

// Helper class that holds the state for one hunk and prints it out to the
// stream.
// It reorders adds/removes when possible to group all removes before all
// adds. It also adds the hunk header before printint into the stream.
class Hunk {
 public:
  Hunk(size_t left_start, size_t right_start)
      : left_start_(left_start), right_start_(right_start), adds_(), removes_(), common_() {}

  void PushLine(char edit, const char* line) {
    switch (edit) {
      case ' ':
        ++common_;
        FlushEdits();
        hunk_.push_back(std::make_pair(' ', line));
        break;
      case '-':
        ++removes_;
        hunk_removes_.push_back(std::make_pair('-', line));
        break;
      case '+':
        ++adds_;
        hunk_adds_.push_back(std::make_pair('+', line));
        break;
    }
  }

  void PrintTo(std::ostream* os) {
    PrintHeader(os);
    FlushEdits();
    for (std::list<std::pair<char, const char*> >::const_iterator it = hunk_.begin();
         it != hunk_.end(); ++it) {
      // NOTE: this part is modified from gtest to give us pretty colored diffs.
      switch (it->first) {
        case '+':
          *os << GREEN_COLOR << it->first << it->second << RESET_COLOR << "\n";
          break;
        case '-':
          *os << RED_COLOR << it->first << it->second << RESET_COLOR << "\n";
          break;
        default:
          *os << it->first << it->second << "\n";
      }
    }
  }

  bool has_edits() const { return adds_ || removes_; }

 private:
  void FlushEdits() {
    hunk_.splice(hunk_.end(), hunk_removes_);
    hunk_.splice(hunk_.end(), hunk_adds_);
  }

  // Print a unified diff header for one hunk.
  // The format is
  //   "@@ -<left_start>,<left_length> +<right_start>,<right_length> @@"
  // where the left/right parts are omitted if unnecessary.
  void PrintHeader(std::ostream* ss) const {
    *ss << "@@ ";
    if (removes_) {
      *ss << "-" << left_start_ << "," << (removes_ + common_);
    }
    if (removes_ && adds_) {
      *ss << " ";
    }
    if (adds_) {
      *ss << "+" << right_start_ << "," << (adds_ + common_);
    }
    *ss << " @@\n";
  }

  size_t left_start_, right_start_;
  size_t adds_, removes_, common_;
  std::list<std::pair<char, const char*> > hunk_, hunk_adds_, hunk_removes_;
};

// Create a list of diff hunks in Unified diff format.
// Each hunk has a header generated by PrintHeader above plus a body with
// lines prefixed with ' ' for no change, '-' for deletion and '+' for
// addition.
// 'context' represents the desired unchanged prefix/suffix around the diff.
// If two hunks are close enough that their contexts overlap, then they are
// joined into one hunk.
std::string CreateUnifiedDiff(const std::vector<std::string>& left,
                              const std::vector<std::string>& right,
                              size_t context) {
  const std::vector<EditType> edits = CalculateOptimalEdits(left, right);

  size_t l_i = 0, r_i = 0, edit_i = 0;
  std::stringstream ss;
  while (edit_i < edits.size()) {
    // Find first edit.
    while (edit_i < edits.size() && edits[edit_i] == kMatch) {
      ++l_i;
      ++r_i;
      ++edit_i;
    }

    // Find the first line to include in the hunk.
    const size_t prefix_context = std::min(l_i, context);
    Hunk hunk(l_i - prefix_context + 1, r_i - prefix_context + 1);
    for (size_t i = prefix_context; i > 0; --i) {
      hunk.PushLine(' ', left[l_i - i].c_str());
    }

    // Iterate the edits until we found enough suffix for the hunk or the input
    // is over.
    size_t n_suffix = 0;
    for (; edit_i < edits.size(); ++edit_i) {
      if (n_suffix >= context) {
        // Continue only if the next hunk is very close.
        auto it = edits.begin() + static_cast<int>(edit_i);
        while (it != edits.end() && *it == kMatch)
          ++it;
        if (it == edits.end() || static_cast<size_t>(it - edits.begin()) - edit_i >= context) {
          // There is no next edit or it is too far away.
          break;
        }
      }

      EditType edit = edits[edit_i];
      // Reset count when a non match is found.
      n_suffix = edit == kMatch ? n_suffix + 1 : 0;

      if (edit == kMatch || edit == kRemove || edit == kReplace) {
        hunk.PushLine(edit == kMatch ? ' ' : '-', left[l_i].c_str());
      }
      if (edit == kAdd || edit == kReplace) {
        hunk.PushLine('+', right[r_i].c_str());
      }

      // Advance indices, depending on edit type.
      l_i += edit != kAdd;
      r_i += edit != kRemove;
    }

    if (!hunk.has_edits()) {
      // We are done. We don't want this hunk.
      break;
    }

    hunk.PrintTo(&ss);
  }
  return ss.str();
}

std::vector<std::string> SplitString(const ::std::string& str, char delimiter = '\n') {
  ::std::vector< ::std::string> parsed;
  ::std::string::size_type pos = 0;
  while (true) {
    const ::std::string::size_type colon = str.find(delimiter, pos);
    if (colon == ::std::string::npos) {
      parsed.push_back(str.substr(pos));
      break;
    } else {
      parsed.push_back(str.substr(pos, colon - pos));
      pos = colon + 1;
    }
  }
  return parsed;
}

}  // namespace

namespace google_diff {
std::string diff_strings(const std::string& lhs, const std::string& rhs) {
  if (!lhs.empty() && !rhs.empty()) {
    const std::vector<std::string> lhs_lines = SplitString(lhs);
    const std::vector<std::string> rhs_lines = SplitString(rhs);
    if (lhs_lines.size() > 1 || rhs_lines.size() > 1) {
      return CreateUnifiedDiff(lhs_lines, rhs_lines, 2);
    }
  }
  return "";
}

std::vector<std::string> split_string(const ::std::string& str, char delimiter) {
  return SplitString(str, delimiter);
}
}  // namespace google_diff
