#pragma once

#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Env.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"

namespace decompiler {

struct LetStats {
  int total_vars = 0;
  int vars_in_lets = 0;

  void operator+=(const LetStats& other) {
    total_vars += other.total_vars;
    vars_in_lets += other.vars_in_lets;
  }
};

LetStats insert_lets(const Function& func,
                     Env& env,
                     FormPool& pool,
                     Form* top_level_form,
                     LetRewriteStats& let_stats);

}  // namespace decompiler
