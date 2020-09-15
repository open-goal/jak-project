#pragma once

#ifndef JAK_CODEGENERATOR_H
#define JAK_CODEGENERATOR_H

#include "Env.h"
#include "goalc/emitter/ObjectGenerator.h"

class CodeGenerator {
 public:
  CodeGenerator(FileEnv* env);
  std::vector<u8> run();

 private:
  void do_function(FunctionEnv* env, int f_idx);
  emitter::ObjectGenerator m_gen;
  FileEnv* m_fe;
};

#endif  // JAK_CODEGENERATOR_H
