/*!
 * @file CodeGenerator.h
 * Generate object files from a FileEnv using an emitter::ObjectGenerator.
 * Populates a DebugInfo.
 * Currently owns the logic for emitting the function prologues.
 */

#pragma once

#include "Env.h"

#include "common/versions/versions.h"

#include "goalc/emitter/ObjectGenerator.h"

class DebugInfo;
class TypeSystem;

class CodeGenerator {
 public:
  CodeGenerator(FileEnv* env, DebugInfo* debug_info, GameVersion version);
  std::vector<u8> run(const TypeSystem* ts);
  emitter::ObjectGeneratorStats get_obj_stats() const { return m_gen.get_stats(); }

 private:
  void do_function(FunctionEnv* env, int f_idx);
  void do_goal_function(FunctionEnv* env, int f_idx);
  void do_asm_function(FunctionEnv* env, int f_idx, bool allow_saved_regs);
  emitter::ObjectGenerator m_gen;
  FileEnv* m_fe = nullptr;
  DebugInfo* m_debug_info = nullptr;
};
