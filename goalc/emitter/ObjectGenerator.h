/*!
 * @file ObjectGenerator.h
 * Generates GOAL object files with linking and debug data.
 */

#pragma once

#ifndef JAK_OBJECTGENERATOR_H
#define JAK_OBJECTGENERATOR_H

#include <cstring>
#include <map>
#include <string>
#include "ObjectFileData.h"
#include "Instruction.h"
#include "goalc/debugger/DebugInfo.h"

struct FunctionDebugInfo;
class TypeSystem;

namespace emitter {

/*!
 * A reference to a function added.
 */
struct FunctionRecord {
  FunctionDebugInfo* debug = nullptr;
  int seg = -1;
  int func_id = -1;
};

/*!
 * A reference to an IR instruction.
 */
struct IR_Record {
  int seg = -1;
  int func_id = -1;
  int ir_id = -1;
};

/*!
 * A reference to an x86 instruction
 */
struct InstructionRecord {
  int seg = -1;
  int func_id = -1;
  int ir_id = -1;
  int instr_id = -1;
};

/*!
 * A reference to static data.
 */
struct StaticRecord {
  int seg = -1;
  int static_id = -1;
};

class ObjectGenerator {
 public:
  ObjectGenerator() = default;
  ObjectFileData generate_data_v3(const TypeSystem* ts);

  FunctionRecord add_function_to_seg(int seg,
                                     FunctionDebugInfo* debug,
                                     int min_align = 16);  // should align and insert function tag
  FunctionRecord get_existing_function_record(int f_idx);
  IR_Record add_ir(const FunctionRecord& func, const std::string& debug_print);
  IR_Record get_future_ir_record(const FunctionRecord& func, int ir_id);
  IR_Record get_future_ir_record_in_same_func(const IR_Record& irec, int ir_id);
  InstructionRecord add_instr(Instruction inst, IR_Record ir);
  void add_instr_no_ir(FunctionRecord func, Instruction inst, InstructionInfo::Kind kind);
  StaticRecord add_static_to_seg(int seg, int min_align = 16);
  std::vector<u8>& get_static_data(const StaticRecord& rec);
  void link_instruction_jump(InstructionRecord jump_instr, IR_Record destination);
  void link_static_type_ptr(StaticRecord rec, int offset, const std::string& type_name);

  void link_instruction_symbol_mem(const InstructionRecord& rec, const std::string& name);
  void link_instruction_symbol_ptr(const InstructionRecord& rec, const std::string& name);
  void link_static_symbol_ptr(StaticRecord rec, int offset, const std::string& name);
  void link_static_pointer(const StaticRecord& source,
                           int source_offset,
                           const StaticRecord& dest,
                           int dest_offset);
  void link_instruction_static(const InstructionRecord& instr,
                               const StaticRecord& target_static,
                               int offset);
  void link_instruction_to_function(const InstructionRecord& instr,
                                    const FunctionRecord& target_func);

 private:
  void handle_temp_static_type_links(int seg);
  void handle_temp_jump_links(int seg);
  void handle_temp_instr_sym_links(int seg);
  void handle_temp_static_sym_links(int seg);
  void handle_temp_rip_data_links(int seg);
  void handle_temp_rip_func_links(int seg);
  void handle_temp_static_ptr_links(int seg);

  void emit_link_table(int seg, const TypeSystem* ts);
  void emit_link_type_pointer(int seg, const TypeSystem* ts);
  void emit_link_symbol(int seg);
  void emit_link_rip(int seg);
  void emit_link_ptr(int seg);
  std::vector<u8> generate_header_v3();

  template <typename T>
  u64 insert_data(int seg, const T& x) {
    auto& data = m_data_by_seg.at(seg);
    auto insert_location = data.size();
    data.resize(insert_location + sizeof(T));
    memcpy(data.data() + insert_location, &x, sizeof(T));
    return insert_location;
  }

  template <typename T>
  void patch_data(int seg, int offset, const T& x) {
    auto& data = m_data_by_seg.at(seg);
    assert(offset >= 0);
    assert(offset + sizeof(T) <= data.size());
    memcpy(data.data() + offset, &x, sizeof(T));
  }

  struct FunctionData {
    std::vector<Instruction> instructions;
    std::vector<int> ir_to_instruction;
    std::vector<int> instruction_to_byte_in_data;
    int min_align = 16;
    FunctionDebugInfo* debug = nullptr;
  };

  struct StaticData {
    std::vector<u8> data;
    int min_align = 16;
    int location = -1;
  };

  struct StaticTypeLink {
    StaticRecord rec;
    int offset = -1;
  };

  struct StaticSymbolLink {
    StaticRecord rec;
    int offset = -1;
  };

  struct StaticPointerLink {
    StaticRecord source;
    int offset_in_source = -1;
    StaticRecord dest;
    int offset_in_dest = -1;
  };

  struct SymbolInstrLink {
    InstructionRecord rec;
    bool is_mem_access = false;
  };

  struct RipFuncLink {
    InstructionRecord instr;
    FunctionRecord target;
  };

  struct RipDataLink {
    InstructionRecord instr;
    StaticRecord data;
    int offset = -1;
  };

  struct RipLink {
    InstructionRecord instr;
    int target_segment = -1;
    int offset_in_segment = -1;
  };

  struct JumpLink {
    InstructionRecord jump_instr;
    IR_Record dest;
  };

  struct PointerLink {
    int segment = -1;
    // both in bytes.
    int source = -1;
    int dest = -1;
  };

  template <typename T>
  using seg_vector = std::array<std::vector<T>, N_SEG>;

  template <typename T>
  using seg_map = std::array<std::map<std::string, std::vector<T>>, N_SEG>;

  // final data
  seg_vector<u8> m_data_by_seg;
  seg_vector<u8> m_link_by_seg;

  // temp data
  seg_vector<FunctionData> m_function_data_by_seg;
  seg_vector<StaticData> m_static_data_by_seg;

  // temp link stuff
  seg_map<StaticTypeLink> m_static_type_temp_links_by_seg;
  seg_vector<JumpLink> m_jump_temp_links_by_seg;
  seg_map<SymbolInstrLink> m_symbol_instr_temp_links_by_seg;
  seg_map<StaticSymbolLink> m_static_sym_temp_links_by_seg;
  seg_vector<StaticPointerLink> m_static_temp_ptr_links_by_seg;
  seg_vector<RipFuncLink> m_rip_func_temp_links_by_seg;
  seg_vector<RipDataLink> m_rip_data_temp_links_by_seg;

  // final link stuff
  seg_map<int> m_type_ptr_links_by_seg;
  seg_map<int> m_sym_links_by_seg;
  seg_vector<RipLink> m_rip_links_by_seg;
  seg_vector<PointerLink> m_pointer_links_by_seg;

  std::vector<FunctionRecord> m_all_function_records;
};
}  // namespace emitter

#endif  // JAK_OBJECTGENERATOR_H
