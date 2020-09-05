#ifndef JAK_OBJECTGENERATOR_H
#define JAK_OBJECTGENERATOR_H

#include <cstring>
#include <map>
#include <string>
#include "ObjectFileData.h"
#include "Instruction.h"

namespace emitter {

struct FunctionRecord {
  int seg = -1;
  int func_id = -1;
};

struct IR_Record {
  int seg = -1;
  int func_id = -1;
  int ir_id = -1;
};

struct InstructionRecord {
  int seg = -1;
  int func_id = -1;
  int ir_id = -1;
  int instr_id = -1;
};

struct StaticRecord {
  int seg = -1;
  int static_id = -1;
};

struct ObjectDebugInfo {};

class ObjectGenerator {
 public:
  ObjectGenerator() = default;
  ObjectFileData generate_data_v3();

  FunctionRecord add_function_to_seg(int seg,
                                     int min_align = 16);  // should align and insert function tag
  IR_Record add_ir(const FunctionRecord& func);
  IR_Record get_future_ir_record(const FunctionRecord& func, int ir_id);
  InstructionRecord add_instr(Instruction inst, IR_Record ir);
  StaticRecord add_static_to_seg(int seg, int min_align = 16);
  void link_instruction_jump(InstructionRecord jump_instr, IR_Record destination);
  void link_static_type_ptr(StaticRecord rec, int offset, const std::string& type_name);

  void link_instruction_symbol_mem(const InstructionRecord& rec, const std::string& name);
  void link_instruction_symbol_ptr(const InstructionRecord& rec, const std::string& name);
  void link_static_symbol_ptr(StaticRecord rec, int offset, const std::string& name);

  void link_instruction_static(const InstructionRecord& instr,
                               const StaticRecord& target_static,
                               int offset);
  void link_instruction_to_function(const InstructionRecord& instr,
                                    const FunctionRecord& target_func);

  ObjectDebugInfo create_debug_info();

 private:
  void handle_temp_static_type_links(int seg);
  void handle_temp_jump_links(int seg);
  void handle_temp_instr_sym_links(int seg);
  void handle_temp_static_sym_links(int seg);
  void handle_temp_rip_data_links(int seg);
  void handle_temp_rip_func_links(int seg);

  void emit_link_table(int seg);
  void emit_link_type_pointer(int seg);
  void emit_link_symbol(int seg);
  void emit_link_rip(int seg);
  std::vector<u8> generate_header_v3();

  template <typename T>
  void insert_data(int seg, const T& x) {
    auto& data = m_data_by_seg.at(seg);
    auto insert_location = data.size();
    data.resize(insert_location + sizeof(T));
    memcpy(data.data() + insert_location, &x, sizeof(T));
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
  seg_vector<RipFuncLink> m_rip_func_temp_links_by_seg;
  seg_vector<RipDataLink> m_rip_data_temp_links_by_seg;

  // final link stuff
  seg_map<int> m_type_ptr_links_by_seg;
  seg_map<int> m_sym_links_by_seg;
  seg_vector<RipLink> m_rip_links_by_seg;
};
}  // namespace emitter

#endif  // JAK_OBJECTGENERATOR_H
