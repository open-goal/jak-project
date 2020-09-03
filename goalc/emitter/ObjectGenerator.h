#ifndef JAK_OBJECTGENERATOR_H
#define JAK_OBJECTGENERATOR_H

#include <cstring>
#include <map>
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
  void link_instruction_symbol();
  void link_instruction_static();
  void link_instruction_to_function();
  void link_static_type_ptr(StaticRecord rec, int offset, const std::string& type_name);
  void link_static_sym();

  void link_type_pointer(const std::string& type_name, const StaticRecord& rec, int offset);

 private:
  void handle_temp_static_type_links(int seg);
  void handle_temp_jump_links(int seg);
  void emit_link_tables();
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
  // jumps, patched before (JumpLink)
  // ptrs/access to statics (RipLink)
  // type pointers
  // symbol links

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

  //  struct SymbolLink {
  //
  //  };
  //
  //  struct RipLink {
  //
  //  };
  //
  struct JumpLink {
    InstructionRecord jump_instr;
    IR_Record dest;
  };

  // final data
  std::array<std::vector<u8>, N_SEG> m_data_by_seg;
  std::array<std::vector<u8>, N_SEG> m_link_by_seg;

  // temp data
  std::array<std::vector<FunctionData>, N_SEG> m_function_data_by_seg;
  std::array<std::vector<StaticData>, N_SEG> m_static_data_by_seg;

  // temp link stuff
  std::array<std::map<std::string, std::vector<StaticTypeLink>>, N_SEG>
      m_static_type_temp_links_by_seg;
  std::array<std::vector<JumpLink>, N_SEG> m_jump_temp_links_by_seg;

  // final link stuff
  std::array<std::map<std::string, std::vector<int>>, N_SEG> m_type_ptr_links_by_seg;
  //  std::array<std::map<std::string, SymbolLink>, N_SEG> m_sym_links_by_seg;
  //  std::array<std::vector<RipLink>, N_SEG> m_rip_links_by_seg;
};
}  // namespace emitter

#endif  // JAK_OBJECTGENERATOR_H
