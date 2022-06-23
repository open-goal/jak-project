#pragma once

#include <unordered_map>

#include "common/util/Assert.h"

#include "decompiler/VuDisasm/VuProgram.h"

namespace decompiler {

struct VuDecodeStep {
  enum class FieldK {
    IEMDT,
    DST_MASK,
    FT,
    FS,
    FD,
    BC,
    IMM11_BRANCH,
    IMM11_SIGNED,
    IMM15_UNSIGNED,
    IMM5_SIGNED,
    IMM24_UNSIGNED,
    FTF,
    FSF,
    NONE,
  } field;

  enum class AtomK {
    IEMDT,
    DST_MASK,
    DST_VF,
    DST_VI,
    DST_ACC,
    DST_Q,
    DST_P,
    SRC_P,
    SRC_Q,
    SRC_I,
    SRC_VF,
    SRC_VI,
    SRC_IMM,
    BC,
    ASSERT_ZERO,
    LOAD_STORE_OFFSET,
    SECOND_SOURCE_FIELD,
    FIRST_SOURCE_FIELD,
    BRANCH_TARGET
  } atom;
};

class VuDisassembler {
 public:
  enum VuKind {
    VU0,
    VU1,
  };
  VuDisassembler(VuKind kind);
  VuProgram disassemble(void* data, int size_bytes, bool debug_print = false);
  std::string to_string(const VuInstruction& instr) const;
  std::string to_cpp(const VuInstruction& instr, bool mips2c_format) const;
  std::string to_string(const VuInstructionPair& pair) const;
  std::string to_string_with_cpp(const VuInstructionPair& pair, bool mips2c_format, int idx) const;
  std::string to_string(const VuProgram& prog) const;
  std::string to_string_with_cpp(const VuProgram& prog, bool mips2c_format) const;
  int add_label(int instr);
  void add_label_with_name(int instr, const std::string& name);

 private:
  VuKind m_kind;
  VuInstrK upper_kind(u32 in);
  VuInstrK lower_kind(u32 in);
  VuInstruction decode(VuInstrK kind, u32 data, int instr_idx);
  s32 get_instruction_index_mask();
  std::unordered_map<int, std::string> m_user_named_instructions;

  struct VuUpperOp6 {
    bool goto_11 = false;
    VuInstrK kind = VuInstrK::INVALID;
    bool known = false;
    void set_11() {
      known = true;
      goto_11 = true;
    }

    void set(VuInstrK _kind) {
      known = true;
      kind = _kind;
    }
  };

  struct VuLowerOp6 {
    bool goto_other = false;
    VuInstrK kind = VuInstrK::INVALID;
    bool known = false;
    void set_other() {
      known = true;
      goto_other = true;
    }

    void set(VuInstrK _kind) {
      known = true;
      kind = _kind;
    }
  };

  struct OpInfo {
    OpInfo() = default;
    OpInfo(const std::string& _name) : name(_name) {}

    OpInfo& iemdt() { return step({VuDecodeStep::FieldK::IEMDT, VuDecodeStep::AtomK::IEMDT}); }
    OpInfo& bc() { return step({VuDecodeStep::FieldK::BC, VuDecodeStep::AtomK::BC}); }
    OpInfo& dst_vf_ft() { return step({VuDecodeStep::FieldK::FT, VuDecodeStep::AtomK::DST_VF}); }
    OpInfo& src_vf_fs() { return step({VuDecodeStep::FieldK::FS, VuDecodeStep::AtomK::SRC_VF}); }
    OpInfo& dss_fd_fs_ft() {
      return step({VuDecodeStep::FieldK::FD, VuDecodeStep::AtomK::DST_VF})
          .step({VuDecodeStep::FieldK::FS, VuDecodeStep::AtomK::SRC_VF})
          .step({VuDecodeStep::FieldK::FT, VuDecodeStep::AtomK::SRC_VF});
    }

    OpInfo& dst_mask() {
      return step({VuDecodeStep::FieldK::DST_MASK, VuDecodeStep::AtomK::DST_MASK});
    }

    OpInfo& dst_acc() { return step({VuDecodeStep::FieldK::NONE, VuDecodeStep::AtomK::DST_ACC}); }
    OpInfo& dst_vfd() { return step({VuDecodeStep::FieldK::FD, VuDecodeStep::AtomK::DST_VF}); }
    OpInfo& dst_q() { return step({VuDecodeStep::FieldK::NONE, VuDecodeStep::AtomK::DST_Q}); }
    OpInfo& dst_p() { return step({VuDecodeStep::FieldK::NONE, VuDecodeStep::AtomK::DST_P}); }
    OpInfo& src_q() { return step({VuDecodeStep::FieldK::NONE, VuDecodeStep::AtomK::SRC_Q}); }
    OpInfo& src_i() { return step({VuDecodeStep::FieldK::NONE, VuDecodeStep::AtomK::SRC_I}); }
    OpInfo& src_p() { return step({VuDecodeStep::FieldK::NONE, VuDecodeStep::AtomK::SRC_P}); }

    OpInfo& step(VuDecodeStep x) {
      decode.push_back(x);
      return *this;
    }

    OpInfo& dst_mask_zero() {
      return step({VuDecodeStep::FieldK::DST_MASK, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    OpInfo& vft_zero() {
      return step({VuDecodeStep::FieldK::FT, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    OpInfo& vit_zero() { return vft_zero(); }

    OpInfo& vis_zero() {
      return step({VuDecodeStep::FieldK::FS, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    OpInfo& vfs_zero() {
      return step({VuDecodeStep::FieldK::FS, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    OpInfo& ftf_zero() {
      return step({VuDecodeStep::FieldK::FTF, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    OpInfo& fsf_zero() {
      return step({VuDecodeStep::FieldK::FSF, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    OpInfo& src_vit() { return step({VuDecodeStep::FieldK::FT, VuDecodeStep::AtomK::SRC_VI}); }

    OpInfo& src_vis() { return step({VuDecodeStep::FieldK::FS, VuDecodeStep::AtomK::SRC_VI}); }

    OpInfo& src_vft() { return step({VuDecodeStep::FieldK::FT, VuDecodeStep::AtomK::SRC_VF}); }

    OpInfo& src_vfs() { return step({VuDecodeStep::FieldK::FS, VuDecodeStep::AtomK::SRC_VF}); }

    OpInfo& rel_branch11() {
      return step({VuDecodeStep::FieldK::IMM11_BRANCH, VuDecodeStep::AtomK::BRANCH_TARGET});
    }

    OpInfo& imm15_unsigned() {
      return step({VuDecodeStep::FieldK::IMM15_UNSIGNED, VuDecodeStep::AtomK::SRC_IMM});
    }

    OpInfo& dst_vft() { return step({VuDecodeStep::FieldK::FT, VuDecodeStep::AtomK::DST_VF}); }

    OpInfo& dst_vfs() { return step({VuDecodeStep::FieldK::FS, VuDecodeStep::AtomK::DST_VF}); }

    OpInfo& dst_vit() { return step({VuDecodeStep::FieldK::FT, VuDecodeStep::AtomK::DST_VI}); }

    OpInfo& dst_vid() { return step({VuDecodeStep::FieldK::FD, VuDecodeStep::AtomK::DST_VI}); }

    OpInfo& ftf_1() {
      return step({VuDecodeStep::FieldK::FTF, VuDecodeStep::AtomK::SECOND_SOURCE_FIELD});
    }
    OpInfo& ftf_0() {
      return step({VuDecodeStep::FieldK::FTF, VuDecodeStep::AtomK::FIRST_SOURCE_FIELD});
    }
    OpInfo& fsf_0() {
      return step({VuDecodeStep::FieldK::FSF, VuDecodeStep::AtomK::FIRST_SOURCE_FIELD});
    }

    OpInfo& src_imm11_load_store() {
      return step({VuDecodeStep::FieldK::IMM11_SIGNED, VuDecodeStep::AtomK::LOAD_STORE_OFFSET});
    }

    OpInfo& imm11_zero() {
      return step({VuDecodeStep::FieldK::IMM11_SIGNED, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    OpInfo& src_imm15_unsigned() {
      return step({VuDecodeStep::FieldK::IMM15_UNSIGNED, VuDecodeStep::AtomK::SRC_IMM});
    }

    OpInfo& src_imm24_unsigned() {
      return step({VuDecodeStep::FieldK::IMM24_UNSIGNED, VuDecodeStep::AtomK::SRC_IMM});
    }

    OpInfo& src_imm5_signed() {
      return step({VuDecodeStep::FieldK::IMM5_SIGNED, VuDecodeStep::AtomK::SRC_IMM});
    }

    OpInfo& imm15_zero() {
      return step({VuDecodeStep::FieldK::IMM15_UNSIGNED, VuDecodeStep::AtomK::ASSERT_ZERO});
    }

    std::string name;
    std::vector<VuDecodeStep> decode;
    bool known = false;
  };

  const OpInfo& info(VuInstrK op) const {
    ASSERT((int)op < (int)VuInstrK::INVALID);
    return m_op_info[(int)op];
  }

  VuUpperOp6 m_upper_op6_table[64];
  VuLowerOp6 m_lower_op6_table[64];
  OpInfo m_op_info[(int)VuInstrK::INVALID];

  std::unordered_map<int, int> m_labels;
  std::vector<std::string> m_label_names;

  void name_labels();

  OpInfo& add_op(VuInstrK kind, const std::string& name);
};

}  // namespace decompiler
