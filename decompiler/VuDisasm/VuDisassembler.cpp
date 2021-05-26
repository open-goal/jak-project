#include <cstring>
#include <algorithm>

#include "VuDisassembler.h"
#include "third-party/fmt/core.h"
#include "common/util/assert.h"
#include "common/util/print_float.h"

namespace decompiler {

namespace {
int upper_op6(u32 in) {
  return in & 0b111111;
}

int upper_op11(u32 in) {
  return in & 0b11111111111;
}

int upper_dest_mask(u32 in) {
  return 0b1111 & (in >> 21);
}

int upper_ft(u32 in) {
  return 0b11111 & (in >> 16);
}

int upper_fs(u32 in) {
  return 0b11111 & (in >> 11);
}

int upper_fd(u32 in) {
  return 0b11111 & (in >> 6);
}

int upper_bc(u32 in) {
  return 0b11 & in;
}

int lower_op(u32 in) {
  return (in >> 25);
}

int upper_imm15_unsigned(u32 in) {
  u32 p1 = (in & 0b11111111111);
  u32 p2 = (in >> 21) & 0b1111;
  return p1 | (p2 << 11);
}

}  // namespace
VuDisassembler::VuDisassembler() {
  m_upper_op6_table[0b000000].set(VuInstrK::ADDbc);   // 0
  m_upper_op6_table[0b000001].set(VuInstrK::ADDbc);   // 1
  m_upper_op6_table[0b000010].set(VuInstrK::ADDbc);   // 2
  m_upper_op6_table[0b000011].set(VuInstrK::ADDbc);   // 3
  m_upper_op6_table[0b000100].set(VuInstrK::SUBbc);   // 4
  m_upper_op6_table[0b000101].set(VuInstrK::SUBbc);   // 5
  m_upper_op6_table[0b000110].set(VuInstrK::SUBbc);   // 6
  m_upper_op6_table[0b000111].set(VuInstrK::SUBbc);   // 7
  m_upper_op6_table[0b001000].set(VuInstrK::MADDbc);  // 8
  m_upper_op6_table[0b001001].set(VuInstrK::MADDbc);  // 9
  m_upper_op6_table[0b001010].set(VuInstrK::MADDbc);  // 10
  m_upper_op6_table[0b001011].set(VuInstrK::MADDbc);  // 11
  m_upper_op6_table[0b001100].set(VuInstrK::MSUBbc);  // 12
  m_upper_op6_table[0b001101].set(VuInstrK::MSUBbc);  // 13
  m_upper_op6_table[0b001110].set(VuInstrK::MSUBbc);  // 14
  m_upper_op6_table[0b001111].set(VuInstrK::MSUBbc);  // 15
  m_upper_op6_table[0b010000].set(VuInstrK::MAXbc);   // 16
  m_upper_op6_table[0b010001].set(VuInstrK::MAXbc);   // 17
  m_upper_op6_table[0b010010].set(VuInstrK::MAXbc);   // 18
  m_upper_op6_table[0b010011].set(VuInstrK::MAXbc);   // 19
  m_upper_op6_table[0b010100].set(VuInstrK::MINIbc);  // 20
  m_upper_op6_table[0b010101].set(VuInstrK::MINIbc);  // 21
  m_upper_op6_table[0b010110].set(VuInstrK::MINIbc);  // 22
  m_upper_op6_table[0b010111].set(VuInstrK::MINIbc);  // 23
  m_upper_op6_table[0b011000].set(VuInstrK::MULbc);   // 24
  m_upper_op6_table[0b011001].set(VuInstrK::MULbc);   // 25
  m_upper_op6_table[0b011010].set(VuInstrK::MULbc);   // 26
  m_upper_op6_table[0b011011].set(VuInstrK::MULbc);   // 27
  m_upper_op6_table[0b011100].set(VuInstrK::MULq);    // 28
  m_upper_op6_table[0b011101].set(VuInstrK::MAXi);    // 29
  m_upper_op6_table[0b011110].set(VuInstrK::MULi);    // 30
  m_upper_op6_table[0b011111].set(VuInstrK::MINIi);   // 31

  m_upper_op6_table[0b100000].set(VuInstrK::ADDq);  // 32
  //  m_upper_op6_table[0b100001].set(VuInstrK::MADDq);   // 33
  m_upper_op6_table[0b100010].set(VuInstrK::ADDi);  // 34
  //  m_upper_op6_table[0b100011].set(VuInstrK::MADDi);   // 35
  //  m_upper_op6_table[0b100100].set(VuInstrK::SUBq);    // 36
  //  m_upper_op6_table[0b100101].set(VuInstrK::MSUBq);   // 37
  //  m_upper_op6_table[0b100110].set(VuInstrK::SUBi);    // 38
  //  m_upper_op6_table[0b100111].set(VuInstrK::MSUBi);   // 39
  m_upper_op6_table[0b101000].set(VuInstrK::ADD);   // 40
  m_upper_op6_table[0b101001].set(VuInstrK::MADD);  // 41
  m_upper_op6_table[0b101010].set(VuInstrK::MUL);   // 42
  m_upper_op6_table[0b101011].set(VuInstrK::MAX);   // 43
  m_upper_op6_table[0b101100].set(VuInstrK::SUB);   // 44
  //  m_upper_op6_table[0b101101].set(VuInstrK::MSUB);    // 45
  m_upper_op6_table[0b101110].set(VuInstrK::OPMSUB);  // 46
  m_upper_op6_table[0b101111].set(VuInstrK::MINI);    // 47
  // ???
  m_upper_op6_table[0b111100].set_11();  // 60
  m_upper_op6_table[0b111101].set_11();  // 61
  m_upper_op6_table[0b111110].set_11();  // 62
  m_upper_op6_table[0b111111].set_11();  // 63

  add_op(VuInstrK::NOP, "nop").iemdt();
  add_op(VuInstrK::LOWER_NOP, "nop");
  add_op(VuInstrK::FTOI4, "ftoi4").iemdt().dst_mask().dst_vf_ft().src_vf_fs();
  add_op(VuInstrK::FTOI0, "ftoi0").iemdt().dst_mask().dst_vf_ft().src_vf_fs();
  add_op(VuInstrK::ITOF0, "itof0").iemdt().dst_mask().dst_vf_ft().src_vf_fs();
  add_op(VuInstrK::ITOF12, "itof12").iemdt().dst_mask().dst_vf_ft().src_vf_fs();
  add_op(VuInstrK::ITOF15, "itof15").iemdt().dst_mask().dst_vf_ft().src_vf_fs();
  add_op(VuInstrK::FTOI12, "ftoi12").iemdt().dst_mask().dst_vf_ft().src_vf_fs();
  add_op(VuInstrK::ADD, "add").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::MULbc, "mul").iemdt().dst_mask().bc().dss_fd_fs_ft();
  add_op(VuInstrK::ADDbc, "add").iemdt().dst_mask().dss_fd_fs_ft().bc();
  add_op(VuInstrK::MAXbc, "max").iemdt().dst_mask().dss_fd_fs_ft().bc();
  add_op(VuInstrK::MULAbc, "mula").iemdt().bc().dst_mask().dst_acc().src_vfs().src_vft();
  add_op(VuInstrK::MADDAbc, "madda").iemdt().bc().dst_mask().dst_acc().src_vfs().src_vft();
  add_op(VuInstrK::MSUBAbc, "msuba").iemdt().bc().dst_mask().dst_acc().src_vfs().src_vft();
  add_op(VuInstrK::MADDbc, "madd").iemdt().dst_mask().dss_fd_fs_ft().bc();
  add_op(VuInstrK::SUBbc, "sub").iemdt().dst_mask().dss_fd_fs_ft().bc();
  add_op(VuInstrK::OPMULA, "opmula").iemdt().dst_mask().dst_acc().src_vfs().src_vft();
  add_op(VuInstrK::OPMSUB, "opmsub").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::MUL, "mul").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::MULq, "mul").iemdt().dst_mask().dst_vfd().src_vfs().src_q().vft_zero();
  add_op(VuInstrK::SUB, "sub").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::MSUBbc, "msub").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::MADDA, "madda").iemdt().dst_mask().dst_acc().src_vfs().src_vft();
  add_op(VuInstrK::MULA, "mula").iemdt().dst_mask().dst_acc().src_vfs().src_vft();
  add_op(VuInstrK::MINIbc, "mini").iemdt().dst_mask().bc().dss_fd_fs_ft();
  add_op(VuInstrK::MAXi, "maxi").iemdt().dst_mask().dst_vfd().src_vfs().vft_zero().src_i();
  add_op(VuInstrK::MINIi, "minii").iemdt().dst_mask().dst_vfd().src_vfs().vft_zero().src_i();
  add_op(VuInstrK::ADDAbc, "adda").iemdt().dst_mask().bc().dst_vfs().src_vft();
  add_op(VuInstrK::CLIP, "clip").iemdt().dst_mask().bc().src_vfs().src_vft();
  add_op(VuInstrK::MINI, "mini").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::MAX, "max").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::ADDA, "adda").iemdt().dst_mask().dst_acc().src_vfs().src_vft();
  add_op(VuInstrK::MADD, "madd").iemdt().dst_mask().dss_fd_fs_ft();
  add_op(VuInstrK::ADDq, "addq").iemdt().dst_mask().vft_zero().dst_vfd().src_vfs().src_q();
  add_op(VuInstrK::MULi, "muli").iemdt().dst_mask().vft_zero().dst_vfd().src_vfs().src_i();
  add_op(VuInstrK::ADDi, "addi").iemdt().dst_mask().vft_zero().dst_vfd().src_vfs().src_i();

  m_lower_op6_table[0b000000].set(VuInstrK::LQ);
  m_lower_op6_table[0b000001].set(VuInstrK::SQ);
  m_lower_op6_table[0b000100].set(VuInstrK::ILW);
  m_lower_op6_table[0b000101].set(VuInstrK::ISW);
  m_lower_op6_table[0b001000].set(VuInstrK::IADDIU);
  m_lower_op6_table[0b001001].set(VuInstrK::ISUBIU);
  //  m_lower_op6_table[0b010000].set(VuInstrK::FCEQ);
  m_lower_op6_table[0b010001].set(VuInstrK::FCSET);
  m_lower_op6_table[0b010010].set(VuInstrK::FCAND);
  m_lower_op6_table[0b010011].set(VuInstrK::FCOR);
  //  m_lower_op6_table[0b010100].set(VuInstrK::FSEQ);
  //  m_lower_op6_table[0b010101].set(VuInstrK::FSSET);
  m_lower_op6_table[0b010110].set(VuInstrK::FSAND);
  //  m_lower_op6_table[0b010111].set(VuInstrK::FSOR);
  //  m_lower_op6_table[0b011000].set(VuInstrK::FMEQ);
  // ??
  m_lower_op6_table[0b011010].set(VuInstrK::FMAND);
  //  m_lower_op6_table[0b011011].set(VuInstrK::FMOR);
  m_lower_op6_table[0b011100].set(VuInstrK::FCGET);
  // ??
  m_lower_op6_table[0b100000].set(VuInstrK::B);
  m_lower_op6_table[0b100001].set(VuInstrK::BAL);
  m_lower_op6_table[0b100100].set(VuInstrK::JR);
  m_lower_op6_table[0b100101].set(VuInstrK::JALR);
  m_lower_op6_table[0b101000].set(VuInstrK::IBEQ);
  m_lower_op6_table[0b101001].set(VuInstrK::IBNE);
  m_lower_op6_table[0b101100].set(VuInstrK::IBLTZ);
  m_lower_op6_table[0b101101].set(VuInstrK::IBGTZ);
  m_lower_op6_table[0b101110].set(VuInstrK::IBLEZ);
  m_lower_op6_table[0b101111].set(VuInstrK::IBGEZ);

  add_op(VuInstrK::IBNE, "ibne").dst_mask_zero().src_vit().src_vis().rel_branch11();
  add_op(VuInstrK::LQ, "lq").dst_mask().dst_vft().src_imm11_load_store().src_vis();
  add_op(VuInstrK::ILW, "ilw").dst_mask().dst_vit().src_imm11_load_store().src_vis();
  add_op(VuInstrK::IADDIU, "iaddiu").dst_vit().src_vis().src_imm15_unsigned();
  add_op(VuInstrK::IADDI, "iaddi").dst_vit().src_vis().src_imm5_signed().dst_mask_zero();
  add_op(VuInstrK::IOR, "ior").dst_vid().src_vis().src_vit().dst_mask_zero();
  add_op(VuInstrK::SQI, "sqi").dst_vfs().src_vit().dst_mask();
  add_op(VuInstrK::ISW, "isw").src_vit().src_imm11_load_store().src_vis().dst_mask();
  add_op(VuInstrK::LQI, "lqi").dst_mask().dst_vft().src_vis();
  add_op(VuInstrK::IADD, "iadd").dst_mask_zero().dst_vid().src_vis().src_vit();
  add_op(VuInstrK::XGKICK, "xgkick").dst_mask_zero().vft_zero().src_vis();
  add_op(VuInstrK::ISUB, "isub").dst_mask_zero().dst_vid().src_vis().src_vit();
  add_op(VuInstrK::SQ, "sq").dst_mask().dst_vfs().src_imm11_load_store().src_vit();
  add_op(VuInstrK::FMAND, "fmand").imm15_zero().dst_vit().src_vis();
  add_op(VuInstrK::DIV, "div").ftf_1().fsf_0().dst_q().src_vfs().src_vft();
  add_op(VuInstrK::MOVE, "move").dst_mask().dst_vft().src_vfs();
  add_op(VuInstrK::MR32, "mr32").dst_mask().dst_vft().src_vfs();
  add_op(VuInstrK::RSQRT, "rsqrt").ftf_1().fsf_0().dst_q().src_vfs().src_vft();
  add_op(VuInstrK::ILWR, "ilwr").dst_mask().dst_vit().src_vis();
  add_op(VuInstrK::MTIR, "mtir").ftf_zero().fsf_0().dst_vit().src_vfs();
  add_op(VuInstrK::JR, "jr").dst_mask_zero().vit_zero().src_vis().imm11_zero();
  add_op(VuInstrK::IAND, "iand").dst_mask_zero().dst_vid().src_vis().src_vit();
  add_op(VuInstrK::IBEQ, "ibeq").src_vit().src_vis().dst_mask_zero().rel_branch11();
  add_op(VuInstrK::B, "b").dst_mask_zero().vit_zero().vis_zero().rel_branch11();
  // add_op(VuInstrK::XITOP, "xitop").dst_mask_zero().vis_zero().src_vit();
  add_op(VuInstrK::XTOP, "xtop").dst_mask_zero().vis_zero().src_vit();
  add_op(VuInstrK::BAL, "bal").dst_mask_zero().vis_zero().dst_vit().rel_branch11();
  add_op(VuInstrK::MFIR, "mfir").dst_mask().dst_vft().src_vis();
  add_op(VuInstrK::IBGTZ, "ibgtz").dst_mask_zero().vit_zero().src_vis().rel_branch11();
  add_op(VuInstrK::FCGET, "fcget").imm15_zero().vis_zero().dst_vit();
  add_op(VuInstrK::ISUBIU, "isubiu").dst_vit().src_vis().imm15_unsigned();
  add_op(VuInstrK::FSAND, "fsand").dst_vit().imm15_unsigned().vis_zero();  // really imm12.
  add_op(VuInstrK::IBLTZ, "ibltz").dst_mask_zero().vit_zero().src_vis().rel_branch11();
  add_op(VuInstrK::FCSET, "fcset").src_imm24_unsigned();
  add_op(VuInstrK::FCAND, "fcand vi01,").src_imm24_unsigned();
  add_op(VuInstrK::FCOR, "fcor vi01,").src_imm24_unsigned();
  add_op(VuInstrK::IBGEZ, "ibgez").dst_mask_zero().vit_zero().src_vis().rel_branch11();
  add_op(VuInstrK::ISWR, "iswr").dst_mask().src_vit().src_vis();
  add_op(VuInstrK::JALR, "jalr").dst_mask_zero().dst_vit().src_vis().imm11_zero();
  add_op(VuInstrK::WAITQ, "waitq").dst_mask_zero().vft_zero().vis_zero();
  add_op(VuInstrK::IBLEZ, "iblez").dst_mask_zero().vit_zero().src_vis().rel_branch11();
  add_op(VuInstrK::SQRT, "sqrt").fsf_zero().ftf_0().vis_zero().dst_q().src_vft();
  add_op(VuInstrK::SQD, "sqd").dst_mask().src_vfs().src_vit();
  add_op(VuInstrK::ERLENG, "erleng").dst_mask().vft_zero().src_vfs().dst_p();
  add_op(VuInstrK::MFP, "mfp").dst_mask().dst_vft().src_p();
}

VuDisassembler::OpInfo& VuDisassembler::add_op(VuInstrK kind, const std::string& name) {
  assert((int)kind < (int)VuInstrK::INVALID);
  auto& elt = m_op_info[(int)kind];
  elt.name = name;
  elt.known = true;
  return elt;
}

VuInstrK VuDisassembler::lower_kind(u32 in) {
  auto op = lower_op(in);
  if (in == 0b10000000000000000000000000110000) {
    return VuInstrK::LOWER_NOP;
  }
  if (op == 0b1000000) {
    switch (in & 0b111111) {
      case 0b110010:
        return VuInstrK::IADDI;
      case 0b110101:
        return VuInstrK::IOR;
      case 0b110000:
        return VuInstrK::IADD;
      case 0b110001:
        return VuInstrK::ISUB;
      case 0b110100:
        return VuInstrK::IAND;
    }
    switch (in & 0b11111111111) {
      case 0b01100'1111'00:
        return VuInstrK::MOVE;
      case 0b01100'1111'01:
        return VuInstrK::MR32;
      case 0b01101'1111'00:
        return VuInstrK::LQI;
      case 0b01101'1111'01:
        return VuInstrK::SQI;
      case 0b01101'1111'11:
        return VuInstrK::SQD;
      case 0b01110'1111'00:
        return VuInstrK::DIV;
      case 0b01110'1111'01:
        return VuInstrK::SQRT;
      case 0b01110'1111'10:
        return VuInstrK::RSQRT;
      case 0b01110'1111'11:
        return VuInstrK::WAITQ;
      case 0b01111'1111'00:
        return VuInstrK::MTIR;
      case 0b01111'1111'01:
        return VuInstrK::MFIR;
      case 0b01111'1111'10:
        return VuInstrK::ILWR;
      case 0b01111'1111'11:
        return VuInstrK::ISWR;
      case 0b11001'1111'00:
        return VuInstrK::MFP;
      case 0b11010'1111'00:
        return VuInstrK::XTOP;
      case 0b11011'1111'00:
        return VuInstrK::XGKICK;
      case 0b11100'1111'11:
        return VuInstrK::ERLENG;
    }
    fmt::print("Unknown lower special: 0b{:b}\n", in);
    assert(false);
  } else {
    assert((op & 0b1000000) == 0);
    assert(op < 64);
    auto elt = m_lower_op6_table[(int)op];
    if (!elt.known) {
      fmt::print("Invalid lower op6: 0b{:b} 0b{:b} 0x{:x}\n", op, in, in);
      assert(false);
    }
    return elt.kind;
  }
}

VuInstrK VuDisassembler::upper_kind(u32 in) {
  auto& upper_info = m_upper_op6_table[upper_op6(in)];
  if (upper_info.goto_11) {
    switch (upper_op11(in)) {
      case 0b00000'1111'00:
      case 0b00000'1111'01:
      case 0b00000'1111'10:
      case 0b00000'1111'11:
        return VuInstrK::ADDAbc;
      case 0b00010'1111'00:
      case 0b00010'1111'01:
      case 0b00010'1111'10:
      case 0b00010'1111'11:
        return VuInstrK::MADDAbc;
      case 0b00011'1111'00:
      case 0b00011'1111'01:
      case 0b00011'1111'10:
      case 0b00011'1111'11:
        return VuInstrK::MSUBAbc;
      case 0b00100'1111'00:
        return VuInstrK::ITOF0;
      case 0b00100'1111'10:
        return VuInstrK::ITOF12;
      case 0b00100'1111'11:
        return VuInstrK::ITOF15;
      case 0b00101'1111'00:
        return VuInstrK::FTOI0;
      case 0b00101'1111'01:
        return VuInstrK::FTOI4;
      case 0b00101'1111'10:
        return VuInstrK::FTOI12;
      case 0b00110'1111'00:
      case 0b00110'1111'01:
      case 0b00110'1111'10:
      case 0b00110'1111'11:
        return VuInstrK::MULAbc;
      case 0b00111'1111'11:
        return VuInstrK::CLIP;
      case 0b01010'1111'00:
        return VuInstrK::ADDA;
      case 0b01010'1111'01:
        return VuInstrK::MADDA;
      case 0b01010'1111'10:
        return VuInstrK::MULA;
      case 0b01011'1111'10:
        return VuInstrK::OPMULA;
      case 0b01011'1111'11:
        assert(upper_dest_mask(in) == 0);
        assert(upper_fs(in) == 0);
        assert(upper_ft(in) == 0);
        return VuInstrK::NOP;
        break;

      default:
        fmt::print("Invalid op11: 0b{:b}\n", upper_op11(in));
        assert(false);
    }
  }
  if (!upper_info.known) {
    fmt::print("Invalid upper op6: 0b{:b}\n", upper_op6(in));
    assert(false);
  }
  return upper_info.kind;
}

VuProgram VuDisassembler::disassemble(void* data, int size_bytes, bool debug_print) {
  auto bytes = (u8*)data;
  // should be 8 byte aligned size.
  assert((size_bytes & 0x7) == 0);
  VuProgram prog;
  int instruction_count = size_bytes / 8;
  for (int i = 0; i < instruction_count; i++) {
    u32 lower, upper;
    memcpy(&lower, bytes + i * 8, 4);
    memcpy(&upper, bytes + i * 8 + 4, 4);

    // decode
    auto upper_instr = decode(upper_kind(upper), upper, i);
    auto lower_instr = upper_instr.i_bit() ? VuInstruction::make_fp_constant(lower)
                                           : decode(lower_kind(lower), lower, i);

    prog.add_instruction(upper_instr, lower_instr);

    // debug
    if (debug_print) {
      fmt::print("{}\n", to_string(VuInstructionPair{upper_instr, lower_instr}));
    }
  }

  name_labels();

  if (debug_print) {
    fmt::print("----------------------------------\n");
    fmt::print("{}\n", to_string(prog));
  }

  return prog;
}

VuInstruction VuDisassembler::decode(VuInstrK kind, u32 data, int instr_idx) {
  VuInstruction instr;
  instr.kind = kind;
  auto& inst = info(kind);
  if (!inst.known) {
    fmt::print("instr idx {} is unknown\n", (int)kind);
    assert(false);
  }
  for (auto& step : inst.decode) {
    s64 value = -1;
    switch (step.field) {
      case VuDecodeStep::FieldK::IEMDT:
        value = data >> 25;
        assert((value & 3) == 0);
        break;
      case VuDecodeStep::FieldK::DST_MASK:
        value = upper_dest_mask(data);
        break;
      case VuDecodeStep::FieldK::FS:
        value = upper_fs(data);
        break;
      case VuDecodeStep::FieldK::FT:
        value = upper_ft(data);
        break;
      case VuDecodeStep::FieldK::FD:
        value = upper_fd(data);
        break;
      case VuDecodeStep::FieldK::BC:
        value = upper_bc(data);
        break;
      case VuDecodeStep::FieldK::NONE:
        break;
      case VuDecodeStep::FieldK::IMM11_BRANCH: {
        s32 signed_11 = upper_op11(data) << 21;
        signed_11 >>= 21;
        value = add_label(signed_11 + instr_idx + 1);
      } break;
      case VuDecodeStep::FieldK::IMM11_SIGNED: {
        s32 signed_value = (data << 21);
        signed_value = (signed_value >> 21);
        value = signed_value;
      } break;
      case VuDecodeStep::FieldK::IMM15_UNSIGNED:
        value = upper_imm15_unsigned(data);
        break;
      case VuDecodeStep::FieldK::IMM5_SIGNED: {
        s32 signed_value = (data << 21);
        value = (signed_value >> 27);
        break;
      }
      case VuDecodeStep::FieldK::FTF:
        value = (data >> 23) & 0b11;
        break;

      case VuDecodeStep::FieldK::FSF:
        value = (data >> 21) & 0b11;
        break;

      case VuDecodeStep::FieldK::IMM24_UNSIGNED:
        value = (data & 0b1111'1111'1111'1111'1111'1111);
        break;

      default:
        assert(false);
    }

    switch (step.atom) {
      case VuDecodeStep::AtomK::IEMDT:
        instr.iemdt = value;
        break;
      case VuDecodeStep::AtomK::DST_MASK:
        instr.mask = value;
        break;
      case VuDecodeStep::AtomK::DST_VF:
        assert(!instr.dst);
        instr.dst = VuInstructionAtom::make_vf(value);
        break;
      case VuDecodeStep::AtomK::DST_VI:
        assert(!instr.dst);
        instr.dst = VuInstructionAtom::make_vi(value);
        break;
      case VuDecodeStep::AtomK::SRC_VF:
        instr.src.push_back(VuInstructionAtom::make_vf(value));
        break;
      case VuDecodeStep::AtomK::BC:
        assert(!instr.bc);
        instr.bc = value;
        break;
      case VuDecodeStep::AtomK::ASSERT_ZERO:
        assert(value == 0);
        break;
      case VuDecodeStep::AtomK::SRC_VI:
        instr.src.push_back(VuInstructionAtom::make_vi(value));
        break;
      case VuDecodeStep::AtomK::BRANCH_TARGET:
        instr.src.push_back(VuInstructionAtom::make_label(value));
        break;
      case VuDecodeStep::AtomK::LOAD_STORE_OFFSET:
        instr.src.push_back(VuInstructionAtom::make_load_store_imm(value));
        break;
      case VuDecodeStep::AtomK::SRC_IMM:
        instr.src.push_back(VuInstructionAtom::make_imm(value));
        break;
      case VuDecodeStep::AtomK::DST_ACC:
        assert(!instr.dst);
        instr.dst = VuInstructionAtom::make_acc();
        break;
      case VuDecodeStep::AtomK::DST_Q:
        assert(!instr.dst);
        instr.dst = VuInstructionAtom::make_q();
        break;
      case VuDecodeStep::AtomK::DST_P:
        assert(!instr.dst);
        instr.dst = VuInstructionAtom::make_p();
        break;
      case VuDecodeStep::AtomK::SRC_Q:
        instr.src.push_back(VuInstructionAtom::make_q());
        break;
      case VuDecodeStep::AtomK::SRC_I:
        instr.src.push_back(VuInstructionAtom::make_i());
        break;
      case VuDecodeStep::AtomK::SRC_P:
        instr.src.push_back(VuInstructionAtom::make_p());
        break;
      case VuDecodeStep::AtomK::SECOND_SOURCE_FIELD:
        instr.second_src_field = value;
        break;
      case VuDecodeStep::AtomK::FIRST_SOURCE_FIELD:
        instr.first_src_field = value;
        break;
      default:
        assert(false);
    }
  }

  return instr;
}

namespace {
char bc_to_part(int x) {
  switch (x) {
    case 0:
      return 'x';
    case 1:
      return 'y';
    case 2:
      return 'z';
    case 3:
      return 'w';
    default:
      return '?';
  }
}
}  // namespace

std::string VuDisassembler::to_string(const VuInstruction& instr) const {
  if (instr.kind == VuInstrK::FP_CONSTANT) {
    return float_to_string(instr.fp);
  }

  auto& in = info(instr.kind);
  if (!in.known) {
    assert(false);
  }

  std::string result;
  result += in.name;

  if (instr.bc) {
    result += bc_to_part(*instr.bc);
  }

  if (instr.mask) {
    u8 val = *instr.mask;
    result += '.';
    if (val & 8) {
      result += 'x';
    }
    if (val & 4) {
      result += 'y';
    }
    if (val & 2) {
      result += 'z';
    }
    if (val & 1) {
      result += 'w';
    }
  }

  bool comma = false;
  if (instr.dst) {
    result += " ";
    result += instr.dst->to_string(m_label_names);
    result += ',';
    comma = true;
  }

  bool close = false;
  int idx = 0;
  for (auto& src : instr.src) {
    if (close) {
    } else {
      result += " ";
    }

    result += src.to_string(m_label_names);

    if (idx == 0 && instr.first_src_field) {
      result += '.';
      result += bc_to_part(*instr.first_src_field);
    }

    if (idx == 1 && instr.second_src_field) {
      result += '.';
      result += bc_to_part(*instr.second_src_field);
    }

    if (src.kind() == VuInstructionAtom::Kind::LOAD_STORE_IMM) {
      result += '(';
      close = true;
    } else {
      if (close) {
        result += ")";
        close = false;
        comma = false;
      } else {
        result += ',';
        comma = true;
      }
    }

    idx++;
  }

  assert(!close);

  if (comma) {
    result.pop_back();
  }

  if (instr.iemdt) {
    u8 val = *instr.iemdt;
    if (val & 0b100) {
      result += " :t";
    }

    if (val & 0b1000) {
      result += " :d";
    }

    if (val & 0b10000) {
      result += " :m";
    }

    if (val & 0b100000) {
      result += " :e";
    }

    if (val & 0b1000000) {
      result += " :i";
    }
  }

  return result;
}

std::string VuDisassembler::to_string(const VuInstructionPair& pair) const {
  return fmt::format("  {:25s}  |  {:25s}", to_string(pair.lower), to_string(pair.upper));
}

std::string VuDisassembler::to_string(const VuProgram& prog) const {
  std::string result;
  for (int i = 0; i < (int)prog.instructions().size(); i++) {
    auto lab = m_labels.find(i);
    if (lab != m_labels.end()) {
      result += m_label_names.at(lab->second);
      result += ':';
      result += '\n';
    }
    result += to_string(prog.instructions().at(i));
    result += '\n';
  }
  return result;
}

int VuDisassembler::add_label(int instr) {
  auto existing = m_labels.find(instr);
  if (existing == m_labels.end()) {
    int new_idx = m_labels.size();
    m_labels[instr] = new_idx;
    return new_idx;
  } else {
    return existing->second;
  }
}

void VuDisassembler::name_labels() {
  std::vector<int> instrs_with_labels;
  for (auto [instr, label_idx] : m_labels) {
    instrs_with_labels.push_back(instr);
  }

  m_label_names.resize(instrs_with_labels.size());
  std::sort(instrs_with_labels.begin(), instrs_with_labels.end());
  int idx = 1;
  for (auto& instr : instrs_with_labels) {
    auto label_idx = m_labels.at(instr);
    m_label_names.at(label_idx) = fmt::format("L{}", idx++);
  }
}
}  // namespace decompiler