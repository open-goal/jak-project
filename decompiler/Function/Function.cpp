#include <cassert>
#include <vector>
#include "Function.h"
#include "common/log/log.h"
#include "decompiler/Disasm/InstructionMatching.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "TypeInspector.h"
#include "decompiler/IR/IR.h"

namespace {
std::vector<Register> gpr_backups = {make_gpr(Reg::GP), make_gpr(Reg::S5), make_gpr(Reg::S4),
                                     make_gpr(Reg::S3), make_gpr(Reg::S2), make_gpr(Reg::S1),
                                     make_gpr(Reg::S0)};

std::vector<Register> fpr_backups = {make_fpr(30), make_fpr(28), make_fpr(26),
                                     make_fpr(24), make_fpr(22), make_fpr(20)};

Register get_expected_gpr_backup(int n, int total) {
  assert(total <= int(gpr_backups.size()));
  assert(n < total);
  return gpr_backups.at((total - 1) - n);
}

Register get_expected_fpr_backup(int n, int total) {
  assert(total <= int(fpr_backups.size()));
  assert(n < total);
  return fpr_backups.at((total - 1) - n);
}

uint32_t align16(uint32_t in) {
  return (in + 15) & (~15);
}

uint32_t align8(uint32_t in) {
  return (in + 7) & (~7);
}

uint32_t align4(uint32_t in) {
  return (in + 3) & (~3);
}

}  // namespace

Function::Function(int _start_word, int _end_word) : start_word(_start_word), end_word(_end_word) {}

/*!
 * Remove the function prologue from the first basic block and populate this->prologue with info.
 */
void Function::analyze_prologue(const LinkedObjectFile& file) {
  int idx = 1;

  // first we look for daddiu sp, sp, -x to determine how much stack is used
  if (is_gpr_2_imm_int(instructions.at(idx), InstructionKind::DADDIU, make_gpr(Reg::SP),
                       make_gpr(Reg::SP), {})) {
    prologue.total_stack_usage = -instructions.at(idx).get_imm_src_int();
    idx++;
  } else {
    prologue.total_stack_usage = 0;
  }

  // don't include type tag
  prologue_end = 1;

  // if we use the stack, we may back up some registers onto it
  if (prologue.total_stack_usage) {
    // heuristics to detect asm functions
    {
      auto& instr = instructions.at(idx);
      // storing stack pointer on the stack is done by some ASM kernel functions
      if (instr.kind == InstructionKind::SW && instr.get_src(0).get_reg() == make_gpr(Reg::SP)) {
        printf("[Warning] %s Suspected ASM function based on this instruction in prologue: %s\n",
               guessed_name.to_string().c_str(), instr.to_string(file).c_str());
        warnings += ";; Flagged as ASM function because of " + instr.to_string(file) + "\n";
        suspected_asm = true;
        return;
      }
    }

    // ra backup is always first
    if (is_no_link_gpr_store(instructions.at(idx), 8, Register(Reg::GPR, Reg::RA), {},
                             Register(Reg::GPR, Reg::SP))) {
      prologue.ra_backed_up = true;
      prologue.ra_backup_offset = get_gpr_store_offset_as_int(instructions.at(idx));
      assert(prologue.ra_backup_offset == 0);
      idx++;
    }

    {
      auto& instr = instructions.at(idx);

      // storing s7 on the stack is done by interrupt handlers, which we probably don't want to
      // support
      if (instr.kind == InstructionKind::SD && instr.get_src(0).get_reg() == make_gpr(Reg::S7)) {
        lg::warn("{} Suspected ASM function based on this instruction in prologue: {}\n",
                 guessed_name.to_string(), instr.to_string(file));
        warnings += ";; Flagged as ASM function because of " + instr.to_string(file) + "\n";
        suspected_asm = true;
        return;
      }
    }

    // next is fp backup
    if (is_no_link_gpr_store(instructions.at(idx), 8, Register(Reg::GPR, Reg::FP), {},
                             Register(Reg::GPR, Reg::SP))) {
      prologue.fp_backed_up = true;
      prologue.fp_backup_offset = get_gpr_store_offset_as_int(instructions.at(idx));
      // in Jak 1 like we never backup fp unless ra is also backed up, so the offset is always 8.
      // but it seems like it could be possible to do one without the other?
      assert(prologue.fp_backup_offset == 8);
      idx++;

      // after backing up fp, we always set it to t9.
      prologue.fp_set = is_gpr_3(instructions.at(idx), InstructionKind::OR, make_gpr(Reg::FP),
                                 make_gpr(Reg::T9), make_gpr(Reg::R0));
      assert(prologue.fp_set);
      idx++;
    }

    // next is gpr backups. these are in reverse order, so we should first find the length
    // GOAL will always do the exact same thing when the same number of gprs needs to be backed up
    // so we just need to determine the number of GPR backups, and we have all the info we need
    int n_gpr_backups = 0;
    int gpr_idx = idx;
    bool expect_nothing_after_gprs = false;

    while (is_no_link_gpr_store(instructions.at(gpr_idx), 16, {}, {}, make_gpr(Reg::SP))) {
      auto store_reg = instructions.at(gpr_idx).get_src(0).get_reg();

      // sometimes stack memory is zeroed immediately after gpr backups, and this fools the previous
      // check.
      if (store_reg == make_gpr(Reg::R0)) {
        printf(
            "[Warning] %s Stack Zeroing Detected in Function::analyze_prologue, prologue may be "
            "wrong\n",
            guessed_name.to_string().c_str());
        warnings += ";; Stack Zeroing Detected, prologue may be wrong\n";
        expect_nothing_after_gprs = true;
        break;
      }

      // this also happens a few times per game.  this a0/r0 check seems to be all that's needed to
      // avoid false positives here!
      if (store_reg == make_gpr(Reg::A0)) {
        suspected_asm = true;
        printf(
            "[Warning] %s Suspected ASM function because register $a0 was stored on the stack!\n",
            guessed_name.to_string().c_str());
        warnings += ";; a0 on stack detected, flagging as asm\n";
        return;
      }

      n_gpr_backups++;
      gpr_idx++;
    }

    if (n_gpr_backups) {
      prologue.gpr_backup_offset = get_gpr_store_offset_as_int(instructions.at(idx));
      for (int i = 0; i < n_gpr_backups; i++) {
        int this_offset = get_gpr_store_offset_as_int(instructions.at(idx + i));
        auto this_reg = instructions.at(idx + i).get_src(0).get_reg();
        assert(this_offset == prologue.gpr_backup_offset + 16 * i);
        if (this_reg != get_expected_gpr_backup(i, n_gpr_backups)) {
          suspected_asm = true;
          printf("[Warning] %s Suspected asm function that isn't flagged due to stack store %s\n",
                 guessed_name.to_string().c_str(),
                 instructions.at(idx + i).to_string(file).c_str());
          warnings += ";; Suspected asm function due to stack store: " +
                      instructions.at(idx + i).to_string(file) + "\n";
          return;
        }
      }
    }
    prologue.n_gpr_backup = n_gpr_backups;
    idx = gpr_idx;

    int n_fpr_backups = 0;
    int fpr_idx = idx;
    if (!expect_nothing_after_gprs) {
      // FPR backups
      while (is_no_ll_fpr_store(instructions.at(fpr_idx), {}, {}, make_gpr(Reg::SP))) {
        // auto store_reg = instructions.at(gpr_idx).get_src(0).get_reg();
        n_fpr_backups++;
        fpr_idx++;
      }

      if (n_fpr_backups) {
        prologue.fpr_backup_offset = instructions.at(idx).get_src(1).get_imm();
        for (int i = 0; i < n_fpr_backups; i++) {
          int this_offset = instructions.at(idx + i).get_src(1).get_imm();
          auto this_reg = instructions.at(idx + i).get_src(0).get_reg();
          assert(this_offset == prologue.fpr_backup_offset + 4 * i);
          if (this_reg != get_expected_fpr_backup(i, n_fpr_backups)) {
            suspected_asm = true;
            printf("[Warning] %s Suspected asm function that isn't flagged due to stack store %s\n",
                   guessed_name.to_string().c_str(),
                   instructions.at(idx + i).to_string(file).c_str());
            warnings += ";; Suspected asm function due to stack store: " +
                        instructions.at(idx + i).to_string(file) + "\n";
            return;
          }
        }
      }
    }
    prologue.n_fpr_backup = n_fpr_backups;
    idx = fpr_idx;

    prologue_start = 1;
    prologue_end = idx;

    prologue.stack_var_offset = 0;
    if (prologue.ra_backed_up) {
      prologue.stack_var_offset = 8;
    }
    if (prologue.fp_backed_up) {
      prologue.stack_var_offset = 16;
    }

    if (n_gpr_backups == 0 && n_fpr_backups == 0) {
      prologue.n_stack_var_bytes = prologue.total_stack_usage - prologue.stack_var_offset;
    } else if (n_gpr_backups == 0) {
      // fprs only
      prologue.n_stack_var_bytes = prologue.fpr_backup_offset - prologue.stack_var_offset;
    } else if (n_fpr_backups == 0) {
      // gprs only
      prologue.n_stack_var_bytes = prologue.gpr_backup_offset - prologue.stack_var_offset;
    } else {
      // both, use gprs
      assert(prologue.fpr_backup_offset > prologue.gpr_backup_offset);
      prologue.n_stack_var_bytes = prologue.gpr_backup_offset - prologue.stack_var_offset;
    }

    assert(prologue.n_stack_var_bytes >= 0);

    // check that the stack lines up by going in order

    // RA backup
    int total_stack = 0;
    if (prologue.ra_backed_up) {
      total_stack = align8(total_stack);
      assert(prologue.ra_backup_offset == total_stack);
      total_stack += 8;
    }

    if (!prologue.ra_backed_up && prologue.fp_backed_up) {
      // GOAL does this for an unknown reason.
      total_stack += 8;
    }

    // FP backup
    if (prologue.fp_backed_up) {
      total_stack = align8(total_stack);
      assert(prologue.fp_backup_offset == total_stack);
      total_stack += 8;
      assert(prologue.fp_set);
    }

    // Stack Variables
    if (prologue.n_stack_var_bytes) {
      // no alignment because we don't know how the stack vars are aligned.
      // stack var padding counts toward this section.
      assert(prologue.stack_var_offset == total_stack);
      total_stack += prologue.n_stack_var_bytes;
    }

    // GPRS
    if (prologue.n_gpr_backup) {
      total_stack = align16(total_stack);
      assert(prologue.gpr_backup_offset == total_stack);
      total_stack += 16 * prologue.n_gpr_backup;
    }

    // FPRS
    if (prologue.n_fpr_backup) {
      total_stack = align4(total_stack);
      assert(prologue.fpr_backup_offset == total_stack);
      total_stack += 4 * prologue.n_fpr_backup;
    }

    total_stack = align16(total_stack);

    // End!
    assert(prologue.total_stack_usage == total_stack);
  }

  // it's fine to have the entire first basic block be the prologue - you could loop back to the
  // first instruction past the prologue.
  assert(basic_blocks.at(0).end_word >= prologue_end);
  basic_blocks.at(0).start_word = prologue_end;
  prologue.decoded = true;

  check_epilogue(file);
}

/*!
 * Print info about the prologue and stack.
 */
std::string Function::Prologue::to_string(int indent) const {
  char buff[512];
  char* buff_ptr = buff;
  std::string indent_str(indent, ' ');
  if (!decoded) {
    return indent_str + ";BAD PROLOGUE";
  }
  buff_ptr += sprintf(buff_ptr, "%s;stack: total 0x%02x, fp? %d ra? %d ep? %d", indent_str.c_str(),
                      total_stack_usage, fp_set, ra_backed_up, epilogue_ok);
  if (n_stack_var_bytes) {
    buff_ptr += sprintf(buff_ptr, "\n%s;stack_vars: %d bytes at %d", indent_str.c_str(),
                        n_stack_var_bytes, stack_var_offset);
  }
  if (n_gpr_backup) {
    buff_ptr += sprintf(buff_ptr, "\n%s;gprs:", indent_str.c_str());
    for (int i = 0; i < n_gpr_backup; i++) {
      buff_ptr += sprintf(buff_ptr, " %s", gpr_backups.at(i).to_string().c_str());
    }
  }
  if (n_fpr_backup) {
    buff_ptr += sprintf(buff_ptr, "\n%s;fprs:", indent_str.c_str());
    for (int i = 0; i < n_fpr_backup; i++) {
      buff_ptr += sprintf(buff_ptr, " %s", fpr_backups.at(i).to_string().c_str());
    }
  }
  return {buff};
}

/*!
 * Check that the epilogue matches the prologue.
 */
void Function::check_epilogue(const LinkedObjectFile& file) {
  (void)file;
  if (!prologue.decoded || suspected_asm) {
    printf("not decoded, or suspected asm, skipping epilogue\n");
    return;
  }

  // start at the end and move up.
  int idx = int(instructions.size()) - 1;

  // seek past alignment nops
  while (is_nop(instructions.at(idx))) {
    idx--;
  }

  epilogue_end = idx;
  // stack restore
  if (prologue.total_stack_usage) {
    // hack - sometimes an asm function has a compiler inserted jr ra/daddu sp sp r0 that follows
    // the "true" return.  We really should have this function flagged as asm, but for now, we can
    // simply skip over the compiler-generated jr ra/daddu sp sp r0.
    if (is_gpr_3(instructions.at(idx), InstructionKind::DADDU, make_gpr(Reg::SP), make_gpr(Reg::SP),
                 make_gpr(Reg::R0))) {
      idx--;
      assert(is_jr_ra(instructions.at(idx)));
      idx--;
      printf(
          "[Warning] %s Double Return Epilogue Hack!  This is probably an ASM function in "
          "disguise\n",
          guessed_name.to_string().c_str());
      warnings += ";; Double Return Epilogue - this is probably an ASM function\n";
    }
    // delay slot should be daddiu sp, sp, offset
    assert(is_gpr_2_imm_int(instructions.at(idx), InstructionKind::DADDIU, make_gpr(Reg::SP),
                            make_gpr(Reg::SP), prologue.total_stack_usage));
    idx--;
  } else {
    // delay slot is always daddu sp, sp, r0...
    assert(is_gpr_3(instructions.at(idx), InstructionKind::DADDU, make_gpr(Reg::SP),
                    make_gpr(Reg::SP), make_gpr(Reg::R0)));
    idx--;
  }

  // jr ra
  assert(is_jr_ra(instructions.at(idx)));
  idx--;

  // restore gprs
  for (int i = 0; i < prologue.n_gpr_backup; i++) {
    int gpr_idx = prologue.n_gpr_backup - (1 + i);
    const auto& expected_reg = gpr_backups.at(gpr_idx);
    auto expected_offset = prologue.gpr_backup_offset + 16 * i;
    assert(is_no_ll_gpr_load(instructions.at(idx), 16, true, expected_reg, expected_offset,
                             make_gpr(Reg::SP)));
    idx--;
  }

  // restore fprs
  for (int i = 0; i < prologue.n_fpr_backup; i++) {
    int fpr_idx = prologue.n_fpr_backup - (1 + i);
    const auto& expected_reg = fpr_backups.at(fpr_idx);
    auto expected_offset = prologue.fpr_backup_offset + 4 * i;
    assert(
        is_no_ll_fpr_load(instructions.at(idx), expected_reg, expected_offset, make_gpr(Reg::SP)));
    idx--;
  }

  // restore fp
  if (prologue.fp_backed_up) {
    assert(is_no_ll_gpr_load(instructions.at(idx), 8, true, make_gpr(Reg::FP),
                             prologue.fp_backup_offset, make_gpr(Reg::SP)));
    idx--;
  }

  // restore ra
  if (prologue.ra_backed_up) {
    assert(is_no_ll_gpr_load(instructions.at(idx), 8, true, make_gpr(Reg::RA),
                             prologue.ra_backup_offset, make_gpr(Reg::SP)));
    idx--;
  }

  assert(!basic_blocks.empty());
  assert(idx + 1 >= basic_blocks.back().start_word);
  basic_blocks.back().end_word = idx + 1;
  prologue.epilogue_ok = true;
  epilogue_start = idx + 1;
}

/*!
 * Look through all blocks in this function for storing the address of a function into a symbol.
 * This indicates the stored function address belongs to a global function with the same name as
 * the symbol.
 *
 * Updates the guessed_name of the function and updates type_info
 */
void Function::find_global_function_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts) {
  int state = 0;
  int label_id = -1;
  Register reg;

  for (const auto& instr : instructions) {
    // look for LUIs always
    if (instr.kind == InstructionKind::LUI && instr.get_src(0).kind == InstructionAtom::LABEL) {
      state = 1;
      reg = instr.get_dst(0).get_reg();
      label_id = instr.get_src(0).get_label();
      assert(label_id != -1);
      continue;
    }

    if (state == 1) {
      // Look for ORI
      if (instr.kind == InstructionKind::ORI && instr.get_src(0).get_reg() == reg &&
          instr.get_src(1).get_label() == label_id) {
        state = 2;
        reg = instr.get_dst(0).get_reg();
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 2) {
      // Look for SW
      if (instr.kind == InstructionKind::SW && instr.get_src(0).get_reg() == reg &&
          instr.get_src(2).get_reg() == make_gpr(Reg::S7)) {
        // done!
        std::string name = instr.get_src(1).get_sym();
        if (!file.label_points_to_code(label_id)) {
          //            printf("discard as not code: %s\n", name.c_str());
        } else {
          auto& func = file.get_function_at_label(label_id);
          assert(func.guessed_name.empty());
          func.guessed_name.set_as_global(name);
          dts.add_symbol(name, "function");
          ;
          // todo - inform function.
        }

      } else {
        state = 0;
      }
    }
  }
}

/*!
 * Look through this function to find calls to method-set! which define methods.
 * Updates the guessed_name of the function and updates type_info.
 */
void Function::find_method_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts) {
  (void)dts;
  int state = 0;
  int label_id = -1;
  int method_id = -1;
  Register lui_reg;
  std::string type_name;

  for (const auto& instr : instructions) {
    // look for lw t9, method-set!(s7)
    if (instr.kind == InstructionKind::LW && instr.get_dst(0).get_reg() == make_gpr(Reg::T9) &&
        instr.get_src(0).kind == InstructionAtom::IMM_SYM &&
        instr.get_src(0).get_sym() == "method-set!" &&
        instr.get_src(1).get_reg() == make_gpr(Reg::S7)) {
      state = 1;
      continue;
    }

    if (state == 1) {
      // look for lw a0, type-name(s7)
      if (instr.kind == InstructionKind::LW && instr.get_dst(0).get_reg() == make_gpr(Reg::A0) &&
          instr.get_src(0).kind == InstructionAtom::IMM_SYM &&
          instr.get_src(1).get_reg() == make_gpr(Reg::S7)) {
        type_name = instr.get_src(0).get_sym();
        state = 2;
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 2) {
      // look for addiu a1, r0, x
      if (instr.kind == InstructionKind::ADDIU && instr.get_dst(0).get_reg() == make_gpr(Reg::A1) &&
          instr.get_src(0).get_reg() == make_gpr(Reg::R0)) {
        method_id = instr.get_src(1).get_imm();
        state = 3;
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 3) {
      // look for lui
      if (instr.kind == InstructionKind::LUI && instr.get_src(0).kind == InstructionAtom::LABEL) {
        state = 4;
        lui_reg = instr.get_dst(0).get_reg();
        label_id = instr.get_src(0).get_label();
        assert(label_id != -1);
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 4) {
      if (instr.kind == InstructionKind::ORI && instr.get_src(0).get_reg() == lui_reg &&
          instr.get_src(1).get_label() == label_id) {
        state = 5;
        lui_reg = instr.get_dst(0).get_reg();
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 5) {
      if (instr.kind == InstructionKind::JALR && instr.get_dst(0).get_reg() == make_gpr(Reg::RA) &&
          instr.get_src(0).get_reg() == make_gpr(Reg::T9)) {
        auto& func = file.get_function_at_label(label_id);
        assert(func.guessed_name.empty());
        func.guessed_name.set_as_method(type_name, method_id);
        func.method_of_type = type_name;
        if (method_id == GOAL_INSPECT_METHOD) {
          func.is_inspect_method = true;
        }

        state = 0;
        continue;
      }
    }
  }
}

void Function::find_type_defs(LinkedObjectFile& file, DecompilerTypeSystem& dts) {
  int state = 0;
  Register temp_reg;
  std::string type_name;
  std::string parent_type;
  int label_idx = -1;

  for (const auto& instr : instructions) {
    // look for lw xx, type(s7)
    if (instr.kind == InstructionKind::LW && instr.get_src(0).kind == InstructionAtom::IMM_SYM &&
        instr.get_src(0).get_sym() == "type" && instr.get_src(1).get_reg() == make_gpr(Reg::S7)) {
      state = 1;
      temp_reg = instr.get_dst(0).get_reg();
      continue;
    }

    if (state == 1) {
      // look for lwu t9, 16, v1
      if (instr.kind == InstructionKind::LWU && instr.get_dst(0).get_reg() == make_gpr(Reg::T9) &&
          instr.get_src(0).get_imm() == 16 && instr.get_src(1).get_reg() == temp_reg) {
        state = 2;
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 2) {
      // look for daddiu a0, s7, name-of-type
      if (instr.kind == InstructionKind::DADDIU &&
          instr.get_dst(0).get_reg() == make_gpr(Reg::A0) &&
          instr.get_src(0).get_reg() == make_gpr(Reg::S7) && instr.get_src(1).is_sym()) {
        state = 3;
        type_name = instr.get_src(1).get_sym();
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 3) {
      // look for lw a1, parent-type(s7)
      if (instr.kind == InstructionKind::LW && instr.get_dst(0).get_reg() == make_gpr(Reg::A1) &&
          instr.get_src(0).kind == InstructionAtom::IMM_SYM &&
          instr.get_src(1).get_reg() == make_gpr(Reg::S7)) {
        state = 4;
        parent_type = instr.get_src(0).get_sym();
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 4) {
      // look for ld a2, LXX(fp)
      if (instr.kind == InstructionKind::LD && instr.get_dst(0).get_reg() == make_gpr(Reg::A2) &&
          instr.get_src(0).is_label() && instr.get_src(1).get_reg() == make_gpr(Reg::FP)) {
        state = 5;
        label_idx = instr.get_src(0).get_label();
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 5) {
      if (instr.kind == InstructionKind::JALR && instr.get_dst(0).get_reg() == make_gpr(Reg::RA) &&
          instr.get_src(0).get_reg() == make_gpr(Reg::T9)) {
        state = 6;
        continue;
      } else {
        state = 0;
      }
    }

    if (state == 6) {
      // look for sll v0, ra, 0
      if (instr.kind == InstructionKind::SLL && instr.get_dst(0).get_reg() == make_gpr(Reg::V0) &&
          instr.get_src(0).get_reg() == make_gpr(Reg::RA) && instr.get_src(1).get_imm() == 0) {
        // done!
        //        fmt::print("Got type {} parent {}\n", type_name, parent_type);
        dts.add_type_parent(type_name, parent_type);
        Label flag_label = file.labels.at(label_idx);
        u64 word = file.read_data_word(flag_label);
        flag_label.offset += 4;
        u64 word2 = file.read_data_word(flag_label);
        word |= (word2 << 32);
        dts.add_type_flags(type_name, word);
        //        fmt::print("Flags are 0x{:x}\n", word);
        state = 0;
        continue;
      }
    } else {
      state = 0;
    }
  }
}

void Function::add_basic_op(std::shared_ptr<IR_Atomic> op, int start_instr, int end_instr) {
  op->is_basic_op = true;
  assert(end_instr > start_instr);

  for (int i = start_instr; i < end_instr; i++) {
    instruction_to_basic_op[i] = basic_ops.size();
  }
  basic_op_to_instruction[basic_ops.size()] = start_instr;
  basic_ops.push_back(op);
}

bool Function::instr_starts_basic_op(int idx) {
  auto op = instruction_to_basic_op.find(idx);
  if (op != instruction_to_basic_op.end()) {
    auto start_instr = basic_op_to_instruction.at(op->second);
    return start_instr == idx;
  }
  return false;
}

std::shared_ptr<IR_Atomic> Function::get_basic_op_at_instr(int idx) {
  return basic_ops.at(instruction_to_basic_op.at(idx));
}

int Function::get_basic_op_count() {
  return basic_ops.size();
}

int Function::get_failed_basic_op_count() {
  int count = 0;
  for (auto& x : basic_ops) {
    if (dynamic_cast<IR_Failed*>(x.get())) {
      count++;
    }
  }
  return count;
}

int Function::get_reginfo_basic_op_count() {
  int count = 0;
  for (auto& x : basic_ops) {
    if (x->reg_info_set) {
      count++;
    }
  }
  return count;
}

/*!
 * Topological sort of basic blocks.
 * Returns a valid ordering + a list of blocks that you can't reach and therefore
 * aren't in the ordering.
 */
BlockTopologicalSort Function::bb_topo_sort() {
  BlockTopologicalSort result;
  std::unordered_set<int> visit_set;
  std::vector<int> visit_queue;
  if (basic_blocks.empty()) {
    assert(false);
  }

  visit_queue.push_back(0);

  while (!visit_queue.empty()) {
    // let's visit the most recently added:
    auto to_visit = visit_queue.back();
    visit_queue.pop_back();
    result.vist_order.push_back(to_visit);

    auto& block = basic_blocks.at(to_visit);
    for (auto next : {block.succ_branch, block.succ_ft}) {
      if (next != -1 && visit_set.find(next) == visit_set.end()) {
        visit_set.insert(next);
        visit_queue.push_back(next);
      }
    }
  }

  for (int i = 0; i < int(basic_blocks.size()); i++) {
    if (visit_set.find(i) == visit_set.end()) {
      result.unreachable.insert(i);
    }
  }

  return result;
}