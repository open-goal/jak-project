#include "inline_asm_rewrite.h"

#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/IR2/OpenGoalMapping.h"
#include "decompiler/analysis/reg_usage.h"

namespace decompiler {

bool rewrite_inline_asm_instructions(Form* top_level_form,
                                     FormPool& pool,
                                     Function& f,
                                     const DecompilerTypeSystem&) {
  assert(top_level_form);

  try {
    RegSet vf_regs;
    // Iterate through all TLFs
    top_level_form->apply_form([&](Form* form) {
      std::vector<FormElement*> new_entries;
      for (auto& entry : form->elts()) {
        // All vector instructions are inline assembly, so we only care to re-write assembly
        // operations
        AsmOpElement* elem = dynamic_cast<AsmOpElement*>(entry);
        if (!elem) {
          new_entries.push_back(entry);
          continue;
        }

        // We then convert the normal AsmOpElement to a more tailor-made FormElement that has
        // OpenGOAL considerations Not _all_ assembly instructors are vector
        OpenGOALAsm asmOp = OpenGOALAsm(elem->op()->instruction());
        if (!asmOp.valid) {
          // If its an invalid or unsupported exception, skip it
          /*lg::warn("[ASM Re-Write] - Unsupported inline assembly instruction kind - [{}]",
                   asmOp.instr.kind);*/
          f.warnings.general_warning("Unsupported inline assembly instruction kind - [{}]",
                                     asmOp.instr.kind);
          new_entries.push_back(entry);
          continue;
        } else if (elem->op()->instruction().kind == InstructionKind::VOPMULA) {
          // So far, the only instruction we deal with in pairs is the outer-product
          // This is kinda a hack, internally the src args of VOPMSUB will be swapped which is
          // correct and the first op we skip.
          // In the future if this needs to support more, it will be worth cleaning this up
          continue;
        } else if (asmOp.todo) {
          // If its an invalid or unsupported exception, skip it
          /*lg::warn("[ASM Re-Write] - Inline assembly instruction marked with TODO - [{}]",
                   asmOp.full_function_name());*/
          f.warnings.general_warning("Inline assembly instruction marked with TODO - [{}]",
                                     asmOp.full_function_name());
        }

        // If we've made it this far, it's an AsmOperation that is also a supported vector
        // instruction by OpenGOAL All we have to do is convert it to the correct `FormElement` that
        // will write the form so it works for OpenGOAL

        OpenGoalAsmOpElement* newElem = pool.alloc_element<OpenGoalAsmOpElement>(elem->op());
        newElem->collect_vf_regs(vf_regs);

        new_entries.push_back(newElem);
      }

      assert(!new_entries.empty());
      form->clear();
      for (auto x : new_entries) {
        form->push_back(x);
      }
    });

    // If we have to wrap the entire function in an 'rlet'
    if (!vf_regs.empty()) {
      Form* body = pool.alloc_empty_form();
      for (auto& entry : top_level_form->elts()) {
        body->push_back(entry);
      }

      RLetElement* rlet = pool.alloc_element<RLetElement>(body, vf_regs);
      top_level_form->clear();
      top_level_form->push_back(rlet);
    }
  } catch (std::exception& e) {
    std::string warning = fmt::format("ASM instruction re-writing failed in {}: {}",
                                      f.guessed_name.to_string(), e.what());
    lg::warn(warning);
    f.warnings.general_warning(";; {}", warning);
    return false;
  }

  return true;
}
}  // namespace decompiler
