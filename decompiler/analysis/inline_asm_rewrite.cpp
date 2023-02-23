#include "inline_asm_rewrite.h"
#include "common/goos/PrettyPrinter.h"
#include "common/log/log.h"
#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"
#include "decompiler/IR2/OpenGoalMapping.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"
#include "decompiler/analysis/reg_usage.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {

bool rewrite_inline_asm_instructions(Form* top_level_form,
                                     FormPool& pool,
                                     Function& f,
                                     const DecompilerTypeSystem&) {
  ASSERT(top_level_form);

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
          auto as_load_store = dynamic_cast<VectorFloatLoadStoreElement*>(entry);
          if (as_load_store) {
            as_load_store->collect_vf_regs(vf_regs);
          }
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
          f.warnings.error("Unsupported inline assembly instruction kind - [{}]",
                           asmOp.m_instr.to_string(f.ir2.env.file->labels));
          new_entries.push_back(entry);
          continue;
        } else if (asmOp.skip) {
          continue;
        } else if (asmOp.todo) {
          // If its an invalid or unsupported exception, skip it
          /*lg::warn("[ASM Re-Write] - Inline assembly instruction marked with TODO - [{}]",
                   asmOp.full_function_name());*/
          f.warnings.error("Inline assembly instruction marked with TODO - [{}]",
                           asmOp.full_function_name());
        }

        // If we've made it this far, it's an AsmOperation that is also a supported vector
        // instruction by OpenGOAL All we have to do is convert it to the correct `FormElement` that
        // will write the form so it works for OpenGOAL

        OpenGoalAsmOpElement* newElem = pool.alloc_element<OpenGoalAsmOpElement>(elem->op());
        newElem->collect_vf_regs(vf_regs);

        new_entries.push_back(newElem);
      }

      ASSERT(!new_entries.empty());
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
    std::string warning =
        fmt::format("ASM instruction re-writing failed in {}: {}", f.name(), e.what());
    lg::warn(warning);
    f.warnings.error(";; {}", warning);
    return false;
  }

  return true;
}
}  // namespace decompiler
