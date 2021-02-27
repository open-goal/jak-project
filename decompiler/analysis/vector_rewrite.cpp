#include "vector_rewrite.h"

#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "common/goos/PrettyPrinter.h"
#include "decompiler/IR2/OpenGoalMapping.h"
#include "decompiler/analysis/reg_usage.h"

namespace decompiler {

bool rewrite_vector_instructions(Form* top_level_form,
                                 FormPool& pool,
                                 Function& f,
                                 const DecompilerTypeSystem& dts) {
  assert(top_level_form);

  try {
    std::vector<FormElement*> new_entries;
    AsmOpElement* previousAsmOp = nullptr;
    RegSet vf_regs;
    for (auto& entry : top_level_form->elts()) {
      // All vector instructions are inline assembly, so we only care to re-write assembly
      // operations
      AsmOpElement* elem = dynamic_cast<AsmOpElement*>(entry);
      if (!elem) {
        new_entries.push_back(entry);
        continue;
      }

      // We then convert the normal AsmOpElement to a more tailor-made FormElement that has OpenGOAL
      // considerations Not _all_ assembly instructors are vector
      OpenGOALAsm asmOp = OpenGOALAsm(elem->op()->instruction());
      if (!asmOp.valid) {
        // If its an invalid or unsupported exception, skip it
        lg::warn(
            "[Vector Re-Write] - Found an unsupported inline assembly instruction kind - [{}]!",
            asmOp.instr.kind);
        new_entries.push_back(entry);
        continue;
      }

      // If we've made it this far, it's an AsmOperation that is also a supported vector instruction
      // by OpenGOAL All we have to do is convert it to the correct `FormElement` that will write
      // the form so it works for OpenGOAL

      // So far, the only instruction we deal with in pairs is the outer-product
      // This is kinda a hack, internally the src args of VOPMSUB will be swapped which is correct
      // and the first op we skip.  In the future if this needs to support more, it will be worth
      // cleaning this up
      if (elem->op()->instruction().kind == InstructionKind::VOPMULA) {
        // We found the first instruction, store it and move on. We'll use this instruction's args
        // combined with the second!
        if (previousAsmOp != nullptr) {
          lg::warn("[Vector Re-Write] - Found a VOPMULA after a preceeding VOPMULA!");
          new_entries.push_back(previousAsmOp);
        }
        previousAsmOp = elem;
        continue;
      }

      OpenGoalAsmOpElement* newElem = pool.alloc_element<OpenGoalAsmOpElement>(elem->op());
      newElem->collect_vf_regs(vf_regs);

      new_entries.push_back(newElem);
    }

    assert(!new_entries.empty());
    top_level_form->clear();
    // NOTE - in the future, there might be a benefit to define an arbitrary init for each vector
    // reg but currently, they are basically constants, so just pass in a RegSet
    if (!vf_regs.empty()) {
      Form* body = pool.alloc_empty_form();
      for (auto x : new_entries) {
        body->push_back(x);
      }
      RLetElement* rlet = pool.alloc_element<RLetElement>(body, vf_regs);
      top_level_form->push_back(rlet);
    } else {
      for (auto x : new_entries) {
        top_level_form->push_back(x);
      }
    }
  } catch (std::exception& e) {
    std::string warning = fmt::format("Vector instruction re-writing failed in {}: {}",
                                      f.guessed_name.to_string(), e.what());
    lg::warn(warning);
    // TODO - changed to what? f.warnings.append(";; " + warning);
    return false;
  }

  return true;
}
}  // namespace decompiler
