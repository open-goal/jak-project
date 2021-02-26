#include "vector_rewrite.h"

#include "decompiler/Function/Function.h"
#include "decompiler/IR2/Form.h"
#include "decompiler/IR2/FormStack.h"
#include "decompiler/util/DecompilerTypeSystem.h"
#include "common/goos/PrettyPrinter.h"
#include <decompiler/IR2/OpenGoalMapping.h>

namespace decompiler {

bool rewrite_vector_instructions(Form* top_level_form,
                                 FormPool& pool,
                                 Function& f,
                                 const DecompilerTypeSystem& dts) {
  assert(top_level_form);

  try {
    std::vector<FormElement*> new_entries;
    AsmOpElement* previousAsmOp = nullptr;
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
      OpenGOALAsm asmOp = OpenGOALAsm(elem->op()->instruction().kind);
      if (!asmOp.valid) {
        // If its an invalid or unsupported exception, skip it
        new_entries.push_back(entry);
        continue;
      }

      // If we've made it this far, it's an AsmOperation that is also a supported vector instruction
      // by OpenGOAL All we have to do is convert it to the correct `FormElement` that will write
      // the form so it works for OpenGOAL

      // There are so far only one set of instructions that come in pairs, the outer-product
      // instructions In OpenGOAL this is a single function call, to support this, we implement
      // atleast 1 instruction look-ahead (this could be expanded upon if needed)

      if (elem->op()->instruction().kind == InstructionKind::VOPMULA) {
        // We found the first instruction, store it and move on. We'll use this instruction's args
        // combined with the second!
        if (previousAsmOp != nullptr) {
          lg::warn("[Vector Re-Write] - Found a VOPMULA after a preceeding VOPMULA!");
          new_entries.push_back(previousAsmOp);
        }
        previousAsmOp = elem;
        continue;
      } else if (previousAsmOp != nullptr &&
                 elem->op()->instruction().kind == InstructionKind::VOPMSUB) {
        // TODO - I'll clean this up later, for now im just going to swap VOPMSUB's secondary args
        // because...i want to see something working!
      }

      OpenGoalAsmOpElement* newElem = pool.alloc_element<OpenGoalAsmOpElement>(elem->op());
      new_entries.push_back(newElem);
    }

    assert(!new_entries.empty());
    top_level_form->clear();
    Form* body = pool.alloc_empty_form();
    for (auto x : new_entries) {
      body->push_back(x);
    }
    // if needed!
    std::vector<std::tuple<std::string, decompiler::RLetElement::RegClass>> regs = {};
    RLetElement* rlet = pool.alloc_element<RLetElement>(body, regs);
    top_level_form->push_back(rlet);
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
