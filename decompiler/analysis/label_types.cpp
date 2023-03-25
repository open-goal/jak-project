#include "label_types.h"

#include "decompiler/IR2/LabelDB.h"
#include "decompiler/ObjectFile/LinkedObjectFile.h"

namespace decompiler {

namespace {
void scan_loads(LabelDB* db, Function* func) {
  if (!func->ir2.atomic_ops_succeeded) {
    return;
  }

  const auto& ao = *func->ir2.atomic_ops;
  for (auto& op : ao.ops) {
    auto as_load = dynamic_cast<const LoadVarOp*>(op.get());
    if (as_load) {
      const auto& src = as_load->src();
      if (src.is_identity() && src.get_arg(0).is_label()) {
        int label_idx = src.get_arg(0).label();
        if (as_load->size() == 8 && as_load->kind() == LoadVarOp::Kind::UNSIGNED) {
          const auto& existing = db->lookup(label_idx);
          if (existing.known && !existing.is_value) {
            throw std::runtime_error(fmt::format(
                "Label {} has an existing definition which is incompatible with a 64-bit load.",
                existing.print()));

          } else if (!existing.known) {
            db->set_and_get_previous(label_idx, TypeSpec("uint64"), true, {});
          }
        } else if (as_load->size() == 4 && (as_load->kind() == LoadVarOp::Kind::SIGNED ||
                                            as_load->kind() == LoadVarOp::Kind::FLOAT)) {
          const auto& existing = db->lookup(label_idx);
          if (existing.known && !existing.is_value) {
            throw std::runtime_error(fmt::format(
                "Label {} has an existing definition which is incompatible with a 32-bit load.",
                existing.print()));

          } else if (!existing.known) {
            db->set_and_get_previous(label_idx, TypeSpec("float"), true, {});
          }
        }
      }
    }
  }
}

void find_functions(LabelDB* db, LinkedObjectFile* file) {
  // loop over all functions.  Each will always have a label
  for (int seg = 0; seg < N_SEG; seg++) {
    for (auto& func : file->functions_by_seg.at(seg)) {
      if (seg != TOP_LEVEL_SEGMENT) {
        // first, the label for the start of the function
        // + 4 bytes for the type tag.
        int offset_of_function = func.start_word * 4 + 4;
        auto idx_of_label = db->try_get_index_by_offset(seg, offset_of_function);
        if (!idx_of_label) {
          func.warnings.warning("Could not find any references to this function: {}", func.name());
        } else {
          auto old = db->set_and_get_previous(*idx_of_label, func.type, false, {});
          if (old.known) {
            throw std::runtime_error(fmt::format(
                "There is a config entry for label {}, but it's a function. Remove the config.",
                old.name));
          }
        }
      }

      // next, labels for things loaded (value labels)
      scan_loads(db, &func);
    }
  }
}

void find_boxed(LabelDB* db, LinkedObjectFile* file) {
  for (auto& lab : file->labels) {
    if ((lab.offset & 7) == BASIC_OFFSET) {
      // it's a basic! probably.
      const auto& word = file->words_by_seg.at(lab.target_segment).at((lab.offset - 4) / 4);
      // the snowball-bank is a weird basic with no fields other than the built-in type.
      // so it can actually share a label with something else.
      if (word.kind() == LinkedWord::TYPE_PTR && word.symbol_name() != "snowball-bank") {
        TypeSpec basic_type(word.symbol_name());
        if (word.symbol_name() == "array") {
          const auto& type_word =
              file->words_by_seg.at(lab.target_segment).at((lab.offset - 4 + 12) / 4);
          if (type_word.kind() == LinkedWord::TYPE_PTR) {
            basic_type.add_arg(TypeSpec(type_word.symbol_name()));
          }
        }
        const auto& existing = db->lookup(lab.name);
        if (existing.known) {
          if (existing.result_type.base_type() != basic_type.base_type()) {
            throw std::runtime_error(fmt::format("Conflicting types for label {}: {} and {}",
                                                 existing.name, existing.result_type.print(),
                                                 basic_type.print()));
          }
        } else {
          db->set_and_get_previous(db->get_index_by_name(lab.name), basic_type, false, {});
        }
      }
    } else if ((lab.offset & 7) == PAIR_OFFSET) {
      auto existing =
          db->set_and_get_previous(db->get_index_by_name(lab.name), TypeSpec("pair"), false, {});
      if (existing.known) {
        throw std::runtime_error(fmt::format(
            "There is an entry for label {}, but it is a pair and does not need an entry",
            existing.print()));
      }
    }
  }
}

}  // namespace
void analyze_labels(LabelDB* db, LinkedObjectFile* file) {
  find_functions(db, file);
  find_boxed(db, file);
}
}  // namespace decompiler
