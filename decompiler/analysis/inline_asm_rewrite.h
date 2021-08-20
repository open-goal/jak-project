#pragma once

namespace decompiler {
class Form;
class Function;
class FormPool;
class DecompilerTypeSystem;
bool rewrite_inline_asm_instructions(Form* top_level_form,
                                     FormPool& pool,
                                     Function& f,
                                     const DecompilerTypeSystem& dts);
}  // namespace decompiler
