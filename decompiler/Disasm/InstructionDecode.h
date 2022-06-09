#pragma once

/*!
 * @file InstructionDecode.h
 * The Instruction Decoder - converts a LinkedWord into a Instruction.
 * This is the part of the disassembler that decodes MIPS instructions.
 */
#include "Instruction.h"

namespace decompiler {
class LinkedWord;
class LinkedObjectFile;

Instruction decode_instruction(LinkedWord& word, LinkedObjectFile& file, int seg_id, int word_id);
}  // namespace decompiler
