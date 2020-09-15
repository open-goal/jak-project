#pragma once

/*!
 * @file InstructionDecode.h
 * The Instruction Decoder - converts a LinkedWord into a Instruction.
 * This is the part of the disassembler that decodes MIPS instructions.
 */

#ifndef NEXT_INSTRUCTIONDECODE_H
#define NEXT_INSTRUCTIONDECODE_H

#include "Instruction.h"

class LinkedWord;
class LinkedObjectFile;

Instruction decode_instruction(LinkedWord& word, LinkedObjectFile& file, int seg_id, int word_id);

#endif  // NEXT_INSTRUCTIONDECODE_H
