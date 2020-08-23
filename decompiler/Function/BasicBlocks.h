#ifndef JAK_DISASSEMBLER_BASICBLOCKS_H
#define JAK_DISASSEMBLER_BASICBLOCKS_H

#include <vector>
#include <memory>

#include "CfgVtx.h"

class LinkedObjectFile;
class Function;

struct BasicBlock {
  int start_word;
  int end_word;

  BasicBlock(int _start_word, int _end_word) : start_word(_start_word), end_word(_end_word) {}
};

std::vector<BasicBlock> find_blocks_in_function(const LinkedObjectFile& file,
                                                int seg,
                                                const Function& func);

#endif  // JAK_DISASSEMBLER_BASICBLOCKS_H
