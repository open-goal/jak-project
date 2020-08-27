#ifndef JAK_CODEGENOUTPUT_H
#define JAK_CODEGENOUTPUT_H

#include <cstdint>
#include <array>
#include <vector>
#include "shared_config.h"

/*!
 * The result of the codegen process.
 * It is stored part by part for debugging purposes at this point
 * but the to_vector() method knows how to combine it into a blob for loading.
 */
struct CodegenOutput {
  // code and objects
  std::array<std::vector<uint8_t>, N_SEG> code;

  // the link data
  std::array<std::vector<uint8_t>, N_SEG> link_tables;

  // maps from instr_idx to offset into code
  std::array<std::vector<int>, N_SEG> instr_offsets;

  // the header data (goes before the link_tables to form the link section)
  std::vector<uint8_t> header;

  // offset into the segment's code for where the static objects start.
  std::array<int, N_SEG> static_start;

  // make into a single blob for loading into the runtime.
  std::vector<uint8_t> to_vector();
};

#endif  // JAK_CODEGENOUTPUT_H
