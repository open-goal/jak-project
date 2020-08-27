#include "CodegenOutput.h"

/*!
 * Convert codegen output to a single binary blob.
 */
std::vector<uint8_t> CodegenOutput::to_vector() {
  std::vector<uint8_t> result;
  // header
  result.insert(result.end(), header.begin(), header.end());

  // link tables
  for (int seg = N_SEG; seg-- > 0;) {
    result.insert(result.end(), link_tables[seg].begin(), link_tables[seg].end());
  }

  // data (code + static objects, by segment)
  for (int seg = N_SEG; seg-- > 0;) {
    result.insert(result.end(), code[seg].begin(), code[seg].end());
  }
  return result;
}