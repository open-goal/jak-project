/*!
 * @file codegen_utils.h
 * Commonly used utility functions in the codegen library.
 */

#ifndef JAK_CODEGEN_UTILS_H
#define JAK_CODEGEN_UTILS_H

#include <vector>

/*!
 * Push a thing into a byte vector.
 */
template <typename T>
uint32_t push_data_to_byte_vector(T data, std::vector<uint8_t>& v) {
  auto* ptr = (uint8_t*)(&data);
  for (std::size_t i = 0; i < sizeof(T); i++) {
    v.push_back(ptr[i]);
  }
  return sizeof(T);
}

#endif  // JAK_CODEGEN_UTILS_H
