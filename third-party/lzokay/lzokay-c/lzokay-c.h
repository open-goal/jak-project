#ifndef LZOKAY_C_INCLUDED
#define LZOKAY_C_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stddef.h>

typedef enum {
  EResult_LookbehindOverrun = -4,
  EResult_OutputOverrun = -3,
  EResult_InputOverrun = -2,
  EResult_Error = -1,
  EResult_Success = 0,
  EResult_InputNotConsumed = 1,
} lzokay_EResult;

lzokay_EResult lzokay_decompress(const uint8_t * src, size_t src_size, 
                                 uint8_t *output, size_t *output_len);

#ifdef __cplusplus
}
#endif

#endif // LZOKAY_C_INCLUDED
