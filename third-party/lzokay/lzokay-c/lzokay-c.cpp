#include "lzokay-c.h"
#include "../lzokay.hpp"

static_assert(EResult_LookbehindOverrun == lzokay_EResult(lzokay::EResult::LookbehindOverrun), "LookbehindOverrun mismatch");
static_assert(EResult_OutputOverrun == lzokay_EResult(lzokay::EResult::OutputOverrun), "OutputOverrun mismatch");
static_assert(EResult_InputOverrun == lzokay_EResult(lzokay::EResult::InputOverrun), "InputOverrun mismatch");
static_assert(EResult_Error == lzokay_EResult(lzokay::EResult::Error), "Error mismatch");
static_assert(EResult_Success == lzokay_EResult(lzokay::EResult::Success), "Success mismatch");
static_assert(EResult_InputNotConsumed == lzokay_EResult(lzokay::EResult::InputNotConsumed), "InputNotConsumed mismatch");

extern "C"
lzokay_EResult lzokay_decompress(const uint8_t * src, size_t src_size, 
                                 uint8_t *output, size_t *output_len)
{
    size_t needed_size = 0;
    lzokay::EResult error =
        lzokay::decompress(src, src_size, output, *output_len, needed_size);
    *output_len = needed_size;
    return lzokay_EResult(error);
}
