#pragma once

#include "common/common_types.h"

void resize_rgba_image(u8* dst,
                       int dst_w,
                       int dst_h,
                       const u8* src,
                       int src_w,
                       int src_h,
                       bool wrap_w,
                       bool wrap_h);