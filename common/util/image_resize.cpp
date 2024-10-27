#include "image_resize.h"

#include <cmath>

#include "common/math/Vector.h"

namespace {
struct BilinearSample {
  int i0, i1;
  double w0, w1;
};

BilinearSample bilinear(int src_i, double sample, bool wrap) {
  BilinearSample result;

  const double px_size = 1. / double(src_i);
  const double px_center_f = (sample - 0.5 * px_size) / px_size;
  const int px_floor = std::floor(px_center_f);
  const double frac = px_center_f - double(px_floor);

  if (px_center_f < 0) {  // off the bottom
    if (wrap) {
      // wrap around!
      result.i0 = 0;
      result.i1 = src_i - 1;
      result.w1 = -px_center_f;
      result.w0 = 1. - result.w1;
    } else {
      // clamp to edge
      result.i0 = 0;
      result.i1 = 0;
      result.w0 = 1;
      result.w1 = 0;
    }
  } else if (px_floor >= (src_i - 1)) {  // off the top
    if (wrap) {
      result.i0 = src_i - 1;
      result.i1 = 0;
      result.w0 = 1. - frac;
      result.w1 = frac;
    } else {
      // clamp
      result.i0 = src_i - 1;
      result.i1 = src_i - 1;
      result.w0 = 1;
      result.w1 = 0;
    }
  } else {
    result.i0 = px_floor;
    result.i1 = px_floor + 1;
    result.w0 = 1. - frac;
    result.w1 = frac;
  }

  return result;
}

math::Vector4<u8> sample1(const u8* src, int w, int h, int x, int y) {
  (void)h;
  int offset = 4 * (y * w + x);
  return math::Vector4<u8>(src[offset], src[offset + 1], src[offset + 2], src[offset + 3]);
}

math::Vector4<u8> sample_bilinear(const u8* src,
                                  int w,
                                  int h,
                                  const BilinearSample& x,
                                  const BilinearSample& y) {
  auto p00 = sample1(src, w, h, x.i0, y.i0).cast<double>();
  auto p01 = sample1(src, w, h, x.i0, y.i1).cast<double>();
  auto p10 = sample1(src, w, h, x.i1, y.i0).cast<double>();
  auto p11 = sample1(src, w, h, x.i1, y.i1).cast<double>();
  auto p_interp = (p00 * x.w0 * y.w0 + p01 * x.w0 * y.w1 + p10 * x.w1 * y.w0 + p11 * x.w1 * y.w1);
  return p_interp.cast<u8>();
}
}  // namespace

void resize_rgba_image(u8* dst,
                       int dst_w,
                       int dst_h,
                       const u8* src,
                       int src_w,
                       int src_h,
                       bool wrap_w,
                       bool wrap_h) {
  const double dst_px_h = 1. / dst_h;
  const double dst_px_w = 1. / dst_w;
  for (int h = 0; h < dst_h; h++) {
    const float h_pos = (double(h) + 0.5) * dst_px_h;
    const auto h_sample = bilinear(src_h, h_pos, wrap_h);
    for (int w = 0; w < dst_w; w++) {
      const float w_pos = (double(w) + 0.5) * dst_px_w;
      const auto w_sample = bilinear(src_w, w_pos, wrap_w);
      auto result = sample_bilinear(src, src_w, src_h, w_sample, h_sample);
      for (int i = 0; i < 4; i++) {
        *dst = result[i];
        dst++;
      }
    }
  }
}
