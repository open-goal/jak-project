#include <cstdio>
#include <vector>

#include "common/common_types.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/image_resize.h"

int main() {
  int src_h = 30;
  int src_w = 30;
  int check_divide = 3;
  int dst_sz = 300;

  std::vector<u8> src;
  for (int h = 0; h < src_h; h++) {
    for (int w = 0; w < src_w; w++) {
      u8 color = (((h / check_divide) & 1) ^ ((w / check_divide) & 1)) ? 0x10 : 0xd0;
      src.push_back(color);
      src.push_back(color);
      src.push_back(color);
      src.push_back(255);
    }
  }

  std::vector<u8> dst(dst_sz * dst_sz * 4);
  Timer timer;
  resize_rgba_image(dst.data(), dst_sz, dst_sz, src.data(), src_w, src_h, true, true);
  printf("resized in %.3f ms\n", timer.getMs());
  file_util::write_rgba_png("test_wrap.png", dst.data(), dst_sz, dst_sz);
  resize_rgba_image(dst.data(), dst_sz, dst_sz, src.data(), src_w, src_h, false, false);
  printf("resized in %.3f ms\n", timer.getMs());
  file_util::write_rgba_png("test_unwrap.png", dst.data(), dst_sz, dst_sz);
  file_util::write_rgba_png("src.png", src.data(), src_w, src_h);

  return 0;
}