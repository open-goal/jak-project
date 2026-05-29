#pragma once

#include <cstring>

#include "game/mips2c/mips2c_private.h"

namespace Mips2C::font_length {

struct Cache {
  void* font_work;
  void* font12_table;
  void* font24_table;
  void* video_params;
};

struct Vec4f {
  float x = 0.f;
  float y = 0.f;
  float z = 0.f;
  float w = 0.f;
};

inline u32 ptr_from_symbol(void* sym_addr) {
  s32 result = 0;
  std::memcpy(&result, static_cast<u8*>(sym_addr) - 1, sizeof(result));
  return result;
}

inline u8 read_u8(u32 addr) {
  u8 result = 0;
  std::memcpy(&result, g_ee_main_mem + addr, sizeof(result));
  return result;
}

inline u32 read_u32(u32 addr) {
  u32 result = 0;
  std::memcpy(&result, g_ee_main_mem + addr, sizeof(result));
  return result;
}

inline float read_f32(u32 addr) {
  float result = 0.f;
  std::memcpy(&result, g_ee_main_mem + addr, sizeof(result));
  return result;
}

inline Vec4f read_vec4f(u32 addr) {
  Vec4f result;
  std::memcpy(&result, g_ee_main_mem + addr, sizeof(result));
  return result;
}

inline void write_u32(u32 addr, u32 value) {
  std::memcpy(g_ee_main_mem + addr, &value, sizeof(value));
}

inline void write_vec4f(u32 addr, const Vec4f& value) {
  std::memcpy(g_ee_main_mem + addr, &value, sizeof(value));
}

inline u64 pack_result(float x, float y) {
  u64 result = 0;
  std::memcpy(&result, &x, sizeof(x));
  std::memcpy(reinterpret_cast<u8*>(&result) + sizeof(x), &y, sizeof(y));
  return result;
}

inline u64 execute(u32 str,
                   const u32 ctx,
                   const Cache& cache,
                   int str_ptr_offset,
                   int flags_offset) {
  constexpr u32 kFlagKerning = 1 << 1;
  constexpr u32 kFlagLarge = 1 << 5;
  constexpr u32 kFlagPcHack = 1 << 6;

  const u32 font_work = ptr_from_symbol(cache.font_work);

  Vec4f pos = read_vec4f(ctx + 12);
  const Vec4f origin = pos;
  u32 flags = read_u32(ctx + 64);

  write_u32(font_work + str_ptr_offset, str);
  write_u32(font_work + flags_offset, flags);

  u32 font_table = 0;
  Vec4f size1;
  Vec4f size2[4];

  auto use_small_font = [&]() {
    font_table = ptr_from_symbol(cache.font12_table);
    size1 = read_vec4f(font_work + 208);
    size2[0] = read_vec4f(font_work + 224);
    size2[1] = size2[0];
    size2[2] = size2[0];
    size2[3] = size2[0];
    for (int i = 0; i < 4; ++i) {
      write_vec4f(font_work + 352 + (i * 16), size2[i]);
    }
  };

  auto use_large_font = [&]() {
    font_table = ptr_from_symbol(cache.font24_table);
    size1 = read_vec4f(font_work + 256);
    for (int i = 0; i < 4; ++i) {
      size2[i] = read_vec4f(font_work + 272 + (i * 16));
      write_vec4f(font_work + 352 + (i * 16), size2[i]);
    }
  };

  auto next_char = [&]() {
    const u8 result = read_u8(str + 4);
    ++str;
    return static_cast<int>(result);
  };

  auto add_glyph = [&](int ch, const Vec4f& fixed_size) {
    const Vec4f glyph = read_vec4f(font_table + (static_cast<u32>(ch) * 16) - 96);
    const float kerned_advance = glyph.w * size1.w;
    if ((flags & kFlagKerning) && !(flags & kFlagPcHack)) {
      pos.x += kerned_advance;
    } else {
      pos.x += fixed_size.w;
    }
  };

  if (flags & kFlagLarge) {
    use_large_font();
  } else {
    use_small_font();
  }

  bool done = false;
  while (!done) {
    int ch = next_char();
    if (ch == 0) {
      break;
    }

    if (ch >= 1 && ch <= 3) {
      flags |= kFlagPcHack;
      const Vec4f& fixed_size = size2[ch == 3 ? 3 : ch == 2 ? 2 : 1];
      ch = next_char();
      add_glyph(ch, fixed_size);
      continue;
    }

    if (ch == '~') {
      ch = next_char();
      int sign = 0;
      int value = 0;
      if (ch == 0) {
        break;
      }
      if (ch == '+' || ch == '-') {
        sign = ch;
      } else if (ch == 'y' || ch == 'Y') {
        write_vec4f(font_work + 464, pos);
        continue;
      } else if (ch == 'z' || ch == 'Z') {
        pos = read_vec4f(font_work + 464);
        continue;
      } else if (ch >= '0' && ch <= '9') {
        value = ch - '0';
      } else {
        flags &= ~kFlagPcHack;
        if (ch == '\n' || ch == '\r') {
          pos.x = origin.x;
        } else {
          add_glyph(ch, size2[0]);
        }
        continue;
      }

      while (true) {
        ch = next_char();
        if (ch == 0) {
          done = true;
          break;
        }

        switch (ch) {
          case 'n':
          case 'N':
            if (value == 0) {
              flags &= ~kFlagLarge;
              use_small_font();
            } else {
              flags |= kFlagLarge;
              use_large_font();
            }
            goto next_loop;
          case 'l':
          case 'L':
          case 'w':
          case 'W':
          case 'j':
          case 'J':
          case 'v':
          case 'V':
            goto next_loop;
          case 'k':
          case 'K':
            if (value == 0) {
              flags &= ~kFlagKerning;
            } else {
              flags |= kFlagKerning;
            }
            goto next_loop;
          case 'h':
          case 'H': {
            const float amount = static_cast<float>(value);
            if (sign == 0) {
              pos.x = amount;
            } else if (sign == '-') {
              pos.x -= amount;
            } else {
              pos.x += amount;
            }
            goto next_loop;
          }
          default:
            if (ch >= '0' && ch <= '9') {
              value = (value * 10) + (ch - '0');
              continue;
            }
            flags &= ~kFlagPcHack;
            if (ch == '\n' || ch == '\r') {
              pos.x = origin.x;
            } else {
              add_glyph(ch, size2[0]);
            }
            goto next_loop;
        }
      }
    } else {
      flags &= ~kFlagPcHack;
      if (ch == '\n' || ch == '\r') {
        pos.x = origin.x;
      } else {
        add_glyph(ch, size2[0]);
      }
    }

  next_loop:;
  }

  float result_x = pos.x - origin.x;
  float result_y = pos.y - origin.y;
  const u32 original_flags = read_u32(ctx + 64);
  if (!(original_flags & kFlagPcHack)) {
    const u32 video_params = ptr_from_symbol(cache.video_params);
    result_x *= read_f32(video_params + 16);
  }
  result_x *= read_f32(ctx + 56);
  return pack_result(result_x, result_y);
}

inline u64 execute(ExecutionContext* c, const Cache& cache, int str_ptr_offset, int flags_offset) {
  return execute(c->gpr_addr(a0), c->gpr_addr(a1), cache, str_ptr_offset, flags_offset);
}

}  // namespace Mips2C::font_length
