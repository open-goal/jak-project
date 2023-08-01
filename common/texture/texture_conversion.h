#pragma once

#include "common/common_types.h"
#include "common/util/Assert.h"

/*!
 * Convert from a pixel location in a texture (x, y, texture buffer width) to VRAM address (byte).
 * Uses the PSMCT32 format.
 * This format is used either to store 8-bit RGBA (texture palettes) or to copy memory.
 * See Ch. 8, Details of GS Local Memory for these tables.
 */
inline u32 psmct32_addr(u32 x, u32 y, u32 width) {
  // XXX_col refers to which XXX you're in (screen)
  // XXX refers to which XXX you're in (memory)
  // XXX_x refers to the pixel within the XXX

  // first, determine the page
  u32 pages_per_row = width / 64;
  u32 page_col = x / 64;
  u32 page_row = y / 32;
  u32 page_x = x % 64;
  u32 page_y = y % 32;
  u32 page = page_col + page_row * pages_per_row;

  // next the block
  u32 block_col = page_x / 8;
  u32 block_row = page_y / 8;
  u32 block_x = page_x % 8;
  u32 block_y = page_y % 8;
  const u32 psm32_table[4][8] = {{0, 1, 4, 5, 16, 17, 20, 21},
                                 {2, 3, 6, 7, 18, 19, 22, 23},
                                 {8, 9, 12, 13, 24, 25, 28, 29},
                                 {10, 11, 14, 15, 26, 27, 30, 31}};

  u32 block = psm32_table[block_row][block_col];

  // next the column (there's only one "column" per column)
  u32 col_row = block_y / 2;
  u32 col_y = block_y % 2;
  u32 col_x = block_x;

  // next the pixel
  const u32 psm32_pix_table[2][8] = {{0, 1, 4, 5, 8, 9, 12, 13}, {2, 3, 6, 7, 10, 11, 14, 15}};
  u32 pixel = psm32_pix_table[col_y][col_x];

  // now the sum
  return ((page * 64 * 32) + (block * 8 * 8) + (col_row * 8 * 2) + pixel) * 4;
}

/*!
 * Convert from a pixel location in a texture (x, y, texture buffer width) to VRAM address (byte).
 * Uses the PSMT8 format.
 * This format is used either to store 8-bit palette indices, used in most textures.
 * See Ch. 8, Details of GS Local Memory for these tables.
 */
inline u32 psmt8_addr(u32 x, u32 y, u32 width) {
  // page is 128, 64
  // block is 16, 16
  // column is 16, 4

  // first determine the page
  // Note: not actually sure what the GS does here...
  u32 pages_per_row = std::max(1u, width / 128);
  u32 page_col = x / 128;
  u32 page_row = y / 64;
  u32 page_x = x % 128;
  u32 page_y = y % 64;
  u32 page = page_col + page_row * pages_per_row;

  // next block
  u32 block_col = page_x / 16;
  u32 block_row = page_y / 16;
  u32 block_x = page_x % 16;
  u32 block_y = page_y % 16;
  const u32 psm32_table[4][8] = {{0, 1, 4, 5, 16, 17, 20, 21},
                                 {2, 3, 6, 7, 18, 19, 22, 23},
                                 {8, 9, 12, 13, 24, 25, 28, 29},
                                 {10, 11, 14, 15, 26, 27, 30, 31}};
  u32 block = psm32_table[block_row][block_col];  // it's the same table!!!

  // both columns and pixels within columns.
  const uint8_t pix_table[16][16] = {
      {0, 4, 16, 20, 32, 36, 48, 52, 2, 6, 18, 22, 34, 38, 50, 54},
      {8, 12, 24, 28, 40, 44, 56, 60, 10, 14, 26, 30, 42, 46, 58, 62},
      {33, 37, 49, 53, 1, 5, 17, 21, 35, 39, 51, 55, 3, 7, 19, 23},
      {41, 45, 57, 61, 9, 13, 25, 29, 43, 47, 59, 63, 11, 15, 27, 31},
      {96, 100, 112, 116, 64, 68, 80, 84, 98, 102, 114, 118, 66, 70, 82, 86},
      {104, 108, 120, 124, 72, 76, 88, 92, 106, 110, 122, 126, 74, 78, 90, 94},
      {65, 69, 81, 85, 97, 101, 113, 117, 67, 71, 83, 87, 99, 103, 115, 119},
      {73, 77, 89, 93, 105, 109, 121, 125, 75, 79, 91, 95, 107, 111, 123, 127},
      {128, 132, 144, 148, 160, 164, 176, 180, 130, 134, 146, 150, 162, 166, 178, 182},
      {136, 140, 152, 156, 168, 172, 184, 188, 138, 142, 154, 158, 170, 174, 186, 190},
      {161, 165, 177, 181, 129, 133, 145, 149, 163, 167, 179, 183, 131, 135, 147, 151},
      {169, 173, 185, 189, 137, 141, 153, 157, 171, 175, 187, 191, 139, 143, 155, 159},
      {224, 228, 240, 244, 192, 196, 208, 212, 226, 230, 242, 246, 194, 198, 210, 214},
      {232, 236, 248, 252, 200, 204, 216, 220, 234, 238, 250, 254, 202, 206, 218, 222},
      {193, 197, 209, 213, 225, 229, 241, 245, 195, 199, 211, 215, 227, 231, 243, 247},
      {201, 205, 217, 221, 233, 237, 249, 253, 203, 207, 219, 223, 235, 239, 251, 255},
  };

  u32 pixel = pix_table[block_y][block_x];
  return (page * 128 * 64) + (block * 16 * 16) + pixel;
}

inline u32 psmct16_addr(u32 x, u32 y, u32 width) {
  // page is 64x64
  // block is 16x8
  // column is 16x2

  // page
  u32 pages_per_row = width / 64;
  u32 page_col = x / 64;
  u32 page_row = y / 64;
  u32 page_x = x % 64;
  u32 page_y = y % 64;
  u32 page = page_col + page_row * pages_per_row;

  // block
  u32 block_col = page_x / 16;
  u32 block_row = page_y / 8;
  u32 block_x = page_x % 16;
  u32 block_y = page_y % 8;
  const u32 psm16_table[8][4] = {{0, 2, 8, 10},    {1, 3, 9, 11},    {4, 6, 12, 14},
                                 {5, 7, 13, 15},   {16, 18, 24, 26}, {17, 19, 25, 27},
                                 {20, 22, 28, 30}, {21, 23, 29, 31}};
  u32 block = psm16_table[block_row][block_col];

  const uint8_t pix_tabel[8][16] = {
      {0, 2, 8, 10, 16, 18, 24, 26, 1, 3, 9, 11, 17, 19, 25, 27},
      {4, 6, 12, 14, 20, 22, 28, 30, 5, 7, 13, 15, 21, 23, 29, 31},
      {32, 34, 40, 42, 48, 50, 56, 58, 33, 35, 41, 43, 49, 51, 57, 59},
      {36, 38, 44, 46, 52, 54, 60, 62, 37, 39, 45, 47, 53, 55, 61, 63},
      {64, 66, 72, 74, 80, 82, 88, 90, 65, 67, 73, 75, 81, 83, 89, 91},
      {68, 70, 76, 78, 84, 86, 92, 94, 69, 71, 77, 79, 85, 87, 93, 95},
      {96, 98, 104, 106, 112, 114, 120, 122, 97, 99, 105, 107, 113, 115, 121, 123},
      {100, 102, 108, 110, 116, 118, 124, 126, 101, 103, 109, 111, 117, 119, 125, 127},
  };
  u32 pixel = pix_tabel[block_y][block_x];
  return 2 * ((page * 64 * 64) + (block * 16 * 8) + pixel);
}

inline u32 psmt4_addr_half_byte(u32 x, u32 y, u32 width) {
  // page is 128, 128
  // block is 32, 16
  // column is 32, 4

  // first determine the page
  u32 pages_per_row = width / 128;
  u32 page_col = x / 128;
  u32 page_row = y / 128;
  u32 page_x = x % 128;
  u32 page_y = y % 128;
  u32 page = page_col + page_row * pages_per_row;

  // next block
  u32 block_col = page_x / 32;
  u32 block_row = page_y / 16;
  u32 block_x = page_x % 32;
  u32 block_y = page_y % 16;
  const u32 psm4_table[8][4] = {{0, 2, 8, 10},    {1, 3, 9, 11},    {4, 6, 12, 14},
                                {5, 7, 13, 15},   {16, 18, 24, 26}, {17, 19, 25, 27},
                                {20, 22, 28, 30}, {21, 23, 29, 31}};
  ASSERT(block_row < 8);
  ASSERT(block_col < 4);
  u32 block = psm4_table[block_row][block_col];  // it's the same table!!!

  // both columns and pixels within columns.
  const uint16_t pix_table[16][32] = {
      {0, 8,  32, 40, 64, 72, 96,  104, 2, 10, 34, 42, 66, 74, 98,  106,
       4, 12, 36, 44, 68, 76, 100, 108, 6, 14, 38, 46, 70, 78, 102, 110},
      {16, 24, 48, 56, 80, 88, 112, 120, 18, 26, 50, 58, 82, 90, 114, 122,
       20, 28, 52, 60, 84, 92, 116, 124, 22, 30, 54, 62, 86, 94, 118, 126},
      {65, 73, 97,  105, 1, 9,  33, 41, 67, 75, 99,  107, 3, 11, 35, 43,
       69, 77, 101, 109, 5, 13, 37, 45, 71, 79, 103, 111, 7, 15, 39, 47},
      {81, 89, 113, 121, 17, 25, 49, 57, 83, 91, 115, 123, 19, 27, 51, 59,
       85, 93, 117, 125, 21, 29, 53, 61, 87, 95, 119, 127, 23, 31, 55, 63},
      {192, 200, 224, 232, 128, 136, 160, 168, 194, 202, 226, 234, 130, 138, 162, 170,
       196, 204, 228, 236, 132, 140, 164, 172, 198, 206, 230, 238, 134, 142, 166, 174},
      {208, 216, 240, 248, 144, 152, 176, 184, 210, 218, 242, 250, 146, 154, 178, 186,
       212, 220, 244, 252, 148, 156, 180, 188, 214, 222, 246, 254, 150, 158, 182, 190},
      {129, 137, 161, 169, 193, 201, 225, 233, 131, 139, 163, 171, 195, 203, 227, 235,
       133, 141, 165, 173, 197, 205, 229, 237, 135, 143, 167, 175, 199, 207, 231, 239},
      {145, 153, 177, 185, 209, 217, 241, 249, 147, 155, 179, 187, 211, 219, 243, 251,
       149, 157, 181, 189, 213, 221, 245, 253, 151, 159, 183, 191, 215, 223, 247, 255},
      {256, 264, 288, 296, 320, 328, 352, 360, 258, 266, 290, 298, 322, 330, 354, 362,
       260, 268, 292, 300, 324, 332, 356, 364, 262, 270, 294, 302, 326, 334, 358, 366},
      {272, 280, 304, 312, 336, 344, 368, 376, 274, 282, 306, 314, 338, 346, 370, 378,
       276, 284, 308, 316, 340, 348, 372, 380, 278, 286, 310, 318, 342, 350, 374, 382},
      {321, 329, 353, 361, 257, 265, 289, 297, 323, 331, 355, 363, 259, 267, 291, 299,
       325, 333, 357, 365, 261, 269, 293, 301, 327, 335, 359, 367, 263, 271, 295, 303},
      {337, 345, 369, 377, 273, 281, 305, 313, 339, 347, 371, 379, 275, 283, 307, 315,
       341, 349, 373, 381, 277, 285, 309, 317, 343, 351, 375, 383, 279, 287, 311, 319},
      {448, 456, 480, 488, 384, 392, 416, 424, 450, 458, 482, 490, 386, 394, 418, 426,
       452, 460, 484, 492, 388, 396, 420, 428, 454, 462, 486, 494, 390, 398, 422, 430},
      {464, 472, 496, 504, 400, 408, 432, 440, 466, 474, 498, 506, 402, 410, 434, 442,
       468, 476, 500, 508, 404, 412, 436, 444, 470, 478, 502, 510, 406, 414, 438, 446},
      {385, 393, 417, 425, 449, 457, 481, 489, 387, 395, 419, 427, 451, 459, 483, 491,
       389, 397, 421, 429, 453, 461, 485, 493, 391, 399, 423, 431, 455, 463, 487, 495},
      {401, 409, 433, 441, 465, 473, 497, 505, 403, 411, 435, 443, 467, 475, 499, 507,
       405, 413, 437, 445, 469, 477, 501, 509, 407, 415, 439, 447, 471, 479, 503, 511},
  };

  ASSERT(block_y < 16);
  ASSERT(block_x < 32);
  u32 pixel = pix_table[block_y][block_x];
  return (page * 128 * 128) + (block * 32 * 16) + pixel;
}

inline u32 rgba16_to_rgba32(u32 in) {
  float ratio = 255.0 / 31.0;
  u32 r = (in & 0b11111) * ratio;
  u32 g = ((in >> 5) & 0b11111) * ratio;
  u32 b = ((in >> 10) & 0b11111) * ratio;

  // rgba has only 1 bit for a and how it gets converted depends on the value of ta1.
  // for now, it looks like they always use 0x80, so this is fine.
  u32 a = (in & 0x8000) ? 0x80 : 0;

  return (a << 24) | (b << 16) | (g << 8) | r;
}

// texture format enums
enum class PSM { PSMCT32 = 0x0, PSMCT16 = 0x02, PSMT8 = 0x13, PSMT4 = 0x14 };
// clut format enums
enum class CPSM { PSMCT32 = 0x0, PSMCT16 = 0x02 };
