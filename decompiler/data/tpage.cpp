/*!
 * @file tpage.cpp
 * Extract textures from a tpage GOAL object file.
 *
 * TODO -
 * support 24-bit textures
 * support 4-bit CLUT
 * support other cpsms
 * export other mips
 * check all data is read
 * export info files
 * investigate null textures
 * report statistics (number of textures, memory, success...)
 * check duplicate names
 */

#include <common/util/FileUtil.h>
#include "tpage.h"
#include "common/versions.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "third-party/fmt/core.h"

namespace decompiler {
namespace {

/*!
 * Convert from a pixel location in a texture (x, y, texture buffer width) to VRAM address (byte).
 * Uses the PSMCT32 format.
 * This format is used either to store 8-bit RGBA (texture palettes) or to copy memory.
 * See Ch. 8, Details of GS Local Memory for these tables.
 */
u32 psmct32_addr(u32 x, u32 y, u32 width) {
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

u32 psmct16_addr(u32 x, u32 y, u32 width) {
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

/*!
 * Convert from a pixel location in a texture (x, y, texture buffer width) to VRAM address (byte).
 * Uses the PSMT8 format.
 * This format is used either to store 8-bit palette indices, used in most textures.
 * See Ch. 8, Details of GS Local Memory for these tables.
 */
u32 psmt8_addr(u32 x, u32 y, u32 width) {
  // page is 128, 64
  // block is 16, 16
  // column is 16, 4

  // first determine the page
  u32 pages_per_row = width / 128;
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

u32 psmt4_addr_half_byte(u32 x, u32 y, u32 width) {
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
  assert(block_row < 8);
  assert(block_col < 4);
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

  assert(block_y < 16);
  assert(block_x < 32);
  u32 pixel = pix_table[block_y][block_x];
  return (page * 128 * 128) + (block * 32 * 16) + pixel;
}

u32 rgba16_to_rgba32(u32 in) {
  float ratio = 255.0 / 31.0;
  u32 r = (in & 0b11111) * ratio;
  u32 g = ((in >> 5) & 0b11111) * ratio;
  u32 b = ((in >> 10) & 0b11111) * ratio;
  u32 a = (in & 0x8000) * 0x1FE00;

  return a | (b << 16) | (g << 8) | r;
}

/*
(deftype texture-page-segment (structure)
  ((block-data pointer :offset-assert 0)
   (size       uint32  :offset-assert 4)
   (dest       uint32  :offset-assert 8)
   )

(deftype texture-page (basic)
  ((info      basic                  :offset-assert 4)
   (name      basic                  :offset-assert 8)
   (id        uint32                 :offset-assert 12)
   (length    int32                  :offset-assert 16)
   (mip0-size uint32                 :offset-assert 20)
   (size      uint32                 :offset-assert 24)
   (segment   texture-page-segment 3 :inline :offset-assert 28)
   (pad       uint32 16              :offset-assert 64)
   (data      uint8 :dynamic         :offset-assert 128)
   )

(deftype file-info (basic)
  ((file-type      basic   :offset-assert 4)
   (file-name      basic   :offset-assert 8)
   (major-version  uint32  :offset-assert 12)
   (minor-version  uint32  :offset-assert 16)
   (maya-file-name basic   :offset-assert 20)
   (tool-debug     basic   :offset-assert 24)
   (mdb-file-name  basic   :offset-assert 28)
   )

(deftype texture (basic)
  ((w            int16     :offset-assert 4)
   (h            int16     :offset-assert 6)
   (num-mips     uint8     :offset-assert 8)
   (tex1-control uint8     :offset-assert 9)
   (psm          uint8     :offset-assert 10)
   (mip-shift    uint8     :offset-assert 11)
   (clutpsm      uint16    :offset-assert 12)
   (dest         uint16 7 :offset-assert 14)
   (clutdest     uint16    :offset-assert 28)
   (width        uint8 7 :offset-assert 30)
   (name         basic     :offset-assert 40)
   (size         uint32    :offset-assert 44)
   (uv-dist      float     :offset-assert 48)
   (masks        uint32 3 :offset-assert 52)
   )
 */

// texture format names.
std::unordered_map<u8, std::string> psms = {{0x02, "PSMCT16"}, {0x13, "PSMT8"}, {0x14, "PSMT4"}};

/*!
 * GOAL texture type. Stores info about a single texture in a texture page.
 */
struct Texture {
  // there are some texture entries that are just #f. I believe these may be CLUTs.
  bool null_texture = false;
  union {
    struct {
      // 0
      s16 w;
      s16 h;

      // 1
      u8 num_mips;
      u8 tex1_control;
      u8 psm;
      u8 mip_shift;

      // 2, 3, 4, 5
      u16 clutpsm;
      u16 dest[7];

      // 6, 7, 8
      u16 clutdest;
      u8 width[8];
      u8 pad;
    };
    u32 packed_info_words[9];
  };

  DecompilerLabel name_label;
  std::string name;
  u32 size;
  float uv_dist;
  u32 masks[3];

  std::string debug_print() const {
    if (null_texture) {
      return "  NULL TEXTURE\n";
    }
    std::string x;
    x += fmt::format(
        "  w: {}, h: {}\n  mips: {}, tex1_control: {}, psm: {}, mip_shift: {}\n  clutpsm: {}\n  "
        "dest: {} {} {} {} {} {} {}\n"
        "  clutdest: {}\n  width: {} {} {} {} {} {} {} {}\n  size: {}\n  uv_dist: {}\n  masks: {} "
        "{} {}\n",
        w, h, num_mips, tex1_control, psm, mip_shift, clutpsm, dest[0], dest[1], dest[2], dest[3],
        dest[4], dest[5], dest[6], clutdest, width[0], width[1], width[2], width[3], width[4],
        width[5], width[6], width[7], size, uv_dist, masks[0], masks[1], masks[2]);
    return x;
  }
};

/*!
 * GOAL texture-page-segment.
 * Unclear what the segments really are, maybe you could split up big tpages if needed?
 */
struct TexturePageSegment {
  DecompilerLabel block_data_label;
  u32 size = 0xffffffff;
  u32 dest = 0xffffffff;
  std::string print_debug() const {
    return fmt::format("   location: {}\n   size: {}\n   dest: {}\n", block_data_label.name, size,
                       dest);
  }
};

/*!
 * GOAL file-info type.
 * This can probably be borrowed for other asset files.
 */
struct FileInfo {
  std::string file_type;
  std::string file_name;
  u32 major_version;
  u32 minor_version;
  std::string maya_file_name;
  std::string tool_debug;
  u32 mdb_file_name;
  // mdb file name
  std::string print_debug() const {
    std::string x;
    x = fmt::format(
        "  type: {}\n  name: {}\n  version: {}.{}\n  maya-name: {}\n  tool-debug: {}\n  mdb-name: "
        "{}\n",
        file_type, file_name, major_version, minor_version, maya_file_name, tool_debug,
        mdb_file_name);
    return x;
  }
};

/*!
 * GOAL texture-page type.
 */
struct TexturePage {
  DecompilerLabel info_label;
  FileInfo info;

  DecompilerLabel name_label;
  std::string name;

  u32 id = 0xffffffff;
  s32 length = -1;
  u32 mip0_size = 0xffffffff;
  u32 size = 0xffffffff;
  TexturePageSegment segments[3];
  u32 pad[16] = {};
  // data...
  std::vector<DecompilerLabel> data;
  std::vector<Texture> textures;

  std::string print_debug() const {
    std::string x;

    x += fmt::format("Texture Page {}\n", name);
    x += " Info:\n";
    x += info.print_debug();
    x += " Name: " + name + "\n";
    x += fmt::format(" id: {}\n length: {}\n mip0-size: {}\n size: {}\n", id, length, mip0_size,
                     size);

    x += " Segments: \n";
    for (const auto& seg : segments) {
      x += seg.print_debug();
    }

    for (const auto& tex : textures) {
      x += fmt::format(" Texture {}\n", tex.name);
      x += tex.debug_print();
    }

    return x;
  }
};

/*!
 * Convert a label to the offset (words) in the object segment.
 * If basic is set, gives you a pointer to the beginning of the memory, if the thing is a basic.
 */
int label_to_word_offset(DecompilerLabel l, bool basic) {
  assert((l.offset & 3) == 0);
  int result = l.offset / 4;
  if (basic) {
    result--;
  }
  return result;
}

std::string get_type_tag(const LinkedWord& word) {
  assert(word.kind == LinkedWord::TYPE_PTR);
  return word.symbol_name;
}

bool is_type_tag(const LinkedWord& word, const std::string& type) {
  return word.kind == LinkedWord::TYPE_PTR && word.symbol_name == type;
}

DecompilerLabel get_label(ObjectFileData& data, const LinkedWord& word) {
  assert(word.kind == LinkedWord::PTR);
  return data.linked_data.labels.at(word.label_id);
}

template <typename T>
T get_word(const LinkedWord& word) {
  T result;
  assert(word.kind == LinkedWord::PLAIN_DATA);
  static_assert(sizeof(T) == 4, "bad get_word size");
  memcpy(&result, &word.data, 4);
  return result;
}

/*!
 * Read a texture object.
 */
Texture read_texture(ObjectFileData& data, const std::vector<LinkedWord>& words, int offset) {
  Texture tex;
  if (!is_type_tag(words.at(offset), "texture")) {
    assert(false);
  }
  offset++;

  for (auto& word : tex.packed_info_words) {
    word = get_word<u32>(words.at(offset));
    offset++;
  }
  assert(tex.pad == 0);

  tex.name_label = get_label(data, words.at(offset));
  offset++;
  tex.name = data.linked_data.get_goal_string_by_label(tex.name_label);

  tex.size = get_word<u32>(words.at(offset));
  offset++;

  tex.uv_dist = get_word<float>(words.at(offset));
  for (auto& mask : tex.masks) {
    mask = get_word<u32>(words.at(offset));
    offset++;
  }

  auto kv = psms.find(tex.psm);
  if (kv == psms.end()) {
    printf("Got unsupported texture 0x%x!\n", tex.psm);
    assert(false);
  }

  return tex;
}

/*!
 * Read a file-info object.
 */
FileInfo read_file_info(ObjectFileData& data, const std::vector<LinkedWord>& words, int offset) {
  FileInfo info;
  if (!is_type_tag(words.at(offset), "file-info")) {
    assert(false);
  }
  offset++;

  info.file_type = get_type_tag(words.at(offset));
  offset++;

  info.file_name = data.linked_data.get_goal_string_by_label(get_label(data, words.at(offset)));
  offset++;

  info.major_version = get_word<u32>(words.at(offset));
  offset++;

  info.minor_version = get_word<u32>(words.at(offset));
  offset++;

  info.maya_file_name =
      data.linked_data.get_goal_string_by_label(get_label(data, words.at(offset)));
  offset++;

  info.tool_debug = data.linked_data.get_goal_string_by_label(get_label(data, words.at(offset)));
  if (!info.tool_debug.empty() && info.tool_debug.back() == '\n') {
    info.tool_debug.pop_back();
  }
  offset++;

  info.mdb_file_name = get_word<u32>(words.at(offset));
  offset++;

  return info;
}

/*!
 * Read a texture-page object.
 */
TexturePage read_texture_page(ObjectFileData& data,
                              const std::vector<LinkedWord>& words,
                              int offset,
                              int end) {
  TexturePage tpage;
  // offset 0 - 4, type tag
  if (!is_type_tag(words.at(offset), "texture-page")) {
    assert(false);
  }
  offset++;

  // offset 4 - 8, info label
  tpage.info_label = get_label(data, words.at(offset));
  tpage.info = read_file_info(data, words, label_to_word_offset(tpage.info_label, true));
  assert(tpage.info.file_type == "texture-page");
  assert(tpage.info.major_version == versions::TX_PAGE_VERSION);
  assert(tpage.info.minor_version == 0);
  assert(tpage.info.maya_file_name == "Unknown");
  assert(tpage.info.mdb_file_name == 0);
  offset++;

  // offset 8 - 12, name
  tpage.name_label = get_label(data, words.at(offset));
  tpage.name = data.linked_data.get_goal_string_by_label(tpage.name_label);
  offset++;

  // offset 12 - 16, id
  tpage.id = get_word<u32>(words.at(offset));
  offset++;

  // offset 16-20, length
  tpage.length = get_word<s32>(words.at(offset));
  offset++;

  // offset 20 - 24, mip0_size
  tpage.mip0_size = get_word<u32>(words.at(offset));
  offset++;

  // offset 24 - 28, size
  tpage.size = get_word<u32>(words.at(offset));
  offset++;

  // loop over segments
  for (auto& segment : tpage.segments) {
    segment.block_data_label = get_label(data, words.at(offset));
    offset++;

    segment.size = get_word<u32>(words.at(offset));
    offset++;

    segment.dest = get_word<u32>(words.at(offset));
    offset++;
  }

  for (unsigned int& i : tpage.pad) {
    i = get_word<u32>(words.at(offset));
    offset++;
    assert(i == 0);
  }

  for (int i = 0; i < tpage.length; i++) {
    if (words.at(offset).kind == LinkedWord::SYM_PTR) {
      if (words.at(offset).symbol_name == "#f") {
        tpage.data.emplace_back();
        Texture null_tex;
        null_tex.null_texture = true;
        tpage.textures.push_back(null_tex);
      } else {
        assert(false);
      }
    } else {
      tpage.data.push_back(get_label(data, words.at(offset)));
      tpage.textures.push_back(
          read_texture(data, words, label_to_word_offset(tpage.data.back(), true)));
    }

    offset++;
  }

  auto aligned_end = (offset + 3) & (~3);
  assert(aligned_end == end);

  return tpage;
}

// texture format enums
enum class PSM { PSMCT16 = 0x02, PSMT8 = 0x13, PSMT4 = 0x14 };
// clut format enums
enum class CPSM { PSMCT32 = 0x0, PSMCT16 = 0x02 };

}  // namespace

/*!
 * Process a texture page.
 * TODO - document
 */
TPageResultStats process_tpage(ObjectFileData& data) {
  TPageResultStats stats;
  auto& words = data.linked_data.words_by_seg.at(0);

  // at the beginning there's a texture-page object.
  // find the size first.
  int end_of_texture_page = -1;
  for (size_t i = 0; i < words.size(); i++) {
    if (is_type_tag(words.at(i), "file-info")) {
      end_of_texture_page = i;
      break;
    }
  }

  assert(end_of_texture_page != -1);
  // todo check it's not too small.

  // Read the texture_page struct
  TexturePage texture_page = read_texture_page(data, words, 0, end_of_texture_page);

  // Get raw data for textures.
  std::vector<u32> tex_data;
  auto tex_start = label_to_word_offset(texture_page.segments[0].block_data_label, false);
  auto tex_size = int(words.size()) - int(tex_start);
  assert(tex_size > 0);
  tex_data.resize(tex_size);
  for (int i = 0; i < tex_size; i++) {
    tex_data[i] = get_word<u32>(words.at(tex_start + i));
  }

  // "VRAM", will be used as temporary storage for scrambled up textures.
  std::vector<u8> vram;
  vram.resize(4 * 1024 * 1024);  // 4 MB, like PS2 VRAM

  // all textures are copied to vram 128 pixels wide, regardless of actual width
  int copy_width = 128;
  // scale the copy height to be whatever it needs to be to transfer the right amount of data.
  int copy_height = tex_size / copy_width;

  // copy texture to "VRAM" in PSMCT32 format, regardless of actual texture format.
  for (int y = 0; y < copy_height; y++) {
    for (int x = 0; x < copy_width; x++) {
      // VRAM address (bytes)
      auto addr32 = psmct32_addr(x, y, copy_width);
      *(u32*)(vram.data() + addr32) = *(u32*)(tex_data.data() + (x + y * copy_width));
    }
  }

  // get all textures in the tpage
  for (auto& tex : texture_page.textures) {
    // I think these get inserted for CLUTs, but I'm not sure.
    if (tex.null_texture) {
      continue;
    }

    stats.total_textures++;

    if (tex.psm == int(PSM::PSMT8) && tex.clutpsm == int(CPSM::PSMCT32)) {
      // this is the only supported texture format for now.

      // will store output pixels, rgba (8888)
      std::vector<u32> out;

      // width is like the TEX0 register, in 64 texel units.
      // not sure what the other widths are yet.
      int read_width = 64 * tex.width[0];

      // loop over pixels in output texture image
      for (int y = 0; y < tex.h; y++) {
        for (int x = 0; x < tex.w; x++) {
          // read as the PSMT8 type. The dest field tells us a block offset.
          auto addr8 = psmt8_addr(x, y, read_width) + tex.dest[0] * 256;
          u8 value = *(u8*)(vram.data() + addr8);

          // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
          // See GS manual 2.7.3 CLUT Storage Mode, IDTEX8 in CSM1 mode.
          u32 clut_chunk = value / 16;
          u32 off_in_chunk = value % 16;
          u8 clx = 0, cly = 0;
          if (clut_chunk & 1) {
            clx = 8;
          }
          cly = (clut_chunk >> 1) * 2;
          if (off_in_chunk >= 8) {
            off_in_chunk -= 8;
            cly++;
          }
          clx += off_in_chunk;

          // the x, y CLUT value is looked up in PSMCT32 mode
          u32 clut_addr = psmct32_addr(clx, cly, 64) + tex.clutdest * 256;
          u32 clut_value = *(u32*)(vram.data() + clut_addr);
          out.push_back(clut_value);
        }
      }

      // write texture to a PNG.
      file_util::create_dir_if_needed(
          file_util::get_file_path({"assets", "textures", texture_page.name}));
      file_util::write_rgba_png(
          fmt::format(file_util::get_file_path(
                          {"assets", "textures", texture_page.name, "{}-{}-{}-{}.png"}),
                      data.name_in_dgo, tex.name, tex.w, tex.h),
          out.data(), tex.w, tex.h);
      stats.successful_textures++;
    } else if (tex.psm == int(PSM::PSMT8) && tex.clutpsm == int(CPSM::PSMCT16)) {
      // will store output pixels, rgba (8888)
      std::vector<u32> out;

      // width is like the TEX0 register, in 64 texel units.
      // not sure what the other widths are yet.
      int read_width = 64 * tex.width[0];

      // loop over pixels in output texture image
      for (int y = 0; y < tex.h; y++) {
        for (int x = 0; x < tex.w; x++) {
          // read as the PSMT8 type. The dest field tells us a block offset.
          auto addr8 = psmt8_addr(x, y, read_width) + tex.dest[0] * 256;
          u8 value = *(u8*)(vram.data() + addr8);

          // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
          // See GS manual 2.7.3 CLUT Storage Mode, IDTEX8 in CSM1 mode.
          u32 clut_chunk = value / 16;
          u32 off_in_chunk = value % 16;
          u8 clx = 0, cly = 0;
          if (clut_chunk & 1) {
            clx = 8;
          }
          cly = (clut_chunk >> 1) * 2;
          if (off_in_chunk >= 8) {
            off_in_chunk -= 8;
            cly++;
          }
          clx += off_in_chunk;

          // the x, y CLUT value is looked up in PSMCT32 mode
          u32 clut_addr = psmct16_addr(clx, cly, 64) + tex.clutdest * 256;
          u32 clut_value = *(u16*)(vram.data() + clut_addr);
          out.push_back(rgba16_to_rgba32(clut_value));
        }
      }

      // write texture to a PNG.
      file_util::create_dir_if_needed(
          file_util::get_file_path({"assets", "textures", texture_page.name}));
      file_util::write_rgba_png(
          fmt::format(file_util::get_file_path(
                          {"assets", "textures", texture_page.name, "{}-{}-{}-{}.png"}),
                      data.name_in_dgo, tex.name, tex.w, tex.h),
          out.data(), tex.w, tex.h);
      stats.successful_textures++;
    } else if (tex.psm == int(PSM::PSMCT16) && tex.clutpsm == 0) {
      // not a clut.
      // will store output pixels, rgba (8888)
      std::vector<u32> out;

      // width is like the TEX0 register, in 64 texel units.
      // not sure what the other widths are yet.
      int read_width = 64 * tex.width[0];

      // loop over pixels in output texture image
      for (int y = 0; y < tex.h; y++) {
        for (int x = 0; x < tex.w; x++) {
          // read as the PSMT8 type. The dest field tells us a block offset.
          auto addr8 = psmct16_addr(x, y, read_width) + tex.dest[0] * 256;
          u16 value = *(u16*)(vram.data() + addr8);
          out.push_back(rgba16_to_rgba32(value));
        }
      }

      // write texture to a PNG.
      file_util::create_dir_if_needed(
          file_util::get_file_path({"assets", "textures", texture_page.name}));
      file_util::write_rgba_png(
          fmt::format(file_util::get_file_path(
                          {"assets", "textures", texture_page.name, "{}-{}-{}-{}.png"}),
                      data.name_in_dgo, tex.name, tex.w, tex.h),
          out.data(), tex.w, tex.h);
      stats.successful_textures++;
    } else if (tex.psm == int(PSM::PSMT4) && tex.clutpsm == int(CPSM::PSMCT16)) {
      // will store output pixels, rgba (8888)
      std::vector<u32> out;

      // width is like the TEX0 register, in 64 texel units.
      // not sure what the other widths are yet.
      int read_width = 64 * tex.width[0];

      // loop over pixels in output texture image
      for (int y = 0; y < tex.h; y++) {
        for (int x = 0; x < tex.w; x++) {
          // read as the PSMT4 type, use half byte addressing
          auto addr4 = psmt4_addr_half_byte(x, y, read_width) + tex.dest[0] * 512;

          // read (half bytes)
          u8 value = *(u8*)(vram.data() + addr4 / 2);
          if (addr4 & 1) {
            value >>= 4;
          } else {
            value = value & 0x0f;
          }

          // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
          // See GS manual 2.7.3 CLUT Storage Mode, IDTEX4 in CSM1 mode.

          u8 clx = value & 0x7;
          u8 cly = value >> 3;

          // the x, y CLUT value is looked up in PSMCT16 mode
          u32 clut_addr = psmct16_addr(clx, cly, 64) + tex.clutdest * 256;
          u32 clut_value = *(u16*)(vram.data() + clut_addr);
          out.push_back(rgba16_to_rgba32(clut_value));
        }
      }

      // write texture to a PNG.
      file_util::create_dir_if_needed(
          file_util::get_file_path({"assets", "textures", texture_page.name}));
      file_util::write_rgba_png(
          fmt::format(file_util::get_file_path(
                          {"assets", "textures", texture_page.name, "{}-{}-{}-{}.png"}),
                      data.name_in_dgo, tex.name, tex.w, tex.h),
          out.data(), tex.w, tex.h);
      stats.successful_textures++;
    } else if (tex.psm == int(PSM::PSMT4) && tex.clutpsm == int(CPSM::PSMCT32)) {
      // will store output pixels, rgba (8888)
      std::vector<u32> out;

      // width is like the TEX0 register, in 64 texel units.
      // not sure what the other widths are yet.
      int read_width = 64 * tex.width[0];

      // loop over pixels in output texture image
      for (int y = 0; y < tex.h; y++) {
        for (int x = 0; x < tex.w; x++) {
          // read as the PSMT4 type, use half byte addressing
          auto addr4 = psmt4_addr_half_byte(x, y, read_width) + tex.dest[0] * 512;

          // read (half bytes)
          u8 value = *(u8*)(vram.data() + addr4 / 2);
          if (addr4 & 1) {
            value >>= 4;
          } else {
            value = value & 0x0f;
          }

          // there's yet another scramble from the CLUT. The palette index turns into an X, Y value
          // See GS manual 2.7.3 CLUT Storage Mode, IDTEX4 in CSM1 mode.

          u8 clx = value & 0x7;
          u8 cly = value >> 3;

          // the x, y CLUT value is looked up in PSMCT16 mode
          u32 clut_addr = psmct32_addr(clx, cly, 64) + tex.clutdest * 256;
          u32 clut_value = *(u32*)(vram.data() + clut_addr);
          out.push_back(clut_value);
        }
      }

      // write texture to a PNG.
      file_util::create_dir_if_needed(
          file_util::get_file_path({"assets", "textures", texture_page.name}));
      file_util::write_rgba_png(
          fmt::format(file_util::get_file_path(
                          {"assets", "textures", texture_page.name, "{}-{}-{}-{}.png"}),
                      data.name_in_dgo, tex.name, tex.w, tex.h),
          out.data(), tex.w, tex.h);
      stats.successful_textures++;
    }

    else {
      printf("Unsupported texture 0x%x 0x%x\n", tex.psm, tex.clutpsm);
    }
  }
  return stats;
}
}  // namespace decompiler