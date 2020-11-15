#include <common/util/FileUtil.h>
#include "tpage.h"
#include "common/versions.h"
#include "decompiler/ObjectFile/ObjectFileDB.h"
#include "third-party/spdlog/include/spdlog/spdlog.h"

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

  // next the column
  u32 col_row = block_y / 2;
  u32 col_y = block_y % 2;
  u32 col_x = block_x;

  // next the pixel
  const u32 psm32_pix_table[2][8] = {{0, 1, 4, 5, 8, 9, 12, 13}, {2, 3, 6, 7, 10, 11, 14, 15}};
  u32 pixel = psm32_pix_table[col_y][col_x];

  // now the sum
  auto result = ((page * 64 * 32) + (block * 8 * 8) + (col_row * 8 * 2) + pixel) * 4;
  if (result == 16384) {
    printf("%d %d p%d b%d c%d px%d -> %d\n", x, y, page, block, col_row, pixel, result);
  }
  return result;
}

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

// TODO - check for gaps!!!

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

std::unordered_map<u8, std::string> psms = {{0x02, "PSMCT16"}, {0x13, "PSMT8"}, {0x14, "PSMT4"}};

struct Texture {
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

  Label name_label;
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

struct TexturePageSegment {
  Label block_data_label;
  u32 size = 0xffffffff;
  u32 dest = 0xffffffff;
  std::string print_debug() const {
    return fmt::format("   location: {}\n   size: {}\n   dest: {}\n", block_data_label.name, size,
                       dest);
  }
};

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

struct TexturePage {
  Label info_label;
  FileInfo info;

  Label name_label;
  std::string name;

  u32 id = 0xffffffff;
  s32 length = -1;
  u32 mip0_size = 0xffffffff;
  u32 size = 0xffffffff;
  TexturePageSegment segments[3];
  u32 pad[16] = {};
  // data...
  std::vector<Label> data;
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
    // todo, segments
    for (const auto& tex : textures) {
      x += fmt::format(" Texture {}\n", tex.name);
      x += tex.debug_print();
    }
    // todo, textures

    return x;
  }
};

namespace {
int label_to_word_offset(Label l, bool basic) {
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

Label get_label(ObjectFileData& data, const LinkedWord& word) {
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

enum class PSM { PSMCT16 = 0x02, PSMT8 = 0x13 };
enum class CPSM { PSMCT32 = 0x0 };

u32 rgb15_to_32(u16 in) {
  u32 r = (in & 0b11111);
  u32 g = (in >> 5) & 0b11111;
  u32 b = (in >> 10) & 0b11111;
  u32 out = (r << 3) + (g << (3 + 8)) + (b << (3 + 16)) + (0xff << 24);
  return out;
}

u32 debug_u8_to_32(u8 in) {
  u32 r = in;
  u32 g = in;
  u32 b = in;
  u32 out = (r) + (g << (8)) + (b << (16)) + (0xff << 24);
  return out;
}

}  // namespace

/*
Texture com-bucket
  w: 64, h: 64
  mips: 4, tex1_control: 9, psm: 19, mip_shift: 0
  clutpsm: 0
  dest: 864 644 222 223 0 0 0
  clutdest: 283
  width: 2 2 2 2 0 0 0 0
  size: 0
  uv_dist: 4.0
  masks: 1082130432 24 1048

 */

void process_tpage(ObjectFileData& data) {
  spdlog::info("Processing tpage \"{}\" total size {} words", data.name_in_dgo,
               data.linked_data.words_by_seg.at(0).size());
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
  fmt::print("\n\n{}", texture_page.print_debug());

  std::vector<u32> tex_data;
  auto tex_start = label_to_word_offset(texture_page.segments[0].block_data_label, false);
  auto tex_size = int(words.size()) - int(tex_start);
  assert(tex_size > 0);
  tex_data.resize(tex_size);
  for (int i = 0; i < tex_size; i++) {
    tex_data[i] = get_word<u32>(words.at(tex_start + i));
  }

  for (auto& tex : texture_page.textures) {
    if (tex.null_texture) {
      continue;
    }
    std::vector<u32> out;
    std::vector<u8> vram;
    vram.resize(4 * 1024 * 1024);
    std::vector<bool> vram_write, vram_read;
    vram_write.resize(4 * 1024 * 1024, false);
    vram_read.resize(4 * 1024 * 1024, false);
    if (tex.psm == int(PSM::PSMT8) &&
        //(tex.name == "com-bucket" || tex.name == "left" || tex.name == "right")
        true
        ) {
      u8* raw_data = (u8*)(tex_data.data()) + tex.dest[0] * 256;

      u32* clut = (u32*)(tex_data.data()) + tex.clutdest * 256 / 4;
      printf("texture %s size %d %d dest %d cpsm 0x%x\n", tex.name.c_str(), tex.w, tex.h, tex.dest[0], tex.clutpsm);

      {
        int copy_width = 128;
        int copy_height = (tex.h * tex.w) / copy_width;
        printf(" copy %d %d\n", copy_width, copy_height);
      }

      int copy_width = 128; //64 * tex.width[0];
      int copy_height = (tex.w * tex.h) / copy_width;

      for (int y = 0; y < copy_height / 4; y++) {
        for (int x = 0; x < copy_width; x++) {
          auto addr32 = psmct32_addr(x, y, copy_width);
          *(u32*)(vram.data() + addr32) = *(u32*)(raw_data + 4 * (x + y * copy_width));
          if (vram_write.at(addr32)) {
            printf("WRITE %d %d -> %d\n", x, y, addr32);
          }
          vram_write.at(addr32) = true;
        }
      }

      {
        int read_width = 128;
        int read_height = tex.h;
      }

      int read_width = 64 * tex.width[0];

      int inc = 0;
      for (int y = 0; y < tex.h; y++) {
        for (int x = 0; x < tex.w; x++) {
          int xx = inc % read_width;
          int yy = inc / read_width;
          auto addr8 = psmt8_addr(xx, yy, read_width);
          u8 value = *(u8*)(vram.data() + addr8);
//          out.push_back(debug_u8_to_32(value));
//          u32 clut_chunk = value / 16;
//          u32 off_in_chunk = value % 16;
//          u8 clx = 0, cly = 0;
//          if (clut_chunk & 1) {
//            clx = 8;
//          }
//          cly = (clut_chunk >> 1) * 2;
//          if(off_in_chunk >= 8) {
//            off_in_chunk -= 8;
//            cly++;
//          }
//          clx += off_in_chunk;


          out.push_back(debug_u8_to_32(value));
          if (vram_read.at(addr8)) {
            printf("READ %d %d -> %d\n", x, y, addr8);
          }
          vram_read.at(addr8) = true;
          if (!vram_write.at(addr8)) {
//            printf("nowrite\n");
          }
          inc++;
        }
      }

      //      file_util::write_binary_file(fmt::format("{}-{}-{}.data", tex.name, tex.w, tex.h),
      //      out.data(), out.size() * 4);
      file_util::write_rgba_png(fmt::format("{}-{}-{}.png", tex.name, tex.w, tex.h), out.data(),
                                tex.w, tex.h);
    }
  }

}
