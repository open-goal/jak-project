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

#include "tpage.h"

#include "common/log/log.h"
#include "common/texture/texture_conversion.h"
#include "common/util/FileUtil.h"
#include "common/versions/versions.h"

#include "decompiler/ObjectFile/ObjectFileDB.h"

#include "third-party/fmt/core.h"

namespace decompiler {
namespace {

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
const std::unordered_map<u8, std::string> psms = {{0x00, "PSM32"},
                                                  {0x02, "PSMCT16"},
                                                  {0x13, "PSMT8"},
                                                  {0x14, "PSMT4"}};

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
  ASSERT((l.offset & 3) == 0);
  int result = l.offset / 4;
  if (basic) {
    result--;
  }
  return result;
}

std::string get_type_tag(const LinkedWord& word) {
  ASSERT(word.kind() == LinkedWord::TYPE_PTR);
  return word.symbol_name();
}

bool is_type_tag(const LinkedWord& word, const std::string& type) {
  return word.kind() == LinkedWord::TYPE_PTR && word.symbol_name() == type;
}

DecompilerLabel get_label(ObjectFileData& data, const LinkedWord& word) {
  ASSERT(word.kind() == LinkedWord::PTR);
  return data.linked_data.labels.at(word.label_id());
}

template <typename T>
T get_word(const LinkedWord& word) {
  T result;
  ASSERT(word.kind() == LinkedWord::PLAIN_DATA);
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
    ASSERT(false);
  }
  offset++;

  for (auto& word : tex.packed_info_words) {
    word = get_word<u32>(words.at(offset));
    offset++;
  }
  ASSERT(tex.pad == 0);

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
    ASSERT_MSG(false, fmt::format("Got unsupported texture 0x{:x}!", tex.psm));
  }

  return tex;
}

/*!
 * Read a file-info object.
 */
FileInfo read_file_info(ObjectFileData& data, const std::vector<LinkedWord>& words, int offset) {
  FileInfo info;
  if (!is_type_tag(words.at(offset), "file-info")) {
    ASSERT(false);
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
    ASSERT(false);
  }
  offset++;

  // offset 4 - 8, info label
  tpage.info_label = get_label(data, words.at(offset));
  tpage.info = read_file_info(data, words, label_to_word_offset(tpage.info_label, true));
  ASSERT(tpage.info.file_type == "texture-page");
  switch (data.linked_data.version) {
    case GameVersion::Jak1:
      ASSERT(tpage.info.major_version == versions::jak1::TX_PAGE_VERSION);
      break;
    case GameVersion::Jak2:
      ASSERT(tpage.info.major_version == versions::jak2::TX_PAGE_VERSION);
      break;
    case GameVersion::Jak3:
      ASSERT(tpage.info.major_version == versions::jak3::TX_PAGE_VERSION);
      break;
    default:
      ASSERT(false);
  }
  ASSERT(tpage.info.minor_version == 0);
  ASSERT(tpage.info.maya_file_name == "Unknown");
  ASSERT(tpage.info.mdb_file_name == 0);
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
    ASSERT(i == 0);
  }

  for (int i = 0; i < tpage.length; i++) {
    if (words.at(offset).kind() == LinkedWord::SYM_PTR) {
      if (words.at(offset).symbol_name() == "#f") {
        tpage.data.emplace_back();
        Texture null_tex;
        null_tex.null_texture = true;
        tpage.textures.push_back(null_tex);
      } else {
        ASSERT(false);
      }
    } else {
      tpage.data.push_back(get_label(data, words.at(offset)));
      tpage.textures.push_back(
          read_texture(data, words, label_to_word_offset(tpage.data.back(), true)));
    }

    offset++;
  }

  auto aligned_end = (offset + 3) & (~3);
  ASSERT(aligned_end == end);

  return tpage;
}

}  // namespace

/*!
 * Process a texture page.
 * TODO - document
 */
TPageResultStats process_tpage(ObjectFileData& data,
                               TextureDB& texture_db,
                               const fs::path& output_path,
                               const std::unordered_set<std::string>& animated_textures,
                               bool save_pngs) {
  TPageResultStats stats;
  auto& words = data.linked_data.words_by_seg.at(0);
  const auto& level_names = data.dgo_names;

  // at the beginning there's a texture-page object.
  // find the size first.
  int end_of_texture_page = -1;
  for (size_t i = 0; i < words.size(); i++) {
    if (is_type_tag(words.at(i), "file-info")) {
      end_of_texture_page = i;
      break;
    }
  }

  ASSERT(end_of_texture_page != -1);
  // todo check it's not too small.

  // Read the texture_page struct
  TexturePage texture_page = read_texture_page(data, words, 0, end_of_texture_page);
  bool ignore_animated = texture_page.name == "sewesc-vis-pris";
  if (ignore_animated) {
    lg::warn(
        "Ignoring animated textures from this tpage ({}) because of weird jakbsmall-finger issue",
        texture_page.name);
  }
  auto texture_dump_dir = output_path / texture_page.name;
  file_util::create_dir_if_needed(texture_dump_dir);

  // Get raw data for textures.
  std::vector<u32> tex_data;
  auto tex_start = label_to_word_offset(texture_page.segments[0].block_data_label, false);
  auto tex_size = int(words.size()) - int(tex_start);
  ASSERT(tex_size > 0);
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
  for (u32 tex_id = 0; tex_id < texture_page.textures.size(); tex_id++) {
    auto& tex = texture_page.textures.at(tex_id);

    // I think these get inserted for CLUTs, but I'm not sure.
    if (tex.null_texture) {
      continue;
    }

    stats.total_textures++;
    stats.num_px += tex.w * tex.h;

    if (animated_textures.count(tex.name) && !ignore_animated) {
      switch (tex.psm) {
        case int(PSM::PSMCT32):
          // no need.
          break;
        case int(PSM::PSMT4):
          // currently not needed.
          break;
        case int(PSM::PSMT8):
          ASSERT(tex.clutpsm == int(CPSM::PSMCT32));
          {
            // will store output pixels, index (u8)
            std::vector<u8> index_out;

            // width is like the TEX0 register, in 64 texel units.
            // not sure what the other widths are yet.
            int read_width = 64 * tex.width[0];

            // loop over pixels in output texture image
            for (int y = 0; y < tex.h; y++) {
              for (int x = 0; x < tex.w; x++) {
                // read as the PSMT8 type. The dest field tells us a block offset.
                auto addr8 = psmt8_addr(x, y, read_width) + tex.dest[0] * 256;
                u8 value = vram[addr8];
                index_out.push_back(value);
              }
            }
            std::array<math::Vector4<u8>, 256> unscrambled_clut;
            for (int i = 0; i < 256; i++) {
              u32 clut_chunk = i / 16;
              u32 off_in_chunk = i % 16;
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
              u32 clut_addr = psmct32_addr(clx, cly, 64) + tex.clutdest * 256;
              memcpy(&unscrambled_clut[i], vram.data() + clut_addr, 4);
            }

            // lg::warn("Adding index texture {} from {}\n", texture_page.name, tex.name);
            texture_db.add_index_texture(texture_page.id, tex_id, index_out, unscrambled_clut,
                                         tex.w, tex.h, tex.name, texture_page.name, level_names);
            stats.successful_textures++;
          }
          break;
        default:
          lg::die("Animated texture {} format {}\n", tex.name, tex.psm);
      }
    }

    if (tex.psm == int(PSM::PSMT8) && tex.clutpsm == int(CPSM::PSMCT32)) {
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
      if (save_pngs) {
        file_util::write_rgba_png(texture_dump_dir / fmt::format("{}.png", tex.name), out.data(),
                                  tex.w, tex.h);
      }
      texture_db.add_texture(texture_page.id, tex_id, out, tex.w, tex.h, tex.name,
                             texture_page.name, level_names, tex.num_mips, tex.dest[0]);
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
      if (save_pngs) {
        file_util::write_rgba_png(texture_dump_dir / fmt::format("{}.png", tex.name), out.data(),
                                  tex.w, tex.h);
      }
      texture_db.add_texture(texture_page.id, tex_id, out, tex.w, tex.h, tex.name,
                             texture_page.name, level_names, tex.num_mips, tex.dest[0]);
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
      if (save_pngs) {
        file_util::write_rgba_png(texture_dump_dir / fmt::format("{}.png", tex.name), out.data(),
                                  tex.w, tex.h);
      }
      texture_db.add_texture(texture_page.id, tex_id, out, tex.w, tex.h, tex.name,
                             texture_page.name, level_names, tex.num_mips, tex.dest[0]);
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
      if (save_pngs) {
        file_util::write_rgba_png(texture_dump_dir / fmt::format("{}.png", tex.name), out.data(),
                                  tex.w, tex.h);
      }
      texture_db.add_texture(texture_page.id, tex_id, out, tex.w, tex.h, tex.name,
                             texture_page.name, level_names, tex.num_mips, tex.dest[0]);
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
      if (save_pngs) {
        file_util::write_rgba_png(texture_dump_dir / fmt::format("{}.png", tex.name), out.data(),
                                  tex.w, tex.h);
      }
      texture_db.add_texture(texture_page.id, tex_id, out, tex.w, tex.h, tex.name,
                             texture_page.name, level_names, tex.num_mips, tex.dest[0]);
      stats.successful_textures++;
    } else if (tex.psm == int(PSM::PSMCT32) && tex.clutpsm == 0) {
      // not a clut.
      // will store output pixels, rgba (8888)
      std::vector<u32> out;

      // width is like the TEX0 register, in 64 texel units.
      // not sure what the other widths are yet.
      int read_width = 64 * tex.width[0];

      // loop over pixels in output texture image
      for (int y = 0; y < tex.h; y++) {
        for (int x = 0; x < tex.w; x++) {
          // read as the PSMT32 type. The dest field tells us a block offset.
          auto addr32 = psmct32_addr(x, y, read_width) + tex.dest[0] * 256;
          u32 value = *(u32*)(vram.data() + addr32);
          out.push_back(value);
        }
      }

      // write texture to a PNG.
      if (save_pngs) {
        file_util::write_rgba_png(texture_dump_dir / fmt::format("{}.png", tex.name), out.data(),
                                  tex.w, tex.h);
      }
      texture_db.add_texture(texture_page.id, tex_id, out, tex.w, tex.h, tex.name,
                             texture_page.name, level_names, tex.num_mips, tex.dest[0]);
      stats.successful_textures++;
    }

    else {
      printf("Unsupported texture 0x%x 0x%x\n", tex.psm, tex.clutpsm);
    }
  }
  return stats;
}
}  // namespace decompiler
