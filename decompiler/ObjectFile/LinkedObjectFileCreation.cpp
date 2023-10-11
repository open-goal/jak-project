/*!
 * @file LinkedObjectFileCreation.cpp
 * Create a LinkedObjectFile from raw object file data.
 * This implements a decoder for the GOAL linking format.
 */

#include "LinkedObjectFileCreation.h"

#include <cstring>

#include "common/link_types.h"
#include "common/log/log.h"
#include "common/util/Assert.h"
#include "common/util/BitUtils.h"

#include "decompiler/config.h"
#include "decompiler/util/DecompilerTypeSystem.h"

namespace decompiler {
// There are three link versions:
// V2 - not really in use anymore, but V4 will resue logic from it (and the game didn't rename the
// functions) V3 - optimized for code and small stuff. Supports segments (main, debug, top-level) V4
// - optimized for data (never code) and big stuff, special optimization possible for large V4
// objects at the end of DGO.
//   internally V4 is really just a V2, but with the link data coming after the object data.
//   there's a V4 header at the beginning, the object data, and then a V2 header and V2 link data.

// Header for link data used for V2, V3, V4 objects.  For V3/V4, this is found at the beginning of
// the object data.
struct LinkHeaderCommon {
  uint32_t type_tag;  // for the basic offset, is 0 or -1 depending on version
  uint32_t length;    // different exact meanings, but length of the link data.
  uint16_t version;   // what version (2, 3, 4)
};

// Per-segment info for V3 and V5 link data
struct SegmentInfo {
  uint32_t relocs;  // offset of relocation table
  uint32_t data;    // offset of segment data
  uint32_t size;    // segment data size (0 if segment doesn't exist)
  uint32_t magic;   // always 0
};

struct LinkHeaderV3 {
  uint32_t type_tag;  // always 0
  uint32_t length;    // length of link data
  uint32_t version;   // always 3
  uint32_t segments;  // always 3
  char name[64];      // name of object file
  SegmentInfo segment_info[3];
};

struct LinkHeaderV5 {
  uint32_t type_tag;               // 0 always 0?
  uint32_t length_to_get_to_code;  // 4 length.. of link data?
  uint16_t version;                // 8
  uint16_t unknown;                // 10
  uint32_t length_to_get_to_link;  // 12
  uint32_t link_length;            // 16
  uint8_t n_segments;              // 20
  char name[59];                   // 21 (really??)
};

// The types of symbol links
enum class SymbolLinkKind {
  EMPTY_LIST,  // link to the empty list
  TYPE,        // link to a type
  SYMBOL       // link to a symbol
};

/*!
 * Handle symbol links for a single symbol in a V2/V4 object file.
 */
static uint32_t c_symlink2(LinkedObjectFile& f,
                           const std::vector<uint8_t>& data,
                           uint32_t code_ptr_offset,
                           uint32_t link_ptr_offset,
                           SymbolLinkKind kind,
                           const char* name,
                           int seg_id,
                           DecompilerTypeSystem& dts) {
  dts.add_symbol(name);
  auto initial_offset = code_ptr_offset;
  do {
    auto table_value = data.at(link_ptr_offset);
    const uint8_t* relocPtr = &data.at(link_ptr_offset);

    // link table has a series of variable-length-encoded integers indicating the seek amount to hit
    // each reference to the symbol.  It ends when the seek is 0, and all references to this symbol
    // have been patched.
    uint32_t seek = table_value;
    uint32_t next_reloc = link_ptr_offset + 1;

    if (seek & 3) {
      // 0b01, 0b10
      seek = (relocPtr[1] << 8) | table_value;
      next_reloc = link_ptr_offset + 2;
      if (seek & 2) {
        // 0b10
        seek = (relocPtr[2] << 16) | seek;
        next_reloc = link_ptr_offset + 3;
        if (seek & 1) {
          // 0b11
          seek = (relocPtr[3] << 24) | seek;
          next_reloc = link_ptr_offset + 4;
        }
      }
    }

    f.stats.total_v2_symbol_links++;
    link_ptr_offset = next_reloc;

    code_ptr_offset += (seek & 0xfffffffc);

    // the value of the code gives us more information
    uint32_t code_value = *(const uint32_t*)(&data.at(code_ptr_offset));
    if (code_value == 0xffffffff) {
      // absolute link - replace entire word with a pointer.
      LinkedWord::Kind word_kind;
      switch (kind) {
        case SymbolLinkKind::SYMBOL:
          word_kind = LinkedWord::SYM_PTR;
          break;
        case SymbolLinkKind::EMPTY_LIST:
          word_kind = LinkedWord::EMPTY_PTR;
          break;
        case SymbolLinkKind::TYPE:
          // hack for jak 2: this symbol is used as a type in village 1 and also the oracle level
          // level info. We'll just leave it out, as we don't really need these definitions.
          if (std::string(name) != "oracle") {
            dts.add_symbol(name, "type", {});
          }
          word_kind = LinkedWord::TYPE_PTR;
          break;
        default:
          throw std::runtime_error("unhandled SymbolLinkKind");
      }

      f.symbol_link_word(seg_id, code_ptr_offset - initial_offset, name, word_kind);
    } else {
      // offset link - replace lower 16 bits with symbol table offset.

      ASSERT((code_value & 0xffff) == 0 || (code_value & 0xffff) == 0xffff);
      ASSERT(kind == SymbolLinkKind::SYMBOL);
      f.symbol_link_offset(seg_id, code_ptr_offset - initial_offset, name,
                           (code_value & 0xffff) == 0xffff);
    }

  } while (data.at(link_ptr_offset));

  // seek past terminating 0.
  return link_ptr_offset + 1;
}

/*!
 * Handle symbol links for a single symbol in a V3 object file.
 */
static uint32_t c_symlink3(LinkedObjectFile& f,
                           const std::vector<uint8_t>& data,
                           uint32_t code_ptr,
                           uint32_t link_ptr,
                           SymbolLinkKind kind,
                           const char* name,
                           int seg,
                           DecompilerTypeSystem& dts) {
  dts.add_symbol(name);
  auto initial_offset = code_ptr;
  do {
    // seek, with a variable length encoding that sucks.
    uint8_t c;
    do {
      c = data.at(link_ptr);
      link_ptr++;
      code_ptr += c * 4;
    } while (c == 0xff);

    // identical logic to symlink 2
    uint32_t code_value = *(const uint32_t*)(&data.at(code_ptr));
    if (code_value == 0xffffffff) {
      f.stats.v3_symbol_link_word++;
      LinkedWord::Kind word_kind;
      switch (kind) {
        case SymbolLinkKind::SYMBOL:
          word_kind = LinkedWord::SYM_PTR;
          break;
        case SymbolLinkKind::EMPTY_LIST:
          word_kind = LinkedWord::EMPTY_PTR;
          break;
        case SymbolLinkKind::TYPE:
          dts.add_symbol(name, "type", {});
          word_kind = LinkedWord::TYPE_PTR;
          break;
        default:
          throw std::runtime_error("unhandled SymbolLinkKind");
      }

      f.symbol_link_word(seg, code_ptr - initial_offset, name, word_kind);
    } else {
      u16 lower = code_value & 0xffff;
      ASSERT(lower == 0 || lower == 0xffff);
      f.stats.v3_symbol_link_offset++;
      ASSERT(kind == SymbolLinkKind::SYMBOL);
      f.symbol_link_offset(seg, code_ptr - initial_offset, name, lower == 0xffff);
    }

  } while (data.at(link_ptr));
  return link_ptr + 1;
}

/*!
 * Process link data for a "V4" or "V2" object file.
 * In reality a V4 seems to be just a V2 object, but with the link data after the real data.
 * There's a V4 header at the very beginning, but another V2 header/link data at the end
 * -----------------------------------------------
 * | V4 header | data | V2 header | V2 link data |
 * -----------------------------------------------
 *
 * V2
 * -----------------------------------
 * | V2 header | V2 link data | data |
 * -----------------------------------
 * The V4 format avoids having to copy the data to the left once the V2 link data is discarded.
 * Presumably once they decided that data could never be relocated after being loaded in,
 * it became worth it to throw away the link data, and avoid the memcpy of the data.
 * The memcpy is surprisingly expensive, when you consider the linker ran for ~3% of a frame each
 * frame and level data is ~10 MB.
 */
static void link_v2_or_v4(LinkedObjectFile& f,
                          const std::vector<uint8_t>& data,
                          const std::string& name,
                          DecompilerTypeSystem& dts,
                          GameVersion version) {
  (void)name;
  const auto* header = (const LinkHeaderV4*)&data.at(0);
  ASSERT(header->version == 4 || header->version == 2);

  // these are different depending on the version.
  uint32_t code_offset, link_data_offset, code_size;

  if (header->version == 4) {
    // code starts immediately after the V4 header
    code_offset = sizeof(LinkHeaderV4);
    // link_data_offset points to a V2 header
    link_data_offset = header->code_size + sizeof(LinkHeaderV4);
    // code size is specified!
    code_size = header->code_size;
  } else {
    // link data starts immediately
    link_data_offset = 0;

    // code is after all the link data
    code_offset = header->length;
    // we have to compute the code size ourself
    code_size = data.size() - code_offset;
    ASSERT(header->type_tag == 0xffffffff);
  }

  f.stats.total_code_bytes += code_size;
  f.stats.total_v2_code_bytes += code_size;

  // add all code
  const uint8_t* code_start = &data.at(code_offset);
  const uint8_t* code_end =
      &data.at(code_offset + code_size - 1) + 1;  // get the pointer to one past the end.

  if (version == GameVersion::Jak2) {
    while (((code_end - code_start) % 4)) {
      code_end++;
    }
  }

  ASSERT(((code_end - code_start) % 4) == 0);
  f.set_segment_count(1);
  for (auto x = code_start; x < code_end; x += 4) {
    f.push_back_word_to_segment(*((const uint32_t*)x), 0);
  }

  // read v2 header after the code
  const uint8_t* link_data = &data.at(link_data_offset);
  uint32_t link_ptr_offset = link_data_offset;
  link_ptr_offset += sizeof(LinkHeaderV2);
  auto* link_header_v2 = (const LinkHeaderV2*)(link_data);
  ASSERT(link_header_v2->type_tag == 0xffffffff);
  ASSERT(link_header_v2->version == 2);
  ASSERT(link_header_v2->length == header->length);
  f.stats.total_v2_link_bytes += link_header_v2->length;

  // first "section" of link data is a list of where all the pointer are.
  if (data.at(link_ptr_offset) == 0) {
    // there are no pointers.
    link_ptr_offset++;
  } else {
    // there are pointers.
    // there are a series of variable-length coded integers, indicating where the pointers are, in
    // the form: seek_amount, number_of_consecutive_pointers, seek_amount,
    // number_of_consecutive_pointers, ... , 0

    uint32_t code_ptr_offset = code_offset;
    bool fixing = false;  // either seeking or fixing

    while (true) {    // loop over entire table
      while (true) {  // loop over current mode (fixing/seeking)
        // get count from table
        auto count = data.at(link_ptr_offset);
        link_ptr_offset++;

        if (!fixing) {
          // then we are seeking
          code_ptr_offset += 4 * count;
          f.stats.total_v2_pointer_seeks++;
        } else {
          // then we are fixing consecutive pointers
          for (uint8_t i = 0; i < count; i++) {
            if (!f.pointer_link_word(0, code_ptr_offset - code_offset, 0,
                                     *((const uint32_t*)(&data.at(code_ptr_offset))))) {
              // was this just a bug in the linker??
              // lg::error("Skipping link in {} because it is out of range!", name.c_str());
            }
            f.stats.total_v2_pointers++;
            code_ptr_offset += 4;
          }
        }

        // check if we are done with the current integer
        if (count != 0xff)
          break;

        // when we "end" an encoded integer on an 0xff, we need an explicit zero byte to change
        // modes. this handles this special case.
        if (data.at(link_ptr_offset) == 0) {
          link_ptr_offset++;
          fixing = !fixing;
        }
      }

      // mode ended, switch
      fixing = !fixing;

      // we got a zero, that means we're done with pointer fixing.
      if (data.at(link_ptr_offset) == 0)
        break;
    }
    link_ptr_offset++;
  }

  // second "section" of link data is a list of symbols to fix up.
  if (data.at(link_ptr_offset) == 0) {
    // no symbols
  } else {
    while (true) {
      uint32_t reloc = data.at(link_ptr_offset);
      link_ptr_offset++;

      const char* s_name;
      SymbolLinkKind kind;

      if ((reloc & 0x80) == 0) {
        // it's a symbol
        if (reloc > 9) {
          // always happens.
          link_ptr_offset--;
        } else {
          ASSERT(false);
        }

        s_name = (const char*)(&data.at(link_ptr_offset));
        kind = SymbolLinkKind::SYMBOL;

      } else {
        // it's a type
        kind = SymbolLinkKind::TYPE;
        uint8_t method_count = reloc & 0x7f;
        s_name = (const char*)(&data.at(link_ptr_offset));
        if (method_count == 0) {
          method_count = 1;
          // hack which will add 44 methods to _newly created_ types
          // I assume the thing generating V2 objects didn't know about method counts.
          // so this was a "safe" backup - if linking a V2 object requires allocating a type.
          // just be on the safe side.
          // (see the !symbolValue case in intern_type_from_c)
        } else {
          ASSERT(false);
        }
      }

      if (std::string("_empty_") == s_name) {
        ASSERT(kind == SymbolLinkKind::SYMBOL);
        kind = SymbolLinkKind::EMPTY_LIST;
      }

      link_ptr_offset += strlen(s_name) + 1;
      f.stats.total_v2_symbol_count++;
      link_ptr_offset = c_symlink2(f, data, code_offset, link_ptr_offset, kind, s_name, 0, dts);
      if (data.at(link_ptr_offset) == 0)
        break;
    }
  }

  // check length
  ASSERT(link_header_v2->length == align64(link_ptr_offset - link_data_offset + 1));
  size_t expected_end = header->version == 4 ? data.size() : link_header_v2->length;
  while (link_ptr_offset < expected_end) {
    ASSERT(data.at(link_ptr_offset) == 0);
    link_ptr_offset++;
  }
}

static void assert_string_empty_after(const char* str, int size) {
  auto ptr = str;
  while (*ptr)
    ptr++;
  while (ptr - str < size) {
    ASSERT(!*ptr);
    ptr++;
  }
}

static void link_v5(LinkedObjectFile& f,
                    const std::vector<uint8_t>& data,
                    const std::string& name,
                    DecompilerTypeSystem& dts) {
  auto header = (const LinkHeaderV5*)(&data.at(0));

  // for jak 3, both code and data use a "v5" format for linking.
  // code has 3 segments (top-level, main, debug), and data has just 1.
  // they appear to be generated by different programs, so there's some hard-coded checks for
  // each.

  // the "v5" format allows for multiple segments (like v3), "split-pointer" linking to support
  // splitting a pointer link between a lui/ori instruction (needed for code), but uses "v2"
  // symbol linking. For a reason that I don't understand, "v3" symlinks uses a less-space efficient
  // encoding of large integers.

  static_assert(0x50 == sizeof(LinkHeaderV5));

  if (header->n_segments == 3) {
    ASSERT(header->type_tag == 0);
    ASSERT(name == header->name);
    // the linker for code placed the link data at the beginning.
    // but we expect the link data to start just after the object file header
    ASSERT(header->length_to_get_to_link == sizeof(LinkHeaderV5));
    // and then the code sould come after that
    ASSERT(header->length_to_get_to_code == sizeof(LinkHeaderV5) + header->link_length);
  } else if (header->n_segments == 1) {
    ASSERT(header->type_tag == UINT32_MAX);
    // name is inconsistent, so don't check is
    // data files have the data first, which is good, as the last object in a DGO gets loaded
    // directly to the heap, and putting the data first means that we can "free" the link data just
    // by bumping the heap pointer back, rather than memcpy the code back to cover the hole if link
    // data came first.
    // the offset is always 0x80, which is bigger than the header, but is needed to make data
    // aligned with the PS2's cache line size (64 bytes), which makes sense.
    ASSERT(header->length_to_get_to_code == 0x80);
  } else {
    lg::die("bad segment count {}", header->n_segments);
  }
  f.set_segment_count(header->n_segments);

  // todo - check this against the code size we actually got.
  //  size_t expected_code_size = data.size() - (header->link_length + 0x50);

  const int n_segs = header->n_segments;

  // the first think in the link data is the segment info array, which we need to find stuff.
  const SegmentInfo* seg_info_array =
      (const SegmentInfo*)(data.data() + header->length_to_get_to_link);

  // for convenience, we'll find the data/link offsets for each segment.
  uint32_t segment_data_offsets[3];
  uint32_t segment_link_offsets[3];
  uint32_t segment_link_ends[3];  // set in linking, once we get to the end.
  for (int i = 0; i < n_segs; i++) {
    segment_data_offsets[i] = header->length_to_get_to_code + seg_info_array[i].data;
    segment_link_offsets[i] = header->length_to_get_to_link + seg_info_array[i].relocs;
    ASSERT(seg_info_array[i].magic == 1);
  }

  // check that the data region is filled
  for (int i = 0; i < n_segs - 1; i++) {
    ASSERT(align16(segment_data_offsets[i] + seg_info_array[i].size) ==
           segment_data_offsets[i + 1]);
  }
  if (n_segs == 3) {
    ASSERT(align16(segment_data_offsets[2] + seg_info_array[2].size) == data.size());
  }

  // loop over segments
  for (int seg_id = n_segs; seg_id-- > 0;) {
    int segment_size = seg_info_array[seg_id].size;
    if (segment_size == 0) {
      continue;
    }

    // the decompiler uses 4-byte words, so pad to 4-bytes.
    while (segment_size % 4) {
      segment_size++;
    }

    // set up pointers for linker.
    auto base_ptr = segment_data_offsets[seg_id];
    auto data_ptr = base_ptr - 4;
    auto link_ptr = segment_link_offsets[seg_id];

    ASSERT((data_ptr % 4) == 0);
    ASSERT((segment_size % 4) == 0);

    // add data to the decompiler.
    auto code_start = (const uint32_t*)(&data.at(data_ptr + 4));
    auto code_end = ((const uint32_t*)(&data.at(data_ptr + segment_size))) + 1;
    for (auto x = code_start; x < code_end; x++) {
      f.push_back_word_to_segment(*((const uint32_t*)x), seg_id);
    }

    // pointer linking.
    bool fixing = false;
    if (data.at(link_ptr)) {
      // we have pointers
      while (true) {
        while (true) {
          if (!fixing) {
            // seeking
            data_ptr += 4 * data.at(link_ptr);
            f.stats.v3_pointer_seeks++;
          } else {
            // fixing.
            for (uint32_t i = 0; i < data.at(link_ptr); i++) {
              f.stats.v3_pointers++;
              uint32_t old_code = *(const uint32_t*)(&data.at(data_ptr));
              if ((old_code >> 24) == 0) {
                f.stats.v3_word_pointers++;
                if (!f.pointer_link_word(seg_id, data_ptr - base_ptr, seg_id, old_code)) {
                  // the art groups just have bogus links. we ignored them in jak 2, so do the same
                  // here. The joint-anim-compressed-control's have a few bogus frames at the end.
                }
              } else {
                f.stats.v3_split_pointers++;
                auto dest_seg = (old_code >> 8) & 0xf;
                auto lo_hi_offset = (old_code >> 12) & 0xf;
                ASSERT(lo_hi_offset);
                ASSERT(dest_seg < 3);
                auto offset_upper = old_code & 0xff;
                uint32_t low_code = *(const uint32_t*)(&data.at(data_ptr + 4 * lo_hi_offset));
                uint32_t offset = low_code & 0xffff;
                if (offset_upper) {
                  offset += (offset_upper << 16);
                }
                f.pointer_link_split_word(seg_id, data_ptr - base_ptr,
                                          data_ptr + 4 * lo_hi_offset - base_ptr, dest_seg, offset);
              }
              data_ptr += 4;
            }
          }

          if (data.at(link_ptr) != 0xff)
            break;
          link_ptr++;
          if (data.at(link_ptr) == 0) {
            link_ptr++;
            fixing = !fixing;
          }
        }

        link_ptr++;
        fixing = !fixing;
        if (data.at(link_ptr) == 0)
          break;
      }
    }
    link_ptr++;

    // symbol linking.
    if (data.at(link_ptr)) {
      auto sub_link_ptr = link_ptr;

      while (true) {
        auto reloc = data.at(sub_link_ptr);
        auto next_link_ptr = sub_link_ptr + 1;
        link_ptr = next_link_ptr;

        if ((reloc & 0x80) == 0) {
          link_ptr = sub_link_ptr + 3;  //
          const char* sname = (const char*)(&data.at(link_ptr));
          link_ptr += strlen(sname) + 1;
          // todo segment data offsets...

          if (std::string("_empty_") == sname) {
            link_ptr = c_symlink2(f, data, segment_data_offsets[seg_id], link_ptr,
                                  SymbolLinkKind::EMPTY_LIST, sname, seg_id, dts);
          } else {
            link_ptr = c_symlink2(f, data, segment_data_offsets[seg_id], link_ptr,
                                  SymbolLinkKind::SYMBOL, sname, seg_id, dts);
          }
        } else if ((reloc & 0x3f) == 0x3f) {
          ASSERT(false);  // todo, does this ever get hit?
        } else {
          /*
          int n_methods_base = reloc & 0x3f;
          int n_methods = n_methods_base * 4;
          if (n_methods_base) {
            n_methods += 3;
          }
          */
          link_ptr += 2;  // ghidra misses some aliasing here and would have you think this is +1!
          const char* sname = (const char*)(&data.at(link_ptr));
          link_ptr += strlen(sname) + 1;
          link_ptr = c_symlink2(f, data, segment_data_offsets[seg_id], link_ptr,
                                SymbolLinkKind::TYPE, sname, seg_id, dts);
        }

        sub_link_ptr = link_ptr;
        if (!data.at(sub_link_ptr))
          break;
      }
    }
    segment_link_ends[seg_id] = link_ptr;
  }

  if (n_segs == 3) {
    ASSERT(segment_link_offsets[0] == 128);

    if (seg_info_array[0].size) {
      ASSERT(segment_link_ends[0] + 1 == segment_link_offsets[1]);
    } else {
      ASSERT(segment_link_offsets[0] + 2 == segment_link_offsets[1]);
    }

    if (seg_info_array[1].size) {
      ASSERT(segment_link_ends[1] + 1 == segment_link_offsets[2]);
    } else {
      ASSERT(segment_link_offsets[1] + 2 == segment_link_offsets[2]);
    }

    ASSERT(align16(segment_link_ends[2] + 2) == segment_data_offsets[0]);
  }
}

static void link_v3(LinkedObjectFile& f,
                    const std::vector<uint8_t>& data,
                    const std::string& name,
                    DecompilerTypeSystem& dts,
                    GameVersion game_version) {
  auto header = (const LinkHeaderV3*)(&data.at(0));
  ASSERT(name == header->name);
  ASSERT(header->segments == 3);

  f.set_segment_count(3);
  assert_string_empty_after(header->name, 64);

  for (int i = 0; i < 3; i++) {
    ASSERT(header->segment_info[i].magic == 0);
    //    printf(" [%d] %d %d %d %d\n", i, header->segment_info[i].size,
    //    header->segment_info[i].data, header->segment_info[i].magic,
    //    header->segment_info[i].relocs);
  }

  f.stats.v3_link_bytes += header->length;
  uint32_t data_ptr_offset = header->length;

  uint32_t segment_data_offsets[3];
  uint32_t segment_link_offsets[3];
  uint32_t segment_link_ends[3];
  for (int i = 0; i < 3; i++) {
    segment_data_offsets[i] = data_ptr_offset + header->segment_info[i].data;
    segment_link_offsets[i] = header->segment_info[i].relocs;
  }

  // check that the data region is filled
  for (int i = 0; i < 2; i++) {
    ASSERT(align16(segment_data_offsets[i] + header->segment_info[i].size) ==
           segment_data_offsets[i + 1]);
  }
  ASSERT(align16(segment_data_offsets[2] + header->segment_info[2].size) == data.size());

  // todo - check link region is filled.

  // loop over segments (reverse order for now)
  for (int seg_id = 3; seg_id-- > 0;) {
    // ?? is this right?
    if (header->segment_info[seg_id].size == 0)
      continue;

    auto segment_size = header->segment_info[seg_id].size;
    f.stats.v3_code_bytes += segment_size;

    // HACK!
    // why is this a thing?
    // HACK!
    if (game_version == GameVersion::Jak1 && name == "level-h" && seg_id == 0) {
      segment_size++;
    }

    if (game_version == GameVersion::Jak2) {
      while (segment_size % 4) {
        segment_size++;
      }
    }

    auto base_ptr = segment_data_offsets[seg_id];
    auto data_ptr = base_ptr - 4;
    auto link_ptr = segment_link_offsets[seg_id];

    ASSERT((data_ptr % 4) == 0);
    ASSERT((segment_size % 4) == 0);

    auto code_start = (const uint32_t*)(&data.at(data_ptr + 4));
    auto code_end = ((const uint32_t*)(&data.at(data_ptr + segment_size))) + 1;
    for (auto x = code_start; x < code_end; x++) {
      f.push_back_word_to_segment(*((const uint32_t*)x), seg_id);
    }
    bool fixing = false;

    if (data.at(link_ptr)) {
      // we have pointers
      while (true) {
        while (true) {
          if (!fixing) {
            // seeking
            data_ptr += 4 * data.at(link_ptr);
            f.stats.v3_pointer_seeks++;
          } else {
            // fixing.
            for (uint32_t i = 0; i < data.at(link_ptr); i++) {
              f.stats.v3_pointers++;
              uint32_t old_code = *(const uint32_t*)(&data.at(data_ptr));
              if ((old_code >> 24) == 0) {
                f.stats.v3_word_pointers++;
                if (!f.pointer_link_word(seg_id, data_ptr - base_ptr, seg_id, old_code)) {
                  printf("WARNING bad pointer_link_word (2) in %s\n", name.c_str());
                }
              } else {
                f.stats.v3_split_pointers++;
                auto dest_seg = (old_code >> 8) & 0xf;
                auto lo_hi_offset = (old_code >> 12) & 0xf;
                ASSERT(lo_hi_offset);
                ASSERT(dest_seg < 3);
                auto offset_upper = old_code & 0xff;
                //                ASSERT(offset_upper == 0);
                uint32_t low_code = *(const uint32_t*)(&data.at(data_ptr + 4 * lo_hi_offset));
                uint32_t offset = low_code & 0xffff;
                if (offset_upper) {
                  // seems to work fine, no need to warn.
                  //                  printf("WARNING - offset upper is set in %s\n", name.c_str());
                  offset += (offset_upper << 16);
                }
                f.pointer_link_split_word(seg_id, data_ptr - base_ptr,
                                          data_ptr + 4 * lo_hi_offset - base_ptr, dest_seg, offset);
              }
              data_ptr += 4;
            }
          }

          if (data.at(link_ptr) != 0xff)
            break;
          link_ptr++;
          if (data.at(link_ptr) == 0) {
            link_ptr++;
            fixing = !fixing;
          }
        }

        link_ptr++;
        fixing = !fixing;
        if (data.at(link_ptr) == 0)
          break;
      }
    }

    link_ptr++;

    while (data.at(link_ptr)) {
      auto reloc = data.at(link_ptr);
      SymbolLinkKind kind;
      link_ptr++;

      const char* s_name = nullptr;
      if ((reloc & 0x80) == 0) {
        // it's a symbol
        kind = SymbolLinkKind::SYMBOL;
        link_ptr--;
        s_name = (const char*)(&data.at(link_ptr));
      } else {
        s_name = (const char*)(&data.at(link_ptr));
        switch (game_version) {
          case GameVersion::Jak1:
            dts.ts.forward_declare_type_method_count(s_name, (reloc & 0x7f));
            break;
          case GameVersion::Jak2:
            dts.ts.forward_declare_type_method_count_multiple_of_4(s_name, (reloc & 0x7f) * 4 + 3);
            break;
          default:
            ASSERT(false);
        }
        kind = SymbolLinkKind::TYPE;
      }

      if (std::string("_empty_") == s_name) {
        ASSERT(kind == SymbolLinkKind::SYMBOL);
        kind = SymbolLinkKind::EMPTY_LIST;
      }

      link_ptr += strlen(s_name) + 1;
      f.stats.v3_symbol_count++;
      link_ptr = c_symlink3(f, data, base_ptr, link_ptr, kind, s_name, seg_id, dts);
    }
    segment_link_ends[seg_id] = link_ptr;
  }

  ASSERT(segment_link_offsets[0] == 128);

  if (header->segment_info[0].size) {
    ASSERT(segment_link_ends[0] + 1 == segment_link_offsets[1]);
  } else {
    ASSERT(segment_link_offsets[0] + 2 == segment_link_offsets[1]);
  }

  if (header->segment_info[1].size) {
    ASSERT(segment_link_ends[1] + 1 == segment_link_offsets[2]);
  } else {
    ASSERT(segment_link_offsets[1] + 2 == segment_link_offsets[2]);
  }

  ASSERT(align16(segment_link_ends[2] + 2) == segment_data_offsets[0]);
}

/*!
 * Main function to generate LinkedObjectFiles from raw object data.
 */
LinkedObjectFile to_linked_object_file(const std::vector<uint8_t>& data,
                                       const std::string& name,
                                       DecompilerTypeSystem& dts,
                                       GameVersion game_version) {
  LinkedObjectFile result(game_version);
  const auto* header = (const LinkHeaderCommon*)&data.at(0);

  // use appropriate linker
  if (header->version == 3) {
    ASSERT(header->type_tag == 0);
    link_v3(result, data, name, dts, game_version);
  } else if (header->version == 4 || header->version == 2) {
    ASSERT(header->type_tag == 0xffffffff);
    link_v2_or_v4(result, data, name, dts, game_version);
  } else if (header->version == 5) {
    link_v5(result, data, name, dts);
  } else {
    ASSERT_MSG(false, fmt::format("Unsupported version {}", header->version));
  }

  return result;
}
}  // namespace decompiler
