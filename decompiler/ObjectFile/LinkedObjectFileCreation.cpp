/*!
 * @file LinkedObjectFileCreation.cpp
 * Create a LinkedObjectFile from raw object file data.
 * This implements a decoder for the GOAL linking format.
 */

#include <cassert>
#include <cstring>
#include "LinkedObjectFileCreation.h"
#include "decompiler/config.h"
#include "decompiler/TypeSystem/TypeInfo.h"

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

// Header for link data used for V2 linking data
struct LinkHeaderV2 {
  uint32_t type_tag;  // always -1
  uint32_t length;    // length of link data
  uint32_t version;   // always 2
};

// Header for link data used for V4
struct LinkHeaderV4 {
  uint32_t type_tag;   // always -1
  uint32_t length;     // length of V2 link data found after object.
  uint32_t version;    // always 4
  uint32_t code_size;  // length of object data before link data starts
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
  uint32_t pad;                    // 12
  uint32_t link_length;            // 16
  uint8_t n_segments;              // 20
  char name[59];                   // 21 (really??)
  SegmentInfo segment_info[3];
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
                           int seg_id) {
  get_type_info().inform_symbol_with_no_type_info(name);
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
      seek = (relocPtr[1] << 8) | table_value;
      next_reloc = link_ptr_offset + 2;
      if (seek & 2) {
        seek = (relocPtr[2] << 16) | seek;
        next_reloc = link_ptr_offset + 3;
        if (seek & 1) {
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
          get_type_info().inform_type(name);
          word_kind = LinkedWord::TYPE_PTR;
          break;
        default:
          throw std::runtime_error("unhandled SymbolLinkKind");
      }

      f.symbol_link_word(seg_id, code_ptr_offset - initial_offset, name, word_kind);
    } else {
      // offset link - replace lower 16 bits with symbol table offset.

      assert((code_value & 0xffff) == 0 || (code_value & 0xffff) == 0xffff);
      assert(kind == SymbolLinkKind::SYMBOL);
      //      assert(false); // this case does not occur in V2/V4.  It does in V3.
      f.symbol_link_offset(seg_id, code_ptr_offset - initial_offset, name);
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
                           int seg) {
  get_type_info().inform_symbol_with_no_type_info(name);
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
          get_type_info().inform_type(name);
          word_kind = LinkedWord::TYPE_PTR;
          break;
        default:
          throw std::runtime_error("unhandled SymbolLinkKind");
      }

      f.symbol_link_word(seg, code_ptr - initial_offset, name, word_kind);
    } else {
      f.stats.v3_symbol_link_offset++;
      assert(kind == SymbolLinkKind::SYMBOL);
      f.symbol_link_offset(seg, code_ptr - initial_offset, name);
    }

  } while (data.at(link_ptr));
  return link_ptr + 1;
}

static uint32_t align64(uint32_t in) {
  return (in + 63) & (~63);
}

static uint32_t align16(uint32_t in) {
  return (in + 15) & (~15);
}


/*!
 * Process link data for a "V4" object file.
 * In reality a V4 seems to be just a V2 object, but with the link data after the real data.
 * There's a V4 header at the very beginning, but another V2 header/link data at the end
 * -----------------------------------------------
 * | V4 header | data | V2 header | V2 link data |
 * -----------------------------------------------
 */
static void link_v4(LinkedObjectFile& f,
                    const std::vector<uint8_t>& data,
                    const std::string& name) {
  // read the V4 header to find where the link data really is
  const auto* header = (const LinkHeaderV4*)&data.at(0);
  uint32_t link_data_offset = header->code_size + sizeof(LinkHeaderV4);  // no basic offset

  // code starts immediately after the header
  uint32_t code_offset = sizeof(LinkHeaderV4);
  uint32_t code_size = header->code_size;

  f.stats.total_code_bytes += code_size;
  f.stats.total_v2_code_bytes += code_size;

  // add all code
  const uint8_t* code_start = &data.at(code_offset);
  const uint8_t* code_end =
      &data.at(code_offset + code_size);  // safe because link data is after code.
  assert(((code_end - code_start) % 4) == 0);
  f.set_segment_count(1);
  for (auto x = code_start; x < code_end; x += 4) {
    f.push_back_word_to_segment(*((const uint32_t*)x), 0);
  }

  // read v2 header after the code
  const uint8_t* link_data = &data.at(link_data_offset);
  const auto* link_header_v2 = (const LinkHeaderV2*)(link_data);  // subtract off type tag
  assert(link_header_v2->type_tag == 0xffffffff);
  assert(link_header_v2->version == 2);
  assert(link_header_v2->length == header->length);
  f.stats.total_v2_link_bytes += link_header_v2->length;
  uint32_t link_ptr_offset = link_data_offset + sizeof(LinkHeaderV2);

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
              printf("WARNING bad link in %s\n", name.c_str());
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
          assert(false);
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
          assert(false);
        }
      }

      if (std::string("_empty_") == s_name) {
        assert(kind == SymbolLinkKind::SYMBOL);
        kind = SymbolLinkKind::EMPTY_LIST;
      }

      link_ptr_offset += strlen(s_name) + 1;
      f.stats.total_v2_symbol_count++;
      link_ptr_offset = c_symlink2(f, data, code_offset, link_ptr_offset, kind, s_name, 0);
      if (data.at(link_ptr_offset) == 0)
        break;
    }
  }

  // check length
  assert(link_header_v2->length == align64(link_ptr_offset - link_data_offset + 1));
  while (link_ptr_offset < data.size()) {
    assert(data.at(link_ptr_offset) == 0);
    link_ptr_offset++;
  }
}

static void assert_string_empty_after(const char* str, int size) {
  auto ptr = str;
  while (*ptr)
    ptr++;
  while (ptr - str < size) {
    assert(!*ptr);
    ptr++;
  }
}

static void link_v5(LinkedObjectFile& f,
                    const std::vector<uint8_t>& data,
                    const std::string& name) {
  auto header = (const LinkHeaderV5*)(&data.at(0));
  if (header->n_segments == 1) {
    printf("abandon %s!\n", name.c_str());
    return;
  }
  assert(header->type_tag == 0);
  assert(name == header->name);
  assert(header->n_segments == 3);
  assert(header->pad == 0x50);
  assert(header->length_to_get_to_code - header->link_length == 0x50);

  f.set_segment_count(3);

  // link v3's data size is data.size() - link_length
  // link v5's data size is data.size() - new_link_length - 0x50.

  // lbp + 4 points to version?
  // lbp points to 4 past start of header.

  // lbp[1] = version + unknown 16 bit thing.
  // lbp[3] = link block length (minus 0x50)

  // todo - check this against the code size we actually got.
  //  size_t expected_code_size = data.size() - (header->link_length + 0x50);

  uint32_t data_ptr_offset = header->length_to_get_to_code;

  uint32_t segment_data_offsets[3];
  uint32_t segment_link_offsets[3];
  uint32_t segment_link_ends[3];
  for (int i = 0; i < 3; i++) {
    segment_data_offsets[i] = data_ptr_offset + header->segment_info[i].data;
    segment_link_offsets[i] = header->segment_info[i].relocs + 0x50;
    assert(header->segment_info[i].magic == 1);
  }

  // check that the data region is filled
  for (int i = 0; i < 2; i++) {
    assert(align16(segment_data_offsets[i] + header->segment_info[i].size) ==
           segment_data_offsets[i + 1]);
  }
  assert(align16(segment_data_offsets[2] + header->segment_info[2].size) == data.size());

  // loop over segments (reverse order for now)
  for (int seg_id = 3; seg_id-- > 0;) {
    // ?? is this right?
    if (header->segment_info[seg_id].size == 0)
      continue;

    auto segment_size = header->segment_info[seg_id].size;
    f.stats.v3_code_bytes += segment_size;

    //    if(gGameVersion == JAK2) {
    bool adjusted = false;
    while (segment_size % 4) {
      segment_size++;
      adjusted = true;
    }

    if (adjusted) {
      printf(
          "Adjusted the size of segment %d in %s, this is fine, but rare (and may indicate a "
          "bigger problem if it happens often)\n",
          seg_id, name.c_str());
    }
    //    }

    auto base_ptr = segment_data_offsets[seg_id];
    auto data_ptr = base_ptr - 4;
    auto link_ptr = segment_link_offsets[seg_id];

    assert((data_ptr % 4) == 0);
    assert((segment_size % 4) == 0);

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
                assert(lo_hi_offset);
                assert(dest_seg < 3);
                auto offset_upper = old_code & 0xff;
                //                assert(offset_upper == 0);
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
                                  SymbolLinkKind::EMPTY_LIST, sname, seg_id);
          } else {
            link_ptr = c_symlink2(f, data, segment_data_offsets[seg_id], link_ptr,
                                  SymbolLinkKind::SYMBOL, sname, seg_id);
          }
        } else if ((reloc & 0x3f) == 0x3f) {
          assert(false);  // todo, does this ever get hit?
        } else {
          int n_methods_base = reloc & 0x3f;
          int n_methods = n_methods_base * 4;
          if (n_methods_base) {
            n_methods += 3;
          }
          link_ptr += 2;  // ghidra misses some aliasing here and would have you think this is +1!
          const char* sname = (const char*)(&data.at(link_ptr));
          link_ptr += strlen(sname) + 1;
          link_ptr = c_symlink2(f, data, segment_data_offsets[seg_id], link_ptr,
                                SymbolLinkKind::TYPE, sname, seg_id);
        }

        sub_link_ptr = link_ptr;
        if (!data.at(sub_link_ptr))
          break;
      }
    }
    segment_link_ends[seg_id] = link_ptr;
  }

  assert(segment_link_offsets[0] == 128);

  if (header->segment_info[0].size) {
    assert(segment_link_ends[0] + 1 == segment_link_offsets[1]);
  } else {
    assert(segment_link_offsets[0] + 2 == segment_link_offsets[1]);
  }

  if (header->segment_info[1].size) {
    assert(segment_link_ends[1] + 1 == segment_link_offsets[2]);
  } else {
    assert(segment_link_offsets[1] + 2 == segment_link_offsets[2]);
  }

  assert(align16(segment_link_ends[2] + 2) == segment_data_offsets[0]);
}

static void link_v3(LinkedObjectFile& f,
                    const std::vector<uint8_t>& data,
                    const std::string& name) {
  auto header = (const LinkHeaderV3*)(&data.at(0));
  assert(name == header->name);
  assert(header->segments == 3);

  f.set_segment_count(3);
  assert_string_empty_after(header->name, 64);

  for (int i = 0; i < 3; i++) {
    assert(header->segment_info[i].magic == 0);
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
    assert(align16(segment_data_offsets[i] + header->segment_info[i].size) ==
           segment_data_offsets[i + 1]);
  }
  assert(align16(segment_data_offsets[2] + header->segment_info[2].size) == data.size());

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
    if (get_config().game_version == 1 && name == "level-h" && seg_id == 0) {
      segment_size++;
    }

    if (get_config().game_version == 2) {
      bool adjusted = false;
      while (segment_size % 4) {
        segment_size++;
        adjusted = true;
      }

      if (adjusted) {
        printf(
            "Adjusted the size of segment %d in %s, this is fine, but rare (and may indicate a "
            "bigger problem if it happens often)\n",
            seg_id, name.c_str());
      }
    }

    auto base_ptr = segment_data_offsets[seg_id];
    auto data_ptr = base_ptr - 4;
    auto link_ptr = segment_link_offsets[seg_id];

    assert((data_ptr % 4) == 0);
    assert((segment_size % 4) == 0);

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
                assert(lo_hi_offset);
                assert(dest_seg < 3);
                auto offset_upper = old_code & 0xff;
                //                assert(offset_upper == 0);
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
        // methods todo

        s_name = (const char*)(&data.at(link_ptr));
        get_type_info().inform_type_method_count(s_name, reloc & 0x7f);
        kind = SymbolLinkKind::TYPE;
      }

      if (std::string("_empty_") == s_name) {
        assert(kind == SymbolLinkKind::SYMBOL);
        kind = SymbolLinkKind::EMPTY_LIST;
      }

      link_ptr += strlen(s_name) + 1;
      f.stats.v3_symbol_count++;
      link_ptr = c_symlink3(f, data, base_ptr, link_ptr, kind, s_name, seg_id);
    }
    segment_link_ends[seg_id] = link_ptr;
  }

  assert(segment_link_offsets[0] == 128);

  if (header->segment_info[0].size) {
    assert(segment_link_ends[0] + 1 == segment_link_offsets[1]);
  } else {
    assert(segment_link_offsets[0] + 2 == segment_link_offsets[1]);
  }

  if (header->segment_info[1].size) {
    assert(segment_link_ends[1] + 1 == segment_link_offsets[2]);
  } else {
    assert(segment_link_offsets[1] + 2 == segment_link_offsets[2]);
  }

  assert(align16(segment_link_ends[2] + 2) == segment_data_offsets[0]);
}

/*!
 * Main function to generate LinkedObjectFiles from raw object data.
 */
LinkedObjectFile to_linked_object_file(const std::vector<uint8_t>& data, const std::string& name) {
  LinkedObjectFile result;
  const auto* header = (const LinkHeaderCommon*)&data.at(0);

  // use appropriate linker
  if (header->version == 3) {
    assert(header->type_tag == 0);
    link_v3(result, data, name);
  } else if (header->version == 4) {
    assert(header->type_tag == 0xffffffff);
    link_v4(result, data, name);
  } else if (header->version == 5) {
    link_v5(result, data, name);
  } else {
    assert(false);
  }

  return result;
}
