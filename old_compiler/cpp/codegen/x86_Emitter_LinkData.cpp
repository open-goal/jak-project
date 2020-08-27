/*!
 * @file x86_Emitter_LinkData.cpp
 * Emitter for converting IR and static objects into GOAL object files for x86 - link table
 * generation
 */

#include "x86_Emitter.h"
#include "codegen_utils.h"
#include "goal/GoalEnv.h"

/*!
 * Add symbol mem access records to link table for the given segment
 */
void x86_Emitter::emit_link_table_symbol_mem_recs(CodegenOutput& out, int seg) {
  // these are
  //  id (1 byte)
  //  name (n bytes)
  //  name-null-terminator (1 byte)
  //  count of links (4 bytes)
  //  offset into segment's code of where to patch (4 bytes * count)
  for (auto& symbol_mem_rec : symbol_mem_access_recs[seg]) {
    out.link_tables[seg].push_back(LINK_SYMBOL_OFFSET);

    // name
    for (char c : symbol_mem_rec.first) {
      out.link_tables[seg].push_back(c);
    }
    out.link_tables[seg].push_back(0);

    // links
    push_data_to_byte_vector<uint32_t>(symbol_mem_rec.second.size(), out.link_tables[seg]);
    for (auto& r : symbol_mem_rec.second) {
      assert(r.seg == seg);
      push_data_to_byte_vector<int32_t>(r.total_offset, out.link_tables[seg]);
    }
  }

  for (auto& sym_rec : type_ptr_recs_in_statics[seg]) {
    uint32_t sym_rec_count = 0;
    for (auto& rec : sym_rec.second) {
      if (rec.kind == StaticLinkRecord::SYMBOL_PTR) {
        sym_rec_count++;
      }
    }

    if (!sym_rec_count)
      continue;

    out.link_tables[seg].push_back(LINK_SYMBOL_OFFSET);

    // name
    for (char c : sym_rec.first) {
      out.link_tables[seg].push_back(c);
    }
    out.link_tables[seg].push_back(0);

    // links
    push_data_to_byte_vector<uint32_t>(sym_rec_count, out.link_tables[seg]);
    for (auto& r : sym_rec.second) {
      //      assert(r.seg == seg);
      if (r.kind == StaticLinkRecord::Kind::SYMBOL_PTR) {
        push_data_to_byte_vector<int32_t>(r.offset, out.link_tables[seg]);
      }
    }
  }
}

/*!
 * Add type pointer records for static data to link table for the given segment.
 */
void x86_Emitter::emit_link_table_type_ptrs(CodegenOutput& out, int seg) {
  // ID, Name (null terminated), method count (u8), count of links, link table
  for (auto& symbol_ptr_rec : type_ptr_recs_in_statics.at(seg)) {
    uint32_t type_rec_count = 0;
    for (auto& rec : symbol_ptr_rec.second) {
      if (rec.kind == StaticLinkRecord::TYPE_PTR) {
        type_rec_count++;
      }
    }

    if (!type_rec_count)
      continue;

    // id
    out.link_tables[seg].push_back(LINK_TYPE_PTR);

    // name
    for (char c : symbol_ptr_rec.first) {
      out.link_tables[seg].push_back(c);
    }
    out.link_tables[seg].push_back(0);

    // method count
    out.link_tables[seg].push_back(0);  // todo!

    // count of links
    push_data_to_byte_vector<uint32_t>(type_rec_count, out.link_tables[seg]);

    // link table
    for (auto& r : symbol_ptr_rec.second) {
      if (r.kind == StaticLinkRecord::Kind::TYPE_PTR) {
        push_data_to_byte_vector<int32_t>(r.offset, out.link_tables[seg]);
      }
    }
  }
}

/*!
 * Add type pointer records for functions to link table for the given segment.
 */
void x86_Emitter::emit_link_table_func_type_ptr(CodegenOutput& out, int seg) {
  // id
  out.link_tables[seg].push_back(LINK_TYPE_PTR);
  std::string name = "function";

  // name
  for (char c : name) {
    out.link_tables[seg].push_back(c);
  }
  out.link_tables[seg].push_back(0);

  // method count
  out.link_tables[seg].push_back(0);  // todo!

  // count of links
  push_data_to_byte_vector<uint32_t>(function_type_ptr_recs[seg].size(), out.link_tables[seg]);

  // link table
  for (auto& r : function_type_ptr_recs[seg]) {
    push_data_to_byte_vector<int32_t>(r, out.link_tables[seg]);
  }
}

/*!
 * Add variable address records to the link table for the given segment
 */
void x86_Emitter::emit_link_table_var_addr(CodegenOutput& out,
                                           std::vector<int>& instruction_offsets,
                                           int seg) {
  // link pointers to variables.
  // currently this is for both variables in your segment and in other segments
  // and it is not super efficient space wise
  for (auto& rec : static_var_addr_recs.at(seg)) {
    // ID, target_seg, offset_into_this_seg,  offset_into_target_seg, patch location

    LinkKind kind;
    switch (rec.size) {
      case 4:
        kind = LINK_DISTANCE_TO_OTHER_SEG_32;
        break;
      case 8:
        kind = LINK_DISTANCE_TO_OTHER_SEG_64;
        break;
      default:
        throw std::runtime_error("unknown size in static_var_addr_recs link target!");
    }
    out.link_tables[seg].push_back(kind);

    uint8_t target_segment = rec.place->object->segment;
    out.link_tables[seg].push_back(target_segment);

    uint32_t offset_into_current_seg = instruction_offsets.at(rec.current_rbp_instr_idx);
    push_data_to_byte_vector<uint32_t>(offset_into_current_seg, out.link_tables[seg]);

    // statics only know their location relative to the start of statics
    uint32_t offset_into_target_seg =
        out.static_start.at(target_segment) + rec.place->object->offset;
    push_data_to_byte_vector<uint32_t>(offset_into_target_seg, out.link_tables[seg]);

    uint32_t patch_location = instruction_offsets.at(rec.instr_idx) + rec.offset_into_instr;
    push_data_to_byte_vector<uint32_t>(patch_location, out.link_tables[seg]);
  }
}

/*!
 * Add function address records to the link table for the given segment
 */
void x86_Emitter::emit_link_table_func_addr(CodegenOutput& out,
                                            std::vector<int>& instruction_offsets,
                                            int seg) {
  for (auto& rec : func_addr_recs.at(seg)) {
    out.link_tables[seg].push_back(LINK_DISTANCE_TO_OTHER_SEG_64);

    uint8_t target_segment = rec.place->func->segment;
    out.link_tables[seg].push_back(target_segment);

    uint32_t offset_into_current_seg = instruction_offsets.at(rec.current_rbp_instr_idx);
    push_data_to_byte_vector<uint32_t>(offset_into_current_seg, out.link_tables[seg]);

    // functions know what their first instruction index is
    uint32_t offset_into_target_seg =
        out.instr_offsets[target_segment].at(rec.place->func->first_instruction);
    push_data_to_byte_vector<uint32_t>(offset_into_target_seg, out.link_tables[seg]);

    uint32_t patch_location = instruction_offsets.at(rec.instr_idx) + rec.offset_into_instr;
    push_data_to_byte_vector<uint32_t>(patch_location, out.link_tables[seg]);
  }
}

/*!
 * Generate the link table data for a segment.
 */
void x86_Emitter::emit_link_table_data(CodegenOutput& out,
                                       std::vector<int>& instruction_offsets,
                                       int seg) {
  emit_link_table_symbol_mem_recs(out, seg);
  emit_link_table_type_ptrs(out, seg);
  emit_link_table_func_type_ptr(out, seg);
  emit_link_table_var_addr(out, instruction_offsets, seg);
  emit_link_table_func_addr(out, instruction_offsets, seg);
  out.link_tables[seg].push_back(LINK_TABLE_END);
}

/*!
 * Generate the header data for the link data.
 * This must run after all code and link table stuff is done
 */
void x86_Emitter::emit_link_table_header(CodegenOutput& out) {
  // fake type tag
  out.header.push_back('G');
  out.header.push_back('O');
  out.header.push_back('A');
  out.header.push_back('L');

  uint32_t offset = 0;
  offset += push_data_to_byte_vector<uint16_t>(GOAL_VERSION_MAJOR, out.header);
  offset += push_data_to_byte_vector<uint16_t>(GOAL_VERSION_MINOR, out.header);
  offset += push_data_to_byte_vector<uint32_t>(3, out.header);
  offset += push_data_to_byte_vector<uint32_t>(N_SEG, out.header);

  offset += sizeof(uint32_t) * N_SEG * 4;
  offset += 4;
  int total_link_size = 0;

  struct SizeOffset {
    uint32_t offset, size;
  };

  struct SizeOffsetTable {
    SizeOffset link_seg[N_SEG];
    SizeOffset code_seg[N_SEG];
  };

  SizeOffsetTable table;

  for (int i = N_SEG; i-- > 0;) {
    table.link_seg[i].offset = offset;
    table.link_seg[i].size = out.link_tables[i].size();
    offset += out.link_tables[i].size();
    total_link_size += out.link_tables[i].size();
  }

  offset = 0;
  for (int i = N_SEG; i-- > 0;) {
    table.code_seg[i].offset = offset;
    table.code_seg[i].size = out.code[i].size();
    offset += out.code[i].size();
  }

  push_data_to_byte_vector<SizeOffsetTable>(table, out.header);
  push_data_to_byte_vector<uint32_t>(64 + 4 + total_link_size, out.header);
}