/*!
 * @file ObjectGenerator.cpp
 * Tool to build GOAL object files. Will eventually support v3 and v4.
 *
 * There are 5 steps:
 * 1. The user adds static data / instructions and specifies links.
 * 2. The functions and static data are laid out in memory
 * 3. The user specified links are updated according to the memory layout, and jumps are patched
 * 4. The link table is generated for each segment
 * 5. All segments and link tables are put into a final object file, along with a header.
 *
 * Step 1 can be done with the add_.... and link_... functions
 * Steps 2 - 5 are done in generate_data_vX()
 */

#include "ObjectGenerator.h"
#include "common/goal_constants.h"
#include "common/versions.h"

namespace emitter {

ObjectFileData ObjectGenerator::generate_data_v3() {
  ObjectFileData out;

  // do functions (step 2, part 1)
  for (int seg = N_SEG; seg-- > 0;) {
    auto& data = m_data_by_seg.at(seg);
    // loop over functions in this segment
    for (auto& function : m_function_data_by_seg.at(seg)) {
      // align
      while (data.size() % function.min_align) {
        insert_data<u8>(seg, 0);
      }

      // add a type tag link
      m_type_ptr_links_by_seg.at(seg)["function"].push_back(data.size());

      // add room for a type tag
      for (int i = 0; i < POINTER_SIZE; i++) {
        insert_data<u8>(seg, 0xae);
      }

      // insert instructions!
      for (const auto& instr : function.instructions) {
        u8 temp[128];
        auto count = instr.emit(temp);
        assert(count < 128);
        function.instruction_to_byte_in_data.push_back(data.size());
        for (int i = 0; i < count; i++) {
          insert_data<u8>(seg, temp[i]);
        }
      }
    }
  }

  // do static data layout (step 2, part 2)
  for (int seg = N_SEG; seg-- > 0;) {
    auto& data = m_data_by_seg.at(seg);
    for (auto& s : m_static_data_by_seg.at(seg)) {
      // align
      while (data.size() % s.min_align) {
        insert_data<u8>(seg, 0);
      }

      s.location = data.size();

      data.insert(data.end(), s.data.begin(), s.data.end());
    }
  }

  // step 3, cleaning up things now that we know the memory layout
  for (int seg = N_SEG; seg-- > 0;) {
    // jumps?
    handle_temp_static_type_links(seg);
    handle_temp_jump_links(seg);
  }

  // actual linking?
  for (int seg = N_SEG; seg-- > 0;) {
  }

  // emit header
  emit_link_tables();
  out.header = generate_header_v3();
  out.segment_data = std::move(m_data_by_seg);
  out.link_tables = std::move(m_link_by_seg);
  return out;
}

/*!
 * Add a new function to seg, and return a FunctionRecord which can be used to specify this
 * new function.
 */
FunctionRecord ObjectGenerator::add_function_to_seg(int seg, int min_align) {
  FunctionRecord rec;
  rec.seg = seg;
  rec.func_id = int(m_function_data_by_seg.at(seg).size());
  m_function_data_by_seg.at(seg).emplace_back();
  m_function_data_by_seg.at(seg).back().min_align = min_align;
  return rec;
}

/*!
 * Add a new IR instruction to the function. An IR instruction may contain 0, 1, or multiple
 * actual Instructions. These Instructions can be added with add_instruction.  The IR_Record
 * can be used as a label for jump targets.
 */
IR_Record ObjectGenerator::add_ir(const FunctionRecord& func) {
  // verify we aren't adding to an old function. not technically an error, but doesn't make sense
  assert(func.func_id == int(m_function_data_by_seg.at(func.seg).size()) - 1);
  IR_Record rec;
  rec.seg = func.seg;
  rec.func_id = func.func_id;
  auto& func_data = m_function_data_by_seg.at(rec.seg).at(rec.func_id);
  rec.ir_id = int(func_data.ir_to_instruction.size());
  func_data.ir_to_instruction.push_back(int(func_data.instructions.size()));
  return rec;
}

/*!
 * Add a new Instruction for the given IR instruction.
 */
InstructionRecord ObjectGenerator::add_instr(Instruction inst, IR_Record ir) {
  // verify we aren't adding to an old instruction or function
  assert(ir.func_id == int(m_function_data_by_seg.at(ir.seg).size()) - 1);
  // only this second condition is an actual error.
  assert(ir.ir_id ==
         int(m_function_data_by_seg.at(ir.seg).at(ir.func_id).ir_to_instruction.size()) - 1);

  InstructionRecord rec;
  rec.seg = ir.seg;
  rec.func_id = ir.func_id;
  rec.ir_id = ir.ir_id;
  auto& func_data = m_function_data_by_seg.at(rec.seg).at(rec.func_id);
  rec.instr_id = int(func_data.instructions.size());
  func_data.instructions.push_back(inst);
  return rec;
}

/*!
 * Create a new static object in the given segment.
 */
StaticRecord ObjectGenerator::add_static_to_seg(int seg, int min_align) {
  StaticRecord rec;
  rec.seg = seg;
  rec.static_id = m_static_data_by_seg.at(seg).size();
  m_static_data_by_seg.at(seg).emplace_back();
  m_static_data_by_seg.at(seg).back().min_align = min_align;
  return rec;
}

/*!
 * Add linking data to add a type pointer in rec at offset.
 * This will add an entry to the linking data, which will get patched at runtime, during linking.
 */
void ObjectGenerator::link_static_type_ptr(StaticRecord rec,
                                           int offset,
                                           const std::string& type_name) {
  StaticTypeLink link;
  link.offset = offset;
  link.rec = rec;
  m_static_type_temp_links_by_seg.at(rec.seg)[type_name].push_back(link);
}

/*!
 * This will patch the jump_instr to jump to destination. This happens during compile time and
 * doesn't add anything to the link table.  The jump_instr must already be emitted, however the
 * destination can be a future IR. To get a reference to a future IR, you must know the index and
 * use get_future_ir.
 */
void ObjectGenerator::link_instruction_jump(InstructionRecord jump_instr, IR_Record destination) {
  // must jump within our own function.
  assert(jump_instr.seg == destination.seg);
  assert(jump_instr.func_id == destination.func_id);
  m_jump_temp_links_by_seg.at(jump_instr.seg).push_back({jump_instr, destination});
}

/*!
 * m_static_type_temp_links_by_seg -> m_type_ptr_links_by_seg
 */
void ObjectGenerator::handle_temp_static_type_links(int seg) {
  for (const auto& type_links : m_static_type_temp_links_by_seg.at(seg)) {
    const auto& type_name = type_links.first;
    for (const auto& link : type_links.second) {
      assert(seg == link.rec.seg);
      const auto& static_object = m_static_data_by_seg.at(seg).at(link.rec.static_id);
      int total_offset = static_object.location + link.offset;
      m_type_ptr_links_by_seg.at(seg)[type_name].push_back(total_offset);
    }
  }
}

/*!
 * m_jump_temp_links_by_seg patching
 */
void ObjectGenerator::handle_temp_jump_links(int seg) {
  for (const auto& link : m_jump_temp_links_by_seg.at(seg)) {
    // we need to compute three offsets, all relative to the start of data.
    // 1). the location of the patch (the immediate of the opcode)
    // 2). the value of RIP at the jump (the instruction after the jump, on x86)
    // 3). the value of RIP we want
    const auto& function = m_function_data_by_seg.at(seg).at(link.jump_instr.func_id);
    assert(link.jump_instr.func_id == link.dest.func_id);
    assert(link.jump_instr.seg == seg);
    assert(link.dest.seg == seg);
    const auto& jump_instr = function.instructions.at(link.jump_instr.instr_id);
    assert(jump_instr.get_imm_size() == 4);

    // 1). patch = instruction location + location of imm in instruction.
    int patch_location = function.instruction_to_byte_in_data.at(link.jump_instr.instr_id) +
                         jump_instr.offset_of_imm();

    // 2). source rip = jump instr + 1 location
    int source_rip = function.instruction_to_byte_in_data.at(link.jump_instr.instr_id + 1);

    // 3). dest rip = first instruction of dest IR
    int dest_rip =
        function.instruction_to_byte_in_data.at(function.ir_to_instruction.at(link.dest.ir_id));

    patch_data<s32>(seg, patch_location, dest_rip - source_rip);
  }
}

void ObjectGenerator::emit_link_tables() {
  // todo!
}

namespace {
template <typename T>
uint32_t push_data(const T& data, std::vector<u8>& v) {
  auto insert = v.size();
  v.resize(insert + sizeof(T));
  memcpy(v.data() + insert, &data, sizeof(T));
  return sizeof(T);
}
}  // namespace

/*!
 * Generate linker header.
 */
std::vector<u8> ObjectGenerator::generate_header_v3() {
  std::vector<u8> result;

  // header starts with a "GOAL" magic word
  result.push_back('G');
  result.push_back('O');
  result.push_back('A');
  result.push_back('L');

  u32 offset = 0;  // the GOAL doesn't count toward the offset, first 4 bytes are killed.
  // then, the version.  todo, bump the version once we use this!
  offset += push_data<u16>(versions::GOAL_VERSION_MAJOR, result);
  offset += push_data<u16>(versions::GOAL_VERSION_MINOR, result);

  // the object file version
  offset += push_data<u32>(3, result);
  // the segment count
  offset += push_data<u32>(N_SEG, result);

  offset += sizeof(u32) * N_SEG * 4;  // 4 u32's per segment

  struct SizeOffset {
    uint32_t offset, size;
  };

  struct SizeOffsetTable {
    SizeOffset link_seg[N_SEG];
    SizeOffset code_seg[N_SEG];
  };

  SizeOffsetTable table;
  int total_link_size = 0;

  for (int i = N_SEG; i-- > 0;) {
    table.link_seg[i].offset = offset;                 // start of the link
    table.link_seg[i].size = m_link_by_seg[i].size();  // size of the link data
    offset += m_link_by_seg[i].size();                 // to next link data
    total_link_size += m_link_by_seg[i].size();        // need to track this.
  }

  offset = 0;
  for (int i = N_SEG; i-- > 0;) {
    table.code_seg[i].offset = offset;
    table.code_seg[i].size = m_data_by_seg[i].size();
    offset += m_data_by_seg[i].size();
  }

  push_data<SizeOffsetTable>(table, result);
  push_data<uint32_t>(64 + 4 + total_link_size, result);  // todo, make these numbers less magic.
  return result;
}
}  // namespace emitter