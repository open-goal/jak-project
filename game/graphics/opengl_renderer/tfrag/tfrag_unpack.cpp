#include "game/graphics/opengl_renderer/tfrag/TFragment.h"

// TFragment VIF unpack implementation

int TFragment::handle_unpack_v4_8_mode0(const VifCode& code,
                                        const DmaTransfer& dma,
                                        int offset,
                                        int cl,
                                        int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);

  // CL x (num/WL)+(num%WL)

  u8* write_base = get_upload_buffer();

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    assert(cl == 2);
    assert(wl == 1);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + 2 * i;
      assert(dest_qw <= 328);
      u32 qw[4];
      qw[0] = dma.read_val<u8>(offset++);
      qw[1] = dma.read_val<u8>(offset++);
      qw[2] = dma.read_val<u8>(offset++);
      qw[3] = dma.read_val<u8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      s32 qw[4];
      qw[0] = dma.read_val<s8>(offset++);
      qw[1] = dma.read_val<s8>(offset++);
      qw[2] = dma.read_val<s8>(offset++);
      qw[3] = dma.read_val<s8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  }

  return offset;
}

int TFragment::handle_unpack_v4_8_mode1(const VifCode& code,
                                        const DmaTransfer& dma,
                                        int offset,
                                        int cl,
                                        int wl,
                                        const u32 row[4]) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);

  // CL x (num/WL)+(num%WL)

  u8* write_base = get_upload_buffer();

  if (unpack.is_unsigned) {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      u32 qw[4];
      qw[0] = row[0] + dma.read_val<u8>(offset++);
      qw[1] = row[1] + dma.read_val<u8>(offset++);
      qw[2] = row[2] + dma.read_val<u8>(offset++);
      qw[3] = row[3] + dma.read_val<u8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  } else {
    // note: formulas below assume this!
    assert(cl == 4);
    assert(wl == 4);
    assert(code.num);
    for (int i = 0; i < code.num; i++) {
      // write every other qw
      int dest_qw = unpack.addr_qw + i;
      assert(dest_qw <= 328);
      s32 qw[4];
      qw[0] = row[0] + dma.read_val<s8>(offset++);
      qw[1] = row[1] + dma.read_val<s8>(offset++);
      qw[2] = row[2] + dma.read_val<s8>(offset++);
      qw[3] = row[3] + dma.read_val<s8>(offset++);
      memcpy(write_base + (dest_qw * 16), qw, 16);
    }
  }

  return offset;
}

int TFragment::handle_unpack_v4_16_mode0(const VifCode& code,
                                         const DmaTransfer& dma,
                                         int offset,
                                         int cl,
                                         int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);

  u8* write_base = get_upload_buffer();
  assert(code.num);
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = dma.read_val<u16>(offset);
    offset += 2;
    qw[1] = dma.read_val<u16>(offset);
    offset += 2;
    qw[2] = dma.read_val<u16>(offset);
    offset += 2;
    qw[3] = dma.read_val<u16>(offset);
    offset += 2;
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;
}

int TFragment::handle_unpack_v4_16_mode1(const VifCode& code,
                                         const DmaTransfer& dma,
                                         int offset,
                                         int cl,
                                         int wl,
                                         const u32 row[4]) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);

  assert(code.num);
  u8* write_base = get_upload_buffer();
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = row[0] + (u32)dma.read_val<u16>(offset);
    offset += 2;
    qw[1] = row[1] + (u32)dma.read_val<u16>(offset);
    offset += 2;
    qw[2] = row[2] + (u32)dma.read_val<u16>(offset);
    offset += 2;
    qw[3] = row[3] + (u32)dma.read_val<u16>(offset);
    offset += 2;

    // fmt::print("  unpack rgba?: {:x} {:x} {:x} {:x}\n", qw[0], qw[1], qw[2], qw[3]);
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;
}

int TFragment::handle_unpack_v3_32(const VifCode& code,
                                   const DmaTransfer& dma,
                                   int offset,
                                   int cl,
                                   int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(!unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 2);
  assert(wl == 1);

  assert(code.num);
  u8* write_base = get_upload_buffer();
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i * 2;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = dma.read_val<u32>(offset);
    offset += 4;
    qw[1] = dma.read_val<u32>(offset);
    offset += 4;
    qw[2] = dma.read_val<u32>(offset);
    offset += 4;
    qw[3] = 0x80;  // this can be anything... but it seems like it tries to load from it sometimes?
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;
}

int TFragment::handle_unpack_v4_32(const VifCode& code,
                                   const DmaTransfer& dma,
                                   int offset,
                                   int cl,
                                   int wl) {
  VifCodeUnpack unpack(code);
  assert(unpack.use_tops_flag);
  assert(!unpack.is_unsigned);

  // note: formulas below assume this!
  assert(cl == 4);
  assert(wl == 4);
  u8* write_base = get_upload_buffer();
  assert(code.num);
  for (int i = 0; i < code.num; i++) {
    // write every other qw
    int dest_qw = unpack.addr_qw + i;
    assert(dest_qw <= 328);
    u32 qw[4];
    qw[0] = dma.read_val<u32>(offset);
    offset += 4;
    qw[1] = dma.read_val<u32>(offset);
    offset += 4;
    qw[2] = dma.read_val<u32>(offset);
    offset += 4;
    qw[3] = dma.read_val<u32>(offset);
    offset += 4;
    memcpy(write_base + (dest_qw * 16), qw, 16);
  }
  return offset;

  //  u8* write_base = get_upload_buffer();
  //  assert(code.num + unpack.addr_qw <= 328);
  //  memcpy(write_base + (unpack.addr_qw * 16), dma.data + offset, code.num * 16);
  //  return offset + code.num * 16;
}
