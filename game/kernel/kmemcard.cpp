/*!
 * @file kmemcard.cpp
 * Memory card interface. Very messy code.
 */

//#include "ps2/SCE_MC.h"
//#include "ps2/SCE_FS.h"
//#include "ps2/common_types.h"
//#include "kernel/kmachine.h"
#include "kmemcard.h"

// static s32 next;
// static s32 language;
// static MemoryCardOperation op;
// static mc_info mc[2];

void kmemcard_init_globals() {
  //  next = 0;
}

///*!
// * Get a new memory card handle.
// * Will never return 0.
// */
// s32 new_mc_handle() {
//  s32 handle = next++;
//
//  // if you wrap around, it avoid the zero handle.
//  // it doesn't seem like you will need billions of memory card handles
//  if(handle == 0) {
//    handle = next++;
//  }
//  return handle;
//}
//
///*!
// * A questionable checksum.
// */
// u32 mc_checksum(Ptr<u8> data, s32 size) {
//  if(size < 0) {
//    size += 3;
//  }
//
//  u32 result = 0;
//  u32* data_u32 = (u32*)data.c();
//  for(s32 i = 0; i < size / 4; i++) {
//    result = result << 1 ^ result >> 0x1f ^ data_u32[i*4] ^ 0x12345678;
//  }
//
//  return result ^ 0xedd1e666;
//}
//
// u32 handle_to_slot(s32 handle, s32 p2) {
//  if(mc[0].p0 == p2 && mc[0].handle == handle) {
//    return 0;
//  }
//  if(mc[1].p0 == p2 && mc[0].handle == handle) {
//    return 1;
//  } else {
//    return -1;
//  }
//}
//
// void MC_run() {
//
//}
//
///*!
// * Set the language or something.
// */
// void MC_set_language(s32 l) {
//  printf("Language set to %d\n", l);
//  language = l;
//}
//
// u64 MC_format(s32 param) {
//  u64 can_add = op.operation == NO_OP;
//  if(can_add) {
//    op.operation = FORMAT;
//    op.result = 0;
//    op.f_10 = 100;
//    op.param = param;
//  }
//  return can_add;
//}
//
//
// u64 MC_unformat(s32 param) {
//  u64 can_add = op.operation == NO_OP;
//  if(can_add) {
//    op.operation = UNFORMAT;
//    op.result = 0;
//    op.f_10 = 100;
//    op.param = param;
//  }
//  return can_add;
//}
//
// u64 MC_createfile(s32 param, Ptr<u8> data) {
//  u64 can_add = op.operation == NO_OP;
//  if(can_add) {
//    op.operation = CREATE_FILE;
//    op.result = 0;
//    op.f_10 = 100;
//    op.param = param;
//    op.data_ptr = data;
//  }
//  return can_add;
//}
//
// u64 MC_save(s32 param, s32 param2, Ptr<u8> data, Ptr<u8> data2) {
//  u64 can_add = op.operation == NO_OP;
//  if(can_add) {
//    op.operation = SAVE;
//    op.result = 0;
//    op.f_10 = 100;
//    op.param = param;
//    op.param2 = param2;
//    op.data_ptr = data;
//    op.data_ptr2 = data2;
//  }
//  return can_add;
//}
//
// u64 MC_load(s32 param, s32 param2, Ptr<u8> data) {
//  u64 can_add = op.operation == NO_OP;
//  if(can_add) {
//    op.operation = LOAD;
//    op.result = 0;
//    op.f_10 = 100;
//    op.param = param;
//    op.param2 = param2;
//    op.data_ptr = data;
//  }
//  return can_add;
//}
//
///*!
// * Some sort of test function for memory card stuff.
// */
// void MC_makefile(s32 port, s32 size) {
//  sceMcMkdir(port, 0, "/BASCUS-00000XXXXXXXX");
//  // wait for operation to complete
//  s32 cmd, result, fd;
//  sceMcSync(0, &cmd, &result);
//
//  if(result == sceMcResSucceed || result == sceMcResNoEntry) {
//    // it worked, or the folder already exists...
//
//    // open file
//    sceMcOpen(port, 0, "/BASCUS-00000XXXXXXXX/BASCUS-00000XXXXXXXX", SCE_CREAT | SCE_WRONLY);
//    sceMcSync(0, &cmd, &fd);
//
//    if(result < 0) {
//      printf("Can\'t open file on memcard [%d]\n", result);
//    } else {
//      // write some random crap into the memory card.
//      sceMcWrite(fd, Ptr<u8>(0x1000000).c(), size);
//      sceMcSync(0, &cmd, &result);
//      if(result != size) {
//        printf("Only written %d bytes\n", result);
//      }
//      sceMcClose(fd);
//      sceMcSync(0, &cmd, &result);
//    }
//  } else {
//    printf("Can\'t create garbage folder [%d]\n", result);
//  }
//}
//
// u32 MC_check_result() {
//  return op.result;
//}
//
// void MC_get_status(s32 slot, Ptr<mc_slot_info> info) {
//  info->handle = 0;
//  info->known = 0;
//  info->formatted = 0;
//  info->initted = 0;
//  for(s32 i = 0; i < 4; i++) {
//    info->files[i].present = 0;
//  }
//  info->last_file = 0xffffffff;
//  info->mem_required = SAVE_SIZE;
//  info->mem_actual = 0;
//
//  switch(mc[slot].p0) {
//    case 1:
//      info->known = 1;
//      break;
//    case 2:
//      info->known = 1;
//      info->handle = mc[slot].handle;
//      break;
//    case 3:
//      info->known = 1;
//      info->handle = mc[slot].handle;
//      info->formatted = 1;
//      if(mc[slot].inited == 0) {
//        info->mem_actual = mc[slot].mem_actual;
//      } else {
//        info->initted = 1;
//        for(s32 file = 0; file < 4; file++) {
//          info->files[file].present = mc[slot].files[file].present;
//          for(s32 i = 0; i < 64; i++) { // actually a loop over u32's
//            info->files[file].data[i] = mc[slot].files[file].data[i];
//          }
//        }
//        info->last_file = mc[slot].last_file;
//
//      }
//  }
//
//}