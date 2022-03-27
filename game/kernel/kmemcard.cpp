/*!
 * @file kmemcard.cpp
 * Memory card interface. Very messy code. Most of it is commented out now, as we've switched away
 * from memory cards to just raw saves.
 */

#include <cstdio>
#include <cstring>
#include <array>

#include "third-party/fmt/core.h"

#include "game/sce/sif_ee.h"
#include "game/sce/sif_ee_memcard.h"
#include "kmemcard.h"
#include "game/kernel/kdgo.h"
#include "game/common/ramdisk_rpc_types.h"
#include "game/kernel/fileio.h"
#include "common/util/FileUtil.h"
#include "common/util/Timer.h"
#include "common/util/Assert.h"

static constexpr bool memcard_debug = false;

using McCallbackFunc = void (*)(s32);

McCallbackFunc callback;

// static s32 next;
static s32 language;
static MemoryCardOperation op;
// static MemoryCard mc[2];
// instead of two memory cards we just simulate the 4 save files (8 banks).
static MemoryCardFile mc_files[4];
// keep track of latest file selected. this is only used in an auto-save mode thats not used
static int mc_last_file = -1;
// static RPC_Ramdisk_LoadCmd ramdisk_cmd;
// static ee::sceMcTblGetDir dirent;

// a random value we will use as the memory card "handle" for the pc port, which has no memcards.
constexpr u32 PC_MEM_CARD_HANDLE = 0x6C616F67;

constexpr u32 MEM_CARD_MAGIC = 0x12345678;

struct McHeader {
  u32 save_count;
  u32 checksum;
  u32 magic;
  u8 preview_data[64];
  u8 data[944];
  u32 unk1_repeated;
};
static_assert(sizeof(McHeader) == 0x400, "McHeader size");
constexpr s32 BANK_TOTAL_SIZE = BANK_SIZE + sizeof(McHeader) * 2;

static McHeader header;

// these are the return value for sceMcGetInfo.
static s32 p1, p2, p3, p4;
using namespace ee;

// void cb_reprobe_format(s32);
// void cb_format_complete(s32);
// void cb_unformat(s32);
// void cb_reprobe_createfile(s32);
// void cb_wait_for_ramdisk(s32);
// void cb_wait_for_ramdisk_load(s32);
// void cb_createfile_erasing(s32);
// void cb_createdir(s32);
// void cb_createdfile(s32);
// void cb_writtenfile(s32);
// void cb_closedfile(s32);
// void cb_reprobe_save(s32);
// void cb_reprobe_load(s32);
// void cb_probe(s32);
// void cb_reprobe(s32);
// void cb_getdir(s32);
// void cb_check_open(s32);
// void cb_check_read(s32);
// void cb_check_close(s32);
// void cb_openedsave(s32);
// void cb_savedheader(s32);
// void cb_saveddata(s32);
// void cb_savedfooter(s32);
// void cb_closedsave(s32);
// void cb_openedload(s32);
// void cb_readload(s32);
// void cb_closedload(s32);

template <typename... Args>
void mc_print(const std::string& str, Args&&... args) {
  if (memcard_debug) {
    fmt::print("[MC] ");
    if (!str.empty() && str.back() == '\n') {
      fmt::print(str, std::forward<Args>(args)...);
    } else {
      fmt::print(str + '\n', std::forward<Args>(args)...);
    }
  }
}

const char* filename[12] = {
    "BASCUS-97124AYBABTU!",           "BASCUS-97124AYBABTU!/icon.sys",
    "BASCUS-97124AYBABTU!/icon.ico",  "BASCUS-97124AYBABTU!/BASCUS-97124AYBABTU!",
    "BASCUS-97124AYBABTU!/bank0.bin", "BASCUS-97124AYBABTU!/bank1.bin",
    "BASCUS-97124AYBABTU!/bank2.bin", "BASCUS-97124AYBABTU!/bank3.bin",
    "BASCUS-97124AYBABTU!/bank4.bin", "BASCUS-97124AYBABTU!/bank5.bin",
    "BASCUS-97124AYBABTU!/bank6.bin", "BASCUS-97124AYBABTU!/bank7.bin"};

void kmemcard_init_globals() {
  // next = 0;
  language = 0;
  op = {};
  // mc[0] = {};
  // mc[1] = {};
  mc_files[0] = {};
  mc_files[1] = {};
  mc_files[2] = {};
  mc_files[3] = {};
  callback = nullptr;
  p1 = 0;
  p2 = 0;
  p3 = 0;
  p4 = 0;
  // memset(&dirent, 0, sizeof(sceMcTblGetDir));
  memset(&header, 0, sizeof(McHeader));
}

/*!
 * Get a new memory card handle.
 * Will never return 0.
 * A handle is a unique integer that can be passed up to the GOAL game code and represents a
 * specific memory card. If the card is removed, the handle will become invalid.
 */
// s32 new_mc_handle() {
//  s32 handle = next++;
//
//  // if you wrap around, it avoids the zero handle.
//  // it doesn't seem like you will need billions of memory card handles
//  if (handle == 0) {
//    handle = next++;
//  }
//  return handle;
//}

/*!
 * A questionable checksum used on memory card data.
 */
u32 mc_checksum(Ptr<u8> data, s32 size) {
  if (size < 0) {
    size += 3;
  }

  u32 result = 0;
  u32* data_u32 = (u32*)data.c();
  for (s32 i = 0; i < size / 4; i++) {
    result = result << 1 ^ (s32)result >> 0x1f ^ data_u32[i] ^ MEM_CARD_MAGIC;
  }

  return result ^ 0xedd1e666;
}

/*!
 * Get the slot for a handle. The card must be in the given state.
 * Return -1 if it fails.
 * There are no slots in the port. This function should not be used.
 */
// s32 handle_to_slot(u32 handle, MemoryCardState state) {
//  if (mc[0].state == state && mc[0].handle == handle) {
//    return 0;
//  }
//  if (mc[1].state == state && mc[0].handle == handle) {
//    return 1;
//  } else {
//    return -1;
//  }
//}

/*!
 * PC port function that returns whether a given bank ID's file exists or not.
 */
bool file_is_present(int id, int bank = 0) {
  auto bankname = file_util::get_user_memcard_dir() / filename[4 + id * 2 + bank];
  if (!std::filesystem::exists(bankname)) {
    // file doesn't exist...
    return false;
  }
  // avoid file check tbh. there shouldn't be any saves with a save count of zero anyway.
  // the file check is quite slow and ultimately not very useful.
  return true;

  /*
  // file exists. but let's see if it's an empty one.
  // this prevents the game from reading a bank but classifying it as corrupt data.
  // which a file full of zeros logically is.
  auto fp = fopen(bankname.c_str(), "rb");

  // we can actually just check if the save count is over zero...
  u32 savecount = 0;
  fread(&savecount, sizeof(u32), 1, fp);
  fclose(fp);
  return savecount > 0;
  */
}

/*!
 * PC port function to set memcard info. We don't use a memory card, instead just the raw savefiles.
 */
void pc_update_card() {
  // int highest_save_count = 0;
  mc_last_file = -1;
  for (s32 file = 0; file < 4; file++) {
    auto bankname = file_util::get_user_memcard_dir() / filename[4 + file * 2];
    mc_files[file].present = file_is_present(file);
    if (mc_files[file].present) {
      auto bankdata = file_util::read_binary_file(bankname.string());
      auto header1 = reinterpret_cast<McHeader*>(bankdata.data());
      if (file_is_present(file, 1)) {
        auto bankname2 = file_util::get_user_memcard_dir() / filename[1 + 4 + file * 2];
        auto bankdata2 = file_util::read_binary_file(bankname2.string());
        auto header2 = reinterpret_cast<McHeader*>(bankdata.data());

        if (header2->save_count > header1->save_count) {
          // use most recent bank here.
          header1 = header2;
        }

        // banks chosen and checked. copy data and set info.
        mc_files[file].last_saved_bank = header1 == header2;
        mc_files[file].most_recent_save_count = header1->save_count;

        memcpy(mc_files[file].data, header1->preview_data, 64);
      } else {
        // banks chosen and checked. copy data and set info.
        mc_files[file].last_saved_bank = 0;
        mc_files[file].most_recent_save_count = header1->save_count;

        memcpy(mc_files[file].data, header1->preview_data, 64);
      }

      // if (mc_files[file].most_recent_save_count > highest_save_count) {
      //  mc_last_file = file;
      //  highest_save_count = mc_files[file].most_recent_save_count;
      // }
    }
  }
}

/*!
 * PC port function to save a file. This does the whole saving at once, synchronously.
 */
void pc_game_save_synch() {
  Timer mc_timer;
  mc_timer.start();
  pc_update_card();
  auto path = file_util::get_user_memcard_dir() / filename[0];
  file_util::create_dir_if_needed_for_file(path.string());

  // cd_reprobe_save //
  if (!file_is_present(op.param2)) {
    mc_print("reprobe save: first time!");
    // first time saving!
    p2 = 0;  // save count 0
    p4 = 0;  // first bank for file
  } else {
    p2 = mc_files[op.param2].most_recent_save_count + 1;  // increment save count
    p4 = mc_files[op.param2].last_saved_bank ^ 1;         // use the other bank
  }

  // reserve 0 as "I never saved" and use 1 instead.
  if (p2 == 0) {
    p2 = 1;
  }

  // file*2 + p4 is the bank (2 banks per file, p4 is 0 or 1 to select the bank)
  // 4 is the first bank file
  mc_print("open {} for saving", filename[op.param2 * 2 + 4 + p4]);
  auto save_path = file_util::get_user_memcard_dir() / filename[op.param2 * 2 + 4 + p4];
  file_util::create_dir_if_needed_for_file(save_path.string());
  auto fd = fopen(save_path.string().c_str(), "wb");
  fmt::print("[MC] synchronous save file open took {:.2f}ms\n", mc_timer.getMs());
  if (fd) {
    // cb_openedsave //
    mc_print("save file opened, writing header...");
    memset(&header, 0, sizeof(McHeader));
    header.save_count = p2;
    header.checksum = mc_checksum(op.data_ptr, BANK_SIZE);
    header.magic = MEM_CARD_MAGIC;
    header.unk1_repeated = p2;
    memcpy(header.preview_data, op.data_ptr2.c(), 64);
    if (fwrite(&header, sizeof(McHeader), 1, fd) == 1) {
      // cb_savedheader //
      mc_print("save file writing main data");
      if (fwrite(op.data_ptr.c(), BANK_SIZE, 1, fd) == 1) {
        // cb_saveddata //
        mc_print("save file writing footer");
        if (fwrite(&header, sizeof(McHeader), 1, fd) == 1) {
          // cb_savedfooter //
          if (fclose(fd) == 0) {
            // cb_closedsave //
            mc_print("All done with saving!!");
            op.operation = MemoryCardOperationKind::NO_OP;
            op.result = McStatusCode::OK;
            mc_files[op.param2].present = 1;
            mc_files[op.param2].most_recent_save_count = p2;
            mc_files[op.param2].last_saved_bank = p4;
            memcpy(mc_files[op.param2].data, op.data_ptr2.c(), 64);
            mc_last_file = op.param2;
          } else {
            op.operation = MemoryCardOperationKind::NO_OP;
            op.result = McStatusCode::INTERNAL_ERROR;
          }
        } else {
          fclose(fd);
          op.operation = MemoryCardOperationKind::NO_OP;
          op.result = McStatusCode::INTERNAL_ERROR;
        }
      } else {
        fclose(fd);
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } else {
      fclose(fd);
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    }
  } else {
    fmt::print("[MC] Error opening file, errno - {}", errno);
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::INTERNAL_ERROR;
  }

  fmt::print("[MC] synchronous save took {:.2f}ms\n", mc_timer.getMs());
}

void pc_game_load_open_file(FILE* fd) {
  if (fd) {
    // cb_openedload //
    size_t read_size = BANK_TOTAL_SIZE;
    mc_print("reading save file...");
    if (fread(op.data_ptr.c() + p2 * read_size, read_size, 1, fd) == 1) {
      // cb_readload //
      mc_print("closing save file..");
      if (fclose(fd) == 0) {
        // cb_closedload //
        p2++;
        // added : check if aux bank exists
        auto new_bankname = file_util::get_user_memcard_dir() / filename[op.param2 * 2 + 4 + p2];
        bool aux_exists = std::filesystem::exists(new_bankname);
        if (p2 < 2 && aux_exists) {
          mc_print("reading next save bank {}", filename[op.param2 * 2 + 4 + p2]);
          auto new_fd = fopen(new_bankname.string().c_str(), "rb");
          pc_game_load_open_file(new_fd);
        } else {
          // let's verify the data.
          McHeader* headers[2];
          McHeader* footers[2];
          bool ok[2];

          headers[0] = (McHeader*)(op.data_ptr.c());
          footers[0] = (McHeader*)(op.data_ptr.c() + sizeof(McHeader) + BANK_SIZE);
          headers[1] = (McHeader*)(op.data_ptr.c() + BANK_TOTAL_SIZE);
          footers[1] =
              (McHeader*)(op.data_ptr.c() + BANK_TOTAL_SIZE + sizeof(McHeader) + BANK_SIZE);
          static_assert(BANK_TOTAL_SIZE * 2 == 0x21000, "save layout");
          ok[0] = true;
          ok[1] = aux_exists;

          for (int idx = 0; idx < 2; idx++) {
            u32 expected_save_count = headers[idx]->save_count;
            if (headers[idx]->unk1_repeated == expected_save_count &&
                footers[idx]->save_count == expected_save_count &&
                footers[idx]->unk1_repeated == expected_save_count) {
              // save count is okay!
              if (headers[idx]->magic == MEM_CARD_MAGIC && footers[idx]->magic == MEM_CARD_MAGIC) {
                // magic numbers okay!
                if (headers[idx]->checksum == footers[idx]->checksum) {
                  // checksum
                  auto expected_checksum = headers[idx]->checksum;
                  if (mc_checksum(make_u8_ptr(headers[idx] + 1), BANK_SIZE) != expected_checksum) {
                    mc_print("failed checksum");
                    ok[idx] = false;
                  }
                } else {
                  mc_print("corrupted checksum");
                  ok[idx] = false;
                }
              } else {
                mc_print("bad magic");
                ok[idx] = false;
              }
            } else {
              mc_print("bad save count");
              ok[idx] = false;
            }
          }

          mc_print("checking loaded banks");

          //
          if (!ok[0] && !ok[1]) {
            // no good data.
            if (headers[0]->save_count == 0 && headers[0]->checksum == 0 &&
                headers[0]->magic == 0 && headers[0]->unk1_repeated == 0 &&
                headers[1]->save_count == 0 && headers[1]->checksum == 0 &&
                headers[1]->magic == 0 && headers[1]->unk1_repeated == 0) {
              // this is a fresh file that you tried to load from...
              mc_print("new game result");
              op.operation = MemoryCardOperationKind::NO_OP;
              op.result = McStatusCode::NEW_GAME;
              mc_last_file = op.param2;
            } else {
              mc_print("corrupted data");
              op.operation = MemoryCardOperationKind::NO_OP;
              op.result = McStatusCode::READ_ERROR;
            }
          } else {
            // pick the bank
            int bank = 0;

            if (!ok[0] || !ok[1]) {
              if (ok[1]) {
                bank = 1;
              }
            } else {
              bank = headers[0]->save_count <= headers[1]->save_count;
            }

            mc_print(fmt::format("loading bank {}", bank));
            u32 current_save_count = headers[bank]->save_count;
            memcpy(op.data_ptr.c(), op.data_ptr.c() + bank * BANK_TOTAL_SIZE + sizeof(McHeader),
                   BANK_SIZE);
            mc_last_file = op.param2;
            mc_files[op.param2].most_recent_save_count = current_save_count;
            mc_files[op.param2].last_saved_bank = bank;
            op.operation = MemoryCardOperationKind::NO_OP;
            op.result = McStatusCode::OK;
            mc_print("load succeeded");
          }
        }
      } else {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } else {
      fclose(fd);
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    }
  } else {
    op.operation = MemoryCardOperationKind::NO_OP;
    op.result = McStatusCode::INTERNAL_ERROR;
  }
}

/*!
 * PC port function to load a file. This does the whole loading at once, synchronously.
 */
void pc_game_load_synch() {
  Timer mc_timer;
  mc_timer.start();
  pc_update_card();

  // cb_reprobe_load //
  p2 = 0;
  mc_print("opening save file {}", filename[op.param2 * 2 + 4]);

  auto path = file_util::get_user_memcard_dir() / filename[op.param2 * 2 + 4];
  auto fd = fopen(path.string().c_str(), "rb");
  pc_game_load_open_file(fd);

  fmt::print("[MC] synchronous load took {:.2f}ms\n", mc_timer.getMs());
}

/*!
 * Run the Memory Card state machine.  This is called once per frame in GOAL.
 * It:
 *  - does nothing if there is an in-progress memory card operation
 *  - if async memory card functions are done, runs their callbacks
 *  - if there is a requested operation, starts running sony functions.
 *  - if there is none of the above, and unknown cards, finds out about them.
 *  - every now and then, recheck cards.
 */
void MC_run() {
  // if we have an in-progress operation, it will have set a callback.
  if (callback) {
    s32 sony_cmd, sony_status;
    // check the status
    s32 status = sceMcSync(1, &sony_cmd, &sony_status);
    McCallbackFunc callback_for_sync = callback;
    if (status == sceMcExecRun) {
      // busy, return.
      return;
    }

    if (status == sceMcExecFinish) {
      // sony function is done. do the callback.
      callback = nullptr;
      (*callback_for_sync)(sony_status);
    } else {
      // sony function is done, but failed.
      callback = nullptr;
      (*callback_for_sync)(0);
    }

    if (callback) {
      // if we got another callback, it means there's another op started by the prev callback.
      // and this case, we want to wait for that operation to finish.
      return;
    }
  }

  // if we got here, there is no in-progress sony function. So start the next one, if we should
  if (op.operation == MemoryCardOperationKind::FORMAT) {
    // format memory card. Not used in PC port, so lets move on.
    return;
    /*
    mc_print("begin format operation");
    // grab the slot. should be open, but not formatted
    p1 = handle_to_slot(op.param, MemoryCardState::OPEN_BUT_UNFORMATTED);
    if (p1 == -1) {
      // no slot in the right state.
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    } else {
      // do a getInfo
      s32 info_result = sceMcGetInfo(p1, 0, &p2, &p3, &p4);
      if (info_result == sceMcResSucceed) {
        callback = cb_reprobe_format;
      }
      // allow some number of errors.
      op.retry_count--;
      if (op.retry_count == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } */
  } else if (op.operation == MemoryCardOperationKind::UNFORMAT) {
    // unformat memory card.
    return;
    /*
    mc_print("begin unformat operation");
    p1 = handle_to_slot(op.param, MemoryCardState::FORMATTED);
    if (p1 == -1) {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    } else {
      s32 rv = sceMcUnformat(p1, 0);
      if (rv == sceMcResSucceed) {
        callback = cb_unformat;
      }
      op.retry_count--;
      if (op.retry_count == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
    */
  } else if (op.operation == MemoryCardOperationKind::CREATE_FILE) {
    // create the game file.
    // there's no cards, keep in mind.
    return;
    /*
    mc_print("begin create file operation");
    p1 = handle_to_slot(op.param, MemoryCardState::FORMATTED);
    if (p1 == -1) {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::BAD_HANDLE;
    } else {
      s32 info_result = sceMcGetInfo(p1, 0, &p2, &p3, &p4);
      if (info_result == sceMcResSucceed) {
        callback = cb_reprobe_createfile;
      }
      // allow some number of errors.
      op.retry_count--;
      if (op.retry_count == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    } */
  } else if (op.operation == MemoryCardOperationKind::SAVE) {
    // write game save.
    // there's no cards, keep in mind.
    pc_game_save_synch();
    // allow some number of errors.
    op.retry_count--;
    if (op.retry_count == 0) {
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::INTERNAL_ERROR;
    }
    // p1 = handle_to_slot(op.param, MemoryCardState::FORMATTED);
    // if (p1 == -1) {
    //  op.operation = MemoryCardOperationKind::NO_OP;
    //  op.result = McStatusCode::BAD_HANDLE;
    //} else {
    //  s32 info_result = sceMcGetInfo(p1, 0, &p2, &p3, &p4);
    //  if (info_result == sceMcResSucceed) {
    //    callback = cb_reprobe_save;
    //  }
    //  // allow some number of errors.
    //  op.retry_count--;
    //  if (op.retry_count == 0) {
    //    op.operation = MemoryCardOperationKind::NO_OP;
    //    op.result = McStatusCode::INTERNAL_ERROR;
    //  }
    //}
  } else if (op.operation == MemoryCardOperationKind::LOAD) {
    // load game save.
    // potato.
    if (!file_is_present(op.param2)) {
      // tried to load, but there's no save data in the file.
      op.operation = MemoryCardOperationKind::NO_OP;
      op.result = McStatusCode::NO_MEMORY;
    } else {
      pc_game_load_synch();
      op.retry_count--;
      if (op.retry_count == 0) {
        op.operation = MemoryCardOperationKind::NO_OP;
        op.result = McStatusCode::INTERNAL_ERROR;
      }
    }
    // mc_print("begin load operation");
    // p1 = handle_to_slot(op.param, MemoryCardState::FORMATTED);
    // if (p1 == -1) {
    //  op.operation = MemoryCardOperationKind::NO_OP;
    //  op.result = McStatusCode::BAD_HANDLE;
    //} else {
    //  if (!mc[p1].files[op.param2].present) {
    //    // tried to load, but there's no save data in the file.
    //    op.operation = MemoryCardOperationKind::NO_OP;
    //    op.result = McStatusCode::NO_MEMORY;
    //  } else {
    //    s32 info_result = sceMcGetInfo(p1, 0, &p2, &p3, &p4);
    //    if (info_result == sceMcResSucceed) {
    //      callback = cb_reprobe_load;
    //    }
    //    op.retry_count--;
    //    if (op.retry_count == 0) {
    //      op.operation = MemoryCardOperationKind::NO_OP;
    //      op.result = McStatusCode::INTERNAL_ERROR;
    //    }
    //  }
    //}
    // below here is just doing maintenance to look for new/removed memory cards.
    // it's cut from the port. we do not use memory cards. just raw saves.
  } /* else if (mc[0].state == MemoryCardState::UNKNOWN) {
    mc_print("begin probe operation for slot 0");
    // don't know anything about port 0, try and see
    p1 = 0;
    if (sceMcGetInfo(0, 0, &p2, &p3, &p4) == sceMcResSucceed) {
      callback = cb_probe;
    }
  } else if (mc[1].state == MemoryCardState::UNKNOWN) {
    mc_print("begin probe operation for slot 1");
    // don't know anything about port 1, try and see
    p1 = 1;
    if (sceMcGetInfo(1, 0, &p2, &p3, &p4) == sceMcResSucceed) {
      callback = cb_probe;
    }
  } else if (mc[0].state == MemoryCardState::KNOWN) {
    if (mc[0].countdown_to_check == 1) {
      // we're about to recheck the memory card. If we're only inknown, reset to unknown so we do a
      // completely fresh probe instead of a reprobe.
      mc[0].state = MemoryCardState::UNKNOWN;
    }
  } else if (mc[0].countdown_to_check == 1) {
    // it's been a while, do a check and see if the memory card is still there.
    p1 = 0;
    mc[0].countdown_to_check--;
    mc_print("begin reprobe operation for slot 0");
    if (sceMcGetInfo(0, 0, &p2, &p3, &p4) == sceMcResSucceed) {
      callback = cb_reprobe;
    }
    return;
  } else {
    // decrement port 0's countdown
    mc[0].countdown_to_check--;

    // do the same thing for port 1
    if (mc[1].state == MemoryCardState::KNOWN) {
      mc[1].countdown_to_check--;
      if (mc[1].countdown_to_check == 0) {
        // hack - will make us do a probe next time we get here.
        mc[1].state = MemoryCardState::UNKNOWN;
      }
    } else {
      mc[1].countdown_to_check--;
      if (mc[1].countdown_to_check == 0) {
        p1 = 1;
        mc_print("begin probe operation for slot 0");
        if (sceMcGetInfo(1, 0, &p2, &p3, &p4) == sceMcResSucceed) {
          callback = cb_reprobe;
        }
      }
    }
  }*/
}

/////////////////////////
// Memory Card Functions
/////////////////////////

// These functions are called from GOAL to start memory card operations.

/*!
 * Set the language or something.
 * Why is this a memory card func?
 */
void MC_set_language(s32 l) {
  printf("Language set to %d\n", l);
  language = l;
}

/*!
 * Set the current memory card operation to FORMAT the given card.
 * Doesn't do anything in the port because we don't use memory cards.
 */
u64 MC_format(s32 /*card_idx*/) {
  return u64(McStatusCode::OK);
  // u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  // mc_print("requested format");
  // if (can_add) {
  //  mc_print("setting op to format");
  //  op.operation = MemoryCardOperationKind::FORMAT;
  //  op.result = McStatusCode::BUSY;
  //  op.retry_count = 100;
  //  op.param = card_idx;
  //}
  // return can_add;
}

/*!
 * Set the current memory card operation to UNFORMAT the given card.
 * You get the idea.
 */
u64 MC_unformat(s32 /*card_idx*/) {
  return u64(McStatusCode::OK);
  // u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  // mc_print("requested unformat");
  // if (can_add) {
  //  mc_print("setting op to unformat");
  //  op.operation = MemoryCardOperationKind::UNFORMAT;
  //  op.result = McStatusCode::BUSY;
  //  op.retry_count = 100;
  //  op.param = card_idx;
  //}
  // return can_add;
}

/*!
 * Set the current memory card operation to create the save file.
 * The data I believe is just an empty buffer used as temporary storage.
 */
u64 MC_createfile(s32 /*param*/, Ptr<u8> /*data*/) {
  return u64(McStatusCode::OK);
  // u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  // mc_print("requested createfile");
  // if (can_add) {
  //  mc_print("setting op to create file");
  //  op.operation = MemoryCardOperationKind::CREATE_FILE;
  //  op.result = McStatusCode::BUSY;
  //  op.retry_count = 100;
  //  op.param = param;
  //  op.data_ptr = data;
  //}
  // return can_add;
}

/*!
 * Set the current operation to SAVE.
 * The "summary data" is data that will be used when previewing save files (number of orbs etc)
 * TODO put synchronous call here
 */
u64 MC_save(s32 card_idx, s32 file_idx, Ptr<u8> save_data, Ptr<u8> save_summary_data) {
  mc_print("requested save");
  u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  if (can_add) {
    mc_print("setting op to save");
    op.operation = MemoryCardOperationKind::SAVE;
    op.result = McStatusCode::BUSY;
    op.retry_count = 100;
    op.param = card_idx;
    op.param2 = file_idx;
    op.data_ptr = save_data;
    op.data_ptr2 = save_summary_data;
  }
  return can_add;
}

/*!
 * Set the current operation to LOAD.
 * TODO put synchronous call here
 */
u64 MC_load(s32 card_idx, s32 file_idx, Ptr<u8> data) {
  mc_print("requested load");
  u64 can_add = op.operation == MemoryCardOperationKind::NO_OP;
  if (can_add) {
    mc_print("setting op to load");
    op.operation = MemoryCardOperationKind::LOAD;
    op.result = McStatusCode::BUSY;
    op.retry_count = 100;
    op.param = card_idx;
    op.param2 = file_idx;
    op.data_ptr = data;
  }
  return can_add;
}

/*!
 * Some sort of test function for memory card stuff.
 * This is exported as a GOAL function, but nothing calls it.
 */
void MC_makefile(s32 port, s32 size) {
  sceMcMkdir(port, 0, "/BASCUS-00000XXXXXXXX");
  // wait for operation to complete
  s32 cmd, result, fd;
  sceMcSync(0, &cmd, &result);

  if (result == sceMcResSucceed || result == sceMcResNoEntry) {
    // it worked, or the folder already exists...

    // open file
    sceMcOpen(port, 0, "/BASCUS-00000XXXXXXXX/BASCUS-00000XXXXXXXX", SCE_CREAT | SCE_WRONLY);
    sceMcSync(0, &cmd, &fd);

    if (result < 0) {
      printf("Can't open file on memcard [%d]\n", result);
    } else {
      // write some random crap into the memory card.
      sceMcWrite(fd, Ptr<u8>(0x1000000).c(), size);
      sceMcSync(0, &cmd, &result);
      if (result != size) {
        printf("Only written %d bytes\n", result);
      }
      sceMcClose(fd);
      sceMcSync(0, &cmd, &result);
    }
  } else {
    printf("Can\'t create garbage folder [%d]\n", result);
  }
}

/*!
 * Get the result of the currently executing (or most recently executed) command
 */
u32 MC_check_result() {
  return (u32)op.result;
}

/*!
 * Update the info for the given slot.
 * You can call this at any time.
 * The slot includes the four save slots (8 banks), and a few other files.
 */
void MC_get_status(s32 slot, Ptr<mc_slot_info> info) {
  ASSERT(slot == 0);  // no memory cards so only allow "slot 0"!!!

  info->handle = 0;
  info->known = 0;
  info->formatted = 0;
  info->initted = 0;
  for (s32 i = 0; i < 4; i++) {
    info->files[i].present = 0;
  }
  info->last_file = 0xffffffff;
  info->mem_required = SAVE_SIZE;
  info->mem_actual = 0;

  pc_update_card();
  info->known = 1;
  info->handle = PC_MEM_CARD_HANDLE;
  info->formatted = 1;
  info->mem_actual = SAVE_SIZE;  // idk TODO does this matter?
  info->initted = 1;
  // copy over the preview data.
  for (s32 file = 0; file < 4; file++) {
    info->files[file].present = mc_files[file].present;
    for (s32 i = 0; i < 64; i++) {  // actually a loop over u32's
      info->files[file].data[i] = mc_files[file].data[i];
    }
  }
  info->last_file = mc_last_file;

  /*
  switch (mc[slot].state) {
    case MemoryCardState::KNOWN:
      info->known = 1;
      break;
    case MemoryCardState::OPEN_BUT_UNFORMATTED:
      info->known = 1;
      info->handle = mc[slot].handle;
      break;
    case MemoryCardState::FORMATTED:
      info->known = 1;
      info->handle = mc[slot].handle;
      info->formatted = 1;
      if (mc[slot].inited == 0) {
        info->mem_actual = mc[slot].mem_size;
      } else {
        info->initted = 1;
        // copy over the preview data.
        for (s32 file = 0; file < 4; file++) {
          info->files[file].present = mc[slot].files[file].present;
          for (s32 i = 0; i < 64; i++) {  // actually a loop over u32's
            info->files[file].data[i] = mc[slot].files[file].data[i];
          }
        }
        info->last_file = mc[slot].last_file;
      }
      break;
    case MemoryCardState::UNKNOWN:
      break;
  }*/
}

/*!
 * Check for an error. Returns true if there is an error and sets op.result as needed
 */
// u64 cb_check(s32 sony_error, McStatusCode goal_error) {
//  if (sony_error < 0) {
//    // sony thing failed.
//    if (sony_error < -9) {
//      // memory card gone. reset state
//      mc[p1].state = MemoryCardState::UNKNOWN;
//      // kill in progress op
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::BAD_HANDLE;
//      return 1;
//    } else {
//      // return the given GOAL error.
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = goal_error;
//      return 1;
//    }
//  }
//  return 0;
//}

/*!
 * Is this sync-result an error? If so, set status to unknown.
 */
// bool cb_pcheck(s32 sync_result) {
//   if (sync_result < 0) {
//    mc[p1].state = MemoryCardState::UNKNOWN;
//  }
//  return sync_result < 0;
//}

/*!
 * Callback for sceMcGetInfo for the first time (assumes nothing about the card)
 * Cut. We don't use memory cards.
 */
// void cb_probe(s32 sync_result) {
//  if (sync_result < -9) {
//    // changed card. We have the card, but we don't know anything about it.
//    mc[p1].state = MemoryCardState::KNOWN;
//    mc[p1].countdown_to_check = 100;
//    mc_print("probe: bad sync, trying again in a bit");
//  } else {
//    // there is a memory card.
//    if (p2 == sceMcTypePS2) {
//      // it is the right type.
//      // create a new handle
//      mc[p1].handle = new_mc_handle();
//      if (p4 == 0) {
//        mc_print("probe: got a card, but it's not formatted");
//        // it's not formatted. But open (we have a handle)
//        mc[p1].state = MemoryCardState::OPEN_BUT_UNFORMATTED;
//        mc[p1].countdown_to_check = 100;
//      } else {
//        mc_print("probe: got a formatted card, trying getdir");
//        // it's formatted. Get the size
//        mc[p1].mem_size = p3;
//        p2 = 0;
//
//        // and get our jak and daxter directory
//        if (sceMcGetDir(p1, 0, filename[0], 0, 1, &dirent) == sceMcResSucceed) {
//          callback = cb_getdir;
//        } else {
//          mc[p1].state = MemoryCardState::UNKNOWN;
//        }
//      }
//
//    } else {
//      mc_print("probe: bad card type, trying again in a bit");
//      mc[p1].state = MemoryCardState::KNOWN;
//      mc[p1].countdown_to_check = 100;
//    }
//  }
//}

/*!
 * Callback for sceMcGetDir.  This runs again and again to check all files
 */
// void cb_getdir(s32 sync_result) {
//  // called after sceMcGetDir
//  if (sync_result == 0) {
//    // didn't find the jak and daxter dir, or some stuff is missing.
//    // call this uninitialized.
//    mc[p1].inited = false;
//    mc[p1].state = MemoryCardState::FORMATTED;
//    mc[p1].countdown_to_check = 100;
//    mc[p1].last_file = -1;
//  } else if (sync_result == 1) {
//    // found whatever we were looking for
//    p2++;
//    if (p2 == 12) {
//      // all done checking for files.
//      mc[p1].inited = true;
//      mc[p1].countdown_to_check = 100;
//      mc[p1].last_file = -1;
//      for (int i = 0; i < 4; i++) {
//        mc[p1].files[i].present = 0;
//      }
//
//      // now open file 4, which is the first bank.
//      p2 = 4;
//      mc_print("opening first bank...");
//      if (sceMcOpen(p1, 0, filename[4], 1) == sceMcResSucceed) {
//        callback = cb_check_open;
//      } else {
//        mc[p1].state = MemoryCardState::UNKNOWN;
//      }
//    } else {
//      // still checking files, check the next one.
//      mc_print("checking {}", filename[p2]);
//      if (sceMcGetDir(p1, 0, filename[p2], 0, 1, &dirent) == sceMcResSucceed) {
//        callback = cb_getdir;
//      } else {
//        mc[p1].state = MemoryCardState::UNKNOWN;
//      }
//    }
//  } else {
//    mc[p1].state = MemoryCardState::UNKNOWN;
//  }
//}

/*!
 * Callback for sceMcOpen for opening bank files.
 */
// void cb_check_open(s32 sync_result) {
//  if (!cb_pcheck(sync_result)) {
//    p3 = sync_result;
//    // read the header.
//    mc_print("read header");
//    if (sceMcRead(sync_result, &header, sizeof(McHeader)) == sceMcResSucceed) {
//      callback = cb_check_read;
//    } else {
//      mc[p1].state = MemoryCardState::UNKNOWN;
//    }
//  }
//}

/*!
 * Callback for sceMcRead for reading the header of a bank file.
 */
// void cb_check_read(s32 sync_result) {
//  if (!cb_pcheck(sync_result)) {
//    if (header.save_count != 0 &&       // we've saved into this bank
//        header.magic == MEM_CARD_MAGIC  // looks valid
//    ) {
//      // each file has two banks. so if you corrupt a save, you hopefully have the other bank.
//      // get the file that goes with this bank
//      s32 file_idx = (p2 - 4) / 2;
//      if (mc_files[file_idx].present == 0 ||  // haven't found any banks for this file
//          header.save_count > mc_files[file_idx].most_recent_save_count  // newer than prev.
//      ) {
//        // this is so far our best guess at the right bank to use.
//        mc_files[file_idx].present = 1;
//        mc_files[file_idx].most_recent_save_count = header.save_count;
//        mc_files[file_idx].last_saved_bank = p2 & 1;
//        // copy header data (for GOAL to read)
//        for (int i = 0; i < 64; i++) {
//          mc_files[file_idx].data[i] = header.preview_data[i];
//        }
//      }
//    }
//
//    if (sceMcClose(p3) == sceMcResSucceed) {
//      // close the file
//      callback = cb_check_close;
//    } else {
//      mc[p1].state = MemoryCardState::UNKNOWN;
//    }
//  }
//}

/*!
 * Callback for closing a file after reading the header
 */
// void cb_check_close(s32 sync_result) {
//  if (!cb_pcheck(sync_result)) {
//    // next bank.
//    p2++;
//    if (p2 == 12) {
//      // done with banks
//      mc[p1].state = MemoryCardState::FORMATTED;
//      // and we're done!
//    } else {
//      // on to the next bank...
//      if (sceMcOpen(p1, 0, filename[p2], 1) == sceMcResSucceed) {
//        callback = cb_check_open;
//      } else {
//        mc[p1].state = MemoryCardState::UNKNOWN;
//      }
//    }
//  }
//}

/*!
 * Callback for checking on a known memory card. If it is swapped, will reset.
 */
// void cb_reprobe(s32 sync_result) {
//  if (sync_result == sceMcResSucceed) {
//    // nobody took out the memory card. try again in 100 frames
//    mc[p1].countdown_to_check = 100;
//  } else {
//    // somebody took it out. reset everything!
//    mc[p1].state = MemoryCardState::UNKNOWN;
//  }
//}

/*!
 * Callback for probe before formatting.
 */
// void cb_reprobe_format(s32 sync_result) {
//  if (sync_result == sceMcResSucceed) {
//    // get info succeeded. we can format.
//    s32 format_result = sceMcFormat(p1, 0);
//    if (format_result == sceMcResSucceed) {
//      callback = cb_format_complete;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  } else {
//    // get info failed.  Revert the state to unknown, this will restart everything.
//    mc[p1].state = MemoryCardState::UNKNOWN;
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::BAD_HANDLE;
//  }
//}

/*!
 * Callback for actual formatting operation
 */
// void cb_format_complete(s32 sync_result) {
//  if (sync_result == sceMcResSucceed) {
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::OK;
//    mc[p1].state = MemoryCardState::FORMATTED;
//    mc[p1].countdown_to_check = 100;
//    mc[p1].inited = 0;
//    for (int i = 0; i < 4; i++) {
//      mc[p1].files[i].present = 0;
//    }
//    mc[p1].last_file = -1;
//    mc[p1].mem_size = 8000;
//  } else {
//    mc[p1].state = MemoryCardState::UNKNOWN;
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::FORMAT_FAILED;
//  }
//}

/*!
 * Callback for actual unformatting operation
 */
// void cb_unformat(s32 sync_result) {
//  if (sync_result == sceMcResSucceed) {
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::OK;
//    mc[p1].state = MemoryCardState::UNKNOWN;
//  } else {
//    mc[p1].state = MemoryCardState::UNKNOWN;
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::FORMAT_FAILED;
//  }
//}

/*!
 * Callback for probe before creating save files.
 */
// void cb_reprobe_createfile(s32 sync_result) {
//  if (sync_result == sceMcResSucceed) {
//    // if the ramdisk is ready, just jump directly to its callback
//    if (!RpcBusy(RAMDISK_RPC_CHANNEL)) {
//      cb_wait_for_ramdisk(0);
//    } else {
//      // I think this is a typo and should be cb_reprobe_createfile again.
//      callback = cb_wait_for_ramdisk;
//    }
//  } else {
//    mc[p1].state = MemoryCardState::UNKNOWN;
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::BAD_HANDLE;
//  }
//}

/*!
 * Actually start the ramdisk load of the icon file to temp buffer
 */
// void cb_wait_for_ramdisk(s32) {
//  ramdisk_cmd.pad = 0;
//  // caller should have given us a temporary buffer.
//  ramdisk_cmd.file_id_or_ee_addr = op.data_ptr.offset;
//  ramdisk_cmd.offset_into_file = 0;
//  ramdisk_cmd.size = 0x1e800;
//  memcpy(ramdisk_cmd.name, "SAVEGAME.ICO", 13);  // was 16.
//  RpcCall(RAMDISK_RPC_CHANNEL, RAMDISK_BYPASS_LOAD_FILE, 1, &ramdisk_cmd, 0x20, nullptr, 0);
//  callback = cb_wait_for_ramdisk_load;
//}

/*!
 * Callback for checking if the ramdisk load is done
 */
// void cb_wait_for_ramdisk_load(s32) {
//  if (RpcBusy(RAMDISK_RPC_CHANNEL) == 0) {
//    // ramdisk is done. start deleting 12 files
//    p2 = 11;  // filenames left to delete
//    mc_print("start delete {}", filename[11]);
//    if (sceMcDelete(p1, 0, filename[11]) == sceMcResSucceed) {
//      callback = cb_createfile_erasing;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  } else {
//    // still waiting...
//    callback = cb_wait_for_ramdisk_load;
//  }
//}

/*!
 * Callback to start erasing previous save files.
 */
// void cb_createfile_erasing(s32 sync_result) {
//  if (sync_result == sceMcResSucceed || sync_result == sceMcResNoEntry ||
//      sync_result == sceMcResNotEmpty) {
//    mc[p1].inited = 0;
//    // delete didn't fail.
//    if (p2 < 1) {
//      // on the last one. move on on to creating the directory again.
//      mc_print("create dir {}", filename[0]);
//      if (sceMcMkdir(p1, 0, filename[0]) == sceMcResSucceed) {
//        callback = cb_createdir;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    } else {
//      p2--;
//      mc_print("delete next {}", filename[p2]);
//      if (sceMcDelete(p1, 0, filename[p2]) == sceMcResSucceed) {
//        callback = cb_createfile_erasing;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    }
//  } else {
//    if (sync_result == sceMcResDeniedPermit) {
//      mc_print("erasing: denied");
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    } else {
//      mc_print("erasing: other bad");
//      mc[p1].state = MemoryCardState::UNKNOWN;
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::BAD_HANDLE;
//    }
//  }
//}

/*!
 * Callback after creating directory for jak save data
 */
// void cb_createdir(s32 sync_result) {
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    // this sets up some stuff for the icon file that we will ignore.
//    // memset(&iconsys,0,0x3c4);
//    // kstrcpy(&iconsys,&DAT_00137000);
//    // if (language == Language::Japanese) {
//    //  kstrcpy(&DAT_00137560,titles[5]);
//    // } else {
//    //  // non japanese need to convert to shift-JIS format.
//    //  ASCII2SJIS(&DAT_00137560,titles[language]);
//    // }
//    // DAT_001374a6 = 0x20;
//    // DAT_001374ac = 0;
//    // memcpy(&DAT_001374b0,bgcolor.610,0x40);
//    // memcpy(&DAT_001374f0,lightdir.611,0x30);
//    // memcpy(&DAT_00137520,lightcol.612,0x30);
//    // memcpy(&DAT_00137550,ambient.613,0x10);
//    // kstrcpy(&DAT_001375a4,"icon.ico");
//    // kstrcpy(&DAT_001375e4,"icon.ico");
//    // kstrcpy(&DAT_00137624,"icon.ico");
//
//    p2 = 1;
//    // move on to creating files
//    mc_print("starting create file at {}", filename[p2]);
//    if (sceMcOpen(p1, 0, filename[1], 0x203) == 0) {
//      callback = cb_createdfile;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}

/*!
 * Callback after creating the file.
 */
// void cb_createdfile(s32 sync_result) {
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    mc_print("create file cb {}", filename[p2]);
//    if (p2 == 1) {
//      p3 = sync_result;  // the fd of the icon file.
//      // actually would write the icon sys file.
//      if (sceMcWrite(sync_result, nullptr, 0) == sceMcResSucceed) {
//        callback = cb_writtenfile;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    } else if (p2 == 2) {
//      p3 = sync_result;
//      // would write the icon data (ramdisk loaded into the temp buffer)
//      if (sceMcWrite(sync_result, nullptr, 0) == sceMcResSucceed) {
//        callback = cb_writtenfile;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    } else if (p2 == 3) {
//      // writes the aybabtu meme file
//      p3 = sync_result;
//      kstrcpy(op.data_ptr.cast<char>().c(), "Nope, the save game data isn\'t in this file!\n");
//      if (sceMcWrite(p3, op.data_ptr.c(), strlen((const char*)op.data_ptr.c())) ==
//          sceMcResSucceed) {
//        callback = cb_writtenfile;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    } else {
//      // writes the actual bank files.
//      p3 = sync_result;
//      memset(op.data_ptr.c(), 0, 0x11800);
//      if (sceMcWrite(p3, op.data_ptr.c(), 0x11800) == sceMcResSucceed) {
//        callback = cb_writtenfile;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    }
//  }
//}

/*!
 * Callback after writing data to files.
 */
// void cb_writtenfile(s32 sync_result) {
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    if (sceMcClose(p3) == sceMcResSucceed) {
//      callback = cb_closedfile;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}

/*!
 * Callback after closing the file.
 */
// void cb_closedfile(s32 sync_result) {
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    p2++;
//    if (p2 < 12) {
//      // open the next one
//      if (sceMcOpen(p1, 0, filename[p2], 0x203) == sceMcResSucceed) {
//        callback = cb_createdfile;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    } else {
//      // done!
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::OK;
//
//      // save data is inited
//      mc[p1].inited = 1;
//      // but no files have anything in it.
//      for (int i = 0; i < 4; i++) {
//        mc[p1].files[i].present = 0;
//      }
//      mc[p1].last_file = -1;
//    }
//  }
//}

/*!
 * Callback after check before saving.
 */
// void cb_reprobe_save(s32 sync_result) {
//  if (sync_result == sceMcResSucceed) {
//    if (!mc[p1].files[op.param2].present) {
//      mc_print("reprobe save: first time!");
//      // first time saving!
//      p2 = 0;  // save count 0
//      p4 = 0;  // first bank for file
//    } else {
//      p2 = mc_files[op.param2].most_recent_save_count + 1;  // increment save count
//      p4 = mc_files[op.param2].last_saved_bank ^ 1;         // use the other bank
//    }
//
//    // reserve 0 as "I never saved" and use 1 instead.
//    if (p2 == 0) {
//      p2 = 1;
//    }
//
//    // file*2 + p4 is the bank (2 banks per file, p4 is 0 or 1 to select the bank)
//    // 4 is the first bank file
//    mc_print("open {} for saving", filename[op.param2 * 2 + 4 + p4]);
//    if (sceMcOpen(p1, 0, filename[op.param2 * 2 + 4 + p4], 2) == sceMcResSucceed) {
//      callback = cb_openedsave;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  } else {
//    // mc[p1].state = MemoryCardState::UNKNOWN;
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::BAD_HANDLE;
//  }
//}
//
// void cb_openedsave(s32 sync_result) {
//  mc_print("save file opened, writing header...");
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    p3 = sync_result;
//    memset(&header, 0, sizeof(McHeader));
//    header.save_count = p2;
//    header.checksum = mc_checksum(op.data_ptr, BANK_SIZE);
//    header.magic = MEM_CARD_MAGIC;
//    header.unk1_repeated = p2;
//    for (int i = 0; i < 64; i++) {
//      header.preview_data[i] = op.data_ptr2.c()[i];
//    }
//    if (sceMcWrite(p3, &header, sizeof(McHeader)) == sceMcResSucceed) {
//      callback = cb_savedheader;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}
//
// void cb_savedheader(s32 sync_result) {
//  mc_print("save file writing main data");
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    if (sceMcWrite(p3, op.data_ptr.c(), BANK_SIZE) == sceMcResSucceed) {
//      callback = cb_saveddata;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}
//
// void cb_saveddata(s32 sync_result) {
//  mc_print("save file writing footer");
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    if (sceMcWrite(p3, &header, sizeof(McHeader)) == sceMcResSucceed) {
//      callback = cb_savedfooter;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}
//
// void cb_savedfooter(s32 sync_result) {
//  mc_print("closing after save");
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    if (sceMcClose(p3) == sceMcResSucceed) {
//      callback = cb_closedsave;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}
//
// void cb_closedsave(s32 sync_result) {
//  if (!cb_check(sync_result, McStatusCode::WRITE_ERROR)) {
//    mc_print("All done with saving!!");
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::OK;
//    // mc_files[op.param2].present = 1;
//    mc_files[op.param2].most_recent_save_count = p2;
//    mc_files[op.param2].last_saved_bank = p4;
//    for (int i = 0; i < 64; i++) {
//      mc_files[op.param2].data[i] = op.data_ptr2.c()[i];
//    }
//    mc_last_file = op.param2;
//    flush_memory_card_to_file();
//  }
//}
//
// void cb_reprobe_load(s32 sync_result) {
//  if (sync_result == 0) {
//    p2 = 0;
//    mc_print("opening save file {}", filename[op.param2 * 2 + 4]);
//    if (sceMcOpen(p1, 0, filename[op.param2 * 2 + 4], 1) == sceMcResSucceed) {
//      callback = cb_openedload;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  } else {
//    // mc[p1].state = MemoryCardState::UNKNOWN;
//    op.operation = MemoryCardOperationKind::NO_OP;
//    op.result = McStatusCode::BAD_HANDLE;
//  }
//}
//
// void cb_openedload(s32 sync_result) {
//  if (cb_check(sync_result, McStatusCode::READ_ERROR) == 0) {
//    p3 = sync_result;
//    size_t read_size = BANK_TOTAL_SIZE;
//    mc_print("reading save file...");
//    if (sceMcRead(sync_result, op.data_ptr.c() + p2 * read_size, read_size) == sceMcResSucceed) {
//      callback = cb_readload;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}
//
// void cb_readload(s32 sync_result) {
//  if (cb_check(sync_result, McStatusCode::READ_ERROR) == 0) {
//    mc_print("closing save file..");
//    if (sceMcClose(p3) == sceMcResSucceed) {
//      callback = cb_closedload;
//    } else {
//      op.operation = MemoryCardOperationKind::NO_OP;
//      op.result = McStatusCode::INTERNAL_ERROR;
//    }
//  }
//}
//
// void cb_closedload(s32 sync_result) {
//  if (cb_check(sync_result, McStatusCode::READ_ERROR) == 0) {
//    p2++;
//    if (p2 < 2) {
//      mc_print("reading next save bank {}", filename[op.param2 * 2 + 4 + p2]);
//      if (sceMcOpen(p1, 0, filename[op.param2 * 2 + 4 + p2], 1) == sceMcResSucceed) {
//        callback = cb_openedload;
//      } else {
//        op.operation = MemoryCardOperationKind::NO_OP;
//        op.result = McStatusCode::INTERNAL_ERROR;
//      }
//    } else {
//      // let's verify the data.
//      McHeader* headers[2];
//      McHeader* footers[2];
//      bool ok[2];
//
//      headers[0] = (McHeader*)(op.data_ptr.c());
//      footers[0] = (McHeader*)(op.data_ptr.c() + sizeof(McHeader) + BANK_SIZE);
//      headers[1] = (McHeader*)(op.data_ptr.c() + BANK_TOTAL_SIZE);
//      footers[1] = (McHeader*)(op.data_ptr.c() + BANK_TOTAL_SIZE + sizeof(McHeader) + BANK_SIZE);
//      static_assert(BANK_TOTAL_SIZE * 2 == 0x21000, "save layout");
//      ok[0] = true;
//      ok[1] = true;
//
//      for (int idx = 0; idx < 2; idx++) {
//        u32 expected_save_count = headers[idx]->save_count;
//        if (headers[idx]->unk1_repeated == expected_save_count &&
//            footers[idx]->save_count == expected_save_count &&
//            footers[idx]->unk1_repeated == expected_save_count) {
//          // save count is okay!
//          if (headers[idx]->magic == MEM_CARD_MAGIC && footers[idx]->magic == MEM_CARD_MAGIC) {
//            // magic numbers okay!
//            if (headers[idx]->checksum == footers[idx]->checksum) {
//              // checksum
//              auto expected_checksum = headers[idx]->checksum;
//              if (mc_checksum(make_u8_ptr(headers[idx] + 1), BANK_SIZE) != expected_checksum) {
//                mc_print("failed checksum");
//                ok[idx] = false;
//              }
//            } else {
//              mc_print("corrupted checksum");
//              ok[idx] = false;
//            }
//          } else {
//            mc_print("bad magic");
//            ok[idx] = false;
//          }
//        } else {
//          mc_print("bad save count");
//          ok[idx] = false;
//        }
//      }
//
//      mc_print("checking loaded banks");
//
//      //
//      if (!ok[0] && !ok[1]) {
//        // no good data.
//        if (headers[0]->save_count == 0 && headers[0]->checksum == 0 && headers[0]->magic == 0 &&
//            headers[0]->unk1_repeated == 0 && headers[1]->save_count == 0 &&
//            headers[1]->checksum == 0 && headers[1]->magic == 0 && headers[1]->unk1_repeated == 0)
//            {
//          // this is a fresh file that you tried to load from...
//          mc_print("new game result");
//          op.operation = MemoryCardOperationKind::NO_OP;
//          op.result = McStatusCode::NEW_GAME;
//          mc_last_file = op.param2;
//        } else {
//          mc_print("corrupted data");
//          op.operation = MemoryCardOperationKind::NO_OP;
//          op.result = McStatusCode::READ_ERROR;
//        }
//      } else {
//        // pick the bank
//        int bank = 0;
//
//        if (!ok[0] || !ok[1]) {
//          if (ok[1]) {
//            bank = 1;
//          }
//        } else {
//          bank = headers[0]->save_count <= headers[1]->save_count;
//        }
//
//        u32 current_save_count = headers[bank]->save_count;
//        memcpy(op.data_ptr.c(), op.data_ptr.c() + bank * BANK_TOTAL_SIZE + sizeof(McHeader),
//        BANK_SIZE); mc_last_file = op.param2; mc_files[op.param2].most_recent_save_count =
//        current_save_count; mc_files[op.param2].last_saved_bank = bank; op.operation =
//        MemoryCardOperationKind::NO_OP; op.result = McStatusCode::OK; mc_print("load succeeded");
//      }
//    }
//  }
//}
