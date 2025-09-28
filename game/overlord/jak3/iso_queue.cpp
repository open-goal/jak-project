#include "iso_queue.h"

#include "common/log/log.h"
#include "common/util/Assert.h"

#include "game/overlord/jak3/basefile.h"
#include "game/overlord/jak3/dma.h"
#include "game/overlord/jak3/iso.h"
#include "game/overlord/jak3/isocommon.h"
#include "game/overlord/jak3/pagemanager.h"
#include "game/overlord/jak3/spustreams.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

using namespace iop;

namespace jak3 {
s32 g_nPriQueueSema = 0;
s32 g_VagCmdSema = 0;
u32 g_auStrmSRAM[6];
u32 g_auTrapSRAM[6];
PriStackEntry gPriStack[2];
extern u32 time_of_last_unknown_rate_drive_op;
u32 g_cmds_with_speed_total = 0;
bool unk_time_mode_flag = false;
ISO_Hdr* g_selected_cmd = nullptr;
bool unk_time_mflag = 0;
s32 unk_sector = 0;
u32 vag_cmd_cnt = 0;
u32 vag_cmd_used = 0;
u32 max_vag_cmd_cnt = 0;
ISO_VAGCommand vag_cmds[16];

static constexpr s32 LOOP_END = 1;
static constexpr s32 LOOP_REPEAT = 2;
static constexpr s32 LOOP_START = 4;

// Empty ADPCM block with loop flags

// clang-format off
u8 VAG_SilentLoop[0x60] = {
    0x0, LOOP_START | LOOP_REPEAT, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_REPEAT,              0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
    0x0, LOOP_END | LOOP_REPEAT,   0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0,
};
// clang-format on

void jak3_overlord_init_globals_iso_queue() {
  g_nPriQueueSema = 0;
  g_VagCmdSema = 0;
  for (auto& x : gPriStack) {
    x = {};
  }
  g_cmds_with_speed_total = 0;
  unk_time_mode_flag = false;
  g_selected_cmd = nullptr;
  unk_time_mflag = 0;
  unk_sector = 0;
  vag_cmd_cnt = 0;
  vag_cmd_used = 0;
  for (auto& x : vag_cmds) {
    x = {};
  }
}

/*!
 * Added function to check if there is a pending DGO load command.
 * On PC, DOG loads are really the only loading time that users see. If there is a pending
 * DGO load, we can modify logic to take advantage of PCs being dramatically faster than PS2 and
 * get much better load times.
 */
bool DgoCmdWaiting() {
  for (auto& level : gPriStack) {
    for (int i = 0; i < level.count; i++) {
      auto* cmd = level.cmds[i];

      if (cmd && cmd->msg_type == ISO_Hdr::MsgType::DGO_LOAD) {
        if (cmd->m_pBaseFile) {
          auto* file = cmd->m_pBaseFile;
          if (file->m_Buffer.m_nDataLength) {
            return true;
          }
        }
      }
    }
  }
  return false;
}

void InitBuffers() {
  SemaParam sema_param;
  sema_param.max_count = 1;
  sema_param.init_count = 1;
  sema_param.attr = 0;
  sema_param.option = 0;

  g_nPriQueueSema = CreateSema(&sema_param);
  ASSERT(g_nPriQueueSema >= 0);
  get_page_manager()->Initialize();

  g_auStrmSRAM[0] = 0x5040;
  g_auTrapSRAM[0] = 0x9040;
  snd_SRAMMarkUsed(0x5040, 0x4040);
  g_auStrmSRAM[1] = 0x9080;
  g_auTrapSRAM[1] = 0xd080;
  snd_SRAMMarkUsed(0x9080, 0x4040);
  g_auStrmSRAM[2] = 0xd0c0;
  g_auTrapSRAM[2] = 0x110c0;
  snd_SRAMMarkUsed(0xd0c0, 0x4040);
  g_auStrmSRAM[3] = 0x11100;
  g_auTrapSRAM[3] = 0x15100;
  snd_SRAMMarkUsed(0x11100, 0x4040);
  g_auStrmSRAM[4] = 0x15140;  // 86384 - 48
  g_auTrapSRAM[4] = 0x19140;
  snd_SRAMMarkUsed(0x15140, 0x4040);
  g_auStrmSRAM[5] = 0x019180;
  g_auTrapSRAM[5] = 0x001d180;
  snd_SRAMMarkUsed(0x19180, 0x4040);

  for (int i = 0; i < 6; i++) {
    if (!DMA_SendToSPUAndSync(VAG_SilentLoop, 0x30, g_auTrapSRAM[i], nullptr, nullptr)) {
      DelayThread(1000);
      ASSERT_NOT_REACHED();
      break;
    }
  }

  sema_param.max_count = 1;
  sema_param.attr = 1;
  sema_param.init_count = 1;
  sema_param.option = 0;
  g_VagCmdSema = CreateSema(&sema_param);
  ASSERT(g_VagCmdSema >= 0);
}

int QueueMessage(ISO_Hdr* msg, int pri) {
  msg->status = EIsoStatus::OK_2;
  msg->priority = pri;
  WaitSema(g_nPriQueueSema);
  int queue_idx = (pri == 5) ? 1 : 0;
  bool ok = gPriStack[queue_idx].count != 8;
  if (ok) {
    gPriStack[queue_idx].cmds[gPriStack[queue_idx].count] = msg;
    gPriStack[queue_idx].count++;
    SignalSema(g_nPriQueueSema);
  } else {
    msg->status = EIsoStatus::FAILED_TO_QUEUE_4;
    SignalSema(g_nPriQueueSema);
    ReturnMessage(msg);
    ASSERT_NOT_REACHED();
  }
  return ok;
}

int UnqueueMessage(ISO_Hdr* msg) {
  WaitSema(g_nPriQueueSema);
  int iVar5 = 0;
  PriStackEntry* stack = gPriStack;
  do {
    int iVar4 = 0;
    ISO_Hdr** cmd = stack->cmds;
    if (0 < stack->count) {
      do {
        if (*cmd == msg)
          break;
        iVar4 = iVar4 + 1;
        cmd++;
      } while (iVar4 < stack->count);
    }
    iVar5 = iVar5 + 1;
    if (iVar4 < stack->count) {
      stack->count = stack->count + -1;
      if (iVar4 < stack->count) {
        ISO_Hdr** ppIVar3 = stack->cmds + iVar4;
        do {
          iVar4 = iVar4 + 1;
          *ppIVar3 = ppIVar3[1];
          ppIVar3 = ppIVar3 + 1;
        } while (iVar4 < stack->count);
      }
      return SignalSema(g_nPriQueueSema);
    }
    stack = stack + 1;
    if (1 < iVar5) {
      return SignalSema(g_nPriQueueSema);
    }
  } while (true);
}

/*!
 * Select which command to read for next
 * This function considers things like seeking time, reading rates of streamed files,
 * and buffer sizing. To be entirely honest, I don't understand it almost at all, and it's not
 * clear that it works as expected. It seems to work good enough, and no commands get entirely
 * starved of data while there are multiple streams.
 */
ISO_Hdr* GetMessage() {
  //  bool been_a_while;
  //  bool bVar2;
  //  int now;
  //  int iVar3;
  //  int iVar4;
  //  CISOCDFile *file;
  //  CISOCDFile *pCVar5;
  //  int iVar6;
  //  CPageList *plist;
  //  uint uVar7;
  //  int iVar8;
  //  int iVar9;
  //  PriStack *local_t2_216;
  //  PriStack *pri_level;
  //  CISOCDFile *tfile4;
  //  code *pcVar10;
  //  CISOCDFile *tfile2;
  //  CISOCDFile *tfile3;
  //  uint uVar11;
  //  ISO_VAGCommand *cmd;
  //  int idx_on_level;
  //  ISO_VAGCommand **ppIVar12;
  //  int iVar13;
  //  CBaseFile *tfile;
  //  int cmds_total;
  //  int iVar14;
  //  uint uVar15;
  ISO_Hdr* cmds_array[16];
  u32 read_rates_array[16];
  int num_pages_array[16];
  int unstepped_pages_array[16];
  int remaining_pages_array[16];
  //  uint its_been_a_while;
  //  int pages_total;
  //  int read_rate_total;
  //  int min_nospeed_pages_total;
  //  int max_pages_total;
  //  int min_speed_pages_total;
  //  ISO_VAGCommand *selected_cmd;
  //  int cmds_with_speed_total;
  //  uint cmd2_read_rate;
  //  int selected_cmd_rem_sectors;
  //  int pri_level_idx;

  // simple logic to select which command to use next.

  u32 now = GetSystemTimeLow();

  bool been_a_while = false;
  if (unk_time_mode_flag == 0) {
    been_a_while = 0x384000 < (now - time_of_last_unknown_rate_drive_op);
  } else {
    unk_time_mode_flag = 0;
    time_of_last_unknown_rate_drive_op = now;
  }

  s32 cmds_total = 0;
  get_page_manager()->GarbageCollect();
  s32 pages_total = 0;
  s32 read_rate_total = 0;
  s32 min_nospeed_pages_total = 0;
  s32 max_pages_total = 0;
  s32 min_speed_pages_total = 0;
LAB_000080e4:
  s32 iVar9 = g_cmds_with_speed_total * 400 + 0x2ee;
  s32 iVar13 = 0x7fffffff;
  ISO_Hdr* selected_cmd = nullptr;
  s32 cmds_with_speed_total = 0;
  s32 cmd2_read_rate = 0;
  s32 selected_cmd_rem_sectors = -1;
  s32 pri_level_idx = 1;
  PriStackEntry* pri_level = gPriStack + 1;
  s32 iVar8 = iVar13;

  // loop over priority levels
  do {
    s32 idx_on_level = pri_level->count + -1;

    // if any exist on this level
    if (-1 < idx_on_level) {
      ISO_Hdr** ppIVar12 = pri_level->cmds + idx_on_level;
      // iVar14 = cmds_total << 2;
      // loop over commands on this level
      do {
        ISO_Hdr* cmd = *ppIVar12;
        CBaseFile* file = nullptr;
        // iVar4 = iVar14;

        // basic check if this command is even valid:
        if (cmd && (file = cmd->m_pBaseFile, file) && cmd->status == EIsoStatus::OK_2 &&
            cmd->active_a != 0) {
          u32 read_rate = file->m_ReadRate;
          read_rate_total = read_rate_total + read_rate;  // maybe this is an inverse rate...

          // build up arrays of info for each command
          read_rates_array[cmds_total] = read_rate;
          cmds_array[cmds_total] = cmd;

          if ((int)read_rate < 1) {
            min_nospeed_pages_total = min_nospeed_pages_total + file->m_Buffer.m_nMinNumPages;
            max_pages_total = max_pages_total + file->m_Buffer.m_nMaxNumPages;
          } else {
            min_speed_pages_total = min_speed_pages_total + file->m_Buffer.m_nMinNumPages;
            cmds_with_speed_total = cmds_with_speed_total + 1;
          }

          CPageList* plist = file->m_Buffer.m_pPageList;

          s32 npages = 0;
          if (plist != (CPageList*)0x0) {
            npages = plist->m_nNumPages;
          }
          pages_total = pages_total + npages;
          num_pages_array[cmds_total] = npages;

          s32 n_untepped_pages = 0;
          if (plist != (CPageList*)0x0) {
            n_untepped_pages = plist->m_nNumUnsteppedPages;
          }
          unstepped_pages_array[cmds_total] = n_untepped_pages;

          s32 n_remaining_pages = 4;
          if (cmd->callback != RunDGOStateMachine) {
            n_remaining_pages = n_untepped_pages + file->m_LengthPages - file->m_PageOffset;
            // lg::warn("remaining pages is {} = {} + {} - {}", n_remaining_pages, n_untepped_pages,
            // file->m_LengthPages, file->m_PageOffset);
          }
          remaining_pages_array[cmds_total] = n_remaining_pages;

          if (remaining_pages_array[cmds_total] < 1) {
            remaining_pages_array[cmds_total] = 1;
          }

          // careful, this increments in a weird spot.
          // I use cmds_total - 1 below...
          int old_cmds_total = cmds_total;
          cmds_total = cmds_total + 1;

          // iVar4 = iVar14 + 4;

          // next, we'll determine a desired page cutoff and discard commands where
          // we have enough pages.
          if (read_rate && plist) {
            // 3/4 * mNumPages, this is if our allocated buffer is 75% full.
            s32 desired_pages = (int)(file->m_nNumPages * 0x30) >> 6;

            // but, we want at least min pages
            if (desired_pages < file->m_Buffer.m_nMinNumPages) {
              desired_pages = file->m_Buffer.m_nMinNumPages;
            }

            // and we want at least 2
            if (desired_pages < 2) {
              desired_pages = 2;
            }

            // but, we never want more pages than we plan to eventually read.
            if (remaining_pages_array[old_cmds_total] < desired_pages) {
              desired_pages = remaining_pages_array[old_cmds_total];
            }

            // if we have that many, we can just forget the command - no point in filling it now.
            if (desired_pages <= unstepped_pages_array[old_cmds_total])
              goto LAB_00008420;
          }

          s32 iVar14 = iVar9;
          if ((0 < (int)read_rate) &&
              (iVar14 = (int)(file->m_Buffer.m_nDataLength * 1000) / (int)read_rate,
               read_rate == 0)) {
            ASSERT_NOT_REACHED();
          }

          s32 pri = cmd->priority;

          // really not sure what this is...
          if (been_a_while) {
            if ((read_rate == 0) || (iVar14 < 0x2ee)) {
              been_a_while = false;
              goto LAB_000080e4;
            }

            // if this isn't the command we said last time.
            if (g_selected_cmd != cmd) {
              s32 current_sector = file->GetSector();
              current_sector = current_sector - unk_sector;
              bool sector_ok = false;
              if (current_sector < 0) {
                current_sector = -current_sector;
                if (unk_time_mflag != 0) {
                LAB_000083ac:
                  current_sector = current_sector + 10000000;
                }
                sector_ok = current_sector < iVar13;
              } else {
                sector_ok = current_sector < iVar13;
                if ((0 < current_sector) && (unk_time_mflag == 0))
                  goto LAB_000083ac;
              }
              if (sector_ok) {
                iVar13 = current_sector;
                iVar8 = iVar14;
                selected_cmd = cmd;
                cmd2_read_rate = read_rate;
              }
            }
          } else {
            // normal selection logic???
            if ((((iVar14 == iVar8) && (selected_cmd_rem_sectors < pri)) || (iVar14 < iVar8)) &&
                (iVar14 <= iVar9)) {
              iVar8 = iVar14;
              selected_cmd = cmd;
              cmd2_read_rate = read_rate;
              selected_cmd_rem_sectors = pri;
            }
          }
        }
      LAB_00008420:
        idx_on_level = idx_on_level + -1;
        /* WARNING: ptrarith problems */
        ppIVar12 = ppIVar12 + -1;
        // iVar14 = iVar4;
      } while (-1 < idx_on_level);
    }
    pri_level--;
    pri_level_idx = pri_level_idx + -1;
  } while (-1 < pri_level_idx);

  if (selected_cmd) {
    if (cmd2_read_rate == 0) {
      unk_time_mode_flag = 1;
      time_of_last_unknown_rate_drive_op = now;
    }
    iVar13 = unk_sector;
    if (selected_cmd->m_pBaseFile) {
      iVar13 = selected_cmd->m_pBaseFile->GetSector();
    }
    if (unk_sector < iVar13) {
      unk_time_mflag = 1;
      unk_sector = iVar13;
    } else {
      been_a_while = iVar13 != unk_sector;
      unk_sector = iVar13;
      if (been_a_while) {
        unk_time_mflag = 0;
        unk_sector = iVar13;
      }
    }
  }
  min_speed_pages_total = min_speed_pages_total + min_nospeed_pages_total;
  g_cmds_with_speed_total = cmds_with_speed_total;
  g_selected_cmd = selected_cmd;
  pages_total = get_page_manager()->m_CCache.m_nNumFreePages + pages_total;
  if ((0 < min_speed_pages_total) && (0 < pages_total)) {
    if (min_speed_pages_total == 0) {
      // trap(0x1c00);
      ASSERT_NOT_REACHED();
    }
    min_speed_pages_total = (min_nospeed_pages_total * pages_total) / min_speed_pages_total;
    if (min_speed_pages_total < min_nospeed_pages_total) {
      min_speed_pages_total = min_nospeed_pages_total;
    }
    if (max_pages_total < min_speed_pages_total) {
      min_speed_pages_total = max_pages_total;
    }
    pages_total = pages_total - min_speed_pages_total;
    iVar9 = 0;
    iVar13 = min_speed_pages_total;
    iVar8 = pages_total;
    if (0 < cmds_total) {
      // iVar14 = 0;
      iVar13 = min_speed_pages_total;
      iVar8 = pages_total;
      do {
        CBaseFile* tfile = cmds_array[iVar9]->m_pBaseFile;
        if (read_rates_array[iVar9] < 1) {
          s32 uVar11 = (tfile->m_Buffer).m_nMaxNumPages;
          if (max_pages_total == 0) {
            ASSERT_NOT_REACHED();
          }
          s32 uVar15 = (tfile->m_Buffer).m_nMinNumPages;
          s32 uVar7 = (int)(uVar11 * min_speed_pages_total) / max_pages_total;
          if ((int)uVar7 < (int)uVar15) {
            uVar7 = uVar15;
          }

          tfile->m_nNumPages = uVar7;
          if ((int)uVar11 < (int)uVar7) {
            uVar7 = uVar11;
          }
          tfile->m_nNumPages = uVar7;
          if (remaining_pages_array[iVar9] < (int)uVar7) {
            uVar7 = remaining_pages_array[iVar9];
          }
          tfile->m_nNumPages = uVar7;
          // lg::warn("num pages mod {}", uVar7);
          iVar13 = iVar13 - uVar7;
        } else {
          s32 uVar11 = (tfile->m_Buffer).m_nMinNumPages;
          s32 uVar7 = (tfile->m_Buffer).m_nMaxNumPages;
          // lg::warn("taking else case: {} {} {}", uVar11, uVar7, tfile->m_nNumPages);

          s32 uVar15 = -1;
          if (read_rate_total == 0) {
          LAB_000085e4:
            // lg::warn("going to min! (rrt is {})", read_rate_total);
            uVar15 = uVar11;
          } else {
            if (read_rate_total == 0) {
              ASSERT_NOT_REACHED();
            }
            uVar15 = (read_rates_array[iVar9] * pages_total) / read_rate_total;
            // lg::warn("read rate math is {}", uVar15);
            if ((int)uVar15 < (int)uVar11)
              goto LAB_000085e4;
          }

          // lg::warn("vals {} {}", uVar7, uVar15);
          if ((int)uVar7 < (int)uVar15) {
            uVar15 = uVar7;
          }
          // lg::warn("vals 2 {} {}", remaining_pages_array[iVar9], uVar15);
          if (remaining_pages_array[iVar9] < (int)uVar15) {
            uVar15 = remaining_pages_array[iVar9];
          }
          iVar8 = iVar8 - uVar15;
          tfile->m_nNumPages = uVar15;
          // lg::warn("num pages mod 2 {}", uVar15);
        }
        iVar9 = iVar9 + 1;
        // iVar14 = iVar9 * 4;
      } while (iVar9 < cmds_total);
    }
    while (0 < iVar13) {
      iVar9 = 0;
      s32 iVar14 = iVar13;
      if (0 < cmds_total) {
        s32 iVar4 = 0;
        iVar14 = iVar13;
        while (0 < iVar14) {
          CBaseFile* tfile2 = cmds_array[iVar4 / 4]->m_pBaseFile;
          iVar9 = iVar9 + 1;
          s32 uVar11;
          if (read_rates_array[iVar4 / 4] == 0 &&
              (uVar11 = tfile2->m_nNumPages, (int)uVar11 < remaining_pages_array[iVar4 / 4]) &&
              ((int)uVar11 < tfile2->m_Buffer.m_nMaxNumPages)) {
            tfile2->m_nNumPages = uVar11 + 1;
            iVar14 = iVar14 + -1;
          }
          if (cmds_total <= iVar9)
            break;
          iVar4 = iVar9 * 4;
        }
      }
      been_a_while = iVar13 == iVar14;
      iVar13 = iVar14;
      if (been_a_while) {
        iVar8 = iVar8 + iVar14;
        iVar13 = 0;
      }
    }
    while (0 < iVar8) {
      iVar13 = 0;
      iVar9 = iVar8;
      if (0 < cmds_total) {
        s32 iVar14 = 0;
        iVar9 = iVar8;
        while (0 < iVar9) {
          CBaseFile* tfile4 = cmds_array[iVar14 / 4]->m_pBaseFile;
          iVar13 = iVar13 + 1;
          s32 uVar11;
          if (((0 < read_rates_array[iVar14 / 4]) &&
               (uVar11 = tfile4->m_nNumPages, (int)uVar11 < remaining_pages_array[iVar14 / 4])) &&
              ((int)uVar11 < tfile4->m_Buffer.m_nMaxNumPages)) {
            tfile4->m_nNumPages = uVar11 + 1;
            iVar9 = iVar9 + -1;
          }
          if (cmds_total <= iVar13)
            break;
          iVar14 = iVar13 * 4;
        }
      }
      been_a_while = iVar8 == iVar9;
      iVar8 = iVar9;
      if (been_a_while) {
        iVar8 = 0;
      }
    }
    iVar13 = 0;
    if (0 < cmds_total) {
      iVar8 = 0;
      do {
        CBaseFile* tfile3 = cmds_array[iVar8 / 4]->m_pBaseFile;

        iVar13 = iVar13 + 1;
        s32 uVar11 = tfile3->m_nNumPages;
        //        lg::warn("mystery values: {} - {} = {} (from {})", num_pages_array[iVar8 / 4],
        //        uVar11,
        //                 num_pages_array[iVar8 / 4] - uVar11,
        //                 cmds_array[iVar8 / 4]->m_pBaseFile->m_FileDef->name.data);
        if (((int)uVar11 < num_pages_array[iVar8 / 4]) &&
            (iVar8 = tfile3->RecoverPages(num_pages_array[iVar8 / 4] - uVar11), 0 < iVar8)) {
          get_page_manager()->GarbageCollectPageList(tfile3->m_Buffer.m_pPageList);
        }
        iVar8 = iVar13 * 4;
      } while (iVar13 < cmds_total);
    }
  }
  return selected_cmd;
}

int ProcessMessageData(ISO_Hdr* tgt_msg) {
  EIsoStatus stat;
  s32 ret = 1;
LAB_000088a4:
  s32 num_remaining = gPriStack[0].count + -1;
  if (num_remaining < 0) {
    return ret;
  }
  auto* cmd_array = gPriStack[0].cmds + num_remaining;
  ISO_Hdr* cmd = nullptr;
  do {
    EIsoStatus (*cb)(ISO_Hdr*) = nullptr;
    cmd = *cmd_array;
    if (cmd && cmd->active_a) {
      stat = cmd->status;
      if (stat == EIsoStatus::OK_2) {
        auto* file = cmd->m_pBaseFile;
        auto* buffer = &file->m_Buffer;
        if (!file) {
          buffer = nullptr;
        }
        if ((buffer && (buffer->m_eBufferType != CBuffer::BufferType::EBT_FREE)) &&
            (cb = cmd->callback, cb != ProcessVAGData)) {
          stat = (cb)(cmd);
        }
      }
      if (stat != EIsoStatus::OK_2)
        break;
      cmd->status = EIsoStatus::OK_2;
    }
    num_remaining = num_remaining + -1;
    cmd_array = cmd_array + -1;
    if (num_remaining < 0) {
      return ret;
    }
  } while (true);
  if (cmd == tgt_msg) {
    ret = 0;
  }
  ReleaseMessage(cmd);
  if (stat != EIsoStatus::IDLE_1) {
    cmd->status = stat;
    ReturnMessage(cmd);
  }
  goto LAB_000088a4;
}

void ReturnMessage(ISO_Hdr* msg) {
  if (msg->mbox_reply == 0) {
    if (msg->thread_to_wake == 0) {
      FreeVAGCommand((ISO_VAGCommand*)msg);
    } else {
      WakeupThread(msg->thread_to_wake);
    }
  } else {
    SendMbx(msg->mbox_reply, msg);
  }
}

void ReleaseMessage(ISO_Hdr* msg) {
  if (GetThreadId() == g_nISOThreadID) {
    auto* file = msg->m_pBaseFile;
    if (file && file->m_Status != EIsoStatus::NONE_0) {
      file->Close();
    }
    UnqueueMessage(msg);
  } else {
    if (msg->msg_type != ISO_Hdr::MsgType::ABADBABE) {
      set_active_a(msg, 0);
      set_active_c(msg, 0);
      msg->msg_type = ISO_Hdr::MsgType::ABADBABE;
      SendMbx(g_nISOMbx, msg);
    }
  }
}

ISO_VAGCommand* GetVAGCommand() {
  int iVar1;
  u32 uVar2;
  u32 uVar3;

  do {
    while (vag_cmd_cnt == 0x1f) {
      DelayThread(1000);
    }
    do {
      iVar1 = WaitSema(g_VagCmdSema);
      uVar2 = 0;
    } while (iVar1 != 0);
    do {
      uVar3 = uVar2 + 1;
      if (((vag_cmd_used >> (uVar2 & 0x1f) ^ 1U) & 1) != 0) {
        vag_cmd_cnt = vag_cmd_cnt + 1;
        vag_cmd_used = vag_cmd_used | 1 << (uVar2 & 0x1f);
        if (max_vag_cmd_cnt < vag_cmd_cnt) {
          max_vag_cmd_cnt = vag_cmd_cnt;
        }
        SignalSema(g_VagCmdSema);
        return vag_cmds + uVar2;
      }
      uVar2 = uVar3;
    } while ((int)uVar3 < 0x1f);
    SignalSema(g_VagCmdSema);
  } while (true);
}

void FreeVAGCommand(ISO_VAGCommand* param_1) {
  u32 uVar1;
  int iVar2;

  uVar1 = param_1 - vag_cmds;
  ASSERT(uVar1 < 16);
  if ((uVar1 < 0x1f) && (((vag_cmd_used >> (uVar1 & 0x1f) ^ 1U) & 1) == 0)) {
    do {
      iVar2 = WaitSema(g_VagCmdSema);
    } while (iVar2 != 0);
    vag_cmd_cnt = vag_cmd_cnt - 1;
    vag_cmd_used = vag_cmd_used & ~(1 << (uVar1 & 0x1f));
    SignalSema(g_VagCmdSema);
  }
}

}  // namespace jak3