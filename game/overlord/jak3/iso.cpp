#include "iso.h"

#include <cstring>

#include "common/util/Assert.h"

#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/iso_cd.h"
#include "game/overlord/jak3/iso_queue.h"
#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/spustreams.h"
#include "game/overlord/jak3/srpc.h"
#include "game/overlord/jak3/stream.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"

using namespace iop;

namespace jak3 {
int g_nISOInitFlag = 0;
u32 s_MsgPacket_NotOnStackSync = 0;
int g_nSyncMbx = -1;
int g_nISOMbx = -1;
int g_nDGOMbx = -1;
int g_nDGOThread = -1;
int g_nSTRThreadID = -1;
int g_nISOThreadID = -1;
int g_nPlayThreadID = -1;
int g_nMusicFadeDir = 0;
int g_nMusicFade = 0;
int g_nMusicTweak = 0;
ISO_DGOCommand sLoadDGO;

void jak3_overlord_init_globals_iso() {
  g_nISOInitFlag = 0;
  s_MsgPacket_NotOnStackSync = 0;
  g_nSyncMbx = -1;
  g_nISOMbx = -1;
  g_nDGOMbx = -1;
  g_nDGOThread = -1;
  g_nSTRThreadID = -1;
  g_nISOThreadID = -1;
  g_nPlayThreadID = -1;
  g_nMusicFadeDir = 0;
  g_nMusicFade = 0;
  g_nMusicTweak = 0;
  sLoadDGO = {};
}

/*!
 * Initialize the filesystem driver, then send a message to the sync messagebox
 */
void InitDriver() {
  // init the filesystem
  if (get_file_system()->Init() == 0) {
    g_nISOInitFlag = 0;
  }
  SendMbx(g_nSyncMbx, &s_MsgPacket_NotOnStackSync);
}

/*!
 * Check if there is anything in the sync mbox of the iso thread.
 */
u32 LookSyncMbx() {
  MsgPacket* msg_packet;
  auto poll_result = PollMbx(&msg_packet, g_nSyncMbx);
  if (poll_result != KE_MBOX_NOMSG) {
    sLoadDGO.sync_mbox_wait_count++;
  }
  return poll_result != KE_MBOX_NOMSG;
}

/*!
 * Wait on the message box to have a message. Unlike older versions, they finally figured out
 * how to call ReceiveMbx instead of polling in a loop!
 */
u32 WaitMbx(s32 box) {
  MsgPacket* msg_packet;
  ReceiveMbx(&msg_packet, box);
}

u32 ISOThread();
u32 DGOThread();

/*!
 * Initialize the Filesystem and ISO Thread.
 */
void InitISOFS() {
  // memset(&sLoadDGO,0,0x110);
  sLoadDGO = {};
  sLoadDGO.last_id = -1;
  g_nISOInitFlag = 1;
  sLoadDGO.acked_cancel_id = -1;
  sLoadDGO.selected_id = -1;
  sLoadDGO.nosync_cancel_pending_flag = 0;
  // g_pFileSystem = &g_ISOCDFileSystem;
  sLoadDGO.request_cancel_id = -1;
  sLoadDGO.nosync_cancel_ack = 0;
  sLoadDGO.sync_sent_count = 0;
  sLoadDGO.sync_mbox_wait_count = 0;
  sLoadDGO.sync_ret_count = 0;

  MbxParam mbx_param;
  mbx_param.option = 0;
  mbx_param.attr = 0;

  g_nISOMbx = CreateMbx(&mbx_param);
  ASSERT(g_nISOMbx >= 0);
  g_nDGOMbx = CreateMbx(&mbx_param);
  ASSERT(g_nDGOMbx >= 0);
  g_nSyncMbx = CreateMbx(&mbx_param);
  ASSERT(g_nSyncMbx >= 0);

  ThreadParam thread_param;

  thread_param.stackSize = 0x1100;
  thread_param.entry = ISOThread;
  thread_param.initPriority = 0x37;
  thread_param.attr = TH_C;
  thread_param.option = 0;
  g_nISOThreadID = CreateThread(&thread_param);
  ASSERT(g_nISOThreadID >= 0);

  thread_param.entry = DGOThread;
  thread_param.initPriority = 0x38;
  thread_param.attr = TH_C;
  thread_param.stackSize = 0x900;
  thread_param.option = 0;
  g_nDGOThread = CreateThread(&thread_param);
  ASSERT(g_nDGOThread >= 0);

  thread_param.entry = STRThread;
  thread_param.initPriority = 0x39;
  thread_param.attr = TH_C;
  thread_param.stackSize = 0x900;
  thread_param.option = 0;
  g_nSTRThreadID = CreateThread(&thread_param);
  ASSERT(g_nSTRThreadID >= 0);

  thread_param.attr = TH_C;
  thread_param.entry = PLAYThread;
  thread_param.initPriority = 0x35;
  thread_param.stackSize = 0x900;
  thread_param.option = 0;
  g_nPlayThreadID = CreateThread(&thread_param);
  ASSERT(g_nPlayThreadID >= 0);

  StartThread(g_nISOThreadID, 0);
  StartThread(g_nDGOThread, 0);
  StartThread(g_nSTRThreadID, 0);
  StartThread(g_nPlayThreadID, 0);
  WaitMbx(g_nSyncMbx);

  const ISOFileDef* vagdir_file = FindISOFile("VAGDIR.AYB");
  ASSERT(vagdir_file);
  int load_status = LoadISOFileToIOP(vagdir_file, &g_VagDir, sizeof(g_VagDir));
  ASSERT(load_status == 0);

  // vgwaddir
  ASSERT(g_VagDir.vag_magic_1 == 0x41574756);
  ASSERT(g_VagDir.vag_magic_2 == 0x52494444);

  // splash screen load was here...
  ASSERT(g_nISOInitFlag == 0);
}

const ISOFileDef* FindISOFile(const char* name) {
  get_file_system()->Find(name);
}

s32 GetISOFileLength(const ISOFileDef* def) {
  return get_file_system()->GetLength(def);
}

void SetVagClock(ISO_VAGCommand* cmd) {
  cmd->clockc = 0;
  cmd->clocka = 0;
  cmd->clockb = 0;
  cmd->clockd = 0;
  if (!cmd->m_pBaseFile) {
    cmd->flags.file_disappeared = 1;
  } else {
    cmd->flags.clocks_set = 1;
    if (cmd->stereo_sibling) {
      cmd->stereo_sibling->flags.clocks_set = 1;
    }
  }
}

/*!
 * Start playing music, from an "external" command (from the ISO system).
 */
void IsoPlayMusicStream(ISO_VAGCommand* user_cmd) {
  // must be flagged as music in the request.
  ASSERT(user_cmd->music_flag);

  const char* name = user_cmd->name;

  // first, let's try to find an existing internal command that's streaming this music
  ISO_VAGCommand* internal_cmd = FindMusicStreamName(name);
  ISO_VAGCommand* stereo_internal_cmd = nullptr;

  if (!internal_cmd) {
    // no existing command, so allocate one.
    internal_cmd = SmartAllocMusicVagCommand(user_cmd, 0);

    if (!internal_cmd) {
      ASSERT_NOT_REACHED();  // was only a warning
      return;
    }

    // set the active flags to false:
    set_active_a(internal_cmd, 0);
    set_active_b(internal_cmd, 0);
    set_active_c(internal_cmd, 0);
    set_active_a(user_cmd, 0);
    set_active_b(user_cmd, 0);
    set_active_c(user_cmd, 0);

    // clear flags related to actual streaming, since we're starting (or restarting) the stream
    // from nothing.
    user_cmd->flags.saw_chunks1 = 0;
    user_cmd->flags.running = 0;

    // if we're reusing an already started stream, need to stop that one first.
    // if ((*(uint*)&internal_cmd->bit_running & 0xffff00) != 0) {
    if (internal_cmd->flags.clocks_set || internal_cmd->flags.file_disappeared) {
      IsoStopVagStream(internal_cmd);
    }

    // copy entire header
    internal_cmd->unka = user_cmd->unka;
    internal_cmd->unkb = user_cmd->unkb;
    internal_cmd->status = user_cmd->status;
    internal_cmd->active_a = user_cmd->active_a;
    internal_cmd->active_b = user_cmd->active_b;
    internal_cmd->active_c = user_cmd->active_c;
    internal_cmd->pad = user_cmd->pad;

    // copy part of the command that the user sets:
    internal_cmd->msg_type = user_cmd->msg_type;
    internal_cmd->mbox_reply = user_cmd->mbox_reply;
    internal_cmd->thread_to_wake = user_cmd->thread_to_wake;
    internal_cmd->callback = user_cmd->callback;

    internal_cmd->m_pBaseFile = user_cmd->m_pBaseFile;
    internal_cmd->unkc = user_cmd->unkc;
    internal_cmd->file_def = user_cmd->file_def;

    internal_cmd->vag_file_def = user_cmd->vag_file_def;
    internal_cmd->vag_dir_entry = user_cmd->vag_dir_entry;
    strncpy(internal_cmd->name, name, 0x30);
    internal_cmd->play_volume = user_cmd->play_volume;
    internal_cmd->id = user_cmd->id;
    internal_cmd->plugin_id = user_cmd->plugin_id;
    internal_cmd->maybe_sound_handler = user_cmd->maybe_sound_handler;
    internal_cmd->oog = user_cmd->oog;
    internal_cmd->some_pan_thing = user_cmd->some_pan_thing;
    internal_cmd->art_flag = user_cmd->art_flag;
    internal_cmd->movie_flag = user_cmd->movie_flag;

    //
    InitVAGCmd(internal_cmd, 1);

    // check for stereo bit:
    if ((internal_cmd->vag_dir_entry->words[1] & 0x400U) != 0) {
      // allocate stereo command
      stereo_internal_cmd = SmartAllocMusicVagCommand(user_cmd, 1);
      if (!stereo_internal_cmd) {
        // it failed - give up on this message!
        ReleaseMessage(internal_cmd);
        FreeVagCmd(internal_cmd);
        internal_cmd = nullptr;
        ASSERT_NOT_REACHED();
      } else {
        // stop existing stereo command
        // if ((*(uint*)&stereo_internal_cmd->bit_running & 0xffff00) != 0) {
        if (stereo_internal_cmd->flags.clocks_set || stereo_internal_cmd->flags.file_disappeared) {
          IsoStopVagStream(stereo_internal_cmd);
        }

        // set up as a stereo secondary
        stereo_internal_cmd->flags.stereo_secondary = 1;
        internal_cmd->stereo_sibling = stereo_internal_cmd;
        stereo_internal_cmd->stereo_sibling = internal_cmd;

        auto name_len = strlen(internal_cmd->name);
        strncpyz(stereo_internal_cmd->name, internal_cmd->name, 0x31);
        if (name_len < 0x30) {
          strncpyz(stereo_internal_cmd->name + name_len, " (stereo)", 0x31 - name_len);
        }
        stereo_internal_cmd->id = ~internal_cmd->id;
        stereo_internal_cmd->vag_dir_entry = internal_cmd->vag_dir_entry;
      }
    }

    // abort if no command
    if (!internal_cmd) {
      return;
    }

    // start us out as paused
    internal_cmd->flags.paused = 1;
    if (stereo_internal_cmd) {
      stereo_internal_cmd->flags.paused = 1;
    }

    if (QueueMessage(internal_cmd, 5) == 0) {
      // failed to queue, clear our commands and release the message.
      FreeVagCmd(internal_cmd);
      if ((internal_cmd->vag_dir_entry->words[1] & 0x400U) != 0) {
        FreeVagCmd(stereo_internal_cmd);
      }
      ReleaseMessage(internal_cmd);
    } else {
      auto* vag_dir_entry = internal_cmd->vag_dir_entry;
      if (!vag_dir_entry) {
        // not really sure how this can happen...
        internal_cmd->m_pBaseFile = nullptr;
        ASSERT_NOT_REACHED();  // just so I can learn when this happens.
      } else {
        // need to understand this better, but it seems like we can pick between two different files
        // to actually load from...
        const ISOFileDef* filedef = nullptr;
        if (((uint)vag_dir_entry->words[1] >> 0xb & 1) == 0) {
          filedef = internal_cmd->file_def;
        } else {
          filedef = internal_cmd->vag_file_def;
        }

        // open the file!!
        auto* base_file = get_file_system()->OpenWAD(filedef, vag_dir_entry->words[1] >> 16);
        internal_cmd->m_pBaseFile = base_file;

        // determine the reading rate of the music
        if (base_file) {
          u32 rate_idx = 0x3c & (internal_cmd->vag_dir_entry->words[1] >> 10);
          ASSERT((rate_idx % 4) == 0);
          constexpr int rates[16] = {0xFA00, 0x1F40, 0x3E80, 0x5DC0, 0x7D00, 0x9C40,
                                     0xBB80, 0xDAC0, 0xAC44, 0x1589, 0x2B11, 0x409A,
                                     0x5622, 0x6BAB, 0x8133, 0x96BC};

          int rate = rates[rate_idx];
          if (((internal_cmd->vag_dir_entry->words[1] >> 10) & 1) == 0) {
            rate = rate << 2;
          } else {
            rate = rate << 3;
          }
          base_file->m_ReadRate = rate / 7;
        }
      }

      SetVagStreamName(internal_cmd, 0x30);
      if (stereo_internal_cmd) {
        SetVagStreamName(stereo_internal_cmd, 0x30);
      }

      internal_cmd->callback = ProcessVAGData;
      internal_cmd->status = 2;
      internal_cmd->flags.paused = 0;

      if (stereo_internal_cmd) {
        stereo_internal_cmd->flags.paused = 0;
      }
      internal_cmd->flags.running = 1;
      if (stereo_internal_cmd) {
        stereo_internal_cmd->flags.running = 1;
      }
      internal_cmd->status = 2;
      set_active_a(internal_cmd, 1);
      set_active_b(internal_cmd, 1);

      // load tweak value
      g_nMusicFadeDir = 0;
      g_nMusicFade = 0x10000;
      g_nMusicTweak = 0x80;
      for (u32 i = 0; i < gMusicTweakInfo.TweakCount; i++) {
        if (strcmp(gMusicTweakInfo.MusicTweak[i].MusicName, name) == 0) {
          g_nMusicTweak = gMusicTweakInfo.MusicTweak[i].VolumeAdjust;
        }
      }
    }

    if (!internal_cmd) {
      return;
    }
  }
  SetVagClock(internal_cmd);
}

void IsoQueueVagStream(ISO_VAGCommand* cmd) {
  ASSERT_NOT_REACHED();  // oof
}

void IsoPlayVagStream(ISO_VAGCommand* cmd) {
  ASSERT_NOT_REACHED();  // oof
}

void IsoStopVagStream(ISO_VAGCommand* cmd) {
  ASSERT_NOT_REACHED();  // oof
}

void ProcessMusic() {
  ASSERT_NOT_REACHED();  // oof
}

u32 ISOThread() {
  ASSERT_NOT_REACHED();  // oof
}

}  // namespace jak3