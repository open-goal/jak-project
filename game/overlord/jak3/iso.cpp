#include "iso.h"

#include <cstring>

#include "common/util/Assert.h"

#include "game/overlord/jak3/dma.h"
#include "game/overlord/jak3/iso_api.h"
#include "game/overlord/jak3/iso_cd.h"
#include "game/overlord/jak3/iso_queue.h"
#include "game/overlord/jak3/overlord.h"
#include "game/overlord/jak3/rpc_interface.h"
#include "game/overlord/jak3/sbank.h"
#include "game/overlord/jak3/spustreams.h"
#include "game/overlord/jak3/srpc.h"
#include "game/overlord/jak3/ssound.h"
#include "game/overlord/jak3/stream.h"
#include "game/overlord/jak3/streamlist.h"
#include "game/overlord/jak3/vag.h"
#include "game/sce/iop.h"
#include "game/sound/sndshim.h"

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
int g_nMusicSemaphore = 0;
bool g_bMusicIsPaused = false;
bool g_bMusicPause = false;
bool g_bAnotherMusicPauseFlag = false;
char g_szCurrentMusicName[0x30];
char g_szTargetMusicName[0x30];
int g_nActiveMusicStreams = 0;
bool g_bVagCmdsInitialized = false;
u32 time_of_last_unknown_rate_drive_op = 0;
ISO_DGOCommand sLoadDGO;
RPC_Dgo_Cmd sRPCBuff[1];
constexpr int kRpcBuffSize = sizeof(RPC_Dgo_Cmd);

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
  g_nMusicSemaphore = 0;
  g_bMusicIsPaused = false;
  g_bMusicPause = false;
  g_szCurrentMusicName[0] = 0;
  g_szTargetMusicName[0] = 0;
  sLoadDGO = {};
  g_nActiveMusicStreams = 0;
  g_bVagCmdsInitialized = false;
  time_of_last_unknown_rate_drive_op = 0;
  sRPCBuff[0] = {};
}

/*!
 * Initialize the filesystem driver, then send a message to the sync messagebox
 */
void InitDriver() {
  // init the filesystem
  if (get_file_system()->Init() == 0) {
    g_nISOInitFlag = 0;
  }
  lg::info("sending mbox for init driver");
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
  return ReceiveMbx(&msg_packet, box);
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
  thread_param.initPriority = 0x80;  // changed to be lower priority
  thread_param.attr = TH_C;
  thread_param.option = 0;
  strcpy(thread_param.name, "ISO");
  g_nISOThreadID = CreateThread(&thread_param);
  ASSERT(g_nISOThreadID >= 0);

  thread_param.entry = DGOThread;
  thread_param.initPriority = 0x38;
  thread_param.attr = TH_C;
  thread_param.stackSize = 0x900;
  thread_param.option = 0;
  strcpy(thread_param.name, "DGO");
  g_nDGOThread = CreateThread(&thread_param);
  ASSERT(g_nDGOThread >= 0);

  thread_param.entry = STRThread;
  thread_param.initPriority = 0x39;
  thread_param.attr = TH_C;
  thread_param.stackSize = 0x900;
  thread_param.option = 0;
  strcpy(thread_param.name, "STR");
  g_nSTRThreadID = CreateThread(&thread_param);
  ASSERT(g_nSTRThreadID >= 0);

  thread_param.attr = TH_C;
  thread_param.entry = PLAYThread;
  thread_param.initPriority = 0x35;
  thread_param.stackSize = 0x900;
  thread_param.option = 0;
  strcpy(thread_param.name, "Play");
  g_nPlayThreadID = CreateThread(&thread_param);
  ASSERT(g_nPlayThreadID >= 0);

  StartThread(g_nISOThreadID, 0);
  StartThread(g_nDGOThread, 0);
  StartThread(g_nSTRThreadID, 0);
  StartThread(g_nPlayThreadID, 0);
  WaitMbx(g_nSyncMbx);

  const ISOFileDef* vagdir_file = FindISOFile("VAGDIR.AYB");
  if (vagdir_file) {
    int load_status = LoadISOFileToIOP(vagdir_file, &g_VagDir, sizeof(g_VagDir));
    if (load_status) {
      ASSERT(g_VagDir.vag_magic_1 == 0x41574756);
      ASSERT(g_VagDir.vag_magic_2 == 0x52494444);
    } else {
      lg::warn("Failed to load vagdir file");
      g_VagDir.num_entries = 0;
    }
  } else {
    lg::warn("Failed to find vagdir file");
    g_VagDir.num_entries = 0;
  }

  // splash screen load was here...
  ASSERT(g_nISOInitFlag == 0);
}

const ISOFileDef* FindISOFile(const char* name) {
  return get_file_system()->Find(name);
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

    internal_cmd->msg_type = user_cmd->msg_type;
    internal_cmd->mbox_reply = user_cmd->mbox_reply;
    internal_cmd->thread_to_wake = user_cmd->thread_to_wake;
    internal_cmd->callback = user_cmd->callback;

    internal_cmd->m_pBaseFile = user_cmd->m_pBaseFile;
    internal_cmd->priority = user_cmd->priority;
    internal_cmd->file_def = user_cmd->file_def;

    // copy part of the command set by the user
    internal_cmd->vag_file_def = user_cmd->vag_file_def;
    internal_cmd->vag_dir_entry = user_cmd->vag_dir_entry;
    ASSERT(strlen(name) < 0x30);
    strncpy(internal_cmd->name, name, 0x30);
    internal_cmd->play_volume = user_cmd->play_volume;
    internal_cmd->id = user_cmd->id;
    internal_cmd->plugin_id = user_cmd->plugin_id;
    internal_cmd->maybe_sound_handler = user_cmd->maybe_sound_handler;
    internal_cmd->oog = user_cmd->oog;
    internal_cmd->dolby_pan_angle = user_cmd->dolby_pan_angle;
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
        if (((u32)vag_dir_entry->words[1] >> 0xb & 1) == 0) {
          filedef = internal_cmd->file_def;
        } else {
          filedef = internal_cmd->vag_file_def;
        }

        // open the file!!
        ovrld_log(LogCategory::VAG_SETUP, "vag dir entry offset is {}",
                  vag_dir_entry->words[1] >> 16);
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
      internal_cmd->status = EIsoStatus::OK_2;
      internal_cmd->flags.paused = 0;

      if (stereo_internal_cmd) {
        stereo_internal_cmd->flags.paused = 0;
      }
      internal_cmd->flags.running = 1;
      if (stereo_internal_cmd) {
        stereo_internal_cmd->flags.running = 1;
      }
      internal_cmd->status = EIsoStatus::OK_2;
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

/*!
 * Get a VAG command ready for playback, but don't actually start the audio yet.
 */
void IsoQueueVagStream(ISO_VAGCommand* user_cmd) {
  ASSERT(user_cmd);
  ASSERT(!user_cmd->music_flag);  // can't use music commands with this function
  ASSERT(user_cmd->id);
  ISO_VAGCommand* internal_stereo_cmd = nullptr;

  // mysterious case to reject a command
  if (user_cmd->vag_dir_entry && (user_cmd->vag_dir_entry->words[1] & 0x400U) != 0 &&
      HowManyBelowThisPriority(user_cmd->priority_pq) < 2) {
    ovrld_log(LogCategory::WARN, "mysterious rejection of a queued vag stream");
    return;
  }

  // see if we already have a command for this
  ISO_VAGCommand* internal_cmd = FindThisVagStream(user_cmd->name, user_cmd->id);
  if (!internal_cmd) {
    // try allocating one
    internal_cmd = SmartAllocVagCmd(user_cmd);
    if (!internal_cmd) {
      // no more commands!
      return;
    }

    ovrld_log(LogCategory::VAG_SETUP, "IsoQueueVagStream allocating for {} {}", user_cmd->name,
              user_cmd->id);
    // clear active flags
    set_active_a(internal_cmd, 0);
    set_active_b(internal_cmd, 0);
    set_active_c(internal_cmd, 0);
    set_active_a(user_cmd, 0);
    set_active_b(user_cmd, 0);
    set_active_c(user_cmd, 0);
    user_cmd->flags.saw_chunks1 = 0;
    user_cmd->flags.running = 0;
    // if ((*(uint*)&internal_cmd->bit_running & 0xffff00) != 0) {
    // if we're playing it, stop it.
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

    internal_cmd->msg_type = user_cmd->msg_type;
    internal_cmd->mbox_reply = user_cmd->mbox_reply;
    internal_cmd->thread_to_wake = user_cmd->thread_to_wake;
    internal_cmd->callback = user_cmd->callback;

    internal_cmd->m_pBaseFile = user_cmd->m_pBaseFile;
    internal_cmd->priority = user_cmd->priority;
    internal_cmd->file_def = user_cmd->file_def;

    internal_cmd->vag_file_def = user_cmd->vag_file_def;
    internal_cmd->vag_dir_entry = user_cmd->vag_dir_entry;
    strncpy(internal_cmd->name, user_cmd->name, 0x30);
    internal_cmd->id = user_cmd->id;
    internal_cmd->play_volume = user_cmd->play_volume;
    internal_cmd->plugin_id = user_cmd->plugin_id;
    internal_cmd->maybe_sound_handler = user_cmd->maybe_sound_handler;
    internal_cmd->oog = user_cmd->oog;
    internal_cmd->dolby_pan_angle = user_cmd->dolby_pan_angle;
    internal_cmd->art_flag = user_cmd->art_flag;
    internal_cmd->movie_flag = user_cmd->movie_flag;

    InitVAGCmd(internal_cmd, 1);

    internal_cmd->flags.scanned = 1;

    // check if we're a stereo command
    if ((internal_cmd->vag_dir_entry) && ((internal_cmd->vag_dir_entry->words[1] & 0x400U) != 0)) {
      internal_stereo_cmd = SmartAllocVagCmd(user_cmd);
      if (!internal_stereo_cmd) {
        // allocating stereo failed, give up.
        internal_cmd->flags.scanned = 0;
        ASSERT_NOT_REACHED();
        ReleaseMessage(internal_cmd);
        RemoveVagCmd(internal_cmd);
        FreeVagCmd(internal_cmd);
        internal_cmd = nullptr;
      } else {
        // if ((*(uint*)&internal_stereo_cmd->bit_running & 0xffff00) != 0) {
        if (internal_stereo_cmd->flags.clocks_set || internal_stereo_cmd->flags.file_disappeared) {
          IsoStopVagStream(internal_stereo_cmd);
        }
        internal_cmd->stereo_sibling = internal_stereo_cmd;
        internal_stereo_cmd->flags.stereo_secondary = 1;
        internal_stereo_cmd->stereo_sibling = internal_cmd;
        auto name_len = strlen(internal_cmd->name);
        strncpyz(internal_stereo_cmd->name, internal_cmd->name, 0x31);
        if (name_len < 0x30) {
          strncpyz(internal_stereo_cmd->name + name_len, " (stereo)", 0x31 - name_len);
        }
        internal_stereo_cmd->id = ~internal_cmd->id;
        internal_stereo_cmd->vag_dir_entry = internal_cmd->vag_dir_entry;
        internal_stereo_cmd->flags.scanned = 1;
      }
    }

    // return if alloc failed
    if (!internal_cmd) {
      return;
    }

    if (QueueMessage(internal_cmd, 5) == 0) {
      // queueing failed.
      internal_cmd->flags.scanned = 0;
      ASSERT_NOT_REACHED();
      RemoveVagCmd(internal_cmd);
      FreeVagCmd(internal_cmd);
      if ((internal_cmd->vag_dir_entry->words[1] & 0x400U) != 0) {
        internal_stereo_cmd->flags.scanned = 0;
        RemoveVagCmd(internal_stereo_cmd);
        FreeVagCmd(internal_stereo_cmd);
      }
      ReleaseMessage(internal_cmd);
    } else {
      auto* vag_dir_entry = internal_cmd->vag_dir_entry;
      if (!vag_dir_entry) {
        // not really sure how this can happen...
        internal_cmd->m_pBaseFile = nullptr;
        // ASSERT_NOT_REACHED();  // just so I can learn when this happens.
      } else {
        // need to understand this better, but it seems like we can pick between two different files
        // to actually load from...
        const ISOFileDef* filedef = nullptr;
        if (((u32)vag_dir_entry->words[1] >> 0xb & 1) == 0) {
          filedef = internal_cmd->file_def;
        } else {
          filedef = internal_cmd->vag_file_def;
        }
        auto* base_file = get_file_system()->OpenWAD(filedef, vag_dir_entry->words[1] >> 16);
        internal_cmd->m_pBaseFile = base_file;
        if (!base_file) {
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
      if (user_cmd->art_flag != 0) {
        internal_cmd->flags.art = 1;
      }
      if (user_cmd->movie_flag != 0) {
        internal_cmd->flags.movie = 1;
      }

      internal_cmd->flags.paused = 1;
      SetNewVagCmdPri(internal_cmd, user_cmd->priority_pq);
      if (internal_stereo_cmd) {
        internal_stereo_cmd->flags.paused = 1;
        internal_stereo_cmd->flags.scanned = 1;
        SetNewVagCmdPri(internal_stereo_cmd, 10);
      }
      SetVagStreamName(internal_cmd, 0x30);
      if (internal_stereo_cmd) {
        SetVagStreamName(internal_stereo_cmd, 0x30);
      }
      internal_cmd->status = EIsoStatus::OK_2;
      internal_cmd->callback = ProcessVAGData;
    }
    if (!internal_cmd) {
      return;
    }
  }
  SetVagClock(internal_cmd);
}

/*!
 * Actually start playback of audio, requested from ISO thread functions.
 */
void IsoPlayVagStream(ISO_VAGCommand* user_cmd) {
  ASSERT(user_cmd);
  ASSERT(!user_cmd->music_flag);
  ISO_VAGCommand* stereo_cmd = user_cmd->stereo_sibling;
  ISO_VAGCommand* internal_cmd = FindThisVagStream(user_cmd->name, user_cmd->id);

  // update to running only if we aren't already.
  if (internal_cmd && (internal_cmd->flags.running == 0)) {
    internal_cmd->play_volume = user_cmd->play_volume;
    if (internal_cmd->flags.paused != 0) {
      if (g_bExtPause) {
        g_bExtResume = true;
      }
      if (internal_cmd->flags.saw_chunks1 == 0) {
        internal_cmd->flags.paused = 0;
        if (stereo_cmd) {
          stereo_cmd->flags.paused = 0;
        }
      } else {
        ovrld_log(LogCategory::VAG_SETUP, "IsoPlayVagStream is unpausing {}", internal_cmd->name);
        UnPauseVAG(internal_cmd);
      }
      if (user_cmd->priority_pq < 3) {
        // this printed a "ruins fix" message, seems like this is a total hack!!
        SetNewVagCmdPri(user_cmd, 7);
      }
    }
    internal_cmd->flags.running = 1;
    if (stereo_cmd) {
      stereo_cmd->flags.running = 1;
    }
    if (internal_cmd) {
      SetVagClock(internal_cmd);
    }
  }
}

void IsoStopVagStream(ISO_VAGCommand* cmd) {
  auto id = cmd->id;
  ISO_VAGCommand* internal_cmd;

  // handle music/audio separately here
  if (cmd->music_flag != 0) {
    if (id == 0) {  // no id, must use name
      if (cmd->name[0] == 0) {
        ASSERT_NOT_REACHED();  // shouldn't happen
        return;
      }

      // terminate all with this name.
      while (internal_cmd = FindMusicStreamName(cmd->name), internal_cmd) {
        ovrld_log(LogCategory::VAG_SETUP, "IsoStopVagStream is terminating {} (1)",
                  internal_cmd->name);
        TerminateVAG(internal_cmd);
      }
      return;
    }

    // we have an id, just terminate that.
    internal_cmd = FindThisMusicStream(cmd->name, id);
    if (!internal_cmd) {
      return;
    }
    ovrld_log(LogCategory::VAG_SETUP, "IsoStopVagStream is terminating {} (2)", internal_cmd->name);
    TerminateVAG(internal_cmd);
    return;
  }

  // flag - this controls if we do the AnyVagRunning check or not.
  // not really sure why...
  bool flag = false;

  if (id == 0) {
    if (cmd->name[0] == 0) {
      return;
    }
    while (internal_cmd = FindVagStreamName(cmd->name), internal_cmd) {
      flag = true;
      ovrld_log(LogCategory::VAG_SETUP, "IsoStopVagStream is terminating {} (3)",
                internal_cmd->name);
      TerminateVAG(internal_cmd);
    }

    if (!flag) {
      return;
    }
  } else {
    internal_cmd = FindThisVagStream(cmd->name, id);
    if (!internal_cmd) {
      return;
    }
    ovrld_log(LogCategory::VAG_SETUP, "IsoStopVagStream is terminating {} (4)", internal_cmd->name);
    TerminateVAG(internal_cmd);
  }

  if (AnyVagRunning() == 0) {
    g_bExtPause = false;
    g_bExtResume = false;
  }
}

void ProcessMusic() {
  ISO_VAGCommand* cmd = nullptr;
  WaitSema(g_nMusicSemaphore);

  // handle unpausing request
  if (g_bMusicIsPaused && !g_bMusicPause && !g_bAnotherMusicPauseFlag) {
    cmd = FindMusicStreamName(g_szCurrentMusicName);
    if (cmd && cmd->id && !cmd->flags.stop) {  // can't unpause if stopped.
      UnPauseVAG(cmd);
    }
    g_bMusicIsPaused = false;
  }

  // handle pausing request.
  if (!g_bMusicIsPaused && g_bMusicPause) {
    cmd = FindMusicStreamName(g_szCurrentMusicName);
    if (cmd && cmd->id && !cmd->flags.stop) {
      PauseVAG(cmd);
    }
    g_bMusicIsPaused = true;
  }

  // handle playing music updates.
  if (g_bMusicIsPaused == 0) {
    // first, see if we're even playing the right music:
    if (strncmp(g_szCurrentMusicName, g_szTargetMusicName, 0xf) == 0) {
      // we are! fade in the music, if it's active:
      if (0 < g_nActiveMusicStreams && g_nMusicFadeDir < 0) {
        g_nMusicFadeDir = 1;
      }

      if ((g_szCurrentMusicName[0] == 0) || (g_nActiveMusicStreams != 0)) {
        SignalSema(g_nMusicSemaphore);
        return;
      }

    } else {
      // we aren't. fade out music if the target is null, or we're currently playing music.
      if ((g_szTargetMusicName[0] == 0) || (g_szCurrentMusicName[0] != 0)) {
        if (g_nMusicFade < 1) {
          cmd = g_aVagCmds + 4;
          int i = 1;
          do {
            i--;
            // stop any non-stereo, but real music.
            if (cmd->music_flag && !cmd->flags.stereo_secondary && cmd->id) {
              IsoStopVagStream(cmd);
            }
            cmd = cmd + 1;
          } while (-1 < i);
        } else {
          g_nMusicFadeDir = -1;
        }
      }
      if (g_nMusicFade != 0) {
        SignalSema(g_nMusicSemaphore);
        return;
      }

      // if we made it through that, we're done playing old stuff.
      strncpyz(g_szCurrentMusicName, g_szTargetMusicName, 0x10);
    }

    if (g_szCurrentMusicName[0] != 0) {
      VagStreamData vsd;
      strncpy(vsd.name, g_szCurrentMusicName, 0x30);
      vsd.id = 0x29a;
      vsd.priority = 9;
      vsd.art_load = 0;
      vsd.movie_art_load = 0;
      vsd.sound_handler = 0;
      ovrld_log(LogCategory::VAG_SETUP, "ProcessMusic is changing the music to {}", vsd.name);
      PlayMusicStream(&vsd);
    }
  }

  SignalSema(g_nMusicSemaphore);
}

u32 ISOThread() {
  int priority = -1;
  g_szCurrentMusicName[0] = 0;
  g_szTargetMusicName[0] = 0;
  g_bMusicIsPaused = false;

  lg::info("top of ISO Thread");

  // file = (CISOCDFile*)0x0;
  InitBuffers();
  // bVar1 = false;
  InitVagCmds();
  g_bVagCmdsInitialized = true;
  InitDriver();

  ISO_Hdr* mbx_cmd = nullptr;
  ISO_LoadSoundbank* load_sbk_cmd = nullptr;
  ISO_LoadCommon* load_cmd = nullptr;
  char local_name[32];
  ISO_VAGCommand* vag_cmd = nullptr;
  ISO_VAGCommand* internal_vag_cmd = nullptr;

  // ISOFileDef* file_def = nullptr;

  while (true) {
    dma_intr_hack();
    // Part 1: Handle incoming messages from the user:

    int poll_result = PollMbx((MsgPacket**)&mbx_cmd, g_nISOMbx);
    if (poll_result == KE_OK) {
      if (mbx_cmd->msg_type == ISO_Hdr::MsgType::ABADBABE) {
        // what is this garbage
        ASSERT_NOT_REACHED();
        ReleaseMessage(mbx_cmd);
      } else {
        set_active_a(mbx_cmd, 0);
        set_active_b(mbx_cmd, 0);
        set_active_c(mbx_cmd, 0);

        // iVar3 = (cmd->header).kind;
        mbx_cmd->callback = NullCallback;
        mbx_cmd->m_pBaseFile = nullptr;
        auto msg_kind = mbx_cmd->msg_type;

        ovrld_log(LogCategory::ISO_QUEUE, "Incoming message to the ISO Queue with type 0x{:x}",
                  (int)msg_kind);

        // if we're a simple file loading command:
        if (msg_kind == ISO_Hdr::MsgType::LOAD_EE || msg_kind == ISO_Hdr::MsgType::LOAD_EE_CHUNK ||
            msg_kind == ISO_Hdr::MsgType::LOAD_IOP ||
            msg_kind == ISO_Hdr::MsgType::LOAD_SOUNDBANK) {
          priority = 3;  // default priority for file loads is 3... unless we're loading a soundbank
          // in which case there's a bizarre special case here:
          load_cmd = (ISO_LoadCommon*)mbx_cmd;

          if (msg_kind == ISO_Hdr::MsgType::LOAD_SOUNDBANK) {
            load_sbk_cmd = (ISO_LoadSoundbank*)mbx_cmd;
            priority = 0;
            if (load_sbk_cmd->priority == 2) {
              priority = 2;
            } else {
              if (load_sbk_cmd->priority == 10) {
                priority = 4;
              }
            }
          }

          if (QueueMessage(mbx_cmd, priority) == 0) {
            ovrld_log(LogCategory::WARN, "Failed to queue incoming iso message");
            goto LAB_00006b18;
          }

          // iVar3 = (cmd->header).kind;
          // handle opening the file:
          switch (msg_kind) {
            case ISO_Hdr::MsgType::LOAD_EE_CHUNK: {
              ovrld_log(LogCategory::ISO_QUEUE, "Opening File {} for EE Chunk Load offset {}",
                        mbx_cmd->file_def->name.data, ((ISO_LoadSingle*)mbx_cmd)->sector_offset);
              mbx_cmd->m_pBaseFile = get_file_system()->Open(
                  mbx_cmd->file_def, ((ISO_LoadSingle*)mbx_cmd)->sector_offset, 1);
            } break;
            case ISO_Hdr::MsgType::LOAD_IOP:
            case ISO_Hdr::MsgType::LOAD_EE:
              ovrld_log(LogCategory::ISO_QUEUE, "Opening File {} for Load {}",
                        msg_kind == ISO_Hdr::MsgType::LOAD_EE ? "EE" : "IOP",
                        mbx_cmd->file_def->name.data);
              mbx_cmd->m_pBaseFile = get_file_system()->Open(mbx_cmd->file_def, -1, 1);
              break;
            case ISO_Hdr::MsgType::LOAD_SOUNDBANK: {
              ovrld_log(LogCategory::ISO_QUEUE, "Opening for LOAD_SOUNDBANK {} ",
                        load_sbk_cmd->name);
              // build name
              ASSERT(load_sbk_cmd->name);
              strncpy(local_name, load_sbk_cmd->name, 0xc);
              local_name[8] = 0;
              strcat(local_name, ".sbk");
              mbx_cmd->file_def = get_file_system()->Find(local_name);
              ASSERT(mbx_cmd->file_def);
              mbx_cmd->m_pBaseFile = get_file_system()->Open(mbx_cmd->file_def, -1, 1);
              ASSERT(mbx_cmd->m_pBaseFile);
            } break;
            default:
              ASSERT_NOT_REACHED();
          }

          // if we failed to open, bail
          if (!mbx_cmd->m_pBaseFile) {
            ASSERT_NOT_REACHED();
            mbx_cmd->status = EIsoStatus::ERROR_OPENING_FILE_8;
            UnqueueMessage(mbx_cmd);
            ReturnMessage(mbx_cmd);
            // this goes somewhere else...
          }

          // set up lengths based on the actual file length on disc.
          load_cmd->dest_ptr = load_cmd->addr;
          load_cmd->progress_bytes = 0;
          load_cmd->length_to_copy = get_file_system()->GetLength(mbx_cmd->file_def);
          // pIVar5 = length
          if (msg_kind == ISO_Hdr::MsgType::LOAD_SOUNDBANK) {
            load_cmd->maxlen = load_cmd->length_to_copy;
          } else {
            ASSERT(load_cmd->length_to_copy);
            // trim copy size to the max buffer length given.
            if (load_cmd->length_to_copy > load_cmd->maxlen) {
              load_cmd->length_to_copy = load_cmd->maxlen;
            }
          }

          // set up callback
          switch (msg_kind) {
            case ISO_Hdr::MsgType::LOAD_IOP:
              mbx_cmd->callback = CopyDataToIOP;
              break;
            case ISO_Hdr::MsgType::LOAD_SOUNDBANK:
              mbx_cmd->callback = CopyDataSbkLoad;
              break;
            case ISO_Hdr::MsgType::LOAD_EE:
            case ISO_Hdr::MsgType::LOAD_EE_CHUNK:
              mbx_cmd->callback = CopyDataToEE;
              break;
            default:
              ASSERT_NOT_REACHED();
          }

          mbx_cmd->status = EIsoStatus::OK_2;
          set_active_a(mbx_cmd, 1);
        } else {
          switch (msg_kind) {
            case ISO_Hdr::MsgType::DGO_LOAD:
              if (QueueMessage(mbx_cmd, 1) != 0) {
                // modified for non compressed dgos
                ovrld_log(LogCategory::ISO_QUEUE, "Opening {} for DGO Load",
                          mbx_cmd->file_def->name.data);
                mbx_cmd->m_pBaseFile = get_file_system()->Open(mbx_cmd->file_def, -1, 1);
                if (mbx_cmd->m_pBaseFile) {
                  mbx_cmd->callback = RunDGOStateMachine;
                  mbx_cmd->status = EIsoStatus::OK_2;
                  ((ISO_DGOCommand*)mbx_cmd)->state = ISO_DGOCommand::State::INIT;
                  set_active_a(mbx_cmd, 1);
                } else {
                  ASSERT_NOT_REACHED();
                  UnqueueMessage(mbx_cmd);
                  ASSERT(sLoadDGO.msg_type != ISO_Hdr::MsgType::MSG_0);
                  SendMbx(g_nISOMbx, &sLoadDGO);
                }
              }
              break;
            case ISO_Hdr::MsgType::VAG_PAUSE:
              ovrld_log(LogCategory::ISO_QUEUE, "VagPause (all of them)");
              if (g_bExtPause == 0) {
                SetVagStreamsNoStart(1);
                int iVar3 = AnyVagRunning();
                if (iVar3 != 0) {
                  PauseVagStreams(0);
                }
                g_bExtPause = true;
                g_bExtResume = iVar3 != 0;
              }
              ReturnMessage(mbx_cmd);
              break;
            case ISO_Hdr::MsgType::VAG_UNPAUSE:
              ovrld_log(LogCategory::ISO_QUEUE, "VagUnPause (all of them)");
              if (g_bExtPause != 0) {
                if (g_bExtResume != false) {
                  UnPauseVagStreams(0);
                }
                g_bExtPause = false;
                g_bExtResume = false;
              }
              SetVagStreamsNoStart(0);
              ReturnMessage(mbx_cmd);
              break;
            case ISO_Hdr::MsgType::VAG_SET_PITCH_VOL:
              vag_cmd = (ISO_VAGCommand*)mbx_cmd;
              ovrld_log(LogCategory::ISO_QUEUE, "VAG_SET_PITCH_VOL (id {})", vag_cmd->id);
              internal_vag_cmd = FindVagStreamId(vag_cmd->id);
              if (internal_vag_cmd) {
                ovrld_log(LogCategory::ISO_QUEUE, "VAG_SET_PITCH_VOL lookup ok, got {}",
                          internal_vag_cmd->name);
                internal_vag_cmd->pitch_cmd = vag_cmd->pitch_cmd;
                SetVAGVol(internal_vag_cmd);
              }
              ReturnMessage(vag_cmd);
              break;
            case ISO_Hdr::MsgType::ADEADBEE:
              ReturnMessage(vag_cmd);
              ExitThread();
              goto LAB_00006b18;
              break;
            default:
              ASSERT_NOT_REACHED();
          }
        }
      }
    } else {
      if (poll_result == -0x1a9) {
        // messagebox was deleted - this means we're shutting down
        return 0;
      }
      if (poll_result != -0x1a8) {
        // unknown messagebox error
        ASSERT_NOT_REACHED();
      }
    }
  LAB_00006b18:
    // Part 2: music update
    // Poll is called here... but we don't use it.
    // (**(code**)(*g_pFileSystem + 4))();
    ProcessMusic();

    // Part 3: service in-progress messages
    // get the top priority message
    bool buffer_ok = false;
    auto* cmd = GetMessage();
    CBaseFile* file = nullptr;
    bool known_read_rate = false;

    if (cmd) {
      // handle the buffering
      //      ovrld_log(LogCategory::ISO_QUEUE, "Processing Command 0x{:x} - allocating buffer\n",
      //                (int)cmd->msg_type);

      // check if we need to initialize a buffer, or if we just need to realloc pages
      file = cmd->m_pBaseFile;
      bool needs_buffer_init = false;
      if (!file) {
        needs_buffer_init = true;
        // we'd need to set buffer_ok = false later on if this is the case,
        // but I dont think this can happen.
        ASSERT_NOT_REACHED();
      } else {
        if (file->m_Buffer.m_eBufferType == CBuffer::BufferType::EBT_FREE) {
          needs_buffer_init = true;
        }
      }

      // set up buffer
      if (needs_buffer_init) {
        buffer_ok =
            file->InitBuffer(cmd->callback == ProcessVAGData ? CBuffer::BufferType::REQUEST_VAG
                                                             : CBuffer::BufferType::REQUEST_NORMAL,
                             cmd);
      } else {
        file->AllocPages();
        buffer_ok = true;
      }

      file = nullptr;
      if (buffer_ok == 0) {
        cmd = nullptr;
        known_read_rate = false;
      } else {
        file = cmd->m_pBaseFile;
        known_read_rate = false;
        if (file && file->m_ReadRate) {
          known_read_rate = true;
        }
        // iVar3 = (**(code**)(file->base).vtable)(file);
        //        ovrld_log(LogCategory::ISO_QUEUE, "Processing Command 0x{:x} - starting read!\n",
        //                  (int)cmd->msg_type);

        // lg::info("ISO - BeginRead");
        cmd->status = file->BeginRead();
        if (cmd->status != EIsoStatus::OK_2) {
          buffer_ok = false;
          if (cmd->m_pBaseFile) {
            cmd->m_pBaseFile->TerminateBuffer();
          }
          cmd = nullptr;
          file = nullptr;
        }
        if (!known_read_rate) {
          time_of_last_unknown_rate_drive_op = GetSystemTimeLow();
        }
      }
    }

    //    ovrld_log(LogCategory::ISO_QUEUE, "Processing Command 0x{:x} - handling message data\n",
    //              (int)cmd->msg_type);

    if (ProcessMessageData(cmd) == 0) {
      cmd = nullptr;
    }

    if (buffer_ok && cmd) {
      EIsoStatus status = EIsoStatus::ERROR_b;
      if (file) {
        status = file->SyncRead();
      }
      if (!known_read_rate) {
        time_of_last_unknown_rate_drive_op = GetSystemTimeLow();
      }
      if (status == EIsoStatus::ERROR_b) {
        if (cmd->m_pBaseFile && cmd->m_pBaseFile->m_Status != EIsoStatus::NONE_0) {
          cmd->status = EIsoStatus::OK_2;
        }
      } else {
        cmd->status = status;
        if (!cmd->active_c) {
          set_active_c(cmd, 1);
        }
      }
    }

    WaitSema(g_RequestedStreamsList.sema);
    if (g_RequestedStreamsList.pending_data == 1) {
      QueueNewStreamsFromList(&g_RequestedStreamsList);
      auto* vag_info = g_NewStreamsList.next;
      for (int i = 0; i < 4; i++) {
        if (vag_info->id) {
          ovrld_log(LogCategory::ISO_QUEUE, "ISO thread: queueing VAG {}", vag_info->name);
          QueueVAGStream(vag_info);
        }
        vag_info = vag_info->next;
      }
    }

    for (int i = 0; i < 4; i++) {
      ISO_VAGCommand* vc = &g_aVagCmds[i];
      if (!vc->music_flag && !vc->flags.stereo_secondary && !vc->flags.scanned && vc->id) {
        ovrld_log(LogCategory::ISO_QUEUE, "ISO thread: stopping {} since it is no longer requested",
                  vc->name);
        IsoStopVagStream(vc);
      }
    }

    SignalSema(g_RequestedStreamsList.sema);
    g_RequestedStreamsList.pending_data = 0;

    for (int i = 4; i < 6; i++) {
      ISO_VAGCommand* vc = &g_aVagCmds[i];
      if (vc->music_flag && !vc->flags.stereo_secondary && vc->flags.stop && vc->id) {
        ovrld_log(LogCategory::ISO_QUEUE, "ISO thread: stopping music {}", vc->name);
        IsoStopVagStream(vc);
      }
    }

    // this logic was changed so that the iso thread doesn't sleep when loading a DGO: otherwise
    // we'd spent most of our time letting the thread sleep.
    // instead, if there's an in progress DGO command, we yield.
    // this yield allows DGO RPCs to run. Additionally, the priority of the ISO thread was lowered
    // to allow the RPCs to run during this time.
    bool sleep = !DgoCmdWaiting();
    if (sleep) {
      if (buffer_ok) {
        DelayThread(4000);
      } else {
        // DelayThread(200);
        DelayThread(2000);
      }
    } else {
      YieldThread();
    }
  }
}

/*!
 * This function runs the state machine for the double-buffered DGO loading system.
 * There are a few tricks here:
 * - Each DGO file contains a number of objects.
 * - The object loading is double buffered - this state machine toggles between loading to two
 *   different buffers. While one buffer is being written, the GOAL linker is processing the other.
 * - The final object is not double buffered. Instead, it is loaded directly to the top of the heap.
 * - New! for jak 2, there is an option to not use the double buffering.
 * - New! for jak 3, there is a very complicated load cancel system
 */
EIsoStatus RunDGOStateMachine(ISO_Hdr* m) {
  auto* cmd = (ISO_DGOCommand*)m;
  // lg::info("ISO - DGO state machine");
  int send_count, receive_count;

  CBaseFile* file = cmd->m_pBaseFile;
  EIsoStatus ret_status = EIsoStatus::OK_2;
  if (!file) {
    return EIsoStatus::OK_2;
  }
  ASSERT(file->m_Buffer.m_pPageList);

  // handle page boundary crossings - after this call, our CBuffer will be set up properly for
  // processing.
  file->CheckPageBoundary();
  CBuffer* buffer = &file->m_Buffer;

  int buffer_len = (file->m_Buffer).m_nDataLength;
  ASSERT(buffer_len >= 0);

  if (cmd->state == ISO_DGOCommand::State::INIT) {
    // these counters are used for debugging the DGO sync stuff.
    cmd->sync_mbox_wait_count = 1;
    cmd->sync_ret_count = 0;
  }
  // CpuSuspendIntr(local_30);

  // process this DGO as normal, unless we've been asked to cancel this.
  if (cmd->nosync_cancel_pending_flag == 0 || cmd->selected_id != cmd->request_cancel_id) {
    // CpuResumeIntr();
    if (buffer_len == 0) {
      // nothing we can do with no data...
      goto out_of_data;
    }
    do {
      switch (cmd->state) {
        case ISO_DGOCommand::State::INIT:
          ovrld_log(LogCategory::DGO, "DGO: Starting state machine");
          cmd->state = ISO_DGOCommand::State::READ_DGO_HEADER;
          cmd->bytes_processed = 0;
          cmd->finished_first_object = 0;
          cmd->want_abort = 0;
          break;
        case ISO_DGOCommand::State::READ_DGO_HEADER: {
          // here, we work on reading the DGO file's header into our command.
          // first, compute how many bytes we want to read right now, as the max of
          // the remaining header size, and what's buffered
          int bytes_needed = sizeof(DgoHeader) - cmd->bytes_processed;
          if (buffer_len < bytes_needed) {
            bytes_needed = buffer_len;
          }

          // loop over pages - the header may span multiple pages that aren't adjacent in memory.
          while (bytes_needed) {
            // determine how many bytes to copy from this page
            int bytes_from_this_page = buffer->m_pPageList->m_pCurrentActivePage->m_pPageMemEnd -
                                       file->m_Buffer.m_pCurrentData + 1;
            if (bytes_needed <= bytes_from_this_page) {
              bytes_from_this_page = bytes_needed;
            }
            ovrld_log(LogCategory::DGO, "DGO: reading {} bytes of dgo header",
                      bytes_from_this_page);
            // copy data from buffer into command
            memcpy(((u8*)&cmd->dgo_header) + cmd->bytes_processed, file->m_Buffer.m_pCurrentData,
                   bytes_from_this_page);

            // advance buffer and page
            buffer->AdvanceCurrentData(bytes_from_this_page);
            file->CheckPageBoundary();

            // advance progress
            cmd->bytes_processed = bytes_from_this_page + cmd->bytes_processed;
            buffer_len = buffer_len - bytes_from_this_page;
            bytes_needed = bytes_needed - bytes_from_this_page;
          }

          // check if we got the whole header
          if (cmd->bytes_processed == sizeof(DgoHeader)) {
            ovrld_log(LogCategory::DGO, "DGO: got dgo header: {} with {} objects",
                      cmd->dgo_header.name, cmd->dgo_header.object_count);
            cmd->bytes_processed = 0;
            cmd->objects_loaded = 0;
            if (cmd->dgo_header.object_count == 1) {
              // if we have only 1 object, go directly to loading to the top buffer
              cmd->ee_dest_buffer = cmd->buffer_top;
              cmd->state = ISO_DGOCommand::State::READ_OBJ_HEADER;
              cmd->buffer_toggle = 0;
            } else {
              // otherwise, start with buffer!
              cmd->buffer_toggle = 1;
              cmd->ee_dest_buffer = cmd->buffer1;
              cmd->state = ISO_DGOCommand::State::READ_OBJ_HEADER;
            }
          }
        } break;
        case ISO_DGOCommand::State::FINISH_OBJ:

          // sync with EE - if we're loading double-buffered, wait on the EE
          // note that we don't wait on the first object, since both buffers start empty,
          // and we can safely fill both with no syncs.
          // the order of synchronization is a little bit strange. The EE must tell us that it's
          // finished processing buffer A before we tell the EE the location of buffer B.
          // This is needed to get the sync right for the last object - we want the EE to run
          // through all the buffers, then we load the final object, then we notify it. If the EE
          // wouldn't tell us it was done until it got the next object, we'd be unable to do this.
          if (cmd->finished_first_object != 0 && cmd->buffer1 != cmd->buffer2) {
            if (LookSyncMbx() == 0)
              goto exit_no_sync;

            ovrld_log(LogCategory::DGO,
                      "DGO: finished object (2buffer), and got sync message from EE or cancel");
            // iVar3 = 6;
            if (cmd->want_abort != 0) {
              ovrld_log(LogCategory::DGO, "DGO: cancel!! (1)");
              cmd->state = ISO_DGOCommand::State::FINISH_DGO;
              break;
            }
          }

          // for double buffer, notify the EE that we've finished loading.
          if (cmd->buffer1 != cmd->buffer2) {
            cmd->status = EIsoStatus::OK_2;
            cmd->selected_buffer = cmd->buffer_toggle != 1 ? cmd->buffer2 : cmd->buffer1;
            ovrld_log(LogCategory::DGO,
                      "DGO: finished object (2buffer) - notifying EE of location");
            ReturnMessage(cmd);
            sLoadDGO.sync_ret_count = sLoadDGO.sync_ret_count + 1;
          }

          // for single buffer, sync with EE so we know the next location to load.
          // note that we've already returned the message for the single buffer case
          if ((cmd->buffer1 == cmd->buffer2) &&
              (cmd->objects_loaded + 1 < (s32)cmd->dgo_header.object_count)) {
            if (LookSyncMbx() == 0)
              goto exit_no_sync;
            ovrld_log(LogCategory::DGO,
                      "DGO: finished object (1buffer), and got sync message from EE or cancel");
            if (cmd->want_abort != 0) {
              ovrld_log(LogCategory::DGO, "DGO: cancel!! (2)");
              cmd->state = ISO_DGOCommand::State::FINISH_DGO;
              break;
            }
          }
          cmd->finished_first_object = 1;
          if (cmd->buffer_toggle == 1) {
            cmd->ee_dest_buffer = cmd->buffer2;
            cmd->buffer_toggle = 2;
          } else {
            cmd->buffer_toggle = 1;
            cmd->ee_dest_buffer = cmd->buffer1;
          }

          if (cmd->objects_loaded + 1 == (int)cmd->dgo_header.object_count) {
            cmd->state = ISO_DGOCommand::State::READ_LAST_OBJ;
          } else {
            cmd->state = ISO_DGOCommand::State::READ_OBJ_HEADER;
          }
          //        LAB_000073f8:
          //          cmd->state = iVar3;
          break;
        case ISO_DGOCommand::State::READ_LAST_OBJ:
          // do an extra sync here to wait for the EE to finish processing both temporary buffers.
          // the next load will be to the heap top, which may overlap the temp buffers.
          // lg::warn("in read last obj!");
          if (LookSyncMbx() == 0)
            goto exit_no_sync;
          ovrld_log(LogCategory::DGO,
                    "DGO: got final object sync message - can start running that now");
          if (cmd->want_abort != 0) {
            cmd->state = ISO_DGOCommand::State::FINISH_DGO;
            ovrld_log(LogCategory::DGO, "DGO: cancel!! (3)");
          } else {
            cmd->ee_dest_buffer = cmd->buffer_top;
            cmd->state = ISO_DGOCommand::State::READ_OBJ_HEADER;
            cmd->buffer_toggle = 0;
          }

          break;
        case ISO_DGOCommand::State::READ_OBJ_HEADER: {
          int bytes_needed = sizeof(ObjectHeader) - cmd->bytes_processed;
          if (buffer_len < bytes_needed) {
            bytes_needed = buffer_len;
          }
          while (bytes_needed) {
            int bytes_from_this_page = buffer->m_pPageList->m_pCurrentActivePage->m_pPageMemEnd -
                                       file->m_Buffer.m_pCurrentData + 1;
            if (bytes_needed <= bytes_from_this_page) {
              bytes_from_this_page = bytes_needed;
            }
            ASSERT(bytes_from_this_page >= 0);
            ovrld_log(LogCategory::DGO, "DGO: reading {} bytes of object header",
                      bytes_from_this_page);
            memcpy(((u8*)&cmd->obj_header) + cmd->bytes_processed, (file->m_Buffer).m_pCurrentData,
                   bytes_from_this_page);
            buffer->AdvanceCurrentData(bytes_from_this_page);
            file->CheckPageBoundary();
            cmd->bytes_processed = bytes_from_this_page + cmd->bytes_processed;
            buffer_len = buffer_len - bytes_from_this_page;
            bytes_needed = bytes_needed - bytes_from_this_page;
          }
          if (cmd->bytes_processed == sizeof(ObjectHeader)) {
            ovrld_log(LogCategory::DGO, "DGO: got object header {} {}", cmd->obj_header.name,
                      cmd->obj_header.size);
            cmd->obj_header.size = (cmd->obj_header.size + 0xf) & 0xfffffff0;
            DMA_SendToEE(cmd->ee_dest_buffer, &cmd->obj_header, sizeof(ObjectHeader), nullptr,
                         nullptr);
            cmd->ee_dest_buffer = cmd->ee_dest_buffer + sizeof(ObjectHeader);
            cmd->state = ISO_DGOCommand::State::READ_OBJ_DATA;
            cmd->bytes_processed = 0;
          }
        } break;
        case ISO_DGOCommand::State::READ_OBJ_DATA: {
          int bytes_needed = cmd->obj_header.size - cmd->bytes_processed;
          if (buffer_len < bytes_needed) {
            bytes_needed = buffer_len;
          }

          while (bytes_needed) {
            auto* page = buffer->m_pPageList->m_pCurrentActivePage;
            int bytes_from_this_page = page->m_pPageMemEnd - file->m_Buffer.m_pCurrentData + 1;
            if (bytes_needed <= bytes_from_this_page) {
              bytes_from_this_page = bytes_needed;
            }
            int ret = page->AddDmaRef();
            ASSERT(ret >= 0);

            DMA_SendToEE(cmd->ee_dest_buffer, (file->m_Buffer).m_pCurrentData, bytes_from_this_page,
                         CopyDataDmaCallback, page);
            buffer->AdvanceCurrentData(bytes_from_this_page);
            file->CheckPageBoundary();
            cmd->ee_dest_buffer = bytes_from_this_page + cmd->ee_dest_buffer;
            cmd->bytes_processed = bytes_from_this_page + cmd->bytes_processed;
            buffer_len = buffer_len - bytes_from_this_page;
            bytes_needed = bytes_needed - bytes_from_this_page;

            if (!file->m_Buffer.m_pCurrentData) {
              buffer_len = 0;
              break;
            }
          }

          if (cmd->bytes_processed == (int)cmd->obj_header.size) {
            cmd->objects_loaded = cmd->objects_loaded + 1;
            if (cmd->objects_loaded < (int)cmd->dgo_header.object_count) {
              if (cmd->buffer1 == cmd->buffer2) {
                cmd->state = ISO_DGOCommand::State::FINISH_OBJ_SINGLE_BUFFER;
              } else {
                cmd->state = ISO_DGOCommand::State::FINISH_OBJ;
              }
              cmd->bytes_processed = 0;
            } else {
              ret_status = EIsoStatus::NONE_0;
              cmd->state = ISO_DGOCommand::State::FINISH_DGO;
            }
          }
        } break;
        case ISO_DGOCommand::State::FINISH_DGO:
          ret_status = EIsoStatus::NONE_0;
          file->m_Buffer.m_pCurrentData = nullptr;
          file->m_Buffer.m_pCurrentPageStart = nullptr;
          goto out_of_data;
        case ISO_DGOCommand::State::FINISH_OBJ_SINGLE_BUFFER:
          cmd->status = EIsoStatus::OK_2;
          if (cmd->buffer_toggle == 1) {
            cmd->selected_buffer = cmd->buffer1;
          } else {
            cmd->selected_buffer = cmd->buffer2;
          }
          ReturnMessage((ISO_VAGCommand*)cmd);
          sLoadDGO.sync_ret_count = sLoadDGO.sync_ret_count + 1;
          cmd->state = ISO_DGOCommand::State::FINISH_OBJ;
      }
    } while (buffer_len);
  exit_no_sync:
    if (ret_status != EIsoStatus::NONE_0)
      goto LAB_0000743c;
  } else {
    cmd->nosync_cancel_ack = 1;
    cmd->nosync_cancel_pending_flag = 0;
    cmd->acked_cancel_id = cmd->request_cancel_id;
    send_count = sLoadDGO.sync_sent_count - sLoadDGO.sync_mbox_wait_count;
    receive_count = sLoadDGO.sync_ret_count - sLoadDGO.sync_mbox_wait_count;
    // CpuResumeIntr(local_30[0]);
    if (0 < send_count) {
      receive_count = receive_count + -1;
      WaitMbx(g_nSyncMbx);
      sLoadDGO.sync_mbox_wait_count = sLoadDGO.sync_mbox_wait_count + 1;
    }
    ret_status = EIsoStatus::IDLE_1;
    if (-1 < receive_count) {
    LAB_0000743c:
      if (buffer_len) {
        file->m_Buffer.m_nDataLength = buffer_len;
        return ret_status;
      }
      goto out_of_data;
    }
    ret_status = EIsoStatus::NONE_0;
  }
  (file->m_Buffer).m_pCurrentData = nullptr;
  (file->m_Buffer).m_pCurrentPageStart = nullptr;
out_of_data:
  (file->m_Buffer).m_nDataLength = 0;
  return ret_status;
}

u32 DGOThread() {
  sceSifQueueData dq;
  sceSifServeData serve;

  // setup RPC.
  CpuDisableIntr();
  sceSifInitRpc(0);
  sceSifSetRpcQueue(&dq, GetThreadId());
  sceSifRegisterRpc(&serve, RpcId::DGO, RPC_DGO, sRPCBuff, kRpcBuffSize, nullptr, nullptr, &dq);
  CpuEnableIntr();
  sceSifRpcLoop(&dq);
  return 0;
}

void* RPC_DGO(unsigned int fno, void* msg_ptr, int) {
  RPC_Dgo_Cmd* cmd = (RPC_Dgo_Cmd*)msg_ptr;
  switch (fno) {
    case DgoFno::LOAD:
      LoadDGO(cmd);
      break;
    case DgoFno::LOAD_NEXT:
      LoadNextDGO(cmd);
      break;
    case DgoFno::CANCEL:
      CancelDGO(cmd);
      break;
    default:
      cmd->status = 1;
      ASSERT_NOT_REACHED();
  }
  return msg_ptr;
}

void* foo = 0;

/*!
 * Send a sync message to unblock the DGO thread when doing an async cancel.
 */
bool NotifyDGO() {
  // CpuSuspendIntr(local_10);
  bool pending_cancel = sLoadDGO.nosync_cancel_ack == 0;
  if (pending_cancel) {
    sLoadDGO.sync_sent_count = sLoadDGO.sync_sent_count + 1;
  }
  // CpuResumeIntr(local_10[0]);
  if (pending_cancel) {
    SendMbx(g_nSyncMbx, &foo);
  }
  return pending_cancel;
}

void LoadDGO(RPC_Dgo_Cmd* cmd) {
  ISOFileDef* file = get_file_system()->Find(cmd->name);

  if (!file) {
    ovrld_log(LogCategory::WARN, "DGO RPC: LoadDGO {} file not found\n", cmd->name);
    cmd->status = 1;
    return;
  }
  if (sLoadDGO.last_id < cmd->cgo_id) {
    ovrld_log(LogCategory::RPC, "DGO RPC: new command ID, starting a load for {}\n", cmd->name);
    CancelDGO(nullptr);
    sLoadDGO.msg_type = ISO_Hdr::MsgType::DGO_LOAD;
    sLoadDGO.selected_id = cmd->cgo_id;
    sLoadDGO.mbox_reply = g_nDGOMbx;
    sLoadDGO.thread_to_wake = 0;
    sLoadDGO.buffer1 = (u8*)(u64)cmd->buffer1;
    sLoadDGO.buffer2 = (u8*)(u64)cmd->buffer2;
    sLoadDGO.buffer_top = (u8*)(u64)cmd->buffer_heap_top;
    sLoadDGO.file_def = file;
    // CpuSuspendIntr(local_18);
    if (0 < cmd->cgo_id - sLoadDGO.last_id) {
      sLoadDGO.last_id = cmd->cgo_id;
    }
    sLoadDGO.sync_sent_count = 1;
    sLoadDGO.nosync_cancel_ack = 0;
    // CpuResumeIntr(local_18[0]);
    ASSERT(sLoadDGO.msg_type != ISO_Hdr::MsgType::MSG_0);
    ovrld_log(LogCategory::RPC, "------------------DGO: RPC sending mbox (reply size is {})",
              MbxSize(g_nDGOMbx));
    SendMbx(g_nISOMbx, &sLoadDGO);
    ovrld_log(LogCategory::RPC, "DGO: RPC waiting mbox (now has {})", MbxSize(g_nDGOMbx));
    WaitMbx(g_nDGOMbx);
    ovrld_log(LogCategory::RPC, "DGO: RPC recv mbox: {}", int(sLoadDGO.status));
    if (sLoadDGO.status == EIsoStatus::OK_2) {
      cmd->status = 2;
      return;
    }
    if (sLoadDGO.status != EIsoStatus::NONE_0) {
      cmd->status = 1;
      sLoadDGO.msg_type = ISO_Hdr::MsgType::MSG_0;
      sLoadDGO.selected_id = -1;
      return;
    }
  } else {
    ovrld_log(LogCategory::WARN, "DGO RPC: old command ID seen for {} (got {}, saw {}), ignoring\n",
              cmd->name, cmd->cgo_id, sLoadDGO.last_id);
  }
  cmd->buffer1 = cmd->buffer_heap_top;
  cmd->status = 0;
  sLoadDGO.msg_type = ISO_Hdr::MsgType::MSG_0;
  sLoadDGO.selected_id = -1;
}

void LoadNextDGO(RPC_Dgo_Cmd* cmd) {
  if (sLoadDGO.msg_type == ISO_Hdr::MsgType::MSG_0) {
    ovrld_log(LogCategory::WARN, "DGO RPC: LoadNextDGO {} load not running! Ignoring\n", cmd->name);
    cmd->status = 1;
    return;
  }

  sLoadDGO.buffer_top = (u8*)(u64)cmd->buffer_heap_top;
  sLoadDGO.buffer1 = (u8*)(u64)cmd->buffer1;
  sLoadDGO.buffer2 = (u8*)(u64)cmd->buffer2;
  bool unblocked = NotifyDGO();  // tell dgo state machine to run
  int status = 3;
  if (unblocked != 0) {
    WaitMbx(g_nDGOMbx);  // wait for it to finish, and return to EE
    if (sLoadDGO.status == EIsoStatus::OK_2) {
      cmd->status = 2;
      cmd->buffer1 = (u32)(u64)sLoadDGO.selected_buffer;
      return;
    }
    status = 11;
    if (sLoadDGO.status == EIsoStatus::NONE_0) {
      cmd->status = 0;
      cmd->buffer1 = cmd->buffer_heap_top;
      sLoadDGO.msg_type = ISO_Hdr::MsgType::MSG_0;
      sLoadDGO.selected_id = -1;
      return;
    }
  } else {
    ovrld_log(LogCategory::WARN, "DGO RPC: LoadNextDGO {} already cancelled! Ignoring\n",
              cmd->name);
  }
  cmd->status = status;
  sLoadDGO.msg_type = ISO_Hdr::MsgType::MSG_0;
  sLoadDGO.selected_id = -1;
}

void CancelDGO(RPC_Dgo_Cmd* param_1) {
  ovrld_log(LogCategory::WARN, "DGO RPC: CancelDGO {}\n", param_1 ? param_1->name : "NO CMD");
  if (sLoadDGO.msg_type != ISO_Hdr::MsgType::MSG_0) {
    sLoadDGO.want_abort = 1;
    if (NotifyDGO()) {
      WaitMbx(g_nDGOMbx);
    }
    if (param_1) {
      param_1->status = 3;
    }
    sLoadDGO.selected_id = -1;
    sLoadDGO.msg_type = ISO_Hdr::MsgType::MSG_0;
  }
}

void CancelDGONoSync(int id) {
  ovrld_log(LogCategory::WARN, "DGO RPC: CancelDGONoSync {}\n", id);
  // CpuSuspendIntr(local_10);
  sLoadDGO.nosync_cancel_pending_flag = 1;
  if (0 < id - sLoadDGO.last_id) {
    sLoadDGO.last_id = id;
  }
  sLoadDGO.request_cancel_id = id;
  // CpuResumeIntr(local_10[0]);
}

EIsoStatus CopyDataToEE(ISO_Hdr* msg) {
  return CopyData((ISO_LoadSingle*)msg, CopyKind::EE);
}

EIsoStatus CopyDataToIOP(ISO_Hdr* msg) {
  return CopyData((ISO_LoadSingle*)msg, CopyKind::IOP);
}

EIsoStatus CopyDataSbkLoad(ISO_Hdr* msg) {
  return CopyData((ISO_LoadSingle*)msg, CopyKind::SBK);
}

void CopyDataDmaCallback(void* in) {
  ((CPage*)in)->ReleaseDmaRef();
}

EIsoStatus CopyData(ISO_LoadCommon* cmd, CopyKind kind) {
  ASSERT(cmd);
  auto* file = cmd->m_pBaseFile;
  if (file == (CISOCDFile*)0x0) {
    return EIsoStatus::ERROR_NO_FILE;
  }

  EIsoStatus status = EIsoStatus::OK_2;
  if (file->m_Buffer.m_eBufferType != CBuffer::BufferType::NORMAL) {
    CBuffer* buffer = &file->m_Buffer;
    CPage* page = nullptr;
    if (buffer->m_pPageList && buffer->m_nDataLength) {
      if (file->CheckPageBoundary()) {
        page = file->m_Buffer.m_pPageList->m_pCurrentActivePage;
      }
      if (page && cmd->progress_bytes < cmd->length_to_copy) {
        int len;
        do {
          // length we want
          len = cmd->length_to_copy - cmd->progress_bytes;

          // trim to buffered
          if (buffer->m_nDataLength < len) {
            len = buffer->m_nDataLength;
          }

          // trim to page
          if (page->m_pPageMemEnd - buffer->m_pCurrentData + 1 < len) {
            len = page->m_pPageMemEnd - buffer->m_pCurrentData + 1;
          }

          if (0 < len) {
            switch (kind) {
              case CopyKind::IOP: {
                if (page->AddRef() < 1) {
                  ASSERT_NOT_REACHED();
                }
                memcpy(cmd->dest_ptr, buffer->m_pCurrentData, len);
                if (page->ReleaseRef() < 0)
                  ASSERT_NOT_REACHED();
              } break;
              case CopyKind::EE: {
                if (page->AddDmaRef() < 1) {
                  ASSERT_NOT_REACHED();
                }
                DMA_SendToEE(cmd->dest_ptr, buffer->m_pCurrentData, len, CopyDataDmaCallback, page);
              } break;
              case CopyKind::SBK: {
                WaitSema(g_n989Semaphore);
                if (g_bSoundEnable == 0) {
                  SignalSema(g_n989Semaphore);
                  return EIsoStatus::ERROR_NO_SOUND;
                }
                if (page->AddRef() < 1) {
                  ASSERT_NOT_REACHED();
                  SignalSema(g_n989Semaphore);
                  return EIsoStatus::OK_2;
                }
                auto* bank_info = ((ISO_LoadSoundbank*)cmd)->bank_info;

                // hack: added
                if (cmd->progress_bytes == 0) {
                  snd_BankLoadFromIOPPartialEx_Start();
                }
                snd_BankLoadFromIOPPartialEx(buffer->m_pCurrentData, len, bank_info->m_nSpuMemLoc,
                                             bank_info->m_nSpuMemSize);
                if (cmd->progress_bytes + len == cmd->length_to_copy) {
                  bank_info->snd_handle = snd_BankLoadFromIOPPartialEx_Completion();
                  snd_ResolveBankXREFS();
                  // TODO: this also set field_0x28... is that needed??
                }

                if (page->ReleaseRef() < 0) {
                  ASSERT_NOT_REACHED();
                }
                SignalSema(g_n989Semaphore);
              } break;
              default:
                ASSERT_NOT_REACHED();
            }

            cmd->dest_ptr = cmd->dest_ptr + len;
            cmd->progress_bytes = len + cmd->progress_bytes;
            buffer->AdvanceCurrentData(len);
            if (!file->CheckPageBoundary())
              break;
            page = buffer->m_pPageList->m_pCurrentActivePage;
          }

          if (!page || len < 1)
            break;
        } while (true);
      }
    }
    if ((u32)cmd->progress_bytes < (u32)cmd->length_to_copy) {
      return status;
    }
    buffer->m_pPageList->CancelActivePages();
    if (status != EIsoStatus::OK_2) {
      return status;
    }
    return EIsoStatus::NONE_0;
  }
  return EIsoStatus::ERROR_NO_FILE;
}

EIsoStatus NullCallback(ISO_Hdr*) {
  return EIsoStatus::NULL_CALLBACK;
}

void set_active_a(ISO_Hdr* cmd, int val) {
  cmd->active_a = val;
}

void set_active_b(ISO_Hdr* cmd, int val) {
  cmd->active_b = val;
}

void set_active_c(ISO_Hdr* cmd, int val) {
  cmd->active_c = val;
}

}  // namespace jak3