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
int g_nMusicSemaphore = 0;
bool g_bMusicIsPaused = false;
bool g_bMusicPause = false;
bool g_bAnotherMusicPauseFlag = false;
char g_szCurrentMusicName[0x30];
char g_szTargetMusicName[0x30];
int g_nActiveMusicStreams = 0;
bool g_bVagCmdsInitialized = false;
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
  g_nMusicSemaphore = 0;
  g_bMusicIsPaused = false;
  g_bMusicPause = false;
  g_szCurrentMusicName[0] = 0;
  g_szTargetMusicName[0] = 0;
  sLoadDGO = {};
  g_nActiveMusicStreams = 0;
  g_bVagCmdsInitialized = false;
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

    internal_cmd->msg_type = user_cmd->msg_type;
    internal_cmd->mbox_reply = user_cmd->mbox_reply;
    internal_cmd->thread_to_wake = user_cmd->thread_to_wake;
    internal_cmd->callback = user_cmd->callback;

    internal_cmd->m_pBaseFile = user_cmd->m_pBaseFile;
    internal_cmd->unkc = user_cmd->unkc;
    internal_cmd->file_def = user_cmd->file_def;

    // copy part of the command set by the user
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
    internal_cmd->unkc = user_cmd->unkc;
    internal_cmd->file_def = user_cmd->file_def;

    internal_cmd->vag_file_def = user_cmd->vag_file_def;
    internal_cmd->vag_dir_entry = user_cmd->vag_dir_entry;
    strncpy(internal_cmd->name, user_cmd->name, 0x30);
    internal_cmd->id = user_cmd->id;
    internal_cmd->play_volume = user_cmd->play_volume;
    internal_cmd->plugin_id = user_cmd->plugin_id;
    internal_cmd->maybe_sound_handler = user_cmd->maybe_sound_handler;
    internal_cmd->oog = user_cmd->oog;
    internal_cmd->some_pan_thing = user_cmd->some_pan_thing;
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
      internal_cmd->status = 2;
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
        TerminateVAG(internal_cmd);
      }
      return;
    }

    // we have an id, just terminate that.
    internal_cmd = FindThisMusicStream(cmd->name, id);
    if (!internal_cmd) {
      return;
    }
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
    if (cmd && cmd->id & !cmd->flags.stop) {
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

  pCVar12 = (CISOCDFile*)0x0;
  InitBuffers();
  bVar1 = false;
  InitVagCmds();
  g_bVagCmdsInitialized = true;
  InitDriver();

  ISO_Hdr* mbx_cmd = nullptr;
  ISO_LoadSoundbank* load_sbk_cmd = nullptr;
  ISO_LoadSingle* load_single_cmd = nullptr;
  ISO_LoadCommon* load_cmd = nullptr;
  char local_name[32];
  ISO_VAGCommand* vag_cmd = nullptr;
  ISO_VAGCommand* internal_vag_cmd = nullptr;

  // ISOFileDef* file_def = nullptr;
  int sector_offset = 0;

  while (true) {
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

          if (QueueMessage(mbx_cmd, priority) == 0)
            goto LAB_00006b18;

          // iVar3 = (cmd->header).kind;
          // handle opening the file:
          switch (msg_kind) {
            case ISO_Hdr::MsgType::LOAD_EE_CHUNK: {
              mbx_cmd->m_pBaseFile = get_file_system()->Open(
                  mbx_cmd->file_def, ((ISO_LoadSingle*)mbx_cmd)->sector_offset, 1);
            } break;
            case ISO_Hdr::MsgType::LOAD_IOP:
            case ISO_Hdr::MsgType::LOAD_EE:
              mbx_cmd->m_pBaseFile = get_file_system()->Open(mbx_cmd->file_def, -1, 1);
              break;
            case ISO_Hdr::MsgType::LOAD_SOUNDBANK: {
              // build name
              ASSERT(load_sbk_cmd->name);
              strncpy(local_name, load_sbk_cmd->name, 0xc);
              local_name[8] = 0;
              strcat(local_name, ".sbk");
              auto* file_def = get_file_system()->Find(local_name);
              ASSERT(file_def);
              mbx_cmd->m_pBaseFile = get_file_system()->Open(file_def, -1, 1);
            } break;
            default:
              ASSERT_NOT_REACHED();
          }

          // if we failed to open, bail
          if (!mbx_cmd->m_pBaseFile) {
            ASSERT_NOT_REACHED();
            mbx_cmd->status = 8;
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

          mbx_cmd->status = 2;
          set_active_a(mbx_cmd, 1);
        } else {
          switch (msg_kind) {
            case ISO_Hdr::MsgType::DGO_LOAD:
              if (QueueMessage(mbx_cmd, 1) != 0) {
                // modified for non compressed dgos
                mbx_cmd->m_pBaseFile = get_file_system()->Open(mbx_cmd->file_def, -1, 1);
                if (mbx_cmd->m_pBaseFile) {
                  mbx_cmd->callback = RunDGOStateMachine;
                  mbx_cmd->status = 2;
                  ((ISO_DGOCommand*)mbx_cmd)->state = 0;
                  set_active_a(mbx_cmd, 1);
                } else {
                  ASSERT_NOT_REACHED();
                  UnqueueMessage(mbx_cmd);
                  SendMbx(g_nISOMbx, &sLoadDGO);
                }
              }
              break;
            case ISO_Hdr::MsgType::VAG_PAUSE:
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
              internal_vag_cmd = FindVagStreamId(vag_cmd->id);
              if (internal_vag_cmd) {
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
        return 0;
      }
      if (poll_result != -0x1a8) {
        ASSERT_NOT_REACHED();
      }
    }
  LAB_00006b18:
    uVar11 = 0;
    // Poll is called here... but we don't use it.
    // (**(code**)(*g_pFileSystem + 4))();
    ProcessMusic();
    cmd = (ISO_VAGCommand*)FUN_00008054();
    if (cmd != (ISO_VAGCommand*)0x0) {
      pCVar12 = (cmd->header).pBaseFile;
      if ((pCVar12 == (CISOCDFile*)0x0) ||
          (bVar1 = (pCVar12->base).m_Buffer.m_eBufferType != 0, uVar11 = (uint)bVar1, !bVar1)) {
        uVar11 = CBaseFile_InitBuffer((CBaseFile*)(cmd->header).pBaseFile,
                                      4 - (uint)((code*)(cmd->header).callback != ProcessVAGData),
                                      (ISO_Msg*)cmd);
      } else {
        CBaseFile_AllocPages((CBaseFile*)pCVar12);
      }
      pCVar12 = (CISOCDFile*)0x0;
      if (uVar11 == 0) {
        cmd = (ISO_VAGCommand*)0x0;
        bVar1 = false;
      } else {
        pCVar12 = (cmd->header).pBaseFile;
        bVar1 = false;
        if ((pCVar12 != (CISOCDFile*)0x0) && ((pCVar12->base).m_ReadRate != 0)) {
          bVar1 = true;
        }
        iVar3 = (**(code**)(pCVar12->base).vtable)(pCVar12);
        (cmd->header).status = iVar3;
        if (iVar3 != 2) {
          pCVar12 = (cmd->header).pBaseFile;
          uVar11 = 0;
          if (pCVar12 != (CISOCDFile*)0x0) {
            CBaseFile_TerminateBuffer((CBaseFile*)pCVar12);
          }
          cmd = (ISO_VAGCommand*)0x0;
          pCVar12 = (CISOCDFile*)0x0;
        }
        if (!bVar1) {
          _DAT_0001bb80 = GetSystemTimeLow();
        }
      }
    }
    iVar3 = ProcessMessageData(cmd);
    if (iVar3 == 0) {
      cmd = (ISO_VAGCommand*)0x0;
    }
    if ((uVar11 != 0) && (cmd != (ISO_VAGCommand*)0x0)) {
      iVar3 = 0xb;
      if (pCVar12 != (CISOCDFile*)0x0) {
        iVar3 = (**(code**)((int)(pCVar12->base).vtable + 4))(pCVar12);
      }
      if (!bVar1) {
        _DAT_0001bb80 = GetSystemTimeLow();
      }
      if (iVar3 == 0xb) {
        pCVar6 = (cmd->header).pBaseFile;
        if ((pCVar6 != (CISOCDFile*)0x0) && ((pCVar6->base).m_Status != 0)) {
          (cmd->header).status = 2;
        }
      } else {
        (cmd->header).status = iVar3;
        if ((cmd->header).active_c == 0) {
          set_active_c(cmd, 1);
        }
      }
    }
    WaitSema(g_RequestedStreamsList.sema);
    if (g_RequestedStreamsList.field_0x18 == 1) {
      iVar3 = 3;
      QueueNewStreamsFromList(&g_RequestedStreamsList);
      pVVar10 = g_NewStreamsList.next;
      do {
        iVar3 = iVar3 + -1;
        if (pVVar10->id != 0) {
          QueueVAGStream(pVVar10);
        }
        pVVar10 = pVVar10->next;
      } while (-1 < iVar3);
    }
    cmd = g_aVagCmds;
    iVar3 = 3;
    do {
      iVar3 = iVar3 + -1;
      if ((((cmd->music_flag == 0) && (cmd->bit_stereo_secondary == 0)) &&
           (cmd->bit_scanned == 0)) &&
          (cmd->id != 0)) {
        IsoStopVagStream(cmd);
      }
      cmd = cmd + 1;
    } while (-1 < iVar3);
    cmd = g_aVagCmds + 4;
    iVar3 = 1;
    SignalSema(g_RequestedStreamsList.sema);
    g_RequestedStreamsList.field_0x18 = 0;
    do {
      iVar3 = iVar3 + -1;
      if (((cmd->music_flag != 0) && (cmd->bit_stereo_secondary == 0)) &&
          ((cmd->id != 0 && (cmd->bit_stop != 0)))) {
        IsoStopVagStream(cmd);
      }
      cmd = cmd + 1;
    } while (-1 < iVar3);
    uVar7 = 4000;
    if (uVar11 != 0) {
      uVar7 = 200;
    }
    DelayThread(uVar7);
  }
}

}  // namespace jak3