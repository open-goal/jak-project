#pragma once

#include "common/common_types.h"

#include "game/overlord/common/iso.h"
#include "game/overlord/common/ssound.h"
#include "game/overlord/jak2/iso.h"
#include "game/overlord/jak2/list.h"

namespace jak2 {

enum VagCmdByte {
  BYTE1 = 1,   // init to 0 (playing?)
  PAUSED = 2,  // init to 1 (paused?)
  BYTE4 = 4,   // streaming?
  BYTE5 = 5,   // init to 0 (maybe something to do with setting sdsetswitch stuff.
  BYTE6 = 6,   // init to 0
  BYTE7_SCANNED = 7,
  // 8
  // 9
  BYTE10 = 10,  // 10
  BYTE11 = 11,  // try not to interrupt this one with SmartAllocVagCmd.
  // 12
  // 13
  // 14
  // 15
  BYTE18 = 18,          // set in spudma if unk_240 set and no stero
  BYTE19 = 19,          // set in spudma if unk_240 unset and no stero
  BYTE23_NOSTART = 23,  // start??
};

struct VagCmd {
  CmdHeader header;            // 0 to ??
  FileRecord* file_record;     // 40
  VagDirEntry* vag_dir_entry;  // 44
  VagCmd* stereo_sibling;      // 48
  u8* dma_iop_mem_ptr;         // 52
  int chan;                    // 56
  int unk_60;                  // 60 (thought it was started, but init to 1)
  int unk_64;
  char name[48];            // 68
  int spu_stream_mem_addr;  // 120
  int spu_trap_mem_addr;    // 124
  int voice;                // 128
  int idx_in_cmd_arr;       // 132 (index in VagCmds)
  int unk_136;              // 136
  int unk_140;              // 140
  int unk_176;              // 176
  int unk_180;              // 180
  int unk_184;              // 184
  int unk_188;              // 188
  int unk_192;              // 192
  int unk_196;              // 196
  int unk_200;              // 200
  int unk_204;              // 204
  union {
    u8 status_bytes[24];  // 208
    struct {
      u8 byte0;
      u8 byte1;
      u8 byte2;
      u8 byte3;
      u8 byte4;
      u8 byte5;
      u8 byte6;
      u8 byte7;
      u8 byte8;
      u8 byte9;
      u8 byte10;
      u8 byte11;
      u8 byte12;
      u8 byte13;
      u8 byte14;
      u8 byte15;
      u8 byte16;
      u8 byte17;
      u8 byte18;
      u8 byte19;
      u8 byte20;
      u8 byte21;
      u8 byte22;
      u8 byte23;
    };
  };
  u8 unk_232;          // 232 wtf is this
  int unk_236;         // 236
  int unk_240_flag0;   // 240
  int xfer_size;       // 244
  int unk_248;         // 248
  int pitch1;          // 252
  int unk_256_pitch2;  // 256
  int unk_260;         // 260
  int unk_264;         // 264 (init to 0x4000)
  int unk_268;         // 268
  int vol_multiplier;  // 272
  int id;              // 276
  int plugin_id;       // 280
  int priority;        // 284
  int unk_288;         // 288
  int unk_292;         // 292
  int unk_296;         // 296
  Vec3w vec3;          // 300
  int fo_min;          // 312 (init to 5)
  int fo_max;          // 316 (init to 316)
  int fo_curve;        // 320 (init to 1)
};

struct VagCmdPriListEntry {
  VagCmd* cmds[4];
};

constexpr int N_VAG_CMDS = 4;
extern VagCmd VagCmds[N_VAG_CMDS];
extern int StreamSRAM[N_VAG_CMDS];
extern int TrapSRAM[N_VAG_CMDS];
extern int StreamVoice[N_VAG_CMDS];

extern int ActiveVagStreams;
extern int MasterVolume[17];

void vag_init_globals();

VagCmd* FindThisVagStream(const char* name, int id);
VagCmd* FindNotQueuedVagCmd();
int CalculateVAGPitch(int param_1, int param_2);
void UnPauseVAG(VagCmd* param_1, int param_2);
int HowManyBelowThisPriority(int pri, int disable_intr);
VagCmd* SmartAllocVagCmd(VagCmd* cmd);
void InitVAGCmd(VagCmd* param_1, int param_2);
void RemoveVagCmd(VagCmd* cmd, int param_2);
void FreeVagCmd(VagCmd* cmd, int param_2);
void SetNewVagCmdPri(VagCmd* cmd, int new_pri, int param_3);
VagCmd* FindVagStreamName(const char* name);
void TerminateVAG(VagCmd* cmd, int param_2);
void PauseVAG(VagCmd* cmd, int param_2);
int AnyVagRunning();
void InitVagCmds();
void VAG_MarkLoopEnd(int8_t* data, int offset);
void VAG_MarkLoopStart(int8_t* param_1);
void RestartVag(VagCmd* param_1, int param_2, int param_3);
void SetVagStreamsNoStart(int param_1, int param_2);
void PauseVagStreams();
void UnPauseVagStreams();
VagCmd* FindVagStreamId(int id);
void SetVAGVol(VagCmd* cmd, int param_2);
void SetAllVagsVol(int vol);
void SetVagStreamsNotScanned();
}  // namespace jak2