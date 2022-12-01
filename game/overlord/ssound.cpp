#include "ssound.h"

#include <cstdio>
#include <cstring>

#include "common/util/Assert.h"

#include "game/overlord/iso.h"
#include "game/overlord/srpc.h"
#include "game/runtime.h"
#include "game/sound/sndshim.h"

using namespace iop;

Sound gSounds[64];
Curve gCurve[12];  // TODO verify count
VolumePair gPanTable[361];

Vec3w gEarTrans[2];
Vec3w gCamTrans;
s32 gCamAngle;

s32 gMusicVol = 0x400;
s32 gMusicFade = 0;
s32 gMusicFadeDir = 0;

u32 gStreamSRAM = 0;
u32 gTrapSRAM = 0;

s32 gSema;

static u32 sLastTick;
static s32 sqrt_table[256] = {
    0,     4096,  5793,  7094,  8192,  9159,  10033, 10837, 11585, 12288, 12953, 13585, 14189,
    14768, 15326, 15864, 16384, 16888, 17378, 17854, 18318, 18770, 19212, 19644, 20066, 20480,
    20886, 21283, 21674, 22058, 22435, 22806, 23170, 23530, 23884, 24232, 24576, 24915, 25249,
    25580, 25905, 26227, 26545, 26859, 27170, 27477, 27780, 28081, 28378, 28672, 28963, 29251,
    29537, 29819, 30099, 30377, 30652, 30924, 31194, 31462, 31727, 31991, 32252, 32511, 32768,
    33023, 33276, 33527, 33776, 34024, 34270, 34514, 34756, 34996, 35235, 35472, 35708, 35942,
    36175, 36406, 36636, 36864, 37091, 37316, 37540, 37763, 37985, 38205, 38424, 38642, 38858,
    39073, 39287, 39500, 39712, 39923, 40132, 40341, 40548, 40755, 40960, 41164, 41368, 41570,
    41771, 41972, 42171, 42369, 42567, 42763, 42959, 43154, 43348, 43541, 43733, 43925, 44115,
    44305, 44494, 44682, 44869, 45056, 45242, 45427, 45611, 45795, 45977, 46160, 46341, 46522,
    46702, 46881, 47059, 47237, 47415, 47591, 47767, 47942, 48117, 48291, 48465, 48637, 48809,
    48981, 49152, 49322, 49492, 49661, 49830, 49998, 50166, 50332, 50499, 50665, 50830, 50995,
    51159, 51323, 51486, 51649, 51811, 51972, 52134, 52294, 52454, 52614, 52773, 52932, 53090,
    53248, 53405, 53562, 53719, 53874, 54030, 54185, 54340, 54494, 54647, 54801, 54954, 55106,
    55258, 55410, 55561, 55712, 55862, 56012, 56162, 56311, 56459, 56608, 56756, 56903, 57051,
    57198, 57344, 57490, 57636, 57781, 57926, 58071, 58215, 58359, 58503, 58646, 58789, 58931,
    59073, 59215, 59357, 59498, 59639, 59779, 59919, 60059, 60199, 60338, 60477, 60615, 60753,
    60891, 61029, 61166, 61303, 61440, 61576, 61712, 61848, 61984, 62119, 62254, 62388, 62523,
    62657, 62790, 62924, 63057, 63190, 63323, 63455, 63587, 63719, 63850, 63982, 64113, 64243,
    64374, 64504, 64634, 64763, 64893, 65022, 65151, 65279, 65408,
};

static s32 atan_table[257] = {
    0,  0,  0,  0,  0,  1,  1,  1,  1,  2,  2,  2,  2,  2,  3,  3,  3,  3,  4,  4,  4,  4,  4,  5,
    5,  5,  5,  6,  6,  6,  6,  6,  7,  7,  7,  7,  8,  8,  8,  8,  8,  9,  9,  9,  9,  9,  10, 10,
    10, 10, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 14, 14, 15, 15, 15,
    15, 15, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 20, 20,
    20, 20, 20, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24,
    25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 29, 29,
    29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 32, 33,
    33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 36, 36,
    36, 37, 37, 37, 37, 37, 37, 37, 37, 38, 38, 38, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 39, 40,
    40, 40, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 42, 42, 42, 42, 43,
    43, 43, 43, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, 44, 45,
};

void CatalogSRAM() {}

static void* SndMemAlloc();
static void SndMemFree(void* ptr);
void InitSound_Overlord() {
  for (auto& s : gSounds) {
    s.id = 0;
  }

  if (g_game_version == GameVersion::Jak1) {
    SetCurve(1, 0, 0);
    SetCurve(2, 4096, 0);
    SetCurve(3, 0, 4096);
    SetCurve(4, 2048, 0);
    SetCurve(5, 2048, 2048);
    SetCurve(6, -4096, 0);
    SetCurve(7, -2048, 0);
  } else {
    SetCurve(2, 0, 0);
    SetCurve(9, 0, 0);
    SetCurve(11, 0, 0);
    SetCurve(10, 0, 0);
    SetCurve(3, 4096, 0);
    SetCurve(4, 0, 4096);
    SetCurve(5, 2048, 0);
    SetCurve(6, 2048, 2048);
    SetCurve(7, -4096, 0);
    SetCurve(8, -2048, 0);
  }

  snd_StartSoundSystem();
  snd_RegisterIOPMemAllocator(SndMemAlloc, SndMemFree);
  snd_LockVoiceAllocator(1);
  u32 voice = snd_ExternVoiceAlloc(2, 0x7f);
  snd_UnlockVoiceAllocator();

  // The voice allocator returns a number in the range 0-47 where voices
  // 0-23 are on SPU Core 0 and 24-47 are on core 2.
  // For some reason we convert it to this format where 0-47 alternate core every step.
  voice = voice / 24 + ((voice % 24) * 2);

  // Allocate SPU RAM for our streams.
  // (Which we don't need on PC)
  gStreamSRAM = snd_SRAMMalloc(0xc030);
  gTrapSRAM = gStreamSRAM + 0xC000;

  snd_SetMixerMode(0, 0);

  for (int i = 0; i < 8; i++) {
    snd_SetGroupVoiceRange(i, 0x10, 0x2f);
  }

  snd_SetGroupVoiceRange(1, 0, 0xf);
  snd_SetGroupVoiceRange(2, 0, 0xf);

  snd_SetReverbDepth(SND_CORE_0 | SND_CORE_1, 0, 0);
  snd_SetReverbType(SND_CORE_0, SD_REV_MODE_OFF);
  snd_SetReverbType(SND_CORE_1, SD_REV_MODE_OFF);

  CatalogSRAM();

  for (int i = 0; i < 91; i++) {
    s16 opposing_front = static_cast<s16>(((i * 0x33ff) / 0x5a) + 0xc00);

    s16 rear_right = static_cast<s16>(((i * -0x2800) / 0x5a) + 0x3400);
    s16 rear_left = static_cast<s16>(((i * -0xbff) / 0x5a) + 0x3fff);

    gPanTable[90 - i].left = 0x3FFF;
    gPanTable[180 - i].left = opposing_front;
    gPanTable[270 - i].left = rear_right;
    gPanTable[360 - i].left = rear_left;

    gPanTable[i].right = opposing_front;
    gPanTable[90 + i].right = 0x3FFF;
    gPanTable[180 + i].right = rear_left;
    gPanTable[270 + i].right = rear_right;
  }

  snd_SetPanTable((s16*)gPanTable);
  snd_SetPlayBackMode(2);

  SemaParam sema;
  sema.attr = SA_THPRI;
  sema.init_count = 1;
  sema.max_count = 1;
  sema.option = 0;

  gSema = CreateSema(&sema);
  if (gSema < 0) {
    while (true)
      ;
  }
}

Sound* LookupSound(s32 id) {
  if (id == 0) {
    return nullptr;
  }

  for (auto& s : gSounds) {
    if (s.id == id) {
      s32 sndid = snd_SoundIsStillPlaying(s.sound_handle);
      s.sound_handle = sndid;

      if (sndid == 0) {
        s.id = 0;
        return nullptr;
      }

      return &s;
    }
  }

  return nullptr;
}

void CleanSounds() {
  for (auto& s : gSounds) {
    if (s.id != 0) {
      s32 sndid = snd_SoundIsStillPlaying(s.sound_handle);
      s.sound_handle = sndid;

      if (sndid == 0) {
        s.id = 0;
      }
    }
  }
}

void KillSoundsInGroup(u8 group) {
  for (auto& s : gSounds) {
    if (s.id != 0) {
      s32 sndid = snd_SoundIsStillPlaying(s.sound_handle);
      s.sound_handle = sndid;

      if (sndid == 0) {
        s.id = 0;
      } else if (s.params.group & group) {
        snd_StopSound(s.sound_handle);
        s.id = 0;
      }
    }
  }
}

Sound* AllocateSound() {
  for (auto& s : gSounds) {
    if (s.id == 0) {
      return &s;
    }
  }

  CleanSounds();
  for (auto& s : gSounds) {
    if (s.id == 0) {
      return &s;
    }
  }

  return nullptr;
}

s32 CalculateFallofVolume(Vec3w* pos, s32 volume, s32 fo_curve, s32 fo_min, s32 fo_max) {
  s32 xdiff = 0;
  s32 ydiff = 0;
  s32 zdiff = 0;

  if (g_game_version == GameVersion::Jak1) {
    if (fo_curve == 0) {
      return volume;
    }

    xdiff = gEarTrans[0].x - pos->x;
    ydiff = gEarTrans[0].y - pos->y;
    zdiff = gEarTrans[0].z - pos->z;
  } else {
    if (fo_curve == 1) {
      return volume;
    }

    if (fo_curve < 9) {
      xdiff = gEarTrans[0].x - pos->x;
      ydiff = gEarTrans[0].y - pos->y;
      zdiff = gEarTrans[0].z - pos->z;
    }

    if (fo_curve == 9) {
      xdiff = gEarTrans[1].x - pos->x;
      ydiff = gEarTrans[1].y - pos->y;
      zdiff = gEarTrans[1].z - pos->z;
    }

    if (fo_curve == 10) {
      xdiff = 0;
      ydiff = gEarTrans[0].y - pos->y;
      zdiff = 0;
    }

    if (fo_curve == 11) {
      xdiff = gEarTrans[1].x - pos->x;
      ydiff = gEarTrans[1].y - pos->y;
      zdiff = gEarTrans[1].z - pos->z;
    }
  }

  if (xdiff < 0) {
    xdiff = -xdiff;
  }
  if (ydiff < 0) {
    ydiff = -ydiff;
  }
  if (zdiff < 0) {
    zdiff = -zdiff;
  }

  s32 min = fo_min << 8;
  s32 max = fo_max << 8;

  if (max < xdiff) {
    return 0;
  }

  if (max < ydiff) {
    return 0;
  }

  if (max < zdiff) {
    return 0;
  }

  while (max > 0x7FFF) {
    max >>= 1;
    min >>= 1;
    xdiff >>= 1;
    ydiff >>= 1;
    zdiff >>= 1;
  }

  u32 distance = xdiff * xdiff + ydiff * ydiff + zdiff * zdiff;
  if (distance != 0) {
    s32 steps = 0;
    while ((distance & 0xc0000000) == 0) {
      distance <<= 2;
      steps++;
    }

    distance = sqrt_table[distance >> 24] >> steps;
  } else {
    distance = 0;
  }

  if (distance <= (u32)min) {
    return volume;
  }

  if (distance >= (u32)max) {
    return 0;
  }

  s32 start = distance - min;
  s32 end = max - min;

  while (start > 0xFFFF) {
    start >>= 1;
    end >>= 1;
  }

  s32 v13 = (start << 16) / end;
  if (v13 == 0x10000) {
    return volume;
  }

  s32 factor = ((gCurve[fo_curve].unk4 << 16) + gCurve[fo_curve].unk3 * v13 +
                gCurve[fo_curve].unk2 * ((v13 * v13) >> 16) +
                gCurve[fo_curve].unk1 * (((((v13 * v13) >> 16) * v13) >> 16) >> 16)) >>
               12;

  if (factor > 0x10000) {
    factor = 0x10000;
  }

  s32 ret = (factor * volume) >> 16;
  if (fo_curve == 11 && ret < 0x180) {
    ret = 0x180;
  }

  return ret;
}

s32 CalculateAngle(Vec3w* trans) {
  s32 diffX = gCamTrans.x - trans->x;
  s32 diffZ = gCamTrans.z - trans->z;

  s32 lookupX = diffX;
  s32 lookupZ = diffZ;

  if (diffX < 0) {
    lookupX = trans->x - gCamTrans.x;
  }

  if (diffZ < 0) {
    lookupZ = trans->z - gCamTrans.z;
  }

  if (lookupX == 0 && lookupZ == 0) {
    return 0;
  }

  s32 angle;
  if (lookupZ >= lookupX) {
    angle = atan_table[(lookupX << 8) / lookupZ];

    if (diffZ >= 0) {
      if (diffX < 0) {
        angle = 360 - angle;
      }
    } else if (diffX >= 0) {
      angle = 180 - angle;
    } else {
      angle += 180;
    }
  } else {
    angle = atan_table[(lookupZ << 8) / lookupX];

    if (diffX >= 0) {
      if (diffZ >= 0) {
        angle = 90 - angle;
      } else {
        angle = angle + 90;
      }
    } else if (diffZ >= 0) {
      angle = angle + 270;
    } else {
      angle = 270 - angle;
    }
  }

  return (angle - gCamAngle + 720) % 360;
}

s32 GetVolume(Sound* sound) {
  return CalculateFallofVolume(&sound->params.trans, sound->params.volume, sound->params.fo_curve,
                               sound->params.fo_min, sound->params.fo_max);
}

s32 GetPan(Sound* sound) {
  return CalculateAngle(&sound->params.trans);
}

static void UpdateLocation(Sound* sound) {
  if (sound->id == 0) {
    return;
  }

  if (g_game_version == GameVersion::Jak1) {
    if ((sound->bank_entry->fallof_params >> 28) == 0) {
      return;
    }
  }

  s32 id = snd_SoundIsStillPlaying(sound->sound_handle);
  if (id == 0) {
    sound->id = 0;
    return;
  }

  s32 volume = GetVolume(sound);
  if (volume == 0) {
    snd_StopSound(sound->sound_handle);
  } else {
    s32 pan = GetPan(sound);
    snd_SetSoundVolPan(id, volume, pan);
  }
}

static void UpdateAutoVol(Sound* sound, s32 ticks) {
  if (ticks < sound->auto_time) {
    s32 nvol = sound->new_volume;
    if (nvol == -4) {
      nvol = 0;
    }

    s32 step = (nvol - sound->params.volume) * ticks;
    step /= sound->auto_time;

    if (step >= 0) {
      if (step == 0) {
        step = 1;
      }

      sound->params.volume += step;
      if (sound->new_volume < sound->params.volume) {
        sound->params.volume = sound->new_volume;
      }

    } else {
      sound->params.volume += step;
      if (sound->new_volume > sound->params.volume) {
        sound->params.volume = sound->new_volume;
      }
    }

    sound->auto_time -= ticks;
    return;
  }

  if (sound->new_volume == -4) {
    snd_StopSound(sound->sound_handle);
    sound->id = 0;
  } else {
    sound->params.volume = sound->new_volume;
  }

  sound->auto_time = 0;
}

void UpdateVolume(Sound* sound) {
  s32 id = snd_SoundIsStillPlaying(sound->sound_handle);
  sound->sound_handle = id;
  if (sound->sound_handle == 0) {
    sound->id = 0;
  } else {
    s32 volume = GetVolume(sound);
    snd_SetSoundVolPan(id, volume, -2);
  }
}

void SetEarTrans(Vec3w* ear_trans1, Vec3w* ear_trans2, Vec3w* cam_trans, s32 cam_angle) {
  s32 tick = snd_GetTick();
  u32 delta = tick - sLastTick;
  sLastTick = tick;

  gEarTrans[0] = *ear_trans1;
  gEarTrans[1] = *ear_trans2;
  gCamTrans = *cam_trans;
  gCamAngle = cam_angle;

  for (auto& s : gSounds) {
    if (s.id != 0 && s.is_music == 0) {
      if (s.auto_time != 0) {
        UpdateAutoVol(&s, delta);
      }
      UpdateLocation(&s);
    }
  }

  SetVAGVol();
}

void PrintActiveSounds() {
  char string[64];

  for (auto& s : gSounds) {
    if (s.id != 0 && s.is_music == 0) {
      if (s.bank_entry != nullptr) {
        u32 len = strlen(s.bank_entry->name);
        if (len > 16) {
          len = 16;
        }
        sprintf(string, "                 : Vol %d", GetVolume(&s));
        memcpy(string, s.bank_entry->name, len);
        printf("%s\n", string);
      } else {  // added for printing jak2 sounds
        u32 len = strlen(s.name);
        if (len > 16) {
          len = 16;
        }
        sprintf(string, "                 : Vol %d, ID %d, Curve %d", GetVolume(&s), s.id,
                s.params.fo_curve);
        memcpy(string, s.name, len);
        printf("%s\n", string);
      }
    }
  }
}

void SetCurve(s32 curve, s32 fallof, s32 ease) {
  gCurve[curve].unk1 = ease << 1;
  gCurve[curve].unk2 = fallof + ease * -3;
  gCurve[curve].unk3 = (ease - fallof) + -0x1000;
  gCurve[curve].unk4 = 0x1000;
}

void SetMusicVol() {
  s32 volume = (gMusicVol * gMusicFade >> 0x10) * gMusicTweak >> 7;
  snd_SetMasterVolume(1, volume);
  snd_SetMasterVolume(2, volume);
}

// Do we even need/want these
// TODO void SetBufferMem() {}
// TODO void ReleaseBufferMem() {}
static void* SndMemAlloc() {
  return nullptr;
}
static void SndMemFree(void* /*ptr*/) {}
