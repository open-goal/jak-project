#include "srpc.h"

#include "game/overlord/common/srpc.h"
#include "game/overlord/common/ssound.h"
#include "game/sce/iop.h"

using namespace iop;
namespace jak2 {

void srpc_init_globals() {

}

void* RPC_Player(unsigned int /*fno*/, void* data, int size) {
  if (!gSoundEnable) {
    return nullptr;
  }

  // gFreeMem = QueryTotalFreeMemSize();
  if (!PollSema(gSema)) {
    if (gMusic) {
      if (!gMusicPause && !LookupSound(666)) {
        Sound* music = AllocateSound();
        if (music != nullptr) {
          gMusicFade = 0;
          gMusicFadeDir = 1;
          SetMusicVol();
          music->sound_handle = snd_PlaySoundVolPanPMPB(gMusic, 0, 0x400, -1, 0, 0);
          music->id = 666;
          music->is_music = 1;
        }
      }
    }

    SignalSema(gSema);
  }

  SetMusicVol();
  Sound* music = LookupSound(666);
  if (music != nullptr) {
    snd_SetSoundVolPan(music->sound_handle, 0x7FFFFFFF, 0);
  }

  int n_messages = size / SRPC_MESSAGE_SIZE;
  SoundRpcCommand* cmd = (SoundRpcCommand*)(data);
  if (!gSoundEnable) {
    return nullptr;
  }

  while (n_messages > 0) {
    switch (cmd->j2command) {
      case Jak2SoundCommand::play: {
        if (!cmd->play.sound_id) {
          break;
        }

        auto sound = LookupSound(cmd->play.sound_id);
        if (sound != nullptr) {
          // update
          sound->params = cmd->play.parms;
          sound->is_music = false;
          SFXUserData data{};
          s32 found = snd_GetSoundUserData(0, nullptr, -1, sound->name, &data);
          if ((sound->params.mask & 0x40) == 0) {
            s16 fo_min = 5;
            if (found && data.data[0])
              fo_min = data.data[0];
            sound->params.fo_min = fo_min;
          }
          if ((sound->params.mask & 0x80) == 0) {
            s16 fo_max = 30;
            if (found && data.data[1])
              fo_max = data.data[1];
            sound->params.fo_max = fo_max;
          }
          if ((sound->params.mask & 0x100) == 0) {
            s16 fo_curve = 2;
            if (found && data.data[2])
              fo_curve = data.data[2];
            sound->params.fo_curve = fo_curve;
          }
          UpdateVolume(sound);
          snd_SetSoundPitchModifier(sound->sound_handle, sound->params.pitch_mod);
          if (sound->params.mask & 0x4) {
            snd_SetSoundPitchBend(sound->sound_handle, sound->params.bend);
          }
          if (sound->params.mask & 0x800) {
            snd_SetSoundReg(sound->sound_handle, 0, sound->params.reg[0]);
          }
          if (sound->params.mask & 0x1000) {
            snd_SetSoundReg(sound->sound_handle, 1, sound->params.reg[1]);
          }
          if (sound->params.mask & 0x2000) {
            snd_SetSoundReg(sound->sound_handle, 2, sound->params.reg[2]);
          }

        } else {
          // new sound
          sound = AllocateSound();
          if (sound == nullptr) {
            // no free sounds
            break;
          }
          strcpy_toupper(sound->name, cmd->play.name);
          // TODO update params struct
          sound->params = cmd->play.parms;
          sound->is_music = false;
          sound->bank_entry = nullptr;

          SFXUserData data{};
          s32 found = snd_GetSoundUserData(0, nullptr, -1, sound->name, &data);
          if ((sound->params.mask & 0x40) == 0) {
            s16 fo_min = 5;
            if (found && data.data[0])
              fo_min = data.data[0];
            sound->params.fo_min = fo_min;
          }
          if ((sound->params.mask & 0x80) == 0) {
            s16 fo_max = 30;
            if (found && data.data[1])
              fo_max = data.data[1];
            sound->params.fo_max = fo_max;
          }
          if ((sound->params.mask & 0x100) == 0) {
            s16 fo_curve = 2;
            if (found && data.data[2])
              fo_curve = data.data[2];
            sound->params.fo_curve = fo_curve;
          }
          // lg::warn("RPC: PLAY {} v:{}, p:{}", sound->name, GetVolume(sound), GetPan(sound));

          s32 handle = snd_PlaySoundByNameVolPanPMPB(0, nullptr, sound->name, GetVolume(sound),
                                                     GetPan(sound), sound->params.pitch_mod,
                                                     sound->params.bend);
          sound->sound_handle = handle;
          if (handle != 0) {
            sound->id = cmd->play.sound_id;
            if (sound->params.mask & 0x800) {
              snd_SetSoundReg(sound->sound_handle, 0, sound->params.reg[0]);
            }
            if (sound->params.mask & 0x1000) {
              snd_SetSoundReg(sound->sound_handle, 1, sound->params.reg[1]);
            }
            if (sound->params.mask & 0x2000) {
              snd_SetSoundReg(sound->sound_handle, 2, sound->params.reg[2]);
            }
          }
        }
      } break;
      case Jak2SoundCommand::pause_sound: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        if (sound != nullptr) {
          snd_PauseSound(sound->sound_handle);
        }
        // TODO vag
      } break;
      case Jak2SoundCommand::stop_sound: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        if (sound != nullptr) {
          snd_StopSound(sound->sound_handle);
        }
        // TODO vag
      } break;
      case Jak2SoundCommand::continue_sound: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        if (sound != nullptr) {
          snd_ContinueSound(sound->sound_handle);
        }
        // TODO vag
      } break;
      case Jak2SoundCommand::set_param: {
        Sound* sound = LookupSound(cmd->sound_id.sound_id);
        u32 mask = cmd->param.parms.mask;
        if (sound != nullptr) {
          if (mask & 1) {
            if (mask & 0x10) {
              sound->auto_time = cmd->param.auto_time;
              sound->new_volume = cmd->param.parms.volume;
            } else {
              sound->params.volume = cmd->param.parms.volume;
            }
          }
          if (mask & 0x20) {
            sound->params.trans = cmd->param.parms.trans;
          }
          if (mask & 0x21) {
            UpdateVolume(sound);
          }
          if (mask & 2) {
            sound->params.pitch_mod = cmd->param.parms.pitch_mod;
            if (mask & 0x10) {
              snd_AutoPitch(sound->sound_handle, sound->params.pitch_mod, cmd->param.auto_time,
                            cmd->param.auto_from);
            } else {
              snd_SetSoundPitchModifier(sound->sound_handle, cmd->param.parms.pitch_mod);
            }
          }
          if (mask & 4) {
            sound->params.bend = cmd->param.parms.bend;
            if (mask & 0x10) {
              snd_AutoPitchBend(sound->sound_handle, sound->params.bend, cmd->param.auto_time,
                                cmd->param.auto_from);
            } else {
              snd_SetSoundPitchBend(sound->sound_handle, cmd->param.parms.bend);
            }
          }
          if (mask & 0x400) {
            sound->params.priority = cmd->param.parms.priority;
          }
          if (mask & 0x8) {
            sound->params.group = cmd->param.parms.group;
          }
          if (mask & 0x40) {
            sound->params.fo_min = cmd->param.parms.fo_min;
          }
          if (mask & 0x80) {
            sound->params.fo_max = cmd->param.parms.fo_max;
          }
          if (mask & 0x100) {
            sound->params.fo_curve = cmd->param.parms.fo_curve;
          }
          if (mask & 0x800) {
            sound->params.reg[0] = cmd->param.parms.reg[0];
            snd_SetSoundReg(sound->sound_handle, 0, cmd->param.parms.reg[0]);
          }
          if (mask & 0x1000) {
            sound->params.reg[1] = cmd->param.parms.reg[1];
            snd_SetSoundReg(sound->sound_handle, 1, cmd->param.parms.reg[1]);
          }
          if (mask & 0x2000) {
            sound->params.reg[2] = cmd->param.parms.reg[2];
            snd_SetSoundReg(sound->sound_handle, 2, cmd->param.parms.reg[2]);
          }
        }
        // TODO vag
      } break;
      case Jak2SoundCommand::set_master_volume: {
        u32 group = cmd->master_volume.group.group;
        // FIXME array of set volumes
        for (int i = 0; i < 32; i++) {
          if (((group >> i) & 1) != 0) {
            if (i == 1) {
              gMusicVol = cmd->master_volume.volume;
            } else if (i == 2) {
              SetDialogVolume(cmd->master_volume.volume);
            } else {
              snd_SetMasterVolume(i, cmd->master_volume.volume);
            }
          }
        }
      } break;
      case Jak2SoundCommand::pause_group: {
        snd_PauseAllSoundsInGroup(cmd->group.group);
        if (cmd->group.group & 2) {
          gMusicPause = 1;
        }
        if (cmd->group.group & 4) {
          // TODO vag
        }
      } break;
      case Jak2SoundCommand::stop_group: {
        KillSoundsInGroup(cmd->group.group);
      } break;
      case Jak2SoundCommand::continue_group: {
        snd_ContinueAllSoundsInGroup(cmd->group.group);
        if (cmd->group.group & 2) {
          gMusicPause = 0;
        }
        if (cmd->group.group & 4) {
          // TODO vag
        }
      } break;
      case Jak2SoundCommand::set_midi_reg: {
        if (cmd->midi_reg.reg == 16) {
          snd_SetGlobalExcite(cmd->midi_reg.value);
        } else {
          Sound* sound = LookupSound(666);
          if (sound != nullptr) {
            snd_SetMIDIRegister(sound->sound_handle, cmd->midi_reg.reg, cmd->midi_reg.value);
          }
        }
      } break;
      case Jak2SoundCommand::set_reverb: {
        lg::warn("RPC_Player: unimplemented set_reverb");
        // TODO reverb
      } break;
      case Jak2SoundCommand::set_ear_trans: {
        SetEarTrans(&cmd->ear_trans_j2.ear_trans0, &cmd->ear_trans_j2.ear_trans1,
                    &cmd->ear_trans_j2.cam_trans, cmd->ear_trans_j2.cam_angle);
      } break;
      case Jak2SoundCommand::shutdown: {
        gSoundEnable = 0;
      } break;
      case Jak2SoundCommand::set_fps: {
        gFPS = cmd->fps.fps;
      } break;
      default:
        ASSERT_MSG(false, fmt::format("Unhandled RPC Player command {}",
                                      magic_enum::enum_name(cmd->j2command)));
    }

    n_messages--;
    cmd++;
  }

  return nullptr;
}
}  // namespace jak2