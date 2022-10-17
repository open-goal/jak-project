#include "sfxgrain.h"

#include "common/log/log.h"

#include "blocksound_handler.h"

namespace snd {

SFXGrain_Null::SFXGrain_Null(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Null::SFXGrain_Null(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_Null::execute(blocksound_handler& handler) {}

SFXGrain_Tone::SFXGrain_Tone(SFXGrain& grain)
    : Grain(grain.Delay), m_tone(grain.GrainParams.tone) {}
SFXGrain_Tone::SFXGrain_Tone(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {
  m_tone = *(Tone*)(data + (grain.OpcodeData.Opcode & 0xFFFFFF));
}

void SFXGrain_Tone::execute(blocksound_handler& handler) {
  auto voice = std::make_shared<vag_voice>(m_tone);

  voice->basevol = handler.m_vm.make_volume(127, 0, handler.m_cur_volume, handler.m_cur_pan,
                                            m_tone.Vol, m_tone.Pan);

  voice->start_note = handler.m_note;
  voice->start_fine = handler.m_fine;
  voice->group = handler.m_group;

  handler.m_vm.start_tone(voice, handler.m_bank);
  handler.m_voices.emplace_front(voice);
}

SFXGrain_XrefID::SFXGrain_XrefID(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_XrefID::SFXGrain_XrefID(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_XrefID::execute(blocksound_handler& handler) {}

SFXGrain_XrefNum::SFXGrain_XrefNum(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_XrefNum::SFXGrain_XrefNum(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_XrefNum::execute(blocksound_handler& handler) {}

SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_LfoSettings::execute(blocksound_handler& handler) {}

SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
void SFXGrain_StartChildSound::execute(blocksound_handler& handler) {}

SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_StopChildSound::execute(blocksound_handler& handler) {}

SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_PluginMessage::execute(blocksound_handler& handler) {}

SFXGrain_Branch::SFXGrain_Branch(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Branch::SFXGrain_Branch(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_Branch::execute(blocksound_handler& handler) {}

SFXGrain_ControlNull::SFXGrain_ControlNull(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_ControlNull::SFXGrain_ControlNull(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_ControlNull::execute(blocksound_handler& handler) {}

SFXGrain_LoopStart::SFXGrain_LoopStart(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LoopStart::SFXGrain_LoopStart(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_LoopStart::execute(blocksound_handler& handler) {}

SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_LoopEnd::execute(blocksound_handler& handler) {}

SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_LoopContinue::execute(blocksound_handler& handler) {}

SFXGrain_Stop::SFXGrain_Stop(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Stop::SFXGrain_Stop(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_Stop::execute(blocksound_handler& handler) {}

SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain& grain) : Grain(grain.Delay) {
  options = grain.GrainParams.control.param[0];
  count = grain.GrainParams.control.param[1];
  previous = grain.GrainParams.control.param[2];
}

SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {
  options = grain.OpcodeData.arg[0];
  count = grain.OpcodeData.arg[1];
  previous = grain.OpcodeData.arg[2];
}

void SFXGrain_RandPlay::execute(blocksound_handler& handler) {
  int rnd = rand() % options;
  if (rnd == previous) {
    rnd++;
    if (rnd >= options) {
      rnd = 0;
    }
  }

  previous = rnd;
  handler.m_next_grain += rnd * count;
  handler.m_grains_to_play = count + 1;
  handler.m_grains_to_skip = (options - 1 - rnd) * count;
  handler.m_skip_grains = true;
}

SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_RandDelay::execute(blocksound_handler& handler) {}

SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_RandPB::execute(blocksound_handler& handler) {}

SFXGrain_PB::SFXGrain_PB(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_PB::SFXGrain_PB(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_PB::execute(blocksound_handler& handler) {}

SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_AddPB::execute(blocksound_handler& handler) {}

SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_SetRegister::execute(blocksound_handler& handler) {}

SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
void SFXGrain_SetRegisterRand::execute(blocksound_handler& handler) {}

SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_IncRegister::execute(blocksound_handler& handler) {}

SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_DecRegister::execute(blocksound_handler& handler) {}

SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_TestRegister::execute(blocksound_handler& handler) {}

SFXGrain_Marker::SFXGrain_Marker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_Marker::SFXGrain_Marker(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_Marker::execute(blocksound_handler& handler) {}

SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_GotoMarker::execute(blocksound_handler& handler) {}

SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
void SFXGrain_GotoRandomMarker::execute(blocksound_handler& handler) {}

SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain2& grain, u8* data)
    : Grain(grain.Delay) {}
void SFXGrain_WaitForAllVoices::execute(blocksound_handler& handler) {}

SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_PlayCycle::execute(blocksound_handler& handler) {}

SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_AddRegister::execute(blocksound_handler& handler) {}

SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_KeyOffVoices::execute(blocksound_handler& handler) {}

SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_KillVoices::execute(blocksound_handler& handler) {}

SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_OnStopMarker::execute(blocksound_handler& handler) {}

SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain& grain) : Grain(grain.Delay) {}
SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain2& grain, u8* data) : Grain(grain.Delay) {}
void SFXGrain_CopyRegister::execute(blocksound_handler& handler) {}

}  // namespace snd
