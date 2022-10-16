#include "sfxgrain.h"
#include "common/log/log.h"

#include "blocksound_handler2.h"

namespace snd {

SFXGrain_Null::SFXGrain_Null(SFXGrain& grain) {}
SFXGrain_Null::SFXGrain_Null(SFXGrain2& grain, u8* data) {}
void SFXGrain_Null::execute(blocksound_handler2& handler) {}

SFXGrain_Tone::SFXGrain_Tone(SFXGrain& grain) : m_tone(grain.GrainParams.tone) {}
SFXGrain_Tone::SFXGrain_Tone(SFXGrain2& grain, u8* data) {
  m_tone = *(Tone*)(data + (grain.OpcodeData.Opcode & 0xFFFFFF));
}

void SFXGrain_Tone::execute(blocksound_handler2& handler) {
  auto voice = std::make_shared<vag_voice>(m_tone);

  voice->basevol = handler.m_vm.make_volume(127, 0, handler.m_cur_volume, handler.m_cur_pan,
                                            m_tone.Vol, m_tone.Pan);

  voice->start_note = handler.m_note;
  voice->start_fine = handler.m_fine;
  voice->group = handler.m_group;

  handler.m_vm.start_tone(voice, handler.m_bank);
  handler.m_voices.emplace_front(voice);
}

SFXGrain_XrefID::SFXGrain_XrefID(SFXGrain& grain) {}
SFXGrain_XrefID::SFXGrain_XrefID(SFXGrain2& grain, u8* data) {}
void SFXGrain_XrefID::execute(blocksound_handler2& handler) {}

SFXGrain_XrefNum::SFXGrain_XrefNum(SFXGrain& grain) {}
SFXGrain_XrefNum::SFXGrain_XrefNum(SFXGrain2& grain, u8* data) {}
void SFXGrain_XrefNum::execute(blocksound_handler2& handler) {}

SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain& grain) {}
SFXGrain_LfoSettings::SFXGrain_LfoSettings(SFXGrain2& grain, u8* data) {}
void SFXGrain_LfoSettings::execute(blocksound_handler2& handler) {}

SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain& grain) {}
SFXGrain_StartChildSound::SFXGrain_StartChildSound(SFXGrain2& grain, u8* data) {}
void SFXGrain_StartChildSound::execute(blocksound_handler2& handler) {}

SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain& grain) {}
SFXGrain_StopChildSound::SFXGrain_StopChildSound(SFXGrain2& grain, u8* data) {}
void SFXGrain_StopChildSound::execute(blocksound_handler2& handler) {}

SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain& grain) {}
SFXGrain_PluginMessage::SFXGrain_PluginMessage(SFXGrain2& grain, u8* data) {}
void SFXGrain_PluginMessage::execute(blocksound_handler2& handler) {}

SFXGrain_Branch::SFXGrain_Branch(SFXGrain& grain) {}
SFXGrain_Branch::SFXGrain_Branch(SFXGrain2& grain, u8* data) {}
void SFXGrain_Branch::execute(blocksound_handler2& handler) {}

SFXGrain_ControlNull::SFXGrain_ControlNull(SFXGrain& grain) {}
SFXGrain_ControlNull::SFXGrain_ControlNull(SFXGrain2& grain, u8* data) {}
void SFXGrain_ControlNull::execute(blocksound_handler2& handler) {}

SFXGrain_LoopStart::SFXGrain_LoopStart(SFXGrain& grain) {}
SFXGrain_LoopStart::SFXGrain_LoopStart(SFXGrain2& grain, u8* data) {}
void SFXGrain_LoopStart::execute(blocksound_handler2& handler) {}

SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain& grain) {}
SFXGrain_LoopEnd::SFXGrain_LoopEnd(SFXGrain2& grain, u8* data) {}
void SFXGrain_LoopEnd::execute(blocksound_handler2& handler) {}

SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain& grain) {}
SFXGrain_LoopContinue::SFXGrain_LoopContinue(SFXGrain2& grain, u8* data) {}
void SFXGrain_LoopContinue::execute(blocksound_handler2& handler) {}

SFXGrain_Stop::SFXGrain_Stop(SFXGrain& grain) {}
SFXGrain_Stop::SFXGrain_Stop(SFXGrain2& grain, u8* data) {}
void SFXGrain_Stop::execute(blocksound_handler2& handler) {}

SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain& grain) {}
SFXGrain_RandPlay::SFXGrain_RandPlay(SFXGrain2& grain, u8* data) {}
void SFXGrain_RandPlay::execute(blocksound_handler2& handler) {}

SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain& grain) {}
SFXGrain_RandDelay::SFXGrain_RandDelay(SFXGrain2& grain, u8* data) {}
void SFXGrain_RandDelay::execute(blocksound_handler2& handler) {}

SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain& grain) {}
SFXGrain_RandPB::SFXGrain_RandPB(SFXGrain2& grain, u8* data) {}
void SFXGrain_RandPB::execute(blocksound_handler2& handler) {}

SFXGrain_PB::SFXGrain_PB(SFXGrain& grain) {}
SFXGrain_PB::SFXGrain_PB(SFXGrain2& grain, u8* data) {}
void SFXGrain_PB::execute(blocksound_handler2& handler) {}

SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain& grain) {}
SFXGrain_AddPB::SFXGrain_AddPB(SFXGrain2& grain, u8* data) {}
void SFXGrain_AddPB::execute(blocksound_handler2& handler) {}

SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain& grain) {}
SFXGrain_SetRegister::SFXGrain_SetRegister(SFXGrain2& grain, u8* data) {}
void SFXGrain_SetRegister::execute(blocksound_handler2& handler) {}

SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain& grain) {}
SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand(SFXGrain2& grain, u8* data) {}
void SFXGrain_SetRegisterRand::execute(blocksound_handler2& handler) {}

SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain& grain) {}
SFXGrain_IncRegister::SFXGrain_IncRegister(SFXGrain2& grain, u8* data) {}
void SFXGrain_IncRegister::execute(blocksound_handler2& handler) {}

SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain& grain) {}
SFXGrain_DecRegister::SFXGrain_DecRegister(SFXGrain2& grain, u8* data) {}
void SFXGrain_DecRegister::execute(blocksound_handler2& handler) {}

SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain& grain) {}
SFXGrain_TestRegister::SFXGrain_TestRegister(SFXGrain2& grain, u8* data) {}
void SFXGrain_TestRegister::execute(blocksound_handler2& handler) {}

SFXGrain_Marker::SFXGrain_Marker(SFXGrain& grain) {}
SFXGrain_Marker::SFXGrain_Marker(SFXGrain2& grain, u8* data) {}
void SFXGrain_Marker::execute(blocksound_handler2& handler) {}

SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain& grain) {}
SFXGrain_GotoMarker::SFXGrain_GotoMarker(SFXGrain2& grain, u8* data) {}
void SFXGrain_GotoMarker::execute(blocksound_handler2& handler) {}

SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain& grain) {}
SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker(SFXGrain2& grain, u8* data) {}
void SFXGrain_GotoRandomMarker::execute(blocksound_handler2& handler) {}

SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain& grain) {}
SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices(SFXGrain2& grain, u8* data) {}
void SFXGrain_WaitForAllVoices::execute(blocksound_handler2& handler) {}

SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain& grain) {}
SFXGrain_PlayCycle::SFXGrain_PlayCycle(SFXGrain2& grain, u8* data) {}
void SFXGrain_PlayCycle::execute(blocksound_handler2& handler) {}

SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain& grain) {}
SFXGrain_AddRegister::SFXGrain_AddRegister(SFXGrain2& grain, u8* data) {}
void SFXGrain_AddRegister::execute(blocksound_handler2& handler) {}

SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain& grain) {}
SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices(SFXGrain2& grain, u8* data) {}
void SFXGrain_KeyOffVoices::execute(blocksound_handler2& handler) {}

SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain& grain) {}
SFXGrain_KillVoices::SFXGrain_KillVoices(SFXGrain2& grain, u8* data) {}
void SFXGrain_KillVoices::execute(blocksound_handler2& handler) {}

SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain& grain) {}
SFXGrain_OnStopMarker::SFXGrain_OnStopMarker(SFXGrain2& grain, u8* data) {}
void SFXGrain_OnStopMarker::execute(blocksound_handler2& handler) {}

SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain& grain) {}
SFXGrain_CopyRegister::SFXGrain_CopyRegister(SFXGrain2& grain, u8* data) {}
void SFXGrain_CopyRegister::execute(blocksound_handler2& handler) {}

}  // namespace snd
