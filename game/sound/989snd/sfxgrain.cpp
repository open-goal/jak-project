#include "sfxgrain.h"

#include "blocksound_handler2.h"

namespace snd {

SFXGrain_Tone::SFXGrain_Tone(SFXGrain& grain) : m_tone(grain.GrainParams.tone) {}
SFXGrain_Tone::SFXGrain_Tone(SFXGrain2& grain, u8* data) {}

void SFXGrain_Tone::execute(blocksound_handler2& handler) {}

SFXGrain_XrefID::SFXGrain_XrefID() {}
void SFXGrain_XrefID::execute(blocksound_handler2& handler) {}

SFXGrain_XrefNum::SFXGrain_XrefNum() {}
void SFXGrain_XrefNum::execute(blocksound_handler2& handler) {}

SFXGrain_LfoSettings::SFXGrain_LfoSettings() {}
void SFXGrain_LfoSettings::execute(blocksound_handler2& handler) {}

SFXGrain_StartChildSound::SFXGrain_StartChildSound() {}
void SFXGrain_StartChildSound::execute(blocksound_handler2& handler) {}

SFXGrain_StopChildSound::SFXGrain_StopChildSound() {}
void SFXGrain_StopChildSound::execute(blocksound_handler2& handler) {}

SFXGrain_PluginMessage::SFXGrain_PluginMessage() {}
void SFXGrain_PluginMessage::execute(blocksound_handler2& handler) {}

SFXGrain_Branch::SFXGrain_Branch() {}
void SFXGrain_Branch::execute(blocksound_handler2& handler) {}

SFXGrain_ControlNull::SFXGrain_ControlNull() {}
void SFXGrain_ControlNull::execute(blocksound_handler2& handler) {}

SFXGrain_LoopStart::SFXGrain_LoopStart() {}
void SFXGrain_LoopStart::execute(blocksound_handler2& handler) {}

SFXGrain_LoopEnd::SFXGrain_LoopEnd() {}
void SFXGrain_LoopEnd::execute(blocksound_handler2& handler) {}

SFXGrain_LoopContinue::SFXGrain_LoopContinue() {}
void SFXGrain_LoopContinue::execute(blocksound_handler2& handler) {}

SFXGrain_Stop::SFXGrain_Stop() {}
void SFXGrain_Stop::execute(blocksound_handler2& handler) {}

SFXGrain_RandPlay::SFXGrain_RandPlay() {}
void SFXGrain_RandPlay::execute(blocksound_handler2& handler) {}

SFXGrain_RandDelay::SFXGrain_RandDelay() {}
void SFXGrain_RandDelay::execute(blocksound_handler2& handler) {}

SFXGrain_RandPB::SFXGrain_RandPB() {}
void SFXGrain_RandPB::execute(blocksound_handler2& handler) {}

SFXGrain_PB::SFXGrain_PB() {}
void SFXGrain_PB::execute(blocksound_handler2& handler) {}

SFXGrain_AddPB::SFXGrain_AddPB() {}
void SFXGrain_AddPB::execute(blocksound_handler2& handler) {}

SFXGrain_SetRegister::SFXGrain_SetRegister() {}
void SFXGrain_SetRegister::execute(blocksound_handler2& handler) {}

SFXGrain_SetRegisterRand::SFXGrain_SetRegisterRand() {}
void SFXGrain_SetRegisterRand::execute(blocksound_handler2& handler) {}

SFXGrain_IncRegister::SFXGrain_IncRegister() {}
void SFXGrain_IncRegister::execute(blocksound_handler2& handler) {}

SFXGrain_DecRegister::SFXGrain_DecRegister() {}
void SFXGrain_DecRegister::execute(blocksound_handler2& handler) {}

SFXGrain_TestRegister::SFXGrain_TestRegister() {}
void SFXGrain_TestRegister::execute(blocksound_handler2& handler) {}
SFXGrain_Marker::SFXGrain_Marker() {}
void SFXGrain_Marker::execute(blocksound_handler2& handler) {}
SFXGrain_GotoMarker::SFXGrain_GotoMarker() {}
void SFXGrain_GotoMarker::execute(blocksound_handler2& handler) {}
SFXGrain_GotoRandomMarker::SFXGrain_GotoRandomMarker() {}
void SFXGrain_GotoRandomMarker::execute(blocksound_handler2& handler) {}
SFXGrain_WaitForAllVoices::SFXGrain_WaitForAllVoices() {}
void SFXGrain_WaitForAllVoices::execute(blocksound_handler2& handler) {}
SFXGrain_PlayCycle::SFXGrain_PlayCycle() {}
void SFXGrain_PlayCycle::execute(blocksound_handler2& handler) {}
SFXGrain_AddRegister::SFXGrain_AddRegister() {}
void SFXGrain_AddRegister::execute(blocksound_handler2& handler) {}
SFXGrain_KeyOffVoices::SFXGrain_KeyOffVoices() {}
void SFXGrain_KeyOffVoices::execute(blocksound_handler2& handler) {}
SFXGrain_KillVoices::SFXGrain_KillVoices() {}
void SFXGrain_KillVoices::execute(blocksound_handler2& handler) {}
SFXGrain_OnStopMarker::SFXGrain_OnStopMarker() {}
void SFXGrain_OnStopMarker::execute(blocksound_handler2& handler) {}
SFXGrain_CopyRegister::SFXGrain_CopyRegister() {}
void SFXGrain_CopyRegister::execute(blocksound_handler2& handler) {}

}  // namespace snd
