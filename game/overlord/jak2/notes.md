### overlord.c
start
  done, removed ramdisk stuff.

ExitIOP
  done (just loops forever)


### iso_cd.c
was ported. calling cd callbacks is a little sus.

## ramdisk.c
ramdisk is believed unused, we will try without it for now

InitRamdisk
Thread_Server
RPC_Ramdisk
gcc2_compiled.
__gnu_compiled_c
gNumFiles
gMemUsed
gMemSize
gRPCBuf
gMem
gReturnBuffer


### fakeiso.c
We make this up. Snd/Music loading is a tiny bit different but Ziemas has patched this.
Should move these to common once we figure out how to deal with FileRecord changes.

### deviso.c
Empty file

### isocommon.c
All can be common. Additionally, some of these are implemented in FileUtils already, and are used in the decompiler.

ISONameFromAnimationName
  technically is game specific, but we've already patched it
MakeISOName
UnmakeISOName

### isodesc.c
Empty file!

### dma.c
Changed a bit for jak 2. there's now DmaVagCmd, which is used for dmaing stuff to the SPU. TODO
The EE stuff has also changed and has a semaphore. However, we will ignore this and use the same instant dma as jak 1.
Downstream stuff that looks at the semaphore will need to be patched.

SpuDmaIntr
 TODO

DMA_SendToEE
  moved to common, instant dma, removed semaphore stuff

DMA_SendToSPUAndSync
  TODO. different
DmaCancelThisVagCmd
  TODO. different
EeDmaIntr
  Not ported yet, just calls iSignalSema on the ee dma semaphore. Don't think we need this.

### iso.c
Note that jak 2 drops the chained buffer system from jak 1 and uses the "pages" system instead.

InitISOFS
IsoQueueVagStream
IsoPlayVagStream
ISOThread
RunDGOStateMachine
LoadDGO
CopyData
FindISOFile
FindVAGFile
GetISOFileLength
NullCallback
IsoStopVagStream
CopyDataToIOP
CopyDataToEE
DGOThread
RPC_DGO
LoadNextDGO
CancelDGO
InitDriver
SetVagClock
gcc2_compiled.
__gnu_compiled_c
iso_thread
dgo_mbx
dgo_thread
str_thread
play_thread
sync_mbx
iso_init_flag
_not_on_stack_sync
sLoadDgo
sRPCBuf

### iso_queue.c
InitBuffers
AllocDataBuffer
AllocateBuffer
FreeBuffer
ReleaseMessage
AllocIsoPages
FreeIsoPages
QueueMessage
UnqueueMessage
GetMessage
ProcessMessageData
ReturnMessage
GetVAGCommand
FreeVAGCommand
CheckForIsoPageBoundaryCrossing
FreeDataBuffer
gcc2_compiled.
__gnu_compiled_c
AllocdBuffersCount
NextBuffer
AllocdStrBuffersCount
NextStrBuffer
VAG_SilentLoop
sFreeBuffer
sFreeStrBuffer
sSema
vag_cmd_used
vag_cmd_cnt
max_vag_cmd_cnt
vag_cmds

### stream.c
RPC_STR
RPC_PLAY
STRThread
PLAYThread
gcc2_compiled.
__gnu_compiled_c
sCache

### srpc.c
RPC_Player
RPC_Loader
VBlank_Handler
Thread_Player
Thread_Loader
SetVagStreamName
SetVagName
gcc2_compiled.
__gnu_compiled_c
gInfoEE
gMusic
languages.8
dmaid
info
gPlayerBuf
gLoaderBuf

### vag.c
InitVagCmds
SmartAllocVagCmd
TerminateVAG
PauseVAG
UnPauseVAG
RestartVag
SetVAGVol
SetVagStreamsNoStart
InitVAGCmd
SetVagStreamsNotScanned
RemoveVagCmd
FindFreeVagCmd
FindNotQueuedVagCmd
FindWhosPlaying
FindVagStreamId
FindVagStreamPluginId
FindVagStreamName
FindThisVagStream
AnyVagRunning
FreeVagCmd
SetNewVagCmdPri
HowManyBelowThisPriority
StopVAG
VAG_MarkLoopEnd
VAG_MarkLoopStart
CalculateVAGPitch
PauseVagStreams
UnPauseVagStreams
SetAllVagsVol
CalculateVAGVolumes
gcc2_compiled.
__gnu_compiled_c
sbank.c
InitBanks
AllocateBankName
LookupBank
gcc2_compiled.
__gnu_compiled_c
gBanks
gCommonBank
gGunBank
gBoardBank
gLevelBanks

### ssound.c
InitSound
AllocateSound
CalculateFalloffVolume
CalculateAngle
SetEarTrans
SndMemAlloc
LookupSound
CleanSounds
UpdateVolume
GetVolume
GetPan
KillSoundsInGroup
SetCurve
SetMusicVol
SetBufferMem
ReleaseBufferMem
SndMemFree
gcc2_compiled.
__gnu_compiled_c
sqrt_table
atan_table
gSounds
gEarTrans
gCamTrans
gCamAngle
last_tick.26
common_bank
common_bank_mem
other_bank
other_bank_mem
mmd_bank
mmd_bank_mem
buffer_mem

### /usr/home/agavin/src/jak2/libs/common/soundcommon.c
ReadBankSoundNames
strcpy_toupper
gcc2_compiled.
__gnu_compiled_c

### iso_api.c
EEVagAndVagwad
QueueVAGStream
LoadISOFileToIOP
LoadISOFileToEE
LoadISOFileChunkToEE
PauseVAGStreams
UnpauseVAGStreams
SetVAGStreamPitch
SetDialogVolume
LoadSoundBank
LoadMusic
UnLoadMusic
PluginVagAndVagWad
gcc2_compiled.
__gnu_compiled_c

### /usr/home/agavin/src/jak2/libs/common/minilzo.c
lzo1x_decompress
__lzo_init2
gcc2_compiled.
__gnu_compiled_c
__lzo_init_done
plugin.c
Init989Plugins
QueueVagStream989
NullPlugin989
PlayQueuedVagStream989
StopVagStream989
SetVagStreamVolume989
StopEmAll989
SetStreamLfo989
gcc2_compiled.
__gnu_compiled_c
PluginId

### streamlfo.c
SineLfo
InitSineLfo
InitRandLfo
InitStreamLfoHandler
RandomLfo
RemoveLfoStreamFromList
CheckLfoList
UpdateLfoVars
RandomLfoSetPitchVars
RandomLfoWaitForPitch
SineLfoSetPitchVars
SineLfoWaitForPitch
StreamLfo
InitStreamLfoList
AddToCircularLfoStreamList
FindLfoStreamInList
gcc2_compiled.
__gnu_compiled_c
sine
_seed

### streamlist.c
InsertVagStreamInList
QueueNewStreamsFromList
CheckPlayList
StreamListThread
InitVagStreamList
FindVagStreamInList
GetVagStreamInList
RemoveVagStreamFromList
EmptyVagStreamList
MergeVagStreamLists
gcc2_compiled.
__gnu_compiled_c

### spustreams.c
ProcessVAGData
GetVAGStreamPos
CheckVAGStreamProgress
CheckVagStreamsProgress
StopVagStream
UpdateIsoBuffer
InitSpuStreamsThread
WakeSpuStreamsUp
GetSpuRamAddress
bswap
ProcessStreamData
gcc2_compiled.
__gnu_compiled_c
StreamsThread

### pages.c
InitPagedMemory
AllocPagesBytes
AllocPages
FreePagesList
StepTopPage
FromPagesCopy
gcc2_compiled.
__gnu_compiled_c

### list.c
InitList
AddToCircularList
MakeCircularList
BreakCircularList
gcc2_compiled.