
// hoping we don't have to actually implement this, just notes here for now
// in jak 1, we replaced iso_cd with fakeiso, which simplifies a lot of the reading stuff.
// but we need to understand how it works in order to replace it.

// IsoCdPagesCallback
// - some crazy callback for the page/sector/buffer stuff

// ReadDirectory: iterate through ISO filesystem. Seems to have the same functionality as jak1,
// but the actual implementation of reading from DVD is different (using SubBufferToRead)

// DecodeDUP: similar to jak 1, using SubBufferToRead stuff.

// LoadMusicTweaks: similar to jak 1.

// LoadDiscID: similar to jak 1.

// FS_Init: pretty complicated, lots of new semaphore and blzo stuff.
// there's now a CDReturnThread.
// calls DecodeDUP, LoadMusicTweaks, LoadDiscID like Jak 1 does, and does a bunch of other setup.
// hopefully can ignore this and use current fakeiso stuff.

// FS_Open takes a flag:
// flag of 0 or 1 will read the first sector on the call to FS_Open, to determine the size of a blzo
// file and set up some streams. A flag of 2 will skip this and assume no blzo.
// There's some new info in the FileRecord.

// DecompressBlock : calls the lzo decompression function, deals with buffer stuff

// FS_PageBeginRead: more crazy page stuff - seems like this kicks of the read similar to jak 1's
// FS_BeginRead.

// handled already by ziemas
// FS_LoadSoundBank
// FS_LoadMusic

// CD_WaitReturn: has vag unpause. But I think this is fine - this waits for the CD to be put in the
// playstation if you take it out

// FS_Find FS_FindIN FS_GetLength are the same

// OpenWad looks the same.
// - it sets size to 0 always.
// - unk and blzo set to 0 too

// FS_Close has the same functionality

// FS_SyncRead doesn't seem to do anything? It behaves like our fakeiso version. On jak 1, it actually did something.

// stub functions:
// - FS_StoreSoundBankInIOP
// - FS_LoadSoundBankFromIOP
// - FS_LoadSoundBankFromEE

// FS_PollDrive has the same functionality

// CdReturn is very similar, just calls FS_PollDrive in a loop.

// DoCdReadPages, CheckPagesReady


/////// RAMDISK IS GONE