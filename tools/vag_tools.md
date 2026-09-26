## VAG Tools
The VAG packer and unpacker can be used to extract and repack the streamed audio (mostly dialogue and cutscene audio) from `VAGDIR.AYB` and the `VAGWAD.xxx` files. They work for Jak 1, 2, 3 and X. To extract audio for listening, `rip_streamed_audio` in the decompiler is enough - these tools are for changing the audio.

### Unpacking
```tools/vag_unpacker <game> <path to output> <path to VAGDIR.AYB> <path to VAGWAD files...> [--wav]```

`game` is one of `jak1`, `jak2`, `jak3`, `jakx`. For example:
```tools/vag_unpacker jak2 vag_out iso_data/jak2/VAG/VAGDIR.AYB iso_data/jak2/VAG/VAGWAD.ENG iso_data/jak2/VAG/VAGWAD.FRE```

It creates a folder per WAD (named after the extension of the WAD) with one `.vag` per entry, and a `vag_description.json`. The `.vag` files are exact copies of the data in the WAD. With `--wav` a `.wav` for each entry is written too so you can listen to them, but these are large. You only need to unpack the WADs you want to change or ship, for example only the English one, but see the note about languages below.

### Repacking
```tools/vag_packer <path to folder with the audio files> <path to vag_description.json>```

It writes `mod_VAGDIR.AYB` and a `mod_VAGWAD.xxx` for each WAD in the description file, in the given folder. Unmodified, the output is byte for byte identical to the original files. To use them, put them in `iso_data/<game>/VAG/` (removing `mod_`) and run the extractor/decompiler as usual, or in `out/<game>/iso/` for the game itself.

### Replacing audio
Edit the `files` of an entry in `vag_description.json` to point to your own file. It can be:
- a `.vag`, used as is
- a `.wav` (16-bit PCM, mono or stereo), which is encoded by the packer. The sample rate of the wave file is kept, the game plays streams at any rate. Convert other formats first, for example `ffmpeg -i in.mp3 -c:a pcm_s16le out.wav`. If the channels don't match the entry (a mono wav for a stereo entry or the other way around), they are converted.

If the new audio is longer than the old audio, the entries after it are moved back, so keep the whole set of files consistent (see below).

You can add entries by adding one to the `entries` list without a `page`. `name` can be at most 8 characters (for Jak 3 only `A-Z`, `0-9`, `-` and space). The `stereo` flag says if the entry is stereo, `international` and `param` are for Jak 3 only (use 0 / false when unsure). The game can hold a limited number of entries (868 in Jak 1, 2728 in Jak 2, 4096 in Jak 3).

### Languages
All language WADs share one `VAGDIR.AYB`, so every entry has the same location in every WAD. The packer knows this and moves entries in all of the WADs in the description file together. If you change the length of an entry, include all the WADs you want to play the game with in the description file, and use `vag_unpacker` on all of them, otherwise the ones you didn't repack will no longer match the directory. In Jak 3 the `INT` WAD (international audio) has its own separate layout.

### Format notes
- A VAG file is a 48 byte header (`VAGp` with big endian fields in Jak 1, `pGAV` with little endian in Jak 2 and 3), followed by PS-ADPCM. Stereo files store 0x2000 byte chunks of the left and right channel alternating, each channel starting with its own header.
- Each entry starts on a "page" (2048 bytes, or 0x8000 in Jak 3).
- The shared code lives in `common/audio/vag.h`.
