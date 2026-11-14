## VAG Tools
Extract and repack the streamed audio (dialogue, cutscene voice) from `VAGDIR.AYB` and `VAGWAD.*`. Works for Jak 1, 2 and 3 (Jak X uses the Jak 3 format, but is untested).

### Unpacking
```tools/vag_unpacker <game> <output folder> <VAGDIR.AYB> <VAGWAD files...> [--wav]```

`game` is `jak1`, `jak2`, `jak3` or `jakx`. It writes one `.vag` per entry (an exact copy) into a folder per WAD, plus a `vag_description.json`. `--wav` also writes a `.wav` per entry so you can listen to them.

### Repacking
```tools/vag_packer <folder> <vag_description.json>```

Writes `mod_VAGDIR.AYB` and `mod_VAGWAD.*` in the folder. Unchanged, they match the originals. Copy them over the files in `out/<game>/iso/`. The game build copies them from `iso_data/<game>/VAG/`, so put them there if you want them to survive a build.

To replace a line, point its `files` entry in the description to a `.vag` or a 16-bit PCM `.wav`. If the new audio is longer, the entries after it move. All languages share one directory, so repack every WAD you use. Names are at most 8 characters (Jak 3: `A-Z 0-9 - space`).
