## DGO Tools
The DGO packer and unpacker can be used to extract and repack files in DGOs.

### Unpacking
Create a folder for the output, then run:
```tools/dgo_unpacker <path to output> <path to DGO files...>```
It will then place the files in the output folder. You may specify multiple DGO files, but this is not recommended because sometimes different DGO files have object files with the same name but different data.  It will also create a DGO description file with the same name as the DGO file but with a `.txt` extension.

### Repacking
```tools/dgo_packer <path to folder with object files> <path to DGO description file>```
It will repack the DGO. The name will be the same as the original DGO, but with `mod_` in the front.